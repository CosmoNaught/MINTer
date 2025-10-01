import torch
import torch.nn as nn
from torch.amp import autocast
import numpy as np
import pandas as pd
import time
from typing import Dict, Optional, Tuple

# ---------------------------
# Target transforms
# ---------------------------

def _clip01(x: np.ndarray, eps: float) -> np.ndarray:
    return np.clip(x, eps, 1.0 - eps)

def transform_targets_np(y: np.ndarray, predictor: str, eps: float) -> np.ndarray:
    if predictor == "prevalence":
        y = _clip01(y, eps)
        return np.log(y / (1.0 - y))  # logit
    else:
        return np.log1p(np.maximum(y, 0.0))

def inverse_transform_np(y: np.ndarray, predictor: str) -> np.ndarray:
    if predictor == "prevalence":
        return 1.0 / (1.0 + np.exp(-y))  # sigmoid
    else:
        return np.expm1(y)

# ---------------------------
# Schema-aware LSTM Model
# ---------------------------

class SchemaAwareLSTM(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, dropout_prob, 
                 num_layers=1, predictor='prevalence',
                 use_film=False, has_jump=False, events_n=0):
        super(SchemaAwareLSTM, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.predictor = predictor
        self.use_film = use_film
        self.has_jump = has_jump
        self.events_n = events_n
        
        self.lstm = nn.LSTM(
            input_size, hidden_size,
            num_layers=num_layers,
            dropout=dropout_prob if num_layers > 1 else 0.0,
            batch_first=False  # CRITICAL: sequence-first [T, B, F]
        )
        self.fc = nn.Linear(hidden_size, output_size)
        self.ln = nn.LayerNorm(hidden_size)
        self.dropout = nn.Dropout(dropout_prob)
        self.activation = nn.Identity()  # No activation in forward pass
        
        self.film = None
        if self.use_film:
            self.film = nn.Sequential(
                nn.Linear(input_size, 128), 
                nn.ReLU(),
                nn.Linear(128, hidden_size * 2)
            )
        
        self.jump_head = None
        if self.has_jump and events_n == 9:
            self.jump_head = nn.Linear(9, 1, bias=False)
    
    def _film_gb(self, x):
        """Compute FiLM gamma and beta from context."""
        ctx = x.mean(dim=0)  # [B, F]
        gamma, beta = torch.chunk(self.film(ctx), 2, dim=-1)
        return gamma.unsqueeze(0), beta.unsqueeze(0)
    
    def forward(self, x):
        """
        x: [T, B, F] - sequence first!
        returns: [T, B, 1]
        """
        out, _ = self.lstm(x)
        
        if self.use_film and self.film is not None:
            gamma, beta = self._film_gb(x)
            out = out * (1 + gamma) + beta
        
        out = self.ln(out)
        out = self.dropout(out)
        base = self.fc(out)
        
        if self.has_jump and self.jump_head is not None and self.events_n == 9:
            pulses_block = x[..., -9:]  # Last 9 features are event pulses
            base = base + self.jump_head(pulses_block)
        
        return self.activation(base)

def predict_full_sequence(model, full_ts, device, predictor, use_amp=False):
    """
    Predict on a single sequence.
    full_ts: [T, F] numpy array
    returns: [T] numpy array (inverse transformed)
    """
    model.eval()
    with torch.no_grad():
        x_torch = torch.tensor(full_ts, dtype=torch.float32).unsqueeze(1).to(device)
        if device.type == 'cuda' and use_amp:
            from torch.amp import autocast
            with autocast(device_type='cuda', dtype=torch.float16):
                pred = model(x_torch).squeeze(-1).squeeze(-1).cpu().numpy()
        else:
            pred = model(x_torch).squeeze(-1).squeeze(-1).cpu().numpy()
    return inverse_transform_np(pred, predictor)

def batch_predict_scenarios(model, scenarios_array, device, predictor, batch_size=32, use_amp=False):
    """
    Batch prediction for multiple scenarios.
    scenarios_array: [N, T, F] numpy array
    returns: [N, T] numpy array (inverse transformed)
    """
    model.eval()
    n_scenarios = scenarios_array.shape[0]
    all_predictions = []

    with torch.no_grad():
        for i in range(0, n_scenarios, batch_size):
            batch_end = min(i + batch_size, n_scenarios)
            batch_data = scenarios_array[i:batch_end]      # [B, T, F]
            batch_data = np.transpose(batch_data, (1, 0, 2))  # [T, B, F]
            x_batch = torch.tensor(batch_data, dtype=torch.float32).to(device)

            if device.type == 'cuda' and use_amp:
                from torch.amp import autocast
                with autocast(device_type='cuda', dtype=torch.float16):
                    batch_pred = model(x_batch)
            else:
                batch_pred = model(x_batch)

            batch_pred = batch_pred.squeeze(-1).permute(1, 0).cpu().numpy()  # [B, T]
            all_predictions.append(batch_pred)

    predictions = np.concatenate(all_predictions, axis=0)
    return inverse_transform_np(predictions, predictor)


# ---------------------------
# Schema inference from checkpoint
# ---------------------------

def safe_load_state(path: str, device: torch.device) -> Dict:
    """Load checkpoint safely handling different formats."""
    try:
        ckpt = torch.load(path, map_location=device, weights_only=True)
    except TypeError:
        ckpt = torch.load(path, map_location=device)
    
    if isinstance(ckpt, dict) and 'model_state_dict' in ckpt:
        state = ckpt['model_state_dict']
    else:
        state = ckpt
    
    if not isinstance(state, dict):
        raise RuntimeError("Unsupported checkpoint format.")
    
    return state

def infer_schema_from_state(state: Dict, static_n: int, use_cyclical_time: bool) -> Dict:
    """
    Infer the feature schema from checkpoint state dict.
    
    Returns dict with:
        - expected_in: total input features
        - cyc: use cyclical time encoding
        - add_year_idx: include year index
        - include_lag: include lagged target
        - events_n: number of event features (0 or 9)
        - has_jump: has jump connection
        - use_film: has FiLM layers
        - extra2: has post-intervention indicators (0 or 2)
    """
    # Infer input size from weight matrix
    if 'lstm.weight_ih_l0' in state:
        expected_in = int(state['lstm.weight_ih_l0'].shape[1])
    else:
        keys = [k for k in state if k.endswith('weight_ih_l0')]
        if not keys:
            raise RuntimeError("Cannot infer input size: no *weight_ih_l0 in checkpoint.")
        expected_in = int(state[keys[0]].shape[1])
    
    has_jump = any(k.startswith('jump_head') for k in state.keys())
    use_film = any(k.startswith('film.') for k in state.keys())
    
    # Try different feature combinations
    candidates = []
    for cyc in (True, False):
        time_dim = 2 if cyc else 1
        for add_year_idx in (1, 0):
            for include_lag in (1, 0):
                for events_n in (9, 0):
                    for extra2 in (2, 0):
                        total = time_dim + add_year_idx + static_n + include_lag + events_n + extra2
                        if total == expected_in:
                            score = 0
                            if cyc == use_cyclical_time: 
                                score += 4
                            if events_n == 9 and has_jump: 
                                score += 4
                            if extra2 == 2: 
                                score += 5
                            if include_lag == 1: 
                                score -= 1
                            
                            candidates.append((score, dict(
                                cyc=cyc,
                                add_year_idx=bool(add_year_idx),
                                include_lag=bool(include_lag),
                                events_n=events_n,
                                has_jump=has_jump,
                                use_film=use_film,
                                expected_in=expected_in,
                                extra2=extra2
                            )))
    
    if not candidates:
        raise RuntimeError(f"Could not map checkpoint input size {expected_in} to any feature combination.")
    
    candidates.sort(key=lambda x: x[0], reverse=True)
    return candidates[0][1]

def load_model_from_checkpoint(model_path, static_n, predictor, device, 
                              use_cyclical_time=True):
    """
    Load SchemaAwareLSTM from checkpoint with automatic schema inference.
    
    Returns: (model, schema_dict)
    """
    state = safe_load_state(model_path, device)
    schema = infer_schema_from_state(state, static_n, use_cyclical_time)
    
    # Infer architecture from state dict
    if 'lstm.weight_ih_l0' in state:
        hidden = state['lstm.weight_ih_l0'].shape[0] // 4
        layers = sum(1 for k in state if k.startswith('lstm.weight_ih_l'))
    else:
        wih0 = [k for k in state if k.endswith('weight_ih_l0')]
        hidden = state[wih0[0]].shape[0] // 4
        layers = len({k.split('.')[1] for k in state if k.startswith('lstm.weight_ih_l')})
    
    model = SchemaAwareLSTM(
        schema['expected_in'], hidden, 1, 
        dropout_prob=0.0,  # No dropout at inference
        num_layers=layers, 
        predictor=predictor,
        use_film=schema['use_film'],
        has_jump=schema['has_jump'],
        events_n=schema['events_n']
    )
    
    model.load_state_dict(state, strict=True)
    model.to(device).eval()
    
    print(f"[INFO] Loaded LSTM: {layers} layers, hidden {hidden}")
    print(f"[INFO] Schema: {schema}")
    
    return model, schema

# Benchmark utilities (unchanged)
class Timer:
    def __init__(self):
        self.times = {}
        
    def start(self, name):
        self.times[f"{name}_start"] = time.perf_counter()
        
    def end(self, name):
        if f"{name}_start" in self.times:
            elapsed = time.perf_counter() - self.times[f"{name}_start"]
            self.times[name] = elapsed
            del self.times[f"{name}_start"]
            return elapsed
        return 0.0
    
    def get_times(self):
        return {k: v for k, v in self.times.items() if not k.endswith("_start")}

benchmark_timer = Timer()

def benchmark_prediction(model, data, device, predictor, name="prediction"):
    benchmark_timer.start(name)
    result = predict_full_sequence(model, data, device, predictor)
    elapsed = benchmark_timer.end(name)
    return result, elapsed

def benchmark_batch_prediction(model, data_batch, device, predictor, name="batch_prediction"):
    benchmark_timer.start(name)
    result = batch_predict_scenarios(model, data_batch, device, predictor)
    elapsed = benchmark_timer.end(name)
    return result, elapsed