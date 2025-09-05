# model_helpers_optimized.py

import torch
import torch.nn as nn
from torch.amp import autocast
import numpy as np
import pandas as pd
import time

class GRUModel(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, dropout_prob, num_layers=1, predictor='prevalence'):
        super(GRUModel, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.predictor = predictor
        
        self.gru = nn.GRU(
            input_size, hidden_size,
            num_layers=num_layers,
            dropout=dropout_prob if num_layers > 1 else 0.0,
            batch_first=True  # More efficient for batching
        )
        self.fc = nn.Linear(hidden_size, output_size)
        self.ln = nn.LayerNorm(hidden_size)
        self.dropout = nn.Dropout(dropout_prob)
        
        if predictor == 'prevalence':
            self.activation = nn.Sigmoid()
        else:
            self.activation = nn.Softplus()
    
    def forward(self, x):
        out, _ = self.gru(x)
        out = self.ln(out)
        out = self.dropout(out)
        out = self.fc(out)
        out = self.activation(out)
        return out

class LSTMModel(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, dropout_prob, num_layers=1, predictor='prevalence'):
        super(LSTMModel, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.predictor = predictor
        
        self.lstm = nn.LSTM(
            input_size, hidden_size,
            num_layers=num_layers,
            dropout=dropout_prob if num_layers > 1 else 0.0,
            batch_first=True  # More efficient for batching
        )
        self.fc = nn.Linear(hidden_size, output_size)
        self.ln = nn.LayerNorm(hidden_size)
        self.dropout = nn.Dropout(dropout_prob)
        
        if predictor == 'prevalence':
            self.activation = nn.Sigmoid()
        else:
            self.activation = nn.Softplus()
    
    def forward(self, x):
        out, _ = self.lstm(x)
        out = self.ln(out)
        out = self.dropout(out)
        out = self.fc(out)
        out = self.activation(out)
        return out

def predict_full_sequence(model, full_ts, device):
    model.eval()
    with torch.no_grad():
        # Note: changed to batch_first format
        x_torch = torch.tensor(full_ts, dtype=torch.float32).unsqueeze(0).to(device)
        
        if device.type == 'cuda':
            with autocast(device_type='cuda', dtype=torch.float16):
                pred = model(x_torch).squeeze(0).squeeze(-1).cpu().numpy()
        else:
            pred = model(x_torch).squeeze(0).squeeze(-1).cpu().numpy()
    return pred

def batch_predict_scenarios(model, scenarios_array, device, batch_size=32):
    model.eval()
    n_scenarios = scenarios_array.shape[0]
    all_predictions = []
    
    # Process in batches for memory efficiency
    with torch.no_grad():
        for i in range(0, n_scenarios, batch_size):
            batch_end = min(i + batch_size, n_scenarios)
            batch_data = scenarios_array[i:batch_end]
            
            # Convert to tensor
            x_batch = torch.tensor(batch_data, dtype=torch.float32).to(device)
            
            # Use mixed precision on GPU for faster inference
            if device.type == 'cuda':
                with autocast(device_type='cuda', dtype=torch.float16):
                    batch_pred = model(x_batch)
            else:
                batch_pred = model(x_batch)
            
            # Move to CPU and convert to numpy
            batch_pred = batch_pred.squeeze(-1).cpu().numpy()
            all_predictions.append(batch_pred)
    
    # Concatenate all predictions
    predictions = np.concatenate(all_predictions, axis=0)
    return predictions

def load_model_from_checkpoint(model_path, input_size, hidden_size, output_size=1, 
                              dropout_prob=0.1, num_layers=1, model_type='gru', 
                              predictor='prevalence'):
    checkpoint = torch.load(model_path, map_location=torch.device('cpu'))
    state_dict = checkpoint['model_state_dict']
    
    # Infer actual architecture from state dict
    actual_num_layers = 1
    for i in range(10):
        if f'{model_type.lower()}.weight_ih_l{i}' in state_dict:
            actual_num_layers = i + 1
        else:
            break
    
    if model_type.lower() == 'gru':
        weight_shape = state_dict[f'{model_type.lower()}.weight_ih_l0'].shape
        actual_hidden_size = weight_shape[0] // 3
    else:
        weight_shape = state_dict[f'{model_type.lower()}.weight_ih_l0'].shape
        actual_hidden_size = weight_shape[0] // 4
    
    # Use actual values
    hidden_size = actual_hidden_size
    num_layers = actual_num_layers
    
    # Create model with batch_first=True
    if model_type.lower() == 'gru':
        model = GRUModel(input_size, hidden_size, output_size, dropout_prob, num_layers, predictor)
    else:
        model = LSTMModel(input_size, hidden_size, output_size, dropout_prob, num_layers, predictor)
    try:
        model.load_state_dict(state_dict)
    except:
        print(f"[WARNING] Adapting {model_type} weights for batch_first format")
        model.load_state_dict(state_dict, strict=False)
    
    return model, hidden_size, num_layers

# Benchmark utilities
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

# Global timer instance
benchmark_timer = Timer()

def benchmark_prediction(model, data, device, name="prediction"):

    benchmark_timer.start(name)
    result = predict_full_sequence(model, data, device)
    elapsed = benchmark_timer.end(name)
    return result, elapsed

def benchmark_batch_prediction(model, data_batch, device, name="batch_prediction"):

    benchmark_timer.start(name)
    result = batch_predict_scenarios(model, data_batch, device)
    elapsed = benchmark_timer.end(name)
    return result, elapsed