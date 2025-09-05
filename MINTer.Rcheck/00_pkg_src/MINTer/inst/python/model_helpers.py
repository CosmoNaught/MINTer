import torch
import torch.nn as nn
from torch.amp import autocast
import numpy as np
import pandas as pd

class GRUModel(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, dropout_prob, num_layers=1, predictor='prevalence'):
        super(GRUModel, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.predictor = predictor
        self.gru = nn.GRU(
            input_size, hidden_size,
            num_layers=num_layers,
            dropout=dropout_prob if num_layers > 1 else 0.0
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
            dropout=dropout_prob if num_layers > 1 else 0.0
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
        x_torch = torch.tensor(full_ts, dtype=torch.float32).unsqueeze(1).to(device)
        
        if device.type == 'cuda':
            with autocast(device_type='cuda'):
                pred = model(x_torch).squeeze(-1).squeeze(-1).cpu().numpy()
        else:
            # No autocast for CPU
            pred = model(x_torch).squeeze(-1).squeeze(-1).cpu().numpy()
    return pred

def load_model_from_checkpoint(model_path, input_size, hidden_size, output_size=1, dropout_prob=0.1, num_layers=1, model_type='gru', predictor='prevalence'):
    checkpoint = torch.load(model_path, map_location=torch.device('cpu'))
    state_dict = checkpoint['model_state_dict']
    
    # Infer actual architecture
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
    
    # Use actual values from checkpoint
    hidden_size = actual_hidden_size
    num_layers = actual_num_layers
    
    if model_type.lower() == 'gru':
        model = GRUModel(input_size, hidden_size, output_size, dropout_prob, num_layers, predictor)
    else:
        model = LSTMModel(input_size, hidden_size, output_size, dropout_prob, num_layers, predictor)
    
    model.load_state_dict(state_dict)
    return model, hidden_size, num_layers
