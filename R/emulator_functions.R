#' Load Emulator Models
#'
#' @param models_base_dir Base directory containing model files
#' @param predictor "prevalence" or "cases"
#' @param device "cpu" or "cuda" (NULL for auto-detect)
#'
#' @return List containing models and configuration
#' @export
load_emulator_models <- function(models_base_dir = ".", predictor = "prevalence", 
                                device = NULL) {
  
  # Make sure torch is imported and use R's handle
  torch <- reticulate::import("torch", delay_load = FALSE)
  pd <- reticulate::import("pandas", delay_load = FALSE)
  
  # Source Python helper functions if not already loaded
  if (!reticulate::py_has_attr(reticulate::py, "load_model_from_checkpoint")) {
    python_script <- system.file("python", "model_helpers.py", package = "MINTer")
    if (!file.exists(python_script)) {
      stop("Could not find model_helpers.py. Please ensure MINTer is properly installed.")
    }
    reticulate::source_python(python_script)
  }
  
  # Set device
  if (is.null(device)) {
    device <- if (torch$cuda$is_available()) "cuda" else "cpu"
  }
  message(sprintf("[INFO] Using device: %s", device))
  device_obj <- torch$device(device)  # uses R handle, no py_eval()
  
  predictor_models_dir <- file.path(models_base_dir, predictor)
  
  # Load training args
  args_path <- file.path(predictor_models_dir, "args.json")
  
  if (!file.exists(args_path)) {
    stop(sprintf("Could not find args.json in %s\nMake sure you have trained models for '%s' predictor.\nExpected files: gru_best.pt, lstm_best.pt, static_scaler.pkl, args.json", 
                predictor_models_dir, predictor))
  }
  
  training_args <- jsonlite::fromJSON(args_path)
  message(sprintf("[INFO] Loaded training parameters from %s", args_path))
  
  # Load scaler
  scaler_path <- file.path(predictor_models_dir, "static_scaler.pkl")
  static_scaler <- pd$read_pickle(scaler_path)
  
  # Define static covariates
  static_covars <- c("eir", "dn0_use", "dn0_future", "Q0", "phi_bednets",
                    "seasonal", "routine", "itn_use", "irs_use", 
                    "itn_future", "irs_future", "lsm")
  
  # Determine input size
  use_cyclical_time <- training_args$use_cyclical_time %||% TRUE
  input_size <- ifelse(use_cyclical_time, 2 + length(static_covars), 1 + length(static_covars))
  
  # Model parameters
  hidden_size <- training_args$hidden_size %||% 256
  num_layers <- training_args$num_layers %||% 4
  dropout <- training_args$dropout %||% 0.05
  
  # Load models using Python function
  gru_path <- file.path(predictor_models_dir, "gru_best.pt")
  lstm_path <- file.path(predictor_models_dir, "lstm_best.pt")
  
  message(sprintf("[INFO] Loading GRU model from %s", gru_path))
  message(sprintf("[INFO] Loading LSTM model from %s", lstm_path))
  
  # Load Python models
  reticulate::py_run_string(sprintf("
import warnings
warnings.filterwarnings('ignore')

# Load GRU model
gru_model, gru_hidden, gru_layers = load_model_from_checkpoint(
    '%s', %d, %d, 1, %f, %d, 'gru', '%s'
)

# Load LSTM model
lstm_model, lstm_hidden, lstm_layers = load_model_from_checkpoint(
    '%s', %d, %d, 1, %f, %d, 'lstm', '%s'
)

# Move to device
gru_model.to(torch.device('%s'))
lstm_model.to(torch.device('%s'))
gru_model.eval()
lstm_model.eval()

# Report actual architectures
print(f'[INFO] GRU model loaded: hidden_size={gru_hidden}, num_layers={gru_layers}')
print(f'[INFO] LSTM model loaded: hidden_size={lstm_hidden}, num_layers={lstm_layers}')
", gru_path, input_size, hidden_size, dropout, num_layers, predictor,
   lstm_path, input_size, hidden_size, dropout, num_layers, predictor,
   device, device))
  
  return(list(
    gru_model = reticulate::py$gru_model,
    lstm_model = reticulate::py$lstm_model,
    static_scaler = static_scaler,
    static_covars = static_covars,
    use_cyclical_time = use_cyclical_time,
    predictor = predictor,
    device = device_obj,
    training_args = training_args,
    models_dir = predictor_models_dir
  ))
}

#' Generate Scenario Predictions
#'
#' @param scenarios Data frame with scenario parameters
#' @param models List from load_emulator_models()
#' @param model_types Vector of model types to use ("GRU", "LSTM", or both)
#' @param time_steps Number of time steps to predict (in days for prevalence)
#'
#' @return List with predictions for each scenario
#' @export
generate_scenario_predictions <- function(scenarios, models, model_types = c("GRU", "LSTM"), 
                                        time_steps = 2190) {  # 6 years = 2190 days
  
  # Validate model types
  model_types <- match.arg(model_types, c("GRU", "LSTM"), several.ok = TRUE)
  
  np <- reticulate::import("numpy")
  
  predictions <- list()
  
  for (i in 1:nrow(scenarios)) {
    scenario <- scenarios[i, ]
    
    # Get static values in correct order
    static_vals <- as.numeric(scenario[models$static_covars])
    static_vals_scaled <- models$static_scaler$transform(matrix(static_vals, nrow = 1))
    
    # Create time series based on predictor type
    if (models$predictor == "prevalence") {
      # For prevalence, use daily timesteps
      t <- 1:time_steps
    } else {
      # For cases, adjust for window size (14-day intervals)
      t <- 1:(time_steps / 14)
    }
    
    # Build input features
    if (models$use_cyclical_time) {
      if (models$predictor == "cases") {
        day_of_year <- (t * 14) %% 365
      } else {
        day_of_year <- t %% 365
      }
      
      sin_t <- sin(2 * pi * day_of_year / 365)
      cos_t <- cos(2 * pi * day_of_year / 365)
      
      X_full <- matrix(0, nrow = length(t), ncol = 2 + length(models$static_covars))
      X_full[, 1] <- sin_t
      X_full[, 2] <- cos_t
      X_full[, 3:ncol(X_full)] <- rep(static_vals_scaled, each = length(t))
    } else {
      t_norm <- (t - min(t)) / (max(t) - min(t))
      X_full <- matrix(0, nrow = length(t), ncol = 1 + length(models$static_covars))
      X_full[, 1] <- t_norm
      X_full[, 2:ncol(X_full)] <- rep(static_vals_scaled, each = length(t))
    }
    
    # Get predictions
    X_full_np <- np$array(X_full, dtype = np$float32)
    
    scenario_preds <- list(
      scenario_index = i,
      timesteps = t,
      parameters = scenario
    )
    
    if ("GRU" %in% model_types) {
      scenario_preds$gru <- reticulate::py$predict_full_sequence(models$gru_model, X_full_np, models$device)
    }
    
    if ("LSTM" %in% model_types) {
      scenario_preds$lstm <- reticulate::py$predict_full_sequence(models$lstm_model, X_full_np, models$device)
    }
    
    predictions[[i]] <- scenario_preds
  }
  
  return(predictions)
}

#' Create Scenarios for Emulator
#'
#' @param ... Named parameters with values
#'
#' @return Data frame of scenarios
#' @export
create_scenarios <- function(...) {
  args <- list(...)
  
  # Check all arguments have same length
  lengths <- sapply(args, length)
  if (length(unique(lengths)) != 1) {
    stop("All scenario parameters must have the same length")
  }
  
  # Create data frame
  scenarios <- as.data.frame(args)
  return(scenarios)
}

#' Create Unified Plot for Emulator Results
#'
#' @param plot_data Data frame with timestep, value, type columns
#' @param predictor "prevalence" or "cases"
#' @param title_text Title for the plot
#' @param window_size Window size for time scaling
#' @param plot_tight Use tight y-axis scaling
#'
#' @return ggplot object
#' @export
create_unified_plot <- function(plot_data, predictor, title_text, 
                               window_size = 14, plot_tight = FALSE) {
  MAX_STEPS <- 156
  # Convert timesteps to years
  ## ---- limit to the first 156 time-steps ----
  plot_data <- dplyr::filter(plot_data, timestep <= MAX_STEPS)
  
  ## ---- convert those steps to calendar years ----
  # Every timestep represents `window_size` days, no matter which predictor
  plot_data$years <- (plot_data$timestep * window_size) / 365
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = years, y = value, color = type)) +
    # Add vertical line at 3 years
    ggplot2::geom_vline(xintercept = 3, linetype = "dashed", color = "black", alpha = 0.5) +
    # Add actual data lines (if present)
    ggplot2::geom_line(data = dplyr::filter(plot_data, type == "Actual"), 
              ggplot2::aes(group = simulation), alpha = 0.5, linetype = "dashed") +
    # Add model predictions
    ggplot2::geom_line(data = dplyr::filter(plot_data, type != "Actual"), 
              linewidth = 1.2) +
    ggplot2::scale_x_continuous(
      breaks = 0:6,
      limits = c(0, 6),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = title_text,
      x = "Years",
      y = ifelse(predictor == "prevalence", "Prevalence", "Cases per 1000"),
      color = "Model"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  # Set y-axis limits
  if (!plot_tight && predictor == "prevalence") {
    p <- p + ggplot2::ylim(0, 1)
  }
  
  return(p)
}