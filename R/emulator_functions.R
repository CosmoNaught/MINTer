#' Load Emulator Models
#'
#' @param models_base_dir Base directory containing model files. If NULL (default),
#'   uses models bundled with the package.
#' @param predictor "prevalence" or "cases"
#' @param device "cpu" or "cuda" (NULL for auto-detect)
#'
#' @return List containing models and configuration
#' @export
load_emulator_models <- function(models_base_dir = NULL, predictor = "prevalence", 
                                device = NULL) {
  
  # Ensure Python is initialized
  initialize_python(verbose = FALSE)
  
  # Source Python helper functions if not already loaded
  if (!reticulate::py_has_attr(reticulate::py, "load_model_from_checkpoint")) {
    # First try system.file (for installed package)
    python_script <- system.file("python", "model_helpers.py", package = "MINTer")
    
    # If empty, try development directory
    if (python_script == "") {
      if (file.exists("inst/python/model_helpers.py")) {
        python_script <- "inst/python/model_helpers.py"
      } else {
        stop("Could not find model_helpers.py. Please ensure MINTer is properly installed or run devtools::load_all() if in development.")
      }
    }
    
    if (!file.exists(python_script)) {
      stop("model_helpers.py not found at expected location.")
    }
    
    reticulate::source_python(python_script)
  }
  
  # Set device
  if (is.null(device)) {
    device <- if (torch$cuda$is_available()) "cuda" else "cpu"
  }
  message(sprintf("[INFO] Using device: %s", device))
  device_obj <- torch$device(device)  # uses R handle, no py_eval()
  
  # Use bundled models if no base directory specified
  if (is.null(models_base_dir)) {
    # First try system.file for models under python directory (for installed package)
    models_base_dir <- system.file("python/models", package = "MINTer")
    
    # If empty, try without python prefix
    if (models_base_dir == "") {
      models_base_dir <- system.file("models", package = "MINTer")
    }
    
    # If still empty, try to find in development mode
    if (models_base_dir == "") {
      # Check if we're in the package directory
      if (file.exists("DESCRIPTION") && file.exists("inst/models")) {
        models_base_dir <- "inst/models"
        message("[INFO] Using models from development directory: inst/models")
      } else if (file.exists("DESCRIPTION") && file.exists("inst/python/models")) {
        models_base_dir <- "inst/python/models"
        message("[INFO] Using models from development directory: inst/python/models")
      } else {
        stop("Could not find bundled models. Please ensure MINTer is properly installed with model files, or run devtools::load_all() if in development.")
      }
    } else {
      message(sprintf("[INFO] Using bundled models from: %s", models_base_dir))
    }
    
    if (!dir.exists(models_base_dir)) {
      stop("Models directory not found. Please ensure the package is properly set up with model files.")
    }
  }
  
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
gru_model.to(torch.float32).to(torch.device('%s'))
lstm_model.to(torch.float32).to(torch.device('%s'))
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

#' Run Emulator for a Parameter
#'
#' @param db_path Path to DuckDB database
#' @param param_index Parameter index to analyze
#' @param models List from load_emulator_models()
#' @param window_size Window size for rolling average
#' @param counterfactual Named list with parameter name and values (optional)
#' @param output_dir Directory to save outputs
#' @param plot_tight Use tight y-axis scaling
#' @param model_types Vector of model types to use
#'
#' @return List with predictions and plot
#' @export
run_emulator <- function(db_path, 
                        param_index,
                        models,
                        window_size = 14,
                        counterfactual = NULL,
                        output_dir = NULL,
                        plot_tight = FALSE,
                        model_types = c("GRU", "LSTM")) {
  
  # Validate model types
  model_types <- match.arg(model_types, c("GRU", "LSTM"), several.ok = TRUE)
  
  # Fetch data
  df <- fetch_rolling_data(db_path, "simulation_results", window_size, 
                          param_index, models$predictor)
  
  df[models$static_covars] <- 
    lapply(df[models$static_covars], function(x) as.numeric(x))
  
  if (nrow(df) == 0) {
    stop(sprintf("No data found for parameter index %d", param_index))
  }
  
  # Get global index
  global_index <- as.numeric(unique(df$global_index)[1])
  
  message(sprintf("\n[INFO] Parameter Information:"))
  message(sprintf("  - Parameter Index: %d", param_index))
  message(sprintf("  - Global Index: %d", global_index))
  message(sprintf("  - Corresponding RDS file: simulation_results_%d.rds", global_index))
  
  # Group by simulation
  sim_groups <- split(df, df$simulation_index)
  num_sims <- length(sim_groups)
  message(sprintf("[INFO] Parameter %d has %d simulations", param_index, num_sims))
  
  # Print parameter values
  first_sim <- sim_groups[[1]]
  param_values <- first_sim[1, models$static_covars]
  message("\n[INFO] Input Parameter Values:")
  for (param_name in names(param_values)) {
    message(sprintf("  - %s: %g", param_name, param_values[[param_name]]))
  }
  
  # Import numpy
  np <- reticulate::import("numpy")
  
  # Prepare for plotting
  target_column <- ifelse(models$predictor == "prevalence", "prevalence", "cases")
  all_predictions <- list()
  cf_predictions <- list()
  
  # Create plot
  plot_data <- list()
  
  # Process each simulation
  for (sim_idx in names(sim_groups)) {
    sim_df <- sim_groups[[sim_idx]]
    sim_df <- sim_df[order(sim_df$timesteps), ]
    
    # Get data
    t <- as.numeric(sim_df$timesteps)
    y_true <- as.numeric(sim_df[[target_column]])
    
    # For consistency, always use actual timesteps (not scaled)
    t_plot <- t
    
    # Add actual data to plot
    plot_data[[length(plot_data) + 1]] <- data.frame(
      timestep = t_plot,
      value = y_true,
      type = "Actual",
      simulation = sim_idx
    )
    
    # Prepare inputs
    static_vals <- as.numeric(sim_df[1, models$static_covars])
    static_vals_scaled <- models$static_scaler$transform(matrix(static_vals, nrow = 1))
    
    # Build input features
    T_len <- nrow(sim_df)
    
    if (models$use_cyclical_time) {
      if (models$predictor == "cases") {
        day_of_year <- (t * window_size) %% 365
      } else {
        day_of_year <- t %% 365
      }
      
      sin_t <- sin(2 * pi * day_of_year / 365)
      cos_t <- cos(2 * pi * day_of_year / 365)
      
      X_full <- matrix(0, nrow = T_len, ncol = 2 + length(models$static_covars))
      X_full[, 1] <- sin_t
      X_full[, 2] <- cos_t
      X_full[, 3:ncol(X_full)] <- rep(static_vals_scaled, each = T_len)
    } else {
      t_norm <- (t - min(t)) / (max(t) - min(t))
      X_full <- matrix(0, nrow = T_len, ncol = 1 + length(models$static_covars))
      X_full[, 1] <- t_norm
      X_full[, 2:ncol(X_full)] <- rep(static_vals_scaled, each = T_len)
    }
    
    # Get predictions using Python
    X_full_np <- np$array(X_full, dtype = np$float32)
    
    if ("GRU" %in% model_types) {
      y_gru <- reticulate::py$predict_full_sequence(models$gru_model, X_full_np, models$device)
    }
    if ("LSTM" %in% model_types) {
      y_lstm <- reticulate::py$predict_full_sequence(models$lstm_model, X_full_np, models$device)
    }
    
    # Store predictions
    for (i in 1:length(t)) {
      pred_item <- list(
        parameter_index = param_index,
        simulation_index = as.numeric(sim_idx),
        global_index = as.numeric(global_index),
        timestep = t_plot[i],
        true_value = y_true[i]
      )
      
      if ("GRU" %in% model_types) {
        pred_item$gru_prediction <- y_gru[i]
      }
      if ("LSTM" %in% model_types) {
        pred_item$lstm_prediction <- y_lstm[i]
      }
      
      all_predictions[[length(all_predictions) + 1]] <- pred_item
    }
    
    # Only plot first simulation's predictions
    if (sim_idx == names(sim_groups)[1]) {
      if ("GRU" %in% model_types) {
        plot_data[[length(plot_data) + 1]] <- data.frame(
          timestep = t_plot,
          value = y_gru,
          type = "GRU",
          simulation = sim_idx
        )
      }
      
      if ("LSTM" %in% model_types) {
        plot_data[[length(plot_data) + 1]] <- data.frame(
          timestep = t_plot,
          value = y_lstm,
          type = "LSTM", 
          simulation = sim_idx
        )
      }
      
      # Run counterfactual if requested
      if (!is.null(counterfactual)) {
        cf_param_name <- names(counterfactual)[1]
        cf_values <- counterfactual[[1]]
        
        for (cf_val in cf_values) {
          # Modify parameter value
          cf_static_vals <- static_vals
          param_idx <- which(models$static_covars == cf_param_name)
          cf_static_vals[param_idx] <- cf_val
          
          # Scale and build features
          cf_static_scaled <- models$static_scaler$transform(matrix(cf_static_vals, nrow = 1))
          
          if (models$use_cyclical_time) {
            X_cf <- X_full
            X_cf[, 3:ncol(X_cf)] <- rep(cf_static_scaled, each = T_len)
          } else {
            X_cf <- X_full
            X_cf[, 2:ncol(X_cf)] <- rep(cf_static_scaled, each = T_len)
          }
          
          # Get counterfactual predictions
          X_cf_np <- np$array(X_cf, dtype = np$float32)
          
          if ("GRU" %in% model_types) {
            cf_gru <- reticulate::py$predict_full_sequence(models$gru_model, X_cf_np, models$device)
            plot_data[[length(plot_data) + 1]] <- data.frame(
              timestep = t_plot,
              value = cf_gru,
              type = sprintf("GRU %s=%g", cf_param_name, cf_val),
              simulation = sim_idx
            )
          }
          
          if ("LSTM" %in% model_types) {
            cf_lstm <- reticulate::py$predict_full_sequence(models$lstm_model, X_cf_np, models$device)
            plot_data[[length(plot_data) + 1]] <- data.frame(
              timestep = t_plot,
              value = cf_lstm,
              type = sprintf("LSTM %s=%g", cf_param_name, cf_val),
              simulation = sim_idx
            )
          }
        }
      }
    }
  }
  
  # Combine plot data
  plot_df <- dplyr::bind_rows(plot_data)
  
  # Create unified plot
  title_text <- sprintf("%s - Parameter Index = %d | Global Index = %d\n(%d Simulations)",
                       ifelse(models$predictor == "prevalence", "Prevalence", "Cases per 1000"),
                       param_index, global_index, num_sims)
  
  p <- create_unified_plot(plot_df, models$predictor, title_text, window_size, plot_tight)
  
  # Save outputs if directory specified
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Save plot
    plot_filename <- sprintf("%s_parameter_%d_global_%d_predictions%s.png",
                           models$predictor, param_index, global_index,
                           ifelse(plot_tight, "_tight", ""))
    ggplot2::ggsave(file.path(output_dir, plot_filename), p, width = 10, height = 6)
    
    # Save predictions
    pred_df <- dplyr::bind_rows(all_predictions)
    utils::write.csv(pred_df, file.path(output_dir, 
                                sprintf("%s_parameter_%d_global_%d_predictions.csv",
                                       models$predictor, param_index, global_index)))
    
    message(sprintf("[INFO] Saved outputs to %s", output_dir))
  }
  
  return(list(
    plot = p,
    predictions = dplyr::bind_rows(all_predictions),
    param_index = param_index,
    global_index = global_index
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
