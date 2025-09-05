#' Optimized Run Malaria Emulator with Batching
#'
#' @param db_path Path to DuckDB database (for database mode)
#' @param param_index Parameter index for database mode (NULL for random)
#' @param scenarios Data frame with scenario parameters (for scenario mode)
#' @param predictor "prevalence" or "cases"
#' @param models_base_dir Base directory with trained models (NULL for bundled)
#' @param counterfactual Named list for counterfactual analysis
#' @param window_size Window size for rolling average
#' @param device "cpu" or "cuda"
#' @param model_types Vector of model types ("GRU", "LSTM", or both)
#' @param time_steps Number of time steps for predictions (in days)
#' @param use_cache Use cached models (default TRUE)
#' @param benchmark Track detailed timing (default FALSE)
#'
#' @return Data frame with columns: index, timestep, value, model_type
#' @export
run_malaria_emulator <- function(db_path = NULL,
                                param_index = NULL,
                                scenarios = NULL,
                                predictor = "prevalence",
                                models_base_dir = NULL,
                                counterfactual = NULL,
                                window_size = 14,
                                device = NULL,
                                model_types = c("GRU", "LSTM"),
                                time_steps = 2190,
                                use_cache = TRUE,
                                benchmark = FALSE) {
  
  # Initialize benchmark tracking
  if (benchmark) {
    bench <- list()
    t_total <- Sys.time()
  }
  
  # Validate inputs
  if (is.null(db_path) && is.null(scenarios)) {
    stop("Either 'db_path' or 'scenarios' must be provided")
  }
  
  if (!is.null(db_path) && !is.null(scenarios)) {
    warning("Both 'db_path' and 'scenarios' provided. Using database mode.")
    scenarios <- NULL
  }
  
  if (!predictor %in% c("prevalence", "cases")) {
    stop("Predictor must be either 'prevalence' or 'cases'")
  }
  
  valid_models <- c("GRU", "LSTM")
  if (!all(model_types %in% valid_models)) {
    stop(sprintf("Invalid model types. Must be one or more of: %s", 
                paste(valid_models, collapse = ", ")))
  }
  
  # Load models (cached or fresh)
  if (benchmark) t_start <- Sys.time()
  
  if (use_cache) {
    # Try to get from cache
    cache_key <- paste0("nn_", predictor)
    models <- .minter_cache[[cache_key]]
    
    if (is.null(models)) {
      message(sprintf("[INFO] Loading and caching %s emulator models...", predictor))
      models <- load_emulator_models_cached(models_base_dir, predictor, device, 
                                           verbose = FALSE)
    } else {
      message(sprintf("[INFO] Using cached %s models", predictor))
    }
  } else {
    message("[INFO] Loading emulator models (cache disabled)...")
    models <- load_emulator_models_cached(models_base_dir, predictor, device,
                                         verbose = FALSE, force_reload = TRUE)
  }
  
  if (benchmark) {
    bench$model_loading <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  }
  
  mode <- ifelse(!is.null(db_path), "database", "scenario")
  
  if (mode == "database") {
    # Database mode (existing logic, not optimized here)
    message("[INFO] Running in database mode")
    
    if (!is.null(counterfactual) && !is.list(counterfactual)) {
      stop("Counterfactual must be a named list")
    }
    
    if (is.null(param_index)) {
      params <- list_available_parameters(db_path)
      if (nrow(params) == 0) {
        stop("No parameters found in database")
      }
      param_index <- sample(params$parameter_index, 1)
      message(sprintf("[INFO] Randomly selected parameter index: %d", param_index))
    }
    
    raw_results <- run_emulator_db(
      db_path = db_path,
      param_index = param_index,
      models = models,
      window_size = window_size,
      counterfactual = counterfactual,
      model_types = model_types
    )
    
    results <- convert_db_results_to_dataframe(raw_results, predictor, model_types)
    
    # Add metadata
    attr(results, "predictor") <- predictor
    attr(results, "model_types") <- model_types
    attr(results, "window_size") <- window_size
    attr(results, "mode") <- "database"
    attr(results, "param_index") <- raw_results$param_index
    attr(results, "global_index") <- raw_results$global_index
    attr(results, "parameters") <- raw_results$parameters
    attr(results, "counterfactual") <- counterfactual
    
    message("\n[INFO] Summary:")
    message(sprintf("  - Mode: Database"))
    message(sprintf("  - Predictor type: %s", predictor))
    message(sprintf("  - Parameter Index: %d", raw_results$param_index))
    message(sprintf("  - Global Index: %d", raw_results$global_index))
    message(sprintf("  - Model types: %s", paste(model_types, collapse = ", ")))
    if (!is.null(counterfactual)) {
      message(sprintf("  - Counterfactual: %s", 
                     paste(names(counterfactual), collapse = ", ")))
    }
    message(sprintf("  - Total predictions: %d rows", nrow(results)))
    
    return(results)
    
  } else {
    # SCENARIO MODE - OPTIMIZED WITH BATCHING
    message("[INFO] Running in scenario mode (optimized)")
    
    if (!is.data.frame(scenarios)) {
      stop("Scenarios must be a data frame")
    }
    
    if (nrow(scenarios) == 0) {
      stop("Scenarios data frame is empty")
    }
    
    required_cols <- models$static_covars
    missing_cols <- setdiff(required_cols, names(scenarios))
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing required columns in scenarios: %s", 
                  paste(missing_cols, collapse = ", ")))
    }
    
    message(sprintf("[INFO] Processing %d scenarios", nrow(scenarios)))
    message(sprintf("[INFO] Using model types: %s", paste(model_types, collapse = ", ")))
    message(sprintf("[INFO] Generating predictions for %.1f years", time_steps/365))
    
    # Import numpy
    np <- reticulate::import("numpy")
    
    # BATCHED PREDICTION
    if (benchmark) t_start <- Sys.time()
    
    predictions <- generate_scenario_predictions_batched(
      scenarios = scenarios,
      models = models,
      model_types = model_types,
      time_steps = time_steps,
      benchmark = benchmark
    )
    
    if (benchmark) {
      bench$neural_network <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
      bench$nn_details <- attr(predictions, "benchmark")
    }
    
    # Convert to dataframe format
    if (benchmark) t_start <- Sys.time()
    
    results_list <- list()
    
    for (i in seq_along(predictions)) {
      pred <- predictions[[i]]
      
      if ("GRU" %in% model_types && !is.null(pred$gru)) {
        gru_df <- data.frame(
          index = i,
          timestep = pred$timesteps,
          value = as.numeric(pred$gru),
          model_type = "GRU",
          stringsAsFactors = FALSE
        )
        results_list[[length(results_list) + 1]] <- gru_df
      }
      
      if ("LSTM" %in% model_types && !is.null(pred$lstm)) {
        lstm_df <- data.frame(
          index = i,
          timestep = pred$timesteps,
          value = as.numeric(pred$lstm),
          model_type = "LSTM",
          stringsAsFactors = FALSE
        )
        results_list[[length(results_list) + 1]] <- lstm_df
      }
    }
    
    results <- dplyr::bind_rows(results_list)
    names(results)[names(results) == "value"] <- predictor
    
    if (benchmark) {
      bench$data_conversion <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    }
    
    # Add metadata
    attr(results, "predictor") <- predictor
    attr(results, "scenarios") <- scenarios
    attr(results, "model_types") <- model_types
    attr(results, "window_size") <- window_size
    attr(results, "time_steps") <- time_steps
    attr(results, "mode") <- "scenario"
    
    if (benchmark) {
      bench$total <- as.numeric(difftime(Sys.time(), t_total, units = "secs"))
      attr(results, "benchmark") <- bench
      
      # Print benchmark summary
      message("\n--- Emulator Performance ---")
      message(sprintf("  Model loading: %.3f seconds", bench$model_loading))
      message(sprintf("  Neural network: %.3f seconds", bench$neural_network))
      if (!is.null(bench$nn_details)) {
        message(sprintf("    - Data prep: %.3f seconds", bench$nn_details$data_prep))
        message(sprintf("    - Python inference: %.3f seconds", bench$nn_details$python_inference))
        message(sprintf("    - Expected (10ms x %d): %.3f seconds", 
                       nrow(scenarios), 0.010 * nrow(scenarios)))
      }
      message(sprintf("  Data conversion: %.3f seconds", bench$data_conversion))
      message(sprintf("  Total: %.3f seconds", bench$total))
    }
    
    # Summary message
    message("\n[INFO] Summary:")
    message(sprintf("  - Mode: Scenario"))
    message(sprintf("  - Predictor type: %s", predictor))
    message(sprintf("  - Number of scenarios: %d", nrow(scenarios)))
    message(sprintf("  - Model types: %s", paste(model_types, collapse = ", ")))
    message(sprintf("  - Time period: %.1f years", time_steps/365))
    message(sprintf("  - Total predictions: %d rows", nrow(results)))
    
    return(results)
  }
}

#' Generate Scenario Predictions with Batching
#'
#' @param scenarios Data frame with scenario parameters
#' @param models List from load_emulator_models()
#' @param model_types Vector of model types
#' @param time_steps Number of time steps
#' @param window_size Window size for rolling average
#' @param benchmark Track timing details
#'
#' @return List with predictions for each scenario
#' @keywords internal
generate_scenario_predictions_batched <- function(scenarios, models, 
                                                 model_types = c("GRU", "LSTM"),
                                                 time_steps = 2190,
                                                 window_size = 14,
                                                 benchmark = FALSE) {
  
  model_types <- match.arg(model_types, c("GRU", "LSTM"), several.ok = TRUE)
  
  if (benchmark) {
    bench <- list()
    t_start <- Sys.time()
  }
  
  n_scenarios <- nrow(scenarios)
  
  # Prepare all scenario data at once
  if (models$predictor == "prevalence") {
      t <- 1:(time_steps / window_size)  # Changed: divide by window_size
  } else {
      t <- 1:(time_steps / window_size)  # Stays the same but now using window_size variable
  }
    
  # Scale all static values at once
  static_matrix <- as.matrix(scenarios[, models$static_covars])
  static_scaled <- models$static_scaler$transform(static_matrix)
  
  # Build feature arrays for all scenarios
  n_timesteps <- length(t)
  n_features <- ifelse(models$use_cyclical_time, 
                       2 + length(models$static_covars),
                       1 + length(models$static_covars))
  
  # Pre-allocate the full array
  X_all <- array(0, dim = c(n_scenarios, n_timesteps, n_features))
  
  if (models$use_cyclical_time) {
    if (models$predictor == "cases") {
      day_of_year <- (t * window_size) %% 365
      sin_t <- sin(2 * pi * day_of_year / 365)
      cos_t <- cos(2 * pi * day_of_year / 365)
    } else {
      day_of_year <- t %% 365
    }
    
    sin_t <- sin(2 * pi * day_of_year / 365)
    cos_t <- cos(2 * pi * day_of_year / 365)
    
    # Vectorized assignment
    for (i in 1:n_scenarios) {
      X_all[i, , 1] <- sin_t
      X_all[i, , 2] <- cos_t
      X_all[i, , 3:n_features] <- matrix(static_scaled[i, ], 
                                         nrow = n_timesteps, 
                                         ncol = length(models$static_covars),
                                         byrow = TRUE)
    }
  } else {
    t_norm <- (t - min(t)) / (max(t) - min(t))
    
    for (i in 1:n_scenarios) {
      X_all[i, , 1] <- t_norm
      X_all[i, , 2:n_features] <- matrix(static_scaled[i, ],
                                         nrow = n_timesteps,
                                         ncol = length(models$static_covars),
                                         byrow = TRUE)
    }
  }
  
  if (benchmark) {
    bench$data_prep <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    t_start <- Sys.time()
  }
  
  # Convert to numpy array once
  np <- reticulate::import("numpy")
  X_all_np <- np$array(X_all, dtype = np$float32)
  
  # Batch predict with all models
  predictions_gru <- NULL
  predictions_lstm <- NULL
  
  if ("GRU" %in% model_types) {
    predictions_gru <- reticulate::py$batch_predict_scenarios(
      models$gru_model, X_all_np, models$device
    )
  }
  
  if ("LSTM" %in% model_types) {
    predictions_lstm <- reticulate::py$batch_predict_scenarios(
      models$lstm_model, X_all_np, models$device
    )
  }
  
  if (benchmark) {
    bench$python_inference <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    t_start <- Sys.time()
  }
  
  # Convert predictions to list format
  predictions <- list()
  
  for (i in 1:n_scenarios) {
    scenario_preds <- list(
      scenario_index = i,
      timesteps = t,
      parameters = scenarios[i, ]
    )
    
    if (!is.null(predictions_gru)) {
      scenario_preds$gru <- predictions_gru[i, ]
    }
    
    if (!is.null(predictions_lstm)) {
      scenario_preds$lstm <- predictions_lstm[i, ]
    }
    
    predictions[[i]] <- scenario_preds
  }
  
  if (benchmark) {
    bench$r_conversion <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    attr(predictions, "benchmark") <- bench
  }
  
  return(predictions)
}

#' Run Emulator Database Mode (Internal)
#'
#' @param db_path Path to DuckDB database
#' @param param_index Parameter index to analyze
#' @param models List from load_emulator_models()
#' @param window_size Window size for rolling average
#' @param counterfactual Named list with parameter name and values (optional)
#' @param model_types Vector of model types to use
#'
#' @return List with predictions and metadata
#' @keywords internal
run_emulator_db <- function(db_path, param_index, models, window_size = 14,
                           counterfactual = NULL, model_types = c("GRU", "LSTM")) {
  
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
  
  # Prepare results
  target_column <- ifelse(models$predictor == "prevalence", "prevalence", "cases")
  all_predictions <- list()
  cf_predictions <- list()
  
  # Process each simulation
  for (sim_idx in names(sim_groups)) {
    sim_df <- sim_groups[[sim_idx]]
    sim_df <- sim_df[order(sim_df$timesteps), ]
    
    # Get data
    t <- as.numeric(sim_df$timesteps)
    y_true <- as.numeric(sim_df[[target_column]])
    
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
        timestep = t[i],
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
    
    # Run counterfactual if requested (only for first simulation)
    if (!is.null(counterfactual) && sim_idx == names(sim_groups)[1]) {
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
          for (i in 1:length(t)) {
            cf_item <- list(
              parameter_index = param_index,
              simulation_index = as.numeric(sim_idx),
              global_index = as.numeric(global_index),
              timestep = t[i],
              true_value = NA,
              counterfactual_param = cf_param_name,
              counterfactual_value = cf_val
            )
            cf_item$gru_prediction <- cf_gru[i]
            cf_predictions[[length(cf_predictions) + 1]] <- cf_item
          }
        }
        
        if ("LSTM" %in% model_types) {
          cf_lstm <- reticulate::py$predict_full_sequence(models$lstm_model, X_cf_np, models$device)
          for (i in 1:length(t)) {
            cf_item <- list(
              parameter_index = param_index,
              simulation_index = as.numeric(sim_idx),
              global_index = as.numeric(global_index),
              timestep = t[i],
              true_value = NA,
              counterfactual_param = cf_param_name,
              counterfactual_value = cf_val
            )
            cf_item$lstm_prediction <- cf_lstm[i]
            cf_predictions[[length(cf_predictions) + 1]] <- cf_item
          }
        }
      }
    }
  }
  
  return(list(
    predictions = all_predictions,
    counterfactual_predictions = cf_predictions,
    param_index = param_index,
    global_index = global_index,
    parameters = param_values,
    num_simulations = num_sims
  ))
}

#' Convert Database Results to Standard Dataframe
#'
#' @param raw_results Results from run_emulator_db
#' @param predictor "prevalence" or "cases"
#' @param model_types Vector of model types used
#'
#' @return Data frame with standardized format
#' @keywords internal
convert_db_results_to_dataframe <- function(raw_results, predictor, model_types) {
  results_list <- list()
  
  # Convert main predictions
  pred_df <- dplyr::bind_rows(raw_results$predictions)
  
  # Get actual data (just from first simulation)
  actual_data <- pred_df[pred_df$simulation_index == min(pred_df$simulation_index), ]
  actual_df <- data.frame(
    index = 1,
    timestep = actual_data$timestep,
    value = actual_data$true_value,
    model_type = "Actual",
    stringsAsFactors = FALSE
  )
  results_list[[length(results_list) + 1]] <- actual_df
  
  # Add model predictions (just from first simulation)
  if ("GRU" %in% model_types && "gru_prediction" %in% names(pred_df)) {
    gru_data <- pred_df[pred_df$simulation_index == min(pred_df$simulation_index), ]
    gru_df <- data.frame(
      index = 1,
      timestep = gru_data$timestep,
      value = gru_data$gru_prediction,
      model_type = "GRU",
      stringsAsFactors = FALSE
    )
    results_list[[length(results_list) + 1]] <- gru_df
  }
  
  if ("LSTM" %in% model_types && "lstm_prediction" %in% names(pred_df)) {
    lstm_data <- pred_df[pred_df$simulation_index == min(pred_df$simulation_index), ]
    lstm_df <- data.frame(
      index = 1,
      timestep = lstm_data$timestep,
      value = lstm_data$lstm_prediction,
      model_type = "LSTM",
      stringsAsFactors = FALSE
    )
    results_list[[length(results_list) + 1]] <- lstm_df
  }
  
  # Add counterfactual predictions if they exist
  if (length(raw_results$counterfactual_predictions) > 0) {
    # Convert to dataframe
    cf_list <- raw_results$counterfactual_predictions
    
    # Group by parameter and value
    cf_groups <- list()
    
    for (cf_item in cf_list) {
      key <- paste(cf_item$counterfactual_param, cf_item$counterfactual_value, sep = "_")
      if (!key %in% names(cf_groups)) {
        cf_groups[[key]] <- list(
          param = cf_item$counterfactual_param,
          value = cf_item$counterfactual_value,
          timesteps = c(),
          gru_predictions = c(),
          lstm_predictions = c()
        )
      }
      
      cf_groups[[key]]$timesteps <- c(cf_groups[[key]]$timesteps, cf_item$timestep)
      
      if (!is.null(cf_item$gru_prediction)) {
        cf_groups[[key]]$gru_predictions <- c(cf_groups[[key]]$gru_predictions, cf_item$gru_prediction)
      }
      if (!is.null(cf_item$lstm_prediction)) {
        cf_groups[[key]]$lstm_predictions <- c(cf_groups[[key]]$lstm_predictions, cf_item$lstm_prediction)
      }
    }
    
    # Create dataframes for each counterfactual
    cf_index <- 2  # Start at index 2
    for (cf_key in names(cf_groups)) {
      cf_group <- cf_groups[[cf_key]]
      
      # Add GRU counterfactual if available
      if (length(cf_group$gru_predictions) > 0 && "GRU" %in% model_types) {
        cf_gru_df <- data.frame(
          index = cf_index,
          timestep = cf_group$timesteps[1:length(cf_group$gru_predictions)],
          value = cf_group$gru_predictions,
          model_type = sprintf("GRU %s=%g", cf_group$param, cf_group$value),
          stringsAsFactors = FALSE
        )
        results_list[[length(results_list) + 1]] <- cf_gru_df
      }
      
      # Add LSTM counterfactual if available
      if (length(cf_group$lstm_predictions) > 0 && "LSTM" %in% model_types) {
        cf_lstm_df <- data.frame(
          index = cf_index,
          timestep = cf_group$timesteps[1:length(cf_group$lstm_predictions)],
          value = cf_group$lstm_predictions,
          model_type = sprintf("LSTM %s=%g", cf_group$param, cf_group$value),
          stringsAsFactors = FALSE
        )
        results_list[[length(results_list) + 1]] <- cf_lstm_df
      }
      
      cf_index <- cf_index + 1
    }
  }
  
  # Combine all results
  results <- dplyr::bind_rows(results_list)
  
  # Rename value column based on predictor
  names(results)[names(results) == "value"] <- predictor
  
  return(results)
}
