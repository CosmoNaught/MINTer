#' Run Malaria Emulator
#'
#' Runs malaria emulator in either database mode or scenario mode.
#' 
#' @param db_path Path to DuckDB database (for database mode)
#' @param param_index Parameter index for database mode (NULL for random selection)
#' @param scenarios Data frame with scenario parameters (for scenario mode)
#' @param predictor "prevalence" or "cases"
#' @param models_base_dir Base directory with trained models. If NULL (default),
#'   uses models bundled with the package.
#' @param counterfactual Named list for counterfactual analysis (database mode only)
#' @param window_size Window size for rolling average
#' @param device "cpu" or "cuda"
#' @param model_types Vector of model types to use ("GRU", "LSTM", or both)
#' @param time_steps Number of time steps for predictions (in days)
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
                                time_steps = 2190) {  # 6 years default

  # Validate inputs - must have either db_path or scenarios
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

  message("[INFO] Loading emulator models...")
  models <- load_emulator_models(models_base_dir, predictor, device)

  mode <- ifelse(!is.null(db_path), "database", "scenario")
  
  # Execute based on mode
  if (mode == "database") {
    message("[INFO] Running in database mode")
    
    # Validate counterfactual if provided
    if (!is.null(counterfactual) && !is.list(counterfactual)) {
      stop("Counterfactual must be a named list (e.g., list(eir = c(1, 10, 100)))")
    }
    
    # Select parameter if not specified
    if (is.null(param_index)) {
      params <- list_available_parameters(db_path)
      if (nrow(params) == 0) {
        stop("No parameters found in database")
      }
      param_index <- sample(params$parameter_index, 1)
      message(sprintf("[INFO] Randomly selected parameter index: %d", param_index))
    }
    
    # Run emulator in database mode and get raw results
    raw_results <- run_emulator_db(
      db_path = db_path,
      param_index = param_index,
      models = models,
      window_size = window_size,
      counterfactual = counterfactual,
      model_types = model_types
    )
    
    # Convert to standardized dataframe format
    results <- convert_db_results_to_dataframe(raw_results, predictor, model_types)
    
    # Add metadata as attributes
    attr(results, "predictor") <- predictor
    attr(results, "model_types") <- model_types
    attr(results, "window_size") <- window_size
    attr(results, "mode") <- "database"
    attr(results, "param_index") <- raw_results$param_index
    attr(results, "global_index") <- raw_results$global_index
    attr(results, "parameters") <- raw_results$parameters
    attr(results, "counterfactual") <- counterfactual
    
    # Summary message
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
    message("[INFO] Running in scenario mode")
    
    # Validate scenarios
    if (!is.data.frame(scenarios)) {
      stop("Scenarios must be a data frame")
    }
    
    if (nrow(scenarios) == 0) {
      stop("Scenarios data frame is empty")
    }
    
    # Check for required columns
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
    
    # Generate predictions
    predictions <- generate_scenario_predictions(
      scenarios = scenarios,
      models = models,
      model_types = model_types,
      time_steps = time_steps
    )

    # Convert predictions to dataframe format
    results_list <- list()
    
    for (i in seq_along(predictions)) {
      pred <- predictions[[i]]
      
      # Create rows for each model type
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

    # Combine all results
    results <- dplyr::bind_rows(results_list)
    
    # Rename value column based on predictor
    names(results)[names(results) == "value"] <- predictor

    # Add metadata as attributes
    attr(results, "predictor") <- predictor
    attr(results, "scenarios") <- scenarios
    attr(results, "model_types") <- model_types
    attr(results, "window_size") <- window_size
    attr(results, "time_steps") <- time_steps
    attr(results, "mode") <- "scenario"

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
