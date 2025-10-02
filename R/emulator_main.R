# emulator_main.R

#' Optimized Run Malaria Emulator with Batching (1:1 with Python defaults)
#'
#' Runs the emulator either in database mode (DuckDB-backed) or scenario mode (data.frame),
#' using the schema-aware LSTM with options to align precision and simulation aggregation.
#'
#' @param db_path Path to DuckDB database (for database mode)
#' @param param_index Parameter index for database mode (NULL for random)
#' @param scenarios Data frame with scenario parameters (for scenario mode)
#' @param predictor "prevalence" or "cases"
#' @param models_base_dir Base directory with trained models (NULL for bundled)
#' @param counterfactual Named list for counterfactual analysis (DB mode; first simulation only)
#' @param window_size Window size for rolling average (days)
#' @param device "cpu" or "cuda" (NULL = auto)
#' @param model_types Vector of model types (currently only "LSTM" is supported)
#' @param time_steps Number of time steps for predictions (in days)
#' @param use_cache Use cached models (default TRUE)
#' @param benchmark Track detailed timing (default FALSE)
#' @param precision Precision control: "fp32" (default, matches Python visualizer) or "amp" (CUDA autocast)
#' @param sim_agg Simulation aggregation in DB mode: "mean" (default), "first", or "median"
#' @param estimint_prev If TRUE in DB mode, compute estiMINT EIR using the Actual mean prevalence
#'   between displayed years 0–1 (days 730–1095) and add a "LSTM+estiMINT" line.
#'   Works for predictor="prevalence" and predictor="cases" (the latter fetches prevalence from DB).
#'
#' @return Data frame with columns: index, timestep, {predictor}, model_type
#' @export
run_malaria_emulator <- function(db_path = NULL,
                                param_index = NULL,
                                scenarios = NULL,
                                predictor = "prevalence",
                                models_base_dir = NULL,
                                counterfactual = NULL,
                                window_size = 14,
                                device = NULL,
                                model_types = c("LSTM"),
                                time_steps = 2190,
                                use_cache = TRUE,
                                benchmark = FALSE,
                                precision = c("fp32","amp"),
                                sim_agg = c("mean","first","median"),
                                estimint_prev = FALSE) {

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

  valid_models <- c("LSTM")
  if (!all(model_types %in% valid_models)) {
    stop(sprintf("Invalid model types. Must be: %s", paste(valid_models, collapse = ", ")))
  }

  precision <- match.arg(precision)
  sim_agg   <- match.arg(sim_agg)
  use_amp   <- identical(precision, "amp")

  # Load models (cached or fresh)
  if (benchmark) t_start <- Sys.time()

  if (use_cache) {
    cache_key <- paste0("nn_", predictor)
    models <- .minter_cache[[cache_key]]
    if (is.null(models)) {
      message(sprintf("[INFO] Loading and caching %s emulator models...", predictor))
      models <- load_emulator_models_cached(models_base_dir, predictor, device, verbose = FALSE)
    } else {
      message(sprintf("[INFO] Using cached %s models", predictor))
    }
  } else {
    message("[INFO] Loading emulator models (cache disabled)...")
    models <- load_emulator_models_cached(models_base_dir, predictor, device, verbose = FALSE, force_reload = TRUE)
  }

  if (benchmark) {
    bench$model_loading <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  }

  mode <- ifelse(!is.null(db_path), "database", "scenario")

  if (mode == "database") {
    # -----------------------
    # DATABASE MODE
    # -----------------------
    message("[INFO] Running in database mode")

    if (!is.null(counterfactual) && !is.list(counterfactual)) {
      stop("Counterfactual must be a named list")
    }

    if (is.null(param_index)) {
      params <- list_available_parameters(db_path)
      if (nrow(params) == 0) stop("No parameters found in database")
      param_index <- sample(params$parameter_index, 1)
      message(sprintf("[INFO] Randomly selected parameter index: %d", param_index))
    }

    raw_results <- run_emulator_db(
      db_path        = db_path,
      param_index    = param_index,
      models         = models,
      window_size    = window_size,
      counterfactual = counterfactual,
      model_types    = model_types,
      use_amp        = use_amp,
      estimint_prev  = estimint_prev
    )

    results <- convert_db_results_to_dataframe(raw_results, predictor, model_types,
                                               sim_agg = sim_agg,
                                               include_estimint = estimint_prev)

    # Add metadata
    attr(results, "predictor")      <- predictor
    attr(results, "model_types")    <- model_types
    attr(results, "window_size")    <- window_size
    attr(results, "mode")           <- "database"
    attr(results, "param_index")    <- raw_results$param_index
    attr(results, "global_index")   <- raw_results$global_index
    attr(results, "parameters")     <- raw_results$parameters
    attr(results, "counterfactual") <- counterfactual

    message("\n[INFO] Summary:")
    message(sprintf("  - Mode: Database"))
    message(sprintf("  - Predictor type: %s", predictor))
    message(sprintf("  - Parameter Index: %d", raw_results$param_index))
    message(sprintf("  - Global Index: %d", raw_results$global_index))
    message(sprintf("  - Model types: %s", paste(model_types, collapse = ", ")))
    if (!is.null(counterfactual)) {
      message(sprintf("  - Counterfactual: %s", paste(names(counterfactual), collapse = ", ")))
    }
    message(sprintf("  - Total predictions: %d rows", nrow(results)))

    if (benchmark) {
      bench$total <- as.numeric(difftime(Sys.time(), t_total, units = "secs"))
      attr(results, "benchmark") <- bench
    }
    return(results)

  } else {
    # -----------------------
    # SCENARIO MODE (BATCHED)
    # -----------------------
    message("[INFO] Running in scenario mode (optimized)")

    if (estimint_prev) {
      warning("'estimint_prev' requires DB mode (needs Actual prevalence). Ignoring in scenario mode.")
    }

    if (!is.data.frame(scenarios)) stop("Scenarios must be a data frame")
    if (nrow(scenarios) == 0) stop("Scenarios data frame is empty")

    required_cols <- models$static_covars
    missing_cols <- setdiff(required_cols, names(scenarios))
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing required columns in scenarios: %s", paste(missing_cols, collapse = ", ")))
    }

    message(sprintf("[INFO] Processing %d scenarios", nrow(scenarios)))
    message(sprintf("[INFO] Using model types: %s", paste(model_types, collapse = ", ")))
    message(sprintf("[INFO] Generating predictions for %.1f years", time_steps/365))

    # BATCHED PREDICTION
    if (benchmark) t_start <- Sys.time()

    predictions <- generate_scenario_predictions_batched(
      scenarios   = scenarios,
      models      = models,
      model_types = model_types,
      time_steps  = time_steps,
      window_size = window_size,
      benchmark   = benchmark,
      use_amp     = use_amp
    )

    if (benchmark) {
      bench$neural_network <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
      bench$nn_details     <- attr(predictions, "benchmark")
    }

    # Convert to dataframe format
    if (benchmark) t_start <- Sys.time()

    results_list <- list()
    for (i in seq_along(predictions)) {
      pred <- predictions[[i]]

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
      bench$total           <- as.numeric(difftime(Sys.time(), t_total, units = "secs"))
      attr(results, "benchmark") <- bench

      # Print benchmark summary
      message("\n--- Emulator Performance ---")
      message(sprintf("  Model loading: %.3f seconds", bench$model_loading))
      message(sprintf("  Neural network: %.3f seconds", bench$neural_network))
      if (!is.null(bench$nn_details)) {
        message(sprintf("    - Data prep: %.3f seconds", bench$nn_details$data_prep))
        message(sprintf("    - Python inference: %.3f seconds", bench$nn_details$python_inference))
        message(sprintf("    - Expected (10ms x %d): %.3f seconds", nrow(scenarios), 0.010 * nrow(scenarios)))
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


#' Generate Scenario Predictions with Batching (passes precision flag)
#'
#' @param scenarios Data frame with scenario parameters
#' @param models List from load_emulator_models_cached()
#' @param model_types Vector of model types (currently "LSTM")
#' @param time_steps Number of time steps (days)
#' @param window_size Window size for rolling average (days)
#' @param benchmark Track timing details
#' @param use_amp Logical; TRUE to use CUDA autocast (fp16) like AMP; FALSE (default) for fp32
#'
#' @return List with predictions for each scenario
#' @keywords internal
generate_scenario_predictions_batched <- function(scenarios, models,
                                                 model_types = c("LSTM"),
                                                 time_steps = 2190,
                                                 window_size = 14,
                                                 benchmark = FALSE,
                                                 use_amp = FALSE) {

  if (benchmark) {
    bench <- list()
    t_start <- Sys.time()
  }

  n_scenarios <- nrow(scenarios)

  # Calculate timesteps based on predictor (both branches are identical here)
  n_timesteps <- time_steps %/% window_size

  # Pre-allocate array for all scenarios
  schema <- models$lstm_schema
  X_all <- array(0, dim = c(n_scenarios, n_timesteps, schema$expected_in))

  # Process each scenario
  for (i in 1:n_scenarios) {
    # Create time series
    last_6_years_day <- 6 * 365
    abs_t <- last_6_years_day + seq(from = 0, length.out = n_timesteps) * window_size
    rel_t <- 1:n_timesteps

    # Build dataframe for this scenario
    df <- data.frame(
      abs_timesteps = abs_t,
      timesteps = rel_t
    )

    # Add static covariates
    for (cov in models$static_covars) {
      df[[cov]] <- scenarios[[cov]][i]
    }

    # Add dummy target for feature prep (won't be used unless schema has lag)
    if (models$predictor == "prevalence") {
      df$prevalence <- 0.1  # Dummy value
    } else {
      df$cases <- 1.0       # Dummy value
    }

    # Prepare features using schema-aware function
    X_i <- prepare_input_features_schema(df, models, window_size)
    X_all[i, , ] <- X_i
  }

  if (benchmark) {
    bench$data_prep <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    t_start <- Sys.time()
  }

  # Convert to numpy array
  np <- reticulate::import("numpy")
  X_all_np <- np$array(X_all, dtype = np$float32)

  # Batch predict (schema-aware LSTM)
  predictions_lstm <- reticulate::py$batch_predict_scenarios(
    models$lstm_model, X_all_np, models$device, models$predictor, use_amp = use_amp
  )

  if (benchmark) {
    bench$python_inference <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  }

  # Convert to list format
  predictions <- vector("list", n_scenarios)
  for (i in 1:n_scenarios) {
    scenario_preds <- list(
      scenario_index = i,
      timesteps = 1:n_timesteps,
      parameters = scenarios[i, ],
      lstm = predictions_lstm[i, ]
    )
    predictions[[i]] <- scenario_preds
  }

  if (benchmark) {
    attr(predictions, "benchmark") <- bench
  }

  return(predictions)
}

#' Run Emulator Database Mode (Internal, 1:1 precision + features)
#'
#' @param db_path Path to DuckDB database
#' @param param_index Parameter index to analyze
#' @param models List from load_emulator_models_cached()
#' @param window_size Window size for rolling average (days)
#' @param counterfactual Named list with parameter name and values (optional; first simulation only)
#' @param model_types Vector of model types to use (currently "LSTM")
#' @param use_amp Logical; TRUE to use CUDA autocast; FALSE (default) for fp32 to match Python visualizer
#' @param estimint_prev If TRUE, add LSTM+estiMINT by estimating EIR from Actual prevalence (years 0–1).
#'   For predictor="cases", prevalence is fetched from DB for the same param_index.
#'
#' @return List with predictions and metadata
#' @keywords internal
run_emulator_db <- function(db_path, param_index, models, window_size = 14,
                           counterfactual = NULL, model_types = c("LSTM"),
                           use_amp = FALSE,
                           estimint_prev = FALSE) {

  # Validate model types
  model_types <- match.arg(model_types, c("LSTM"), several.ok = TRUE)

  # Fetch main frame for the chosen predictor
  message(sprintf("[INFO] Connecting to DuckDB at %s", db_path))
  df <- fetch_rolling_data(db_path, "simulation_results", window_size,
                           param_index, models$predictor)

  # Ensure numeric types for static covariates
  df[models$static_covars] <- lapply(df[models$static_covars], function(x) as.numeric(x))

  if (nrow(df) == 0) stop(sprintf("No data found for parameter index %d", param_index))

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

  # If we need estiMINT and we're predicting CASES, prefetch prevalence series for the same param
  prev_groups <- NULL
  if (estimint_prev) {
    if (models$predictor == "cases") {
      df_prev <- tryCatch(
        fetch_rolling_data(db_path, "simulation_results", window_size, param_index, "prevalence"),
        error = function(e) NULL
      )
      if (!is.null(df_prev) && nrow(df_prev) > 0) {
        prev_groups <- split(df_prev[, c("simulation_index","timesteps","prevalence")], df_prev$simulation_index)
      } else {
        message("[WARN] Could not fetch prevalence series for estiMINT with predictor='cases'. Will fall back to any 'prevalence' present in the cases frame, else to 0.1.")
      }
    }
  }

  # Optional: load estiMINT once
  pretrained_eir <- NULL
  if (estimint_prev) {
    pretrained_eir <- estiMINT::load_xgb_model()
  }

  # Helpers
  np <- reticulate::import("numpy")
  get_param_val <- function(df1row, candidates) {
    for (nm in candidates) if (nm %in% names(df1row)) return(as.numeric(df1row[[nm]]))
    return(NA_real_)
  }

  # Prepare results
  target_column <- ifelse(models$predictor == "prevalence", "prevalence", "cases")
  all_predictions <- list()
  cf_predictions <- list()

  # Window indices for years 0–1 in the displayed range
  start_i <- ceiling(730 / window_size)
  end_i   <- floor(1095 / window_size)

  # Process each simulation
  for (sim_idx in names(sim_groups)) {
    sim_df <- sim_groups[[sim_idx]]
    sim_df <- sim_df[order(sim_df$timesteps), ]

    # Ensure abs_timesteps column exists
    if (!"abs_timesteps" %in% names(sim_df)) {
      last_6_years_day <- 6 * 365
      sim_df$abs_timesteps <- last_6_years_day + (sim_df$timesteps - 1) * window_size
    }

    # Targets
    y_true <- as.numeric(sim_df[[target_column]])
    if (nrow(sim_df) == 0) {
      warning(sprintf("Skipping simulation %s: no data after processing", sim_idx))
      next
    }

    # Standard features + LSTM
    X_full <- prepare_input_features_schema(sim_df, models, window_size)
    X_full_np <- np$array(X_full, dtype = np$float32)

    y_lstm <- reticulate::py$predict_full_sequence(
      models$lstm_model, X_full_np, models$device, models$predictor, use_amp = use_amp
    )

    # ---------- estiMINT branch ----------
    y_lstm_esti <- NULL
    if (!is.null(pretrained_eir)) {
      # Find a prevalence series for this simulation
      if ("prevalence" %in% names(sim_df)) {
        prev_series <- sim_df$prevalence
        prev_t      <- sim_df$timesteps
      } else if (!is.null(prev_groups) && !is.null(prev_groups[[sim_idx]])) {
        prev_series <- prev_groups[[sim_idx]]$prevalence
        prev_t      <- prev_groups[[sim_idx]]$timesteps
      } else {
        prev_series <- NA_real_
        prev_t      <- NA_integer_
      }

      # Compute baseline prevalence (years 0–1)
      if (all(is.na(prev_series))) {
        base_prev <- NA_real_
      } else {
        in_win <- which(prev_t >= start_i & prev_t <= end_i)
        base_prev <- mean(prev_series[in_win], na.rm = TRUE)
        if (!is.finite(base_prev)) base_prev <- mean(prev_series, na.rm = TRUE)
      }
      if (!is.finite(base_prev)) {
        base_prev <- 0.1
        message(sprintf("[WARN] Prevalence window mean unavailable for sim %s; using fallback 0.1", sim_idx))
      }

      # Build runtime covariates from the first row
      srow <- sim_df[1, , drop=FALSE]
      runtime <- data.frame(
        prevalence  = base_prev,
        dn0_use     = get_param_val(srow, c("dn0_use","dn0")),
        Q0          = get_param_val(srow, c("Q0")),
        phi_bednets = get_param_val(srow, c("phi_bednets","phi")),
        seasonal    = get_param_val(srow, c("seasonal","season")),
        itn_use     = get_param_val(srow, c("itn_use")),
        irs_use     = get_param_val(srow, c("irs_use","irs"))
      )

      # Estimate EIR and override
      eir_hat <- as.numeric(estiMINT::run_xgb_model(runtime, pretrained_eir))

      sim_df_est <- sim_df
      if (!("eir" %in% names(sim_df_est))) {
        sim_df_est$eir <- eir_hat
      } else {
        sim_df_est$eir[] <- eir_hat
      }

      # Recompute features and predictions with overridden EIR (for current predictor)
      X_est <- prepare_input_features_schema(sim_df_est, models, window_size)
      X_est_np <- np$array(X_est, dtype = np$float32)

      y_lstm_esti <- reticulate::py$predict_full_sequence(
        models$lstm_model, X_est_np, models$device, models$predictor, use_amp = use_amp
      )
    }
    # ---------- end estiMINT branch ----------

    # Store per-timestep predictions
    for (i in 1:length(y_true)) {
      pred_item <- list(
        parameter_index = param_index,
        simulation_index = as.numeric(sim_idx),
        global_index = as.numeric(global_index),
        timestep = as.numeric(sim_df$timesteps[i]),
        true_value = y_true[i],
        lstm_prediction = y_lstm[i]
      )
      if (!is.null(y_lstm_esti)) {
        pred_item$lstm_estimint_prediction <- y_lstm_esti[i]
      }
      all_predictions[[length(all_predictions) + 1]] <- pred_item
    }

    # Counterfactual (first simulation only) — unchanged
    if (!is.null(counterfactual) && sim_idx == names(sim_groups)[1]) {
      cf_param_name <- names(counterfactual)[1]
      cf_values <- counterfactual[[1]]

      for (cf_val in cf_values) {
        sim_df_cf <- sim_df
        sim_df_cf[[cf_param_name]] <- cf_val

        X_cf <- prepare_input_features_schema(sim_df_cf, models, window_size)
        X_cf_np <- np$array(X_cf, dtype = np$float32)

        cf_lstm <- reticulate::py$predict_full_sequence(
          models$lstm_model, X_cf_np, models$device, models$predictor, use_amp = use_amp
        )

        for (i in 1:length(y_true)) {
          cf_item <- list(
            parameter_index = param_index,
            simulation_index = as.numeric(sim_idx),
            global_index = as.numeric(global_index),
            timestep = as.numeric(sim_df$timesteps[i]),
            true_value = NA,
            counterfactual_param = cf_param_name,
            counterfactual_value = cf_val,
            lstm_prediction = cf_lstm[i]
          )
          cf_predictions[[length(cf_predictions) + 1]] <- cf_item
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


#' Convert Database Results to Standard Dataframe (with sim aggregation)
#'
#' Aggregates across simulations per timestep to match the Python visualizer
#' (default = "mean"). Use "first" to reproduce the legacy behavior (first simulation).
#'
#' @param raw_results Results from run_emulator_db()
#' @param predictor "prevalence" or "cases"
#' @param model_types Vector of model types used
#' @param sim_agg One of "mean" (default), "first", "median"
#' @param include_estimint If TRUE, include aggregated "LSTM+estiMINT" if available
#'
#' @return Data frame with standardized format
#' @keywords internal
convert_db_results_to_dataframe <- function(raw_results, predictor, model_types,
                                            sim_agg = c("mean","first","median"),
                                            include_estimint = FALSE) {
  sim_agg <- match.arg(sim_agg)

  # Flatten
  pred_df <- dplyr::bind_rows(raw_results$predictions)

  if (!all(c("timestep", "true_value") %in% names(pred_df))) {
    stop("Unexpected structure in raw_results$predictions.")
  }

  # Choose aggregation scope
  if (sim_agg == "first") {
    min_sim <- min(pred_df$simulation_index, na.rm = TRUE)
    pred_df_use <- pred_df[pred_df$simulation_index == min_sim, , drop = FALSE]
    agg_fun <- function(x) x[1]
  } else if (sim_agg == "median") {
    pred_df_use <- pred_df
    agg_fun <- function(x) stats::median(x, na.rm = TRUE)
  } else {
    pred_df_use <- pred_df
    agg_fun <- function(x) mean(x, na.rm = TRUE)
  }

  results_list <- list()

  # Actual
  actual_df <- pred_df_use |>
    dplyr::group_by(timestep) |>
    dplyr::summarise(value = agg_fun(true_value), .groups = "drop") |>
    dplyr::mutate(index = 1L, model_type = "Actual") |>
    dplyr::select(index, timestep, value, model_type)
  results_list[[length(results_list)+1]] <- actual_df

  # LSTM
  if ("LSTM" %in% model_types && "lstm_prediction" %in% names(pred_df_use)) {
    lstm_df <- pred_df_use |>
      dplyr::group_by(timestep) |>
      dplyr::summarise(value = agg_fun(lstm_prediction), .groups = "drop") |>
      dplyr::mutate(index = 1L, model_type = "LSTM") |>
      dplyr::select(index, timestep, value, model_type)
    results_list[[length(results_list)+1]] <- lstm_df
  }

  # LSTM+estiMINT (optional)
  if (include_estimint && "lstm_estimint_prediction" %in% names(pred_df_use)) {
    lstm_esti_df <- pred_df_use |>
      dplyr::group_by(timestep) |>
      dplyr::summarise(value = agg_fun(lstm_estimint_prediction), .groups = "drop") |>
      dplyr::mutate(index = 1L, model_type = "LSTM+estiMINT") |>
      dplyr::select(index, timestep, value, model_type)
    results_list[[length(results_list)+1]] <- lstm_esti_df
  }

  results <- dplyr::bind_rows(results_list)
  names(results)[names(results) == "value"] <- predictor
  return(results)
}
