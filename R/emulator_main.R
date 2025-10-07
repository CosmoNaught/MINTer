# emulator_main.R

#' Optimized Run Malaria Emulator with Batching (1:1 with Python defaults)
#'
#' Runs the emulator either in database mode scenario mode (data.frame),
#' using the schema-aware LSTM with options to align precision and simulation aggregation.
#'
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
#' @return A data frame with columns:
#' \code{index}, \code{timestep}, \code{predictor} (the numeric column named
#' according to the selected predictor, e.g. \code{prevalence} or \code{cases}),
#' and \code{model_type}.
#' @export
run_malaria_emulator <- function(param_index = NULL,
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
                                sim_agg = c("mean","first","median")) {

  # Initialize benchmark tracking
  if (benchmark) {
    bench <- list()
    t_total <- Sys.time()
  }

  # Validate inputs
  if (is.null(scenarios)) {
    stop(" 'scenarios' must be provided")
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

    # -----------------------
    # SCENARIO MODE (BATCHED)
    # -----------------------
    message("[INFO] Running in scenario mode (optimized)")


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
