# emulator_functions.R

#' Load Emulator Models with Caching
#'
#' @param models_base_dir Base directory (NULL for bundled)
#' @param predictor "prevalence" or "cases"
#' @param device "cpu" or "cuda" (NULL for auto)
#' @param verbose Print loading messages
#' @param force_reload Force reload even if cached
#'
#' @return List containing models and configuration
#' @export
load_emulator_models_cached <- function(models_base_dir = NULL, 
                                       predictor = "prevalence",
                                       device = NULL,
                                       verbose = TRUE,
                                       force_reload = FALSE) {
  
  # Check cache first unless force_reload
  cache_key <- paste0("nn_", predictor)
  if (!force_reload && !is.null(.minter_cache[[cache_key]])) {
    if (verbose) message(sprintf("[INFO] Using cached %s models", predictor))
    return(.minter_cache[[cache_key]])
  }
  
  # Initialize Python if needed
  initialize_python(verbose = FALSE)
  
  # Set device
  if (is.null(device)) {
    device <- if (torch$cuda$is_available()) "cuda" else "cpu"
  }
  if (verbose) message(sprintf("[INFO] Loading %s models on device: %s", predictor, device))
  device_obj <- torch$device(device)
  
  # Find model directory
  if (is.null(models_base_dir)) {
    models_base_dir <- system.file("python/models", package = "MINTer")
    if (models_base_dir == "") {
      models_base_dir <- system.file("models", package = "MINTer")
    }
    if (models_base_dir == "" && file.exists("inst/models")) {
      models_base_dir <- "inst/models"
    } else if (models_base_dir == "" && file.exists("inst/python/models")) {
      models_base_dir <- "inst/python/models"
    }
    
    if (!dir.exists(models_base_dir)) {
      stop("Models directory not found")
    }
  }
  
  predictor_models_dir <- file.path(models_base_dir, predictor)
  
  # Load configuration
  args_path <- file.path(predictor_models_dir, "args.json")
  if (!file.exists(args_path)) {
    stop(sprintf("Could not find args.json in %s", predictor_models_dir))
  }
  
  training_args <- jsonlite::fromJSON(args_path)
  
  # Load scaler
  scaler_path <- file.path(predictor_models_dir, "static_scaler.pkl")
  static_scaler <- pd$read_pickle(scaler_path)
  
  # Define static covariates
  static_covars <- c("eir", "dn0_use", "dn0_future", "Q0", "phi_bednets",
                    "seasonal", "routine", "itn_use", "irs_use", 
                    "itn_future", "irs_future", "lsm")
  
  after9_covars <- c("dn0_future", "itn_future", "irs_future", "lsm", "routine")
  intervention_day <- 9 * 365
  
  # Model configuration
  use_cyclical_time <- training_args$use_cyclical_time %||% TRUE
  eps_prevalence <- training_args$eps_prevalence %||% 1e-5
  event_jitter_days <- training_args$event_jitter_days %||% 7
  
  # Load LSTM model with schema inference
  lstm_path <- file.path(predictor_models_dir, "lstm_best.pt")
  
  if (verbose) message(sprintf("[INFO] Loading LSTM model from %s", lstm_path))
  
  # Call Python function to load model and infer schema
  reticulate::py_run_string(sprintf("
import warnings
warnings.filterwarnings('ignore')

# Load LSTM model with schema inference
lstm_model, lstm_schema = load_model_from_checkpoint(
    '%s', 
    static_n=%d, 
    predictor='%s',
    device=torch.device('%s'),
    use_cyclical_time=%s
)
", lstm_path, length(static_covars), predictor, device, 
   ifelse(use_cyclical_time, "True", "False")))
  
  # Extract schema from Python
  lstm_schema <- reticulate::py$lstm_schema
  
  if (verbose) {
    message(sprintf("[INFO] LSTM model loaded successfully"))
    message(sprintf("[INFO] Expected input features: %d", lstm_schema$expected_in))
    message(sprintf("[INFO] Schema: cyc=%s, year_idx=%s, lag=%s, events=%d, extra2=%d",
                   lstm_schema$cyc, lstm_schema$add_year_idx, 
                   lstm_schema$include_lag, lstm_schema$events_n, lstm_schema$extra2))
  }
  
  # Create model object
  models <- list(
    lstm_model = reticulate::py$lstm_model,
    lstm_schema = lstm_schema,
    static_scaler = static_scaler,
    static_covars = static_covars,
    after9_covars = after9_covars,
    intervention_day = intervention_day,
    use_cyclical_time = use_cyclical_time,
    predictor = predictor,
    device = device_obj,
    training_args = training_args,
    models_dir = predictor_models_dir,
    eps_prevalence = eps_prevalence,
    event_jitter_days = event_jitter_days
  )
  
  # Cache the models
  .minter_cache[[cache_key]] <- models
  
  if (verbose) {
    message(sprintf("[INFO] %s models loaded and cached", predictor))
  }
  
  return(models)
}

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

#' Create event pulse features
#' @keywords internal
create_event_pulses <- function(abs_t, events, jitter_days) {
  if (length(events) == 0) {
    return(rep(0, length(abs_t)))
  }
  
  sig <- jitter_days
  result <- numeric(length(abs_t))
  
  for (i in seq_along(abs_t)) {
    result[i] <- sum(exp(-0.5 * ((abs_t[i] - events) / sig)^2))
  }
  
  return(result)
}

#' Create time-since-event features
#' @keywords internal
create_time_since <- function(abs_t, events) {
  if (length(events) == 0) {
    return(rep(0, length(abs_t)))
  }
  
  result <- numeric(length(abs_t))
  
  for (i in seq_along(abs_t)) {
    idx <- sum(events <= abs_t[i])
    if (idx == 0) {
      result[i] <- 0
    } else {
      result[i] <- max(0, (abs_t[i] - events[idx]) / 365)
    }
  }
  
  return(result)
}

#' Prepare input features with full schema support
#' @keywords internal
prepare_input_features_schema <- function(df, models, window_size) {
  T_len <- nrow(df)
  abs_t <- df$abs_timesteps
  rel_t <- df$timesteps
  schema <- models$lstm_schema
  
  # Base static features
  row0 <- df[1, ]
  base_static <- as.numeric(row0[models$static_covars])
  raw_matrix <- matrix(rep(base_static, T_len), nrow = T_len, byrow = TRUE)
  
  # Gate future-only covariates before intervention
  post_mask <- abs_t >= models$intervention_day
  for (cov in models$after9_covars) {
    j <- which(models$static_covars == cov)
    raw_matrix[!post_mask, j] <- 0.0
  }
  
  # Scale static features
  scaled <- models$static_scaler$transform(raw_matrix)
  
  # Build feature columns based on schema
  cols <- list()
  
  # Time encoding
  if (schema$cyc) {
    day_of_year <- abs_t %% 365
    sin_t <- sin(2 * pi * day_of_year / 365)
    cos_t <- cos(2 * pi * day_of_year / 365)
    cols[[length(cols) + 1]] <- cbind(sin_t, cos_t)
  } else {
    t_min <- min(rel_t)
    t_max <- max(rel_t)
    t_norm <- if (t_max > t_min) (rel_t - t_min) / (t_max - t_min) else rel_t
    cols[[length(cols) + 1]] <- matrix(t_norm, ncol = 1)
  }
  
  # Year index
  if (schema$add_year_idx) {
    cols[[length(cols) + 1]] <- matrix(abs_t / 365, ncol = 1)
  }
  
  # Static covariates
  cols[[length(cols) + 1]] <- scaled
  
  # Extra post-intervention features
  if (schema$extra2 == 2) {
    post9 <- as.numeric(abs_t >= models$intervention_day)
    t_since9_years <- pmax(0, abs_t - models$intervention_day) / 365
    cols[[length(cols) + 1]] <- cbind(post9, t_since9_years)
  }
  
  # Lagged target (if needed)
  if (schema$include_lag) {
    target_col <- if (models$predictor == "prevalence") "prevalence" else "cases"
    y <- df[[target_col]]
    # Transform target
    if (models$predictor == "prevalence") {
      y_clip <- pmin(pmax(y, models$eps_prevalence), 1 - models$eps_prevalence)
      y_tf <- log(y_clip / (1 - y_clip))
    } else {
      y_tf <- log1p(pmax(y, 0))
    }
    y_lag <- c(y_tf[1], y_tf[-length(y_tf)])
    cols[[length(cols) + 1]] <- matrix(y_lag, ncol = 1)
  }
  
  # Event features
  if (schema$events_n == 9) {
    itn_future <- as.numeric(row0$itn_future)
    
    # ITN events
    if (itn_future > 0) {
      itn_events <- c(0, 1095, 2190, 3285)
    } else {
      itn_events <- c(0)  # Historical only
    }
    
    # IRS events
    irs_all <- seq(0, 4380, by = 365)
    irs_future <- as.numeric(row0$irs_future)
    if (irs_future > 0) {
      irs_events <- irs_all
    } else {
      irs_events <- irs_all[irs_all < models$intervention_day]
    }
    
    # LSM events
    lsm <- as.numeric(row0$lsm)
    lsm_events <- if (lsm > 0) c(3285) else numeric(0)
    
    # Create pulse and time-since features
    p_itn <- create_event_pulses(abs_t, itn_events, models$event_jitter_days)
    p_irs <- create_event_pulses(abs_t, irs_events, models$event_jitter_days)
    p_lsm <- create_event_pulses(abs_t, lsm_events, models$event_jitter_days)
    
    is_post_itn <- if (itn_future > 0) as.numeric(abs_t >= models$intervention_day) else rep(0, T_len)
    is_post_irs <- as.numeric(abs_t >= (if (length(irs_events) > 0) irs_events[1] else 1e9))
    is_post_lsm <- if (length(lsm_events) > 0) as.numeric(abs_t >= 3285) else rep(0, T_len)
    
    tau_itn <- create_time_since(abs_t, itn_events)
    tau_irs <- create_time_since(abs_t, irs_events)
    tau_lsm <- create_time_since(abs_t, lsm_events)
    
    event_matrix <- cbind(
      p_itn, p_irs, p_lsm,
      is_post_itn, is_post_irs, is_post_lsm,
      tau_itn, tau_irs, tau_lsm
    )
    
    cols[[length(cols) + 1]] <- event_matrix
  }
  
  # Combine all features
  X <- do.call(cbind, cols)
  
  # Verify dimensions
  if (ncol(X) != schema$expected_in) {
    stop(sprintf("Feature width %d != checkpoint expected %d", ncol(X), schema$expected_in))
  }
  
  return(X)
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
