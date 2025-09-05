.minter_cache <- new.env(parent = emptyenv())

np <- NULL
torch <- NULL
pd <- NULL
sklearn <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  
  # Declare Python dependencies
  reticulate::py_require("numpy")
  reticulate::py_require("pandas")  
  reticulate::py_require("torch")
  reticulate::py_require("scikit-learn")
  
  # Import with delayed loading
  assign("np", reticulate::import("numpy", delay_load = TRUE), 
         envir = parent.env(environment()))
  assign("torch", reticulate::import("torch", delay_load = TRUE), 
         envir = parent.env(environment()))
  assign("pd", reticulate::import("pandas", delay_load = TRUE), 
         envir = parent.env(environment()))
  assign("sklearn", reticulate::import("sklearn", delay_load = TRUE),
         envir = parent.env(environment()))
  
  # Only preload if not during R CMD INSTALL!!!!!
  if (!identical(Sys.getenv("R_INSTALL_PKG"), "MINTer")) {
    preload_all_models(verbose = TRUE)
  }
}

#' Preload All Models
#'
#' Pre-loads all models into cache for faster execution
#' @param verbose Print loading messages
#' @param force Force reload even if already cached
#' @export
preload_all_models <- function(verbose = FALSE, force = FALSE) {
  # Check if already loaded (unless forcing)
  if (!force) {
    all_loaded <- !is.null(.minter_cache$eir_models) && 
                  !is.null(.minter_cache$case_models) && 
                  !is.null(.minter_cache$nn_prevalence) && 
                  !is.null(.minter_cache$nn_cases)
    
    if (all_loaded) {
      if (verbose) message("[INFO] All models already loaded in cache")
      return(invisible(TRUE))
    }
  }
  
  if (verbose) message("[INFO] Pre-loading all models into cache...")
  
  # Initialize Python if needed
  initialize_python(verbose = verbose)
  
  # Load EIR models
  if (force || is.null(.minter_cache$eir_models)) {
    if (verbose) message("  - Loading EIR models...")
    .minter_cache$eir_models <- estiMINT::load_pretrained_eir_models()
  } else if (verbose) {
    message("  - EIR models already cached")
  }
  
  # Load case models
  if (force || is.null(.minter_cache$case_models)) {
    if (verbose) message("  - Loading case models...")
    .minter_cache$case_models <- estiMINT::load_pretrained_case_models()
  } else if (verbose) {
    message("  - Case models already cached")
  }
  
  # Load neural network models for both predictors
  for (predictor in c("prevalence", "cases")) {
    cache_key <- paste0("nn_", predictor)
    if (force || is.null(.minter_cache[[cache_key]])) {
      if (verbose) message(sprintf("  - Loading %s neural network models...", predictor))
      .minter_cache[[cache_key]] <- load_emulator_models_cached(
        predictor = predictor, 
        verbose = FALSE,
        force_reload = force
      )
    } else if (verbose) {
      message(sprintf("  - %s neural network models already cached", predictor))
    }
  }
  
  if (verbose) message("[INFO] All models pre-loaded successfully")
  invisible(TRUE)
}

#' Get Cached Models or Load if Missing
#'
#' @param model_type "eir", "cases", or "nn_prevalence", "nn_cases"
#' @return Cached model object
get_cached_model <- function(model_type) {
  model <- .minter_cache[[model_type]]
  
  if (is.null(model)) {
    # Load model if not in cache
    if (model_type == "eir_models") {
      .minter_cache$eir_models <- estiMINT::load_pretrained_eir_models()
      model <- .minter_cache$eir_models
    } else if (model_type == "case_models") {
      .minter_cache$case_models <- estiMINT::load_pretrained_case_models()
      model <- .minter_cache$case_models
    } else if (startsWith(model_type, "nn_")) {
      predictor <- sub("nn_", "", model_type)
      .minter_cache[[model_type]] <- load_emulator_models_cached(
        predictor = predictor,
        verbose = FALSE
      )
      model <- .minter_cache[[model_type]]
    }
  }
  
  return(model)
}

#' Initialize Python with Model Helpers
#'
#' @param verbose Print messages
#' @export
initialize_python <- function(verbose = TRUE) {
  # Check if already initialized
  if (reticulate::py_available(initialize = FALSE) && 
      reticulate::py_has_attr(reticulate::py, "batch_predict_scenarios")) {
    if (verbose) message("Python dependencies already initialized.")
    return(invisible(TRUE))
  }
  
  # Force initialization
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Python is not available. Please install Python and required packages.")
  }
  
  # Force module loading
  np_module <- np
  
  # Source optimized Python helpers
  python_script <- system.file("python", "model_helpers_optimized.py", 
                              package = "MINTer")
  
  if (python_script == "" && file.exists("inst/python/model_helpers_optimized.py")) {
    python_script <- "inst/python/model_helpers_optimized.py"
  }
  
  if (!file.exists(python_script)) {
    # Fall back to original if optimized doesn't exist
    python_script <- system.file("python", "model_helpers.py", package = "MINTer")
    if (python_script == "" && file.exists("inst/python/model_helpers.py")) {
      python_script <- "inst/python/model_helpers.py"
    }
  }
  
  if (!file.exists(python_script)) {
    stop("Could not find model_helpers.py")
  }
  
  reticulate::source_python(python_script)
  
  if (verbose) message("Python dependencies initialized successfully.")
  invisible(TRUE)
}