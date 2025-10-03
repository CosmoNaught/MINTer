# .minter_cache <- new.env(parent = emptyenv())

# np <- NULL
# torch <- NULL
# pd <- NULL
# sklearn <- NULL

# .onLoad <- function(libname, pkgname) {
#   reticulate::configure_environment(pkgname)
  
#   # Declare Python dependencies
#   reticulate::py_require("numpy")
#   reticulate::py_require("pandas") 
#   reticulate::py_require("torch")
#   reticulate::py_require("scikit-learn")
  
#   # Import with delayed loading
#   assign("np", reticulate::import("numpy", delay_load = TRUE), 
#          envir = parent.env(environment()))
#   assign("torch", reticulate::import("torch", delay_load = TRUE), 
#          envir = parent.env(environment()))
#   assign("pd", reticulate::import("pandas", delay_load = TRUE), 
#          envir = parent.env(environment()))
#   assign("sklearn", reticulate::import("sklearn", delay_load = TRUE),
#          envir = parent.env(environment()))
  
#   # Pre-load all models in background (non-blocking)

#   preload_all_models(verbose = TRUE)

# }

# #' Preload All Models
# #'
# #' Pre-loads all models into cache for faster execution
# #' @param verbose Print loading messages
# #' @param force Force reload even if already cached
# #' @export
# preload_all_models <- function(verbose = FALSE, force = FALSE) {
#   # Check if already loaded (unless forcing)
#   if (!force) {
#     all_loaded <- !is.null(.minter_cache$eir_models) && 
#                   !is.null(.minter_cache$case_models) && 
#                   !is.null(.minter_cache$nn_prevalence) && 
#                   !is.null(.minter_cache$nn_cases)
    
#     if (all_loaded) {
#       if (verbose) message("[INFO] All models already loaded in cache")
#       return(invisible(TRUE))
#     }
#   }
  
#   if (verbose) message("[INFO] Pre-loading all models into cache...")
  
#   # Initialize Python if needed
#   initialize_python(verbose = verbose)
  
#   # Load EIR models
#   if (force || is.null(.minter_cache$eir_models)) {
#     if (verbose) message("  - Loading EIR models...")
#     .minter_cache$eir_models <- estiMINT::load_xgb_model()
#   } else if (verbose) {
#     message("  - EIR models already cached")
#   }
  
#   # Load neural network models for both predictors
#   for (predictor in c("prevalence", "cases")) {
#     cache_key <- paste0("nn_", predictor)
#     if (force || is.null(.minter_cache[[cache_key]])) {
#       if (verbose) message(sprintf("  - Loading %s neural network models...", predictor))
#       .minter_cache[[cache_key]] <- load_emulator_models_cached(
#         predictor = predictor, 
#         verbose = FALSE,
#         force_reload = force
#       )
#     } else if (verbose) {
#       message(sprintf("  - %s neural network models already cached", predictor))
#     }
#   }
  
#   if (verbose) message("[INFO] All models pre-loaded successfully")
#   invisible(TRUE)
# }

# #' Get Cached Models or Load if Missing
# #'
# #' @param model_type "eir", "cases", or "nn_prevalence", "nn_cases"
# #' @return Cached model object
# get_cached_model <- function(model_type) {
#   model <- .minter_cache[[model_type]]
  
#   if (is.null(model)) {
#     # Load model if not in cache
#     if (model_type == "eir_models") {
#       .minter_cache$eir_models <- estiMINT::load_xgb_model()
#       model <- .minter_cache$eir_models
#     } else if (startsWith(model_type, "nn_")) {
#       predictor <- sub("nn_", "", model_type)
#       .minter_cache[[model_type]] <- load_emulator_models_cached(
#         predictor = predictor,
#         verbose = FALSE
#       )
#       model <- .minter_cache[[model_type]]
#     }
#   }
  
#   return(model)
# }

# #' Initialize Python with Model Helpers
# #'
# #' @param verbose Print messages
# #' @export
# initialize_python <- function(verbose = TRUE) {
#   # Check if already initialized
#   if (reticulate::py_available(initialize = FALSE) && 
#       reticulate::py_has_attr(reticulate::py, "batch_predict_scenarios")) {
#     if (verbose) message("Python dependencies already initialized.")
#     return(invisible(TRUE))
#   }
  
#   # Force initialization
#   if (!reticulate::py_available(initialize = TRUE)) {
#     stop("Python is not available. Please install Python and required packages.")
#   }
  
#   # Force module loading
#   np_module <- np
  
#   # Source optimized Python helpers
#   python_script <- system.file("python", "model_helpers_optimized.py", 
#                               package = "MINTer")
  
#   if (python_script == "" && file.exists("inst/python/model_helpers_optimized.py")) {
#     python_script <- "inst/python/model_helpers_optimized.py"
#   }
  
#   if (!file.exists(python_script)) {
#     # Fall back to original if optimized doesn't exist
#     python_script <- system.file("python", "model_helpers.py", package = "MINTer")
#     if (python_script == "" && file.exists("inst/python/model_helpers.py")) {
#       python_script <- "inst/python/model_helpers.py"
#     }
#   }
  
#   if (!file.exists(python_script)) {
#     stop("Could not find model_helpers.py")
#   }
  
#   reticulate::source_python(python_script)
  
#   if (verbose) message("Python dependencies initialized successfully.")
#   invisible(TRUE)
# }
# zzz.R — MINTer
# Robust reticulate init that avoids uv's ephemeral builds,
# loads models on load, and keeps original function signatures.

.minter_cache <- new.env(parent = emptyenv())

np <- NULL
torch <- NULL
pd <- NULL
sklearn <- NULL

# --- internal helpers -------------------------------------------------------

# Prefer a stable, user-owned Python over any ephemeral uv build.
.detect_python <- function() {
  # 1) Respect explicit pin
  p <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (nzchar(p)) return(path.expand(p))

  # 2) Active virtualenv (if any)
  ve <- Sys.getenv("VIRTUAL_ENV", unset = "")
  if (nzchar(ve)) {
    cand <- file.path(path.expand(ve), "bin", "python")
    if (file.exists(cand)) return(cand)
  }

  # 3) pyenv real path (avoids the shim when possible)
  pyenv_bin <- Sys.which("pyenv")
  if (nzchar(pyenv_bin)) {
    res <- tryCatch(
      system2(pyenv_bin, c("which", "python"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    if (length(res) && nzchar(res[1])) {
      cand <- path.expand(res[1])
      if (file.exists(cand)) return(cand)
    }
    # Fallback to shim if that's all we have
    shim <- path.expand("~/.pyenv/shims/python")
    if (file.exists(shim)) return(shim)
  }

  # 4) System python3 / python
  for (cand in c(Sys.which("python3"), Sys.which("python"))) {
    if (nzchar(cand) && file.exists(cand)) return(cand)
  }

  ""
}

# --- lifecycle --------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  # Unless the user explicitly opted in, disable uv for reticulate to
  # avoid ephemeral build paths like ~/.cache/uv/builds-v0/.tmpXXXX/bin/python
  if (identical(Sys.getenv("RETICULATE_USE_UV", unset = ""), "")) {
    Sys.setenv(RETICULATE_USE_UV = "0")
  }

  # Choose a stable Python early so reticulate doesn't auto-provision with uv
  py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (!nzchar(py)) {
    py <- .detect_python()
    if (nzchar(py)) Sys.setenv(RETICULATE_PYTHON = py)
  }
  if (nzchar(py) && file.exists(path.expand(py))) {
    # Don't error here; let initialize_python() do hard checks
    try(reticulate::use_python(path.expand(py), required = FALSE), silent = TRUE)
  }

  # Delayed imports: resolved on first use
  assign("np",      reticulate::import("numpy",  delay_load = TRUE), envir = parent.env(environment()))
  assign("torch",   reticulate::import("torch",  delay_load = TRUE), envir = parent.env(environment()))
  assign("pd",      reticulate::import("pandas", delay_load = TRUE), envir = parent.env(environment()))
  assign("sklearn", reticulate::import("sklearn",delay_load = TRUE), envir = parent.env(environment()))

  # User requirement: initialize Python + preload models on load
  initialize_python(verbose = TRUE)
  preload_all_models(verbose = TRUE)
}

# --- caching & loading ------------------------------------------------------

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
    .minter_cache$eir_models <- estiMINT::load_xgb_model()
  } else if (verbose) {
    message("  - EIR models already cached")
  }

  # Load neural network models for both predictors
  for (predictor in c("prevalence", "cases")) {
    cache_key <- paste0("nn_", predictor)
    if (force || is.null(.minter_cache[[cache_key]])) {
      if (verbose) message(sprintf("  - Loading %s neural network models...", predictor))
      .minter_cache[[cache_key]] <- load_emulator_models_cached(
        predictor   = predictor,
        verbose     = FALSE,
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
#' @param model_type "eir_models", "nn_prevalence", or "nn_cases"
#' @return Cached model object
get_cached_model <- function(model_type) {
  model <- .minter_cache[[model_type]]

  if (is.null(model)) {
    # Load model if not in cache
    if (model_type == "eir_models") {
      .minter_cache$eir_models <- estiMINT::load_xgb_model()
      model <- .minter_cache$eir_models
    } else if (startsWith(model_type, "nn_")) {
      predictor <- sub("nn_", "", model_type)
      .minter_cache[[model_type]] <- load_emulator_models_cached(
        predictor = predictor,
        verbose   = FALSE
      )
      model <- .minter_cache[[model_type]]
    }
  }

  model
}

#' Initialize Python with Model Helpers
#'
#' @param verbose Print messages
#' @export
initialize_python <- function(verbose = TRUE) {
  # If already initialized and helpers are present, skip heavy work
  if (reticulate::py_available(initialize = FALSE)) {
    # If helpers already sourced, you can early-return
    if (reticulate::py_has_attr(reticulate::py, "batch_predict_scenarios")) {
      if (verbose) message("Python dependencies already initialized.")
      return(invisible(TRUE))
    }
  }

  # Keep uv off unless explicitly requested by the user
  if (identical(Sys.getenv("RETICULATE_USE_UV", unset = ""), "")) {
    Sys.setenv(RETICULATE_USE_UV = "0")
  }

  # Choose a stable Python interpreter
  py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (!nzchar(py)) {
    py <- .detect_python()
    if (nzchar(py)) Sys.setenv(RETICULATE_PYTHON = py)
  }
  if (nzchar(py)) {
    reticulate::use_python(path.expand(py), required = TRUE)
  }

  # Initialize a Python session
  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python is not available for MINTer. ",
      "Set RETICULATE_PYTHON to a valid interpreter (e.g., ~/.pyenv/versions/<env>/bin/python) and restart R."
    )
  }

  # Validate required Python modules (fail fast with a clear message)
  required_modules <- c("numpy", "pandas", "torch", "sklearn")
  missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]
  if (length(missing)) {
    py_exec <- tryCatch(reticulate::py_config()$python, error = function(e) Sys.getenv("RETICULATE_PYTHON"))
    stop(
      sprintf(
        "Missing Python modules for MINTer: %s\nInstall them in your selected Python with:\n  %s -m pip install %s",
        paste(missing, collapse = ", "),
        ifelse(nzchar(py_exec), py_exec, "python"),
        paste(missing, collapse = " ")
      )
    )
  }

  # Force module loading for delayed imports so helpers can rely on them
  invisible(np); invisible(pd); invisible(torch); invisible(sklearn)

  # Source optimized Python helpers, with fallback
  python_script <- system.file("python", "model_helpers_optimized.py", package = "MINTer")
  if (python_script == "" || !file.exists(python_script)) {
    # Fallback to original if optimized doesn't exist (works for dev installs)
    cand_pkg <- system.file("python", "model_helpers.py", package = "MINTer")
    cand_src <- file.path("inst", "python", "model_helpers.py")
    python_script <- if (cand_pkg != "" && file.exists(cand_pkg)) cand_pkg else cand_src
  }
  if (!file.exists(python_script)) {
    stop("Could not find model_helpers.py in the MINTer installation.")
  }

  reticulate::source_python(python_script)

  # Safety: refuse to run if helpers didn't bind expected symbols
  if (!reticulate::py_has_attr(reticulate::py, "batch_predict_scenarios")) {
    stop("Python helpers did not initialize correctly (missing 'batch_predict_scenarios').")
  }

  if (verbose) {
    cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
    if (!is.null(cfg)) {
      # Warn loudly if the interpreter looks like a transient uv build
      if (grepl("/\\.cache/uv/builds", cfg$python)) {
        warning(
          "Using a temporary uv Python interpreter at: ", cfg$python, "\n",
          "Set RETICULATE_PYTHON to a stable interpreter (e.g., a pyenv/venv path)."
        )
      }
      message("Python dependencies initialized successfully (", cfg$python, ").")
    } else {
      message("Python dependencies initialized successfully.")
    }
  }

  invisible(TRUE)
}
