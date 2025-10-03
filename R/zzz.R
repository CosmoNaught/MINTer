# zzz.R — package init & Python/Model bootstrapping for MINTer

.minter_cache <- new.env(parent = emptyenv())

# Delayed-import placeholders (bound in .onLoad)
np      <- NULL
torch   <- NULL
pd      <- NULL
sklearn <- NULL

# ---------- internal helpers -------------------------------------------------

.in_ci <- function() {
  val <- Sys.getenv("CI", unset = "")
  tolower(val) %in% c("1", "true", "yes")
}

.install_py_packages <- function(pkgs, verbose = TRUE) {
  ok <- tryCatch({
    reticulate::py_install(packages = pkgs, envname = NULL, method = "auto", pip = TRUE)
    TRUE
  }, error = function(e) FALSE)

  # Fallback for torch CPU wheels if needed
  if (!ok && "torch" %in% pkgs) {
    ok <- tryCatch({
      reticulate::py_run_string(
        "import sys, subprocess; subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'torch', '--index-url', 'https://download.pytorch.org/whl/cpu'])"
      )
      TRUE
    }, error = function(e) FALSE)
  }

  if (!ok && verbose) message("[WARN] Automatic Python package installation failed.")
  invisible(ok)
}

# ---------- lifecycle --------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  # Let reticulate manage a per-package env (honors Config/reticulate in DESCRIPTION)
  reticulate::configure_environment(pkgname)

  # Ensure core Python deps are importable (module names)
  # NOTE: sklearn is the import name (pip package is scikit-learn)
  reticulate::py_require("numpy")
  reticulate::py_require("pandas")
  reticulate::py_require("torch")
  reticulate::py_require("sklearn")

  # Bind delayed imports so other funcs can reference them
  assign("np",      reticulate::import("numpy",  delay_load = TRUE), envir = parent.env(environment()))
  assign("torch",   reticulate::import("torch",  delay_load = TRUE), envir = parent.env(environment()))
  assign("pd",      reticulate::import("pandas", delay_load = TRUE), envir = parent.env(environment()))
  assign("sklearn", reticulate::import("sklearn",delay_load = TRUE), envir = parent.env(environment()))

  # Heavy work (Python init + model preload) — but NEVER during R CMD INSTALL test-load
  if (!identical(Sys.getenv("R_INSTALL_PKG"), pkgname)) {
    initialize_python(verbose = TRUE)
    preload_all_models(verbose = TRUE)
  }
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("MINTer: Python initialized & models preloaded. (Set MINTER_AUTO_PY_INSTALL=true to allow auto-install in fresh envs.)")
  }
}

# ---------- caching & loading -----------------------------------------------

#' Preload All Models
#'
#' Pre-loads all models into cache for faster execution.
#' Loads:
#'   - EIR models via estiMINT::load_xgb_model()
#'   - Neural net emulators for 'prevalence' and 'cases' via load_emulator_models_cached()
#'
#' @param verbose logical; print progress
#' @param force logical; force reload even if cached
#' @export
preload_all_models <- function(verbose = FALSE, force = FALSE) {
  if (!force) {
    all_loaded <- !is.null(.minter_cache$eir_models) &&
                  !is.null(.minter_cache$nn_prevalence) &&
                  !is.null(.minter_cache$nn_cases)
    if (all_loaded) {
      if (verbose) message("[INFO] All models already loaded in cache")
      return(invisible(TRUE))
    }
  }

  if (verbose) message("[INFO] Pre-loading all models into cache...")

  initialize_python(verbose = verbose)

  # EIR models (estiMINT)
  if (force || is.null(.minter_cache$eir_models)) {
    if (verbose) message("  - Loading EIR models (estiMINT::load_xgb_model)...")
    .minter_cache$eir_models <- estiMINT::load_xgb_model()
  } else if (verbose) {
    message("  - EIR models already cached")
  }

  # Neural network emulator models for both predictors
  for (predictor in c("prevalence", "cases")) {
    cache_key <- paste0("nn_", predictor)
    if (force || is.null(.minter_cache[[cache_key]])) {
      if (verbose) message(sprintf("  - Loading %s neural network models...", predictor))
      .minter_cache[[cache_key]] <- load_emulator_models_cached(
        predictor    = predictor,
        verbose      = FALSE,
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
#' @return Cached model object (may trigger a load if absent)
get_cached_model <- function(model_type) {
  model <- .minter_cache[[model_type]]
  if (is.null(model)) {
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

# ---------- Python bootstrap -------------------------------------------------

#' Initialize Python and source model helpers
#'
#' Ensures required modules (numpy, pandas, torch, sklearn) are available in the
#' reticulate-managed environment, optionally auto-installs them (set
#' MINTER_AUTO_PY_INSTALL=true or run in CI), and sources the Python helper module.
#'
#' @param verbose logical; print progress
#' @export
initialize_python <- function(verbose = TRUE) {
  # If already initialized & helpers present, bail early
  if (reticulate::py_available(initialize = FALSE) &&
      reticulate::py_has_attr(reticulate::py, "batch_predict_scenarios")) {
    if (verbose) message("Python dependencies already initialized.")
    return(invisible(TRUE))
  }

  # Force interpreter startup (uses env configured in .onLoad)
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Python is not available for MINTer. Ensure reticulate is configured and Python is installed.")
  }

  required_modules <- c("numpy", "pandas", "torch", "sklearn")
  missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]

  auto <- tolower(Sys.getenv("MINTER_AUTO_PY_INSTALL",
                             if (.in_ci()) "true" else "false")) %in% c("1","true","yes")
  if (length(missing) && auto) {
    if (verbose) message("[INFO] Installing missing Python packages: ", paste(missing, collapse = ", "))
    .install_py_packages(
      # map module->pip where needed
      pkgs = replace(missing, missing == "sklearn", "scikit-learn"),
      verbose = verbose
    )
    # Re-check after attempted install
    missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]
  }

  if (length(missing)) {
    cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
    py_exec <- if (!is.null(cfg)) cfg$python else "python"
    stop(
      sprintf(
        "Missing Python modules for MINTer: %s\nInstall them with:\n  %s -m pip install %s",
        paste(missing, collapse = ", "),
        py_exec,
        paste(replace(missing, missing == "sklearn", "scikit-learn"), collapse = " ")
      )
    )
  }

  # Touch modules so delay-load bindings are realized
  invisible(np); invisible(pd); invisible(torch); invisible(sklearn)

  # Source optimized helpers with fallback
  python_script <- system.file("python", "model_helpers_optimized.py", package = "MINTer")
  if (python_script == "" || !file.exists(python_script)) {
    cand_pkg <- system.file("python", "model_helpers.py", package = "MINTer")
    cand_src <- file.path("inst", "python", "model_helpers.py")
    python_script <- if (cand_pkg != "" && file.exists(cand_pkg)) cand_pkg else cand_src
  }
  if (!file.exists(python_script)) stop("Could not find model_helpers.py in the MINTer installation.")

  reticulate::source_python(python_script)

  if (!reticulate::py_has_attr(reticulate::py, "batch_predict_scenarios")) {
    stop("Python helpers did not initialize correctly (missing 'batch_predict_scenarios').")
  }

  if (verbose) {
    cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
    if (!is.null(cfg)) {
      message("Python dependencies initialized successfully (", cfg$python, ").")
    } else {
      message("Python dependencies initialized successfully.")
    }
  }

  invisible(TRUE)
}
