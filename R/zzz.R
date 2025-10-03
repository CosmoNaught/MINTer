.minter_cache <- new.env(parent = emptyenv())

np <- NULL
torch <- NULL
pd <- NULL
sklearn <- NULL

# ---------- internal helpers -------------------------------------------------

.in_ci <- function() {
  val <- Sys.getenv("CI", unset = "")
  tolower(val) %in% c("1", "true", "yes")
}

# Prefer a stable, user-owned Python over ephemeral uv builds.
.detect_python <- function() {
  # 1) Respect explicit pin
  p <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (nzchar(p)) return(path.expand(p))

  # 2) Active virtualenv
  ve <- Sys.getenv("VIRTUAL_ENV", unset = "")
  if (nzchar(ve)) {
    cand <- file.path(path.expand(ve), "bin", "python")
    if (file.exists(cand)) return(cand)
  }

  # 3) pyenv real path (avoid shims if we can)
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
    shim <- path.expand("~/.pyenv/shims/python")
    if (file.exists(shim)) return(shim)
  }

  # 4) System python
  for (cand in c(Sys.which("python3"), Sys.which("python"))) {
    if (nzchar(cand) && file.exists(cand)) return(cand)
  }

  ""
}

.install_py_packages <- function(pkgs, verbose = TRUE) {
  # Try reticulate's pip first (in the already-selected interpreter)
  ok <- tryCatch({
    reticulate::py_install(
      packages = pkgs,
      envname = NULL,
      method = "auto",
      pip = TRUE
    )
    TRUE
  }, error = function(e) FALSE)

  # If torch failed via default index, try CPU wheels fallback (no-op if not needed)
  if (!ok && "torch" %in% pkgs) {
    ok <- tryCatch({
      reticulate::py_run_string(
        "import sys, subprocess; subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'torch', '--index-url', 'https://download.pytorch.org/whl/cpu'])"
      )
      TRUE
    }, error = function(e) FALSE)
  }

  if (!ok) {
    if (verbose) message("Automatic Python package installation failed.")
  }
  invisible(ok)
}

# ---------- lifecycle --------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  # Default away from uv unless user explicitly opts in
  if (identical(Sys.getenv("RETICULATE_USE_UV", unset = ""), "")) {
    Sys.setenv(RETICULATE_USE_UV = "0")
  }

  # Choose a stable Python early (user can still override with RETICULATE_PYTHON)
  py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (!nzchar(py)) {
    py <- .detect_python()
    if (nzchar(py)) Sys.setenv(RETICULATE_PYTHON = py)
  }
  if (nzchar(py) && file.exists(path.expand(py))) {
    # Soft preference now; hard requirement enforced in initialize_python()
    try(reticulate::use_python(path.expand(py), required = FALSE), silent = TRUE)
  }

  # Delayed imports (resolved on first actual use)
  assign("np",      reticulate::import("numpy",  delay_load = TRUE), envir = parent.env(environment()))
  assign("torch",   reticulate::import("torch",  delay_load = TRUE), envir = parent.env(environment()))
  assign("pd",      reticulate::import("pandas", delay_load = TRUE), envir = parent.env(environment()))
  assign("sklearn", reticulate::import("sklearn",delay_load = TRUE), envir = parent.env(environment()))

  # Requirement: initialize Python + preload models during load
  initialize_python(verbose = TRUE)
  preload_all_models(verbose = TRUE)
}

# ---------- caching & loading -----------------------------------------------

#' Preload All Models
#'
#' Pre-loads all models into cache for faster execution
#' @param verbose Print loading messages
#' @param force Force reload even if already cached
#' @export
preload_all_models <- function(verbose = FALSE, force = FALSE) {
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

  initialize_python(verbose = verbose)

  if (force || is.null(.minter_cache$eir_models)) {
    if (verbose) message("  - Loading EIR models...")
    .minter_cache$eir_models <- estiMINT::load_xgb_model()
  } else if (verbose) {
    message("  - EIR models already cached")
  }

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
#' @return Cached model object
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

#' Initialize Python with Model Helpers
#'
#' @param verbose Print messages
#' @export
initialize_python <- function(verbose = TRUE) {
  if (reticulate::py_available(initialize = FALSE) &&
      reticulate::py_has_attr(reticulate::py, "batch_predict_scenarios")) {
    if (verbose) message("Python dependencies already initialized.")
    return(invisible(TRUE))
  }

  # Default away from uv unless explicitly requested
  if (identical(Sys.getenv("RETICULATE_USE_UV", unset = ""), "")) {
    Sys.setenv(RETICULATE_USE_UV = "0")
  }

  # Choose interpreter (respect override)
  py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (!nzchar(py)) {
    py <- .detect_python()
    if (nzchar(py)) Sys.setenv(RETICULATE_PYTHON = py)
  }
  if (nzchar(py)) {
    reticulate::use_python(path.expand(py), required = TRUE)
  }

  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python is not available for MINTer. ",
      "Set RETICULATE_PYTHON to a valid interpreter (e.g., ~/.pyenv/versions/<env>/bin/python) and restart R."
    )
  }

  required_modules <- c("numpy", "pandas", "torch", "sklearn")
  missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]

  # Auto-install path for CI/docker (opt-in via env); keeps onLoad behavior but self-heals
  auto <- tolower(Sys.getenv("MINTER_AUTO_PY_INSTALL",
                             if (.in_ci()) "true" else "false")) %in% c("1","true","yes")
  if (length(missing) && auto) {
    if (verbose) message("[INFO] Installing missing Python packages: ", paste(missing, collapse = ", "))
    .install_py_packages(missing, verbose = verbose)
    # Recompute missing after attempted install
    missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]
  }

  if (length(missing)) {
    cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
    py_exec <- if (!is.null(cfg)) cfg$python else Sys.getenv("RETICULATE_PYTHON")
    stop(
      sprintf(
        "Missing Python modules for MINTer: %s\nInstall them with:\n  %s -m pip install %s",
        paste(missing, collapse = ", "),
        ifelse(nzchar(py_exec), py_exec, "python"),
        paste(missing, collapse = " ")
      )
    )
  }

  # Force module loading so helpers can rely on them
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
      if (grepl("/\\.cache/uv/builds", cfg$python)) {
        warning(
          "Using a temporary uv Python interpreter at: ", cfg$python, "\n",
          "Set RETICULATE_PYTHON to a stable interpreter (e.g., a pyenv/venv path) for reproducibility."
        )
      }
      message("Python dependencies initialized successfully (", cfg$python, ").")
    } else {
      message("Python dependencies initialized successfully.")
    }
  }

  invisible(TRUE)
}
