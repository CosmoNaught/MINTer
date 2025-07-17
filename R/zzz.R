np <- NULL
torch <- NULL
pd <- NULL

.onLoad <- function(libname, pkgname) {

  reticulate::configure_environment(pkgname)
  
  # Declare Python dependencies using py_require
  # This creates a manifest but doesn't install anything yet
  # Only when these modules are first used will they be installed
  reticulate::py_require("numpy")
  reticulate::py_require("pandas")
  reticulate::py_require("torch")
  reticulate::py_require("scikit-learn")
  
  # Import with delayed loading
  assign("np", reticulate::import("numpy", delay_load = TRUE), envir = parent.env(environment()))
  assign("torch", reticulate::import("torch", delay_load = TRUE), envir = parent.env(environment()))
  assign("pd", reticulate::import("pandas", delay_load = TRUE), envir = parent.env(environment()))
}

#' Initialize Python Dependencies
#'
#' This function initialises the Python dependencies required for the emulator.
#' It is called automatically when needed, but can be called manually to
#' pre-load the dependencies.
#'
#' @param verbose Logical, whether to print messages (default: TRUE)
#' @export
initialize_python <- function(verbose = TRUE) {
  # First ensure Python is available (this will trigger initialization if needed)
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Python is not available. Please install Python and required packages.")
  }
  
  # Force Python initialization by accessing a module (numpy is the most robust using this!!!!)
  # This triggers the delayed load and installs packages if needed
  np_module <- np  # This accesses the delay-loaded numpy module
  
  if (reticulate::py_has_attr(reticulate::py, "load_model_from_checkpoint")) {
    if (verbose) message("Python dependencies already initialized.")
    return(invisible(TRUE))
  }
  
  # Source Python helper functions
  python_script <- system.file("python", "model_helpers.py", package = "MINTer")
  
  # Development mode fallback ONLY
  if (python_script == "" && file.exists("inst/python/model_helpers.py")) {
    python_script <- "inst/python/model_helpers.py"
  }
  
  if (!file.exists(python_script)) {
    stop("Could not find model_helpers.py. Please ensure MINTer is properly installed.")
  }
  
  reticulate::source_python(python_script)
  if (verbose) message("Python dependencies initialized successfully.")
  invisible(TRUE)
}