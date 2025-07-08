# # zzz.R  ────────────────────────────────────────────────────────────────────────
# .onLoad <- function(libname, pkgname) {
#   ns <- asNamespace(pkgname)
#   packageStartupMessage("MINTer: Malaria INTervention Emulator Runner")

#   ## ------------------------------------------------------------------
#   ## 1.  Tell reticulate which Python to use, falling back to system
#   ## ------------------------------------------------------------------
#   py_exec <- Sys.getenv("RETICULATE_PYTHON", unset = Sys.which("python3"))
#   if (nzchar(py_exec))
#     try(reticulate::use_python(py_exec, required = FALSE), silent = TRUE)

#   ## ------------------------------------------------------------------
#   ## 2.  Check for required modules (but never abort install)
#   ## ------------------------------------------------------------------
#   required <- c("numpy", "torch", "pandas")

#   for (pkg in required) {
#     ok <- tryCatch(reticulate::py_module_available(pkg), error = function(e) FALSE)
#     if (!ok) {
#       packageStartupMessage(
#         sprintf("Missing Python module '%s'. Install with:\n  reticulate::py_install('%s')",
#                 pkg, pkg)
#       )
#     }
#   }

#   ## ------------------------------------------------------------------
#   ## 3.  Active bindings so `torch`, `np`, `pd` work in R
#   ## ------------------------------------------------------------------
#   makeAB <- function(sym, mod) makeActiveBinding(
#     sym, function() reticulate::import(mod, delay_load = FALSE), ns
#   )
#   makeAB("np",    "numpy")
#   makeAB("torch", "torch")
#   makeAB("pd",    "pandas")
# }

# #' Initialise (or re-initialise) Python dependencies
# #' @export
# initialize_python <- function() {
#   required <- c("numpy", "torch", "pandas")
#   missing <- required[!vapply(required, reticulate::py_module_available, logical(1))]
#   if (length(missing))
#     stop("Missing Python modules: ", paste(missing, collapse = ", "),
#          "\nInstall with reticulate::py_install().", call. = FALSE)

#   invisible(lapply(required, reticulate::import, delay_load = FALSE))
#   message("Python dependencies initialised.")
#   TRUE
# }
# Package startup functions

# Python modules (loaded on demand)
np <- NULL
torch <- NULL
pd <- NULL

.onLoad <- function(libname, pkgname) {
  # Configure reticulate to use a specific Python environment if needed
  # reticulate::use_python("/usr/bin/python3", required = FALSE)
  
  # Message about Python dependencies
  packageStartupMessage("MINTer: Malaria INTervention Emulator and Runner")
  packageStartupMessage("Note: Python dependencies (numpy, torch, pandas) will be loaded when needed.")
  
  # Initialize Python modules on first use
  delay_load_python()
}

#' Initialize Python Dependencies
#'
#' This function initializes the Python dependencies required for the emulator.
#' It is called automatically when needed, but can be called manually to 
#' pre-load the dependencies.
#'
#' @export
initialize_python <- function() {
  # Check if already initialized
  if (reticulate::py_has_attr(reticulate::py, "load_model_from_checkpoint")) {
    message("Python dependencies already initialized.")
    return(invisible(TRUE))
  }
  
  if (!reticulate::py_module_available("numpy")) {
    stop("Python module 'numpy' is required. Please install it using:\n",
         "reticulate::py_install('numpy')")
  }
  
  if (!reticulate::py_module_available("torch")) {
    stop("Python module 'torch' is required. Please install it using:\n",
         "reticulate::py_install('torch')")
  }
  
  if (!reticulate::py_module_available("pandas")) {
    stop("Python module 'pandas' is required. Please install it using:\n",
         "reticulate::py_install('pandas')")
  }
  
  # Import Python modules
  np <<- reticulate::import("numpy", delay_load = FALSE)
  torch <<- reticulate::import("torch", delay_load = FALSE)
  pd <<- reticulate::import("pandas", delay_load = FALSE)
  
  # Source Python helper functions
  python_script <- system.file("python", "model_helpers.py", package = "MINTer")
  if (!file.exists(python_script)) {
    stop("Could not find model_helpers.py. Please ensure MINTer is properly installed.")
  }
  reticulate::source_python(python_script)
  
  message("Python dependencies initialized successfully.")
  invisible(TRUE)
}

#' Delay Load Python Modules
#'
#' Sets up delayed loading of Python modules to avoid loading them
#' until they are actually needed.
#'
#' @keywords internal
delay_load_python <- function() {
  np <<- reticulate::import("numpy", delay_load = TRUE)
  torch <<- reticulate::import("torch", delay_load = TRUE)
  pd <<- reticulate::import("pandas", delay_load = TRUE)
}

.onAttach <- function(libname, pkgname) {
  # Optional: Display a message when the package is attached
  # packageStartupMessage("Type ?MINTer for package documentation.")
}