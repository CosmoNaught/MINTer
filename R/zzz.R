
# Python modules (loaded on demand)
np <- NULL
torch <- NULL
pd <- NULL

.onLoad <- function(libname, pkgname) {
  # Configure reticulate to use a specific Python environment if needed
  # reticulate::use_python("/usr/bin/python3", required = FALSE)
  
  # Use py_require to handle safe environment installation and follow best practices
  reticulate::py_require("numpy")
  reticulate::py_require("pandas")
  reticulate::py_require("torch")
  reticulate::py_require("scikit-learn")
  
  # Import with delayed loading
  np <<- reticulate::import("numpy", delay_load = TRUE)
  torch <<- reticulate::import("torch", delay_load = TRUE)
  pd <<- reticulate::import("pandas", delay_load = TRUE)
  
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