skip_if_no_python_pkgs <- function(pkgs = c("numpy", "pandas", "torch", "sklearn")) {
  found <- lapply(pkgs, function(x) {
    if (!reticulate::py_module_available(x)) {
      skip(paste(x, "not available for testing"))
    }
  })
}

mac_thread_safe_setup <- function() {
  if (Sys.info()[["sysname"]] == "Darwin") {
    reticulate::py_run_string("
import torch
torch.set_num_threads(1)
torch.set_num_interop_threads(1)
")
    message("[INFO] PyTorch threads set to 1 for macOS stability.")
  }
  
}