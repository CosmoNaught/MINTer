skip_if_no_python_pkgs <- function(pkgs = c("numpy", "pandas", "torch", "sklearn")) {
  found <- lapply(pkgs, function(x) {
    if (!reticulate::py_module_available(x)) {
      skip(paste(x, "not available for testing"))
    }
  })
}