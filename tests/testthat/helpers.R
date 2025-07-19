python_pkgs_available <- function(pkgs = c("numpy", "pandas", "torch", "sklearn")) {
  found <- unlist(lapply(pkgs, reticulate::py_module_available))
  all(found)
}

skip_if_no_python_pkgs <- function(pkgs = c("numpy", "pandas", "torch", "sklearn")) {
  if(!python_pkgs_available(pkgs)){
    skip(paste(x, "not available for testing"))
  }
}
