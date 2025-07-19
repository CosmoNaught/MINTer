# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(MINTer)

# set once here to get threads set up right for mac
if (all(unlist(lapply(pkgs, reticulate::py_module_available)))) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    reticulate::py_run_string("
import torch
torch.set_num_threads(1)
torch.set_num_interop_threads(1)
")
    message("[INFO] PyTorch threads set to 1 for macOS stability.")
  }
  
}

# then check
test_check("MINTer")
