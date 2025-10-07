test_that("package loads", {
  expect_true("MINTer" %in% .packages(all.available = TRUE))
})
