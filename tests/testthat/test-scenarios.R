test_that("create_malariasim_scenarios works correctly", {
  # Test basic functionality
  scenarios <- create_malariasim_scenarios(
    eir = c(5.2, 35.8),
    dn0_use = c(0.15, 0.35),
    dn0_future = c(0.20, 0.45),
    Q0 = c(0.65, 0.75),
    phi_bednets = c(0.45, 0.65),
    seasonal = c(0, 1),
    routine = c(0, 0),
    itn_use = c(0.25, 0.55),
    irs_use = c(0.10, 0.35),
    itn_future = c(0.30, 0.60),
    irs_future = c(0.15, 0.40),
    lsm = c(0.05, 0.45)
  )
  
  expect_equal(nrow(scenarios), 2)
  expect_equal(ncol(scenarios), 12)
  expect_true(file.exists("Data/malariasim_scenarios.csv"))
  
  # Clean up
  unlink("Data", recursive = TRUE)
})

test_that("create_malariasim_scenarios handles unequal lengths", {
  expect_error(
    create_malariasim_scenarios(
      eir = c(5.2, 35.8, 100),  # 3 elements
      dn0_use = c(0.15, 0.35),   # 2 elements
      dn0_future = c(0.20, 0.45),
      Q0 = c(0.65, 0.75),
      phi_bednets = c(0.45, 0.65),
      seasonal = c(0, 1),
      routine = c(0, 0),
      itn_use = c(0.25, 0.55),
      irs_use = c(0.10, 0.35),
      itn_future = c(0.30, 0.60),
      irs_future = c(0.15, 0.40),
      lsm = c(0.05, 0.45)
    ),
    "All input vectors must have the same length"
  )
})

test_that("create_scenarios works correctly", {
  scenarios <- create_scenarios(
    eir = c(5.2, 35.8),
    dn0_use = c(0.15, 0.35),
    dn0_future = c(0.20, 0.45)
  )
  
  expect_equal(nrow(scenarios), 2)
  expect_equal(ncol(scenarios), 3)
  expect_equal(scenarios$eir, c(5.2, 35.8))
})

test_that("create_scenarios handles unequal lengths", {
  expect_error(
    create_scenarios(
      eir = c(5.2, 35.8, 100),
      dn0_use = c(0.15, 0.35)
    ),
    "All scenario parameters must have the same length"
  )
})