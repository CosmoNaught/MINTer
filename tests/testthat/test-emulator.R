test_that("GRU scenarios run", {
  
  skip_if_no_python_pkgs()
  mac_thread_safe_setup()
  testthat::skip_on_cran()
  
  # Create intervention scenarios
  scenarios <- create_scenarios(
    eir = c(5.2, 35.8, 180.5),
    dn0_use = c(0.15, 0.35, 0.55),
    dn0_future = c(0.20, 0.45, 0.65),
    Q0 = c(0.65, 0.75, 0.85),
    phi_bednets = c(0.45, 0.65, 0.75),
    seasonal = c(0, 1, 1),
    routine = c(0, 0, 1),
    itn_use = c(0.25, 0.55, 0.85),
    irs_use = c(0.10, 0.35, 0.70),
    itn_future = c(0.30, 0.60, 0.90),
    irs_future = c(0.15, 0.40, 0.75),
    lsm = c(0.05, 0.45, 0.85)
  )
  
  # Run emulator - returns a dataframe
  results_prevalence <- MINTer::run_malaria_emulator(
    scenarios = scenarios,
    predictor = 'prevalence',
    model_types = c('GRU', 'LSTM')
  )
  
  # correct data structure
  expect_named(results_prevalence, c("index", "timestep", "prevalence", "model_type"))
  
  # both models loaded
  expect_true(all(unique(results_prevalence$model_type) == c("GRU", "LSTM")))
  
})
