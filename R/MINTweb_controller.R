#' Run MINT Scenarios for Malaria Intervention Analysis

#' @param res_use Numeric vector of current resistance levels
#' @param res_future Numeric vector of future resistance levels after next campaign
#' @param py_only Numeric vector of pyrethroid-only net proportions
#' @param py_pbo Numeric vector of pyrethroid-PBO net proportions
#' @param py_pyrrole Numeric vector of pyrethroid-pyrrole net proportions
#' @param py_ppf Numeric vector of pyrethroid-PPF net proportions
#' @param prev_vec Numeric vector of malaria prevalence levels
#' @param Q0_vec Numeric vector of Q0 values
#' @param phi_vec Numeric vector of phi bednet values
#' @param season_vec Numeric vector of seasonality indicators
#' @param routine_vec Numeric vector of routine treatment indicators
#' @param irs_vec Numeric vector of current IRS coverage
#' @param irs_future_vec Numeric vector of future IRS coverage
#' @param lsm_vec Numeric vector of LSM coverage
#' @param eir_models Character vector of models to use for EIR prediction (default: "xgboost")
#' @param prevalence_models Character vector of models for prevalence prediction (default: "LSTM")
#' @param predictor Character vector of predictors to run (default: c("prevalence", "cases"))
#' 
#' @return List of prevalence and cases predictions
#' @export
run_mint_scenarios <- function(
  # Net combination settings
  res_use,
  res_future = NULL,
  py_only,
  py_pbo,
  py_pyrrole,
  py_ppf,
  net_type_future = NULL,
  itn_future,
  # Malaria environment settings
  prev_vec,
  Q0_vec,
  phi_vec,
  season_vec,
  routine_vec,
  irs_vec,
  irs_future_vec,
  lsm_vec,
  # Model settings (with defaults)
  eir_models = "xgboost",
  prevalence_models = "LSTM",
  predictor = c("prevalence", "cases"),
  year_start,
  year_end
) {

  # Validate inputs
  n_scenarios <- length(res_use)
  if (length(py_only) != n_scenarios ||
      length(py_pbo) != n_scenarios ||
      length(py_pyrrole) != n_scenarios ||
      length(py_ppf) != n_scenarios) {
    stop("All net combination vectors must have the same length")
  }
 
  n_settings <- length(prev_vec)
  if (length(Q0_vec) != n_settings || 
      length(phi_vec) != n_settings ||
      length(season_vec) != n_settings ||
      length(routine_vec) != n_settings ||
      length(irs_vec) != n_settings ||
      length(irs_future_vec) != n_settings ||
      length(lsm_vec) != n_settings) {
    stop("All malaria environment vectors must have the same length")
  }

  # If resistance is sustained through campaigns default to resistance use
  if (is.null(res_future) || length(res_future) == 0) {
    res_future <- res_use
  }
  
  # Validate predictors
  predictor <- match.arg(predictor, c("prevalence", "cases"), several.ok = TRUE)
  
  # Load pretrained models
  pretrained <- estiMINT::load_pretrained_eir_models()
  
  # Internal function to run a single scenario
  run_single_scenario <- function(i) {
    
    # Calculate net effectiveness for current and future
    net_now <- calculate_overall_dn0(
      resistance_level   = res_use[i],
      pyrethroid_only    = py_only[i],
      pyrethroid_pbo     = py_pbo[i],
      pyrethroid_pyrrole = py_pyrrole[i],
      pyrethroid_ppf     = py_ppf[i]
    )
    if (is.null(net_type_future)){
      net_next <- calculate_overall_dn0(
        resistance_level   = res_future[i],
        pyrethroid_only    = py_only[i],
        pyrethroid_pbo     = py_pbo[i],
        pyrethroid_pyrrole = py_pyrrole[i],
        pyrethroid_ppf     = py_ppf[i]
      )
    } else {
      # User picked a specific future net type
      pyo   <- 0
      pypbo <- 0
      pypyr <- 0
      pppf <- 0

      switch(net_type_future[i],
        pyrethroid_only    = {
          pyo <- itn_future[i]
        },
        pyrethroid_pbo     = {
          pypbo <- itn_future[i]
        },
        pyrethroid_pyrrole = {
          pypyr <- itn_future[i]
        },
        pyrethroid_ppf     = {
          pppf <- itn_future[i]
        }
      )

      net_next <- calculate_overall_dn0(
        resistance_level   = res_future[i],
        pyrethroid_only    = pyo,
        pyrethroid_pbo     = pypbo,
        pyrethroid_pyrrole = pypyr,
        pyrethroid_ppf     = pppf
      )
    }
    
    # Prepare runtime data for EIR prediction
    runtime <- data.frame(
      prevalence  = prev_vec[i],
      dn0_use     = net_now$dn0,
      Q0          = Q0_vec[i],
      phi_bednets = phi_vec[i],
      seasonal    = season_vec[i],
      routine     = routine_vec[i],
      itn_use     = net_now$itn_use,
      irs_use     = irs_vec[i]
    )
    
    # Calculate EIR using specified models
    eir_predictions <- list()
    
    if ("xgboost" %in% eir_models) {
      eir_predictions$xgboost <- estiMINT::predict_initial_eir(
        pretrained$xgboost, runtime, pretrained$feature_cols
      )
    }
    
    if ("rf" %in% eir_models) {
      eir_predictions$rf <- estiMINT::predict_initial_eir(
        pretrained$rf_model, runtime, pretrained$feature_cols
      )
    }
    
    # Average the predictions
    eir <- Reduce(`+`, eir_predictions) / length(eir_predictions)
    
    # Create scenarios
    scen <- create_scenarios(
      eir          = eir,
      dn0_use      = net_now$dn0,
      dn0_future   = net_next$dn0,
      Q0           = Q0_vec[i],
      phi_bednets  = phi_vec[i],
      seasonal     = season_vec[i],
      routine      = routine_vec[i],
      itn_use      = net_now$itn_use,
      irs_use      = irs_vec[i],
      itn_future   = net_next$itn_use,
      irs_future   = irs_future_vec[i],
      lsm          = lsm_vec[i]
    )

    # Run emulator for each predictor
    results <- list()
    
    if ("prevalence" %in% predictor) {
      results$prevalence <- run_malaria_emulator(
        scenarios   = scen,
        predictor   = "prevalence",
        model_types = prevalence_models
      )
      results$prevalence$scenario <- paste0("Scenario", i)
    }
    
    if ("cases" %in% predictor) {
      pretrained_cases <- estiMINT::load_pretrained_case_models()
      
      # Predict annual cases for years 3-4, 4-5, 5-6 per 1000 pop
      years <- year_start:year_end
      new_data_cases <- tidyr::crossing(scen, year = years)
      
      xgb_predictions <- estiMINT::predict_annual_cases(
        pretrained_cases$xgboost_cases,
        new_data_cases,
        pretrained_cases$feature_cols
      )
      
      rf_predictions <- estiMINT::predict_annual_cases(
        pretrained_cases$rf_cases,
        new_data_cases,
        pretrained_cases$feature_cols
      )
      
      new_data_cases$ensemble_cases_per_1000 <- (xgb_predictions + rf_predictions) / 2
      
      # Store results
      results$cases <- new_data_cases
      results$cases$scenario <- paste0("Scenario", i)
    }
    
    results
  }
  
  # Run all scenarios
  runs <- lapply(seq_len(n_scenarios), run_single_scenario)
  
  # Combine results
  out <- list()
  
  if ("prevalence" %in% predictor) {
    out$prevalence <- do.call(rbind, lapply(runs, `[[`, "prevalence")) |>
      dplyr::group_by(scenario) |>                 # <–– NEW
      dplyr::mutate(index = dplyr::cur_group_id()) |>
      dplyr::ungroup()                             # <–– NEW
  }

  if ("cases" %in% predictor) {
    out$cases <- do.call(rbind, lapply(runs, `[[`, "cases")) |>
      dplyr::group_by(scenario) |>                 # <–– NEW
      dplyr::mutate(index = dplyr::cur_group_id()) |>
      dplyr::ungroup()                             # <–– NEW
  }
  
  return(out)
}