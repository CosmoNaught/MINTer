#' Run MINT Scenarios for Malaria Intervention Analysis
#'
#' @param res_use Numeric vector of current resistance levels
#' @param res_future Numeric vector of future resistance levels after next campaign
#' @param py_only Numeric vector of pyrethroid-only net proportions
#' @param py_pbo Numeric vector of pyrethroid-PBO net proportions
#' @param py_pyrrole Numeric vector of pyrethroid-pyrrole net proportions
#' @param py_ppf Numeric vector of pyrethroid-PPF net proportions
#' @param prev Numeric vector of malaria prevalence levels
#' @param Q0 Numeric vector of Q0 values
#' @param phi Numeric vector of phi bednet values
#' @param season Numeric vector of seasonality indicators
#' @param routine Numeric vector of routine treatment indicators
#' @param irs Numeric vector of current IRS coverage
#' @param irs_future Numeric vector of future IRS coverage
#' @param lsm Numeric vector of LSM coverage
#' @param eir_models Character vector of models to use for EIR prediction (default: "xgboost")
#' @param prevalence_models Character vector of models for prevalence prediction (default: "LSTM")
#' @param predictor Character vector of predictors to run (default: c("prevalence", "cases"))
#' @param year_start,year_end Integers for case-year range
#' @param scenario_tag Optional character vector of scenario identifiers. If provided,
#' it must have length equal to the number of scenarios. Defaults to "Scenario1", "Scenario2", ...
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
  itn_future = NULL,
  # Malaria environment settings
  prev,
  Q0,
  phi,
  season,
  routine,
  irs,
  irs_future,
  lsm,
  # Model settings (with defaults)
  eir_models = "xgboost",
  prevalence_models = "LSTM",
  predictor = c("prevalence", "cases"),
  year_start,
  year_end,
  # NEW
  scenario_tag = NULL
) {

  # Validate scenario vector lengths
  n_scenarios <- length(res_use)
  if (length(py_only)    != n_scenarios ||
      length(py_pbo)     != n_scenarios ||
      length(py_pyrrole) != n_scenarios ||
      length(py_ppf)     != n_scenarios) {
    stop("All net combination vectors must have the same length as res_use (", n_scenarios, ").")
  }

  if (!is.null(res_future) && length(res_future) > 0 && length(res_future) != n_scenarios) {
    stop("If provided, res_future must have length ", n_scenarios, ".")
  }

  # If either future net selector is provided, require both and validate lengths
  if (!is.null(net_type_future) || !is.null(itn_future)) {
    if (is.null(net_type_future) || is.null(itn_future)) {
      stop("If specifying a future net type, provide both `net_type_future` and `itn_future`.")
    }
    if (length(net_type_future) != n_scenarios || length(itn_future) != n_scenarios) {
      stop("`net_type_future` and `itn_future` must each have length ", n_scenarios, ".")
    }
  }

  # Malaria environment vectors
  n_settings <- length(prev)
  if (length(Q0)         != n_settings ||
      length(phi)        != n_settings ||
      length(season)     != n_settings ||
      length(routine)    != n_settings ||
      length(irs)        != n_settings ||
      length(irs_future) != n_settings ||
      length(lsm)        != n_settings) {
    stop("All malaria environment vectors must have the same length as prev (", n_settings, ").")
  }

  # Resistance default: if not supplied, carry over current resistance
  if (is.null(res_future) || length(res_future) == 0) {
    res_future <- res_use
  }

  # Validate predictors
  predictor <- match.arg(predictor, c("prevalence", "cases"), several.ok = TRUE)

  # Scenario identifiers (NEW)
  scenario_ids <- if (is.null(scenario_tag)) {
    paste0("Scenario", seq_len(n_scenarios))
  } else {
    if (length(scenario_tag) != n_scenarios) {
      stop("`scenario_tag` must have length ", n_scenarios, " to match the scenario inputs.")
    }
    as.character(scenario_tag)
  }

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

    if (is.null(net_type_future)) {
      net_next <- calculate_overall_dn0(
        resistance_level   = res_future[i],
        pyrethroid_only    = py_only[i],
        pyrethroid_pbo     = py_pbo[i],
        pyrethroid_pyrrole = py_pyrrole[i],
        pyrethroid_ppf     = py_ppf[i]
      )
    } else {
      # User picked a specific future net type
      pyo <- pypbo <- pypyr <- pppf <- 0
      switch(net_type_future[i],
        pyrethroid_only    = { pyo   <- itn_future[i] },
        pyrethroid_pbo     = { pypbo <- itn_future[i] },
        pyrethroid_pyrrole = { pypyr <- itn_future[i] },
        pyrethroid_ppf     = { pppf  <- itn_future[i] },
        { stop("Unknown net_type_future '", net_type_future[i], "'.") }
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
      prevalence  = prev[i],
      dn0_use     = net_now$dn0,
      Q0          = Q0[i],
      phi_bednets = phi[i],
      seasonal    = season[i],
      routine     = routine[i],
      itn_use     = net_now$itn_use,
      irs_use     = irs[i]
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
    if (length(eir_predictions) == 0L) {
      stop("No valid `eir_models` specified. Choose from 'xgboost' or 'rf'.")
    }
    eir <- Reduce(`+`, eir_predictions) / length(eir_predictions)

    # Create scenarios
    scen <- create_scenarios(
      eir          = eir,
      dn0_use      = net_now$dn0,
      dn0_future   = net_next$dn0,
      Q0           = Q0[i],
      phi_bednets  = phi[i],
      seasonal     = season[i],
      routine      = routine[i],
      itn_use      = net_now$itn_use,
      irs_use      = irs[i],
      itn_future   = net_next$itn_use,
      irs_future   = irs_future[i],
      lsm          = lsm[i]
    )

    # Run emulator for each predictor
    results <- list()

    if ("prevalence" %in% predictor) {
      results$prevalence <- run_malaria_emulator(
        scenarios   = scen,
        predictor   = "prevalence",
        model_types = prevalence_models
      )
      results$prevalence$scenario <- scenario_ids[i]  # NEW
    }

    if ("cases" %in% predictor) {
      pretrained_cases <- estiMINT::load_pretrained_case_models()

      years <- year_start:year_end
      new_data_cases <- tidyr::crossing(scen, year = years)

      xgb_model <- if (is.list(pretrained_cases$xgboost_cases) && "model" %in% names(pretrained_cases$xgboost_cases)) {
        pretrained_cases$xgboost_cases$model
      } else {
        pretrained_cases$xgboost_cases
      }
      xgb_predictions <- estiMINT::predict_annual_cases(
        xgb_model, new_data_cases, pretrained_cases$feature_cols
      )

      rf_model <- if (is.list(pretrained_cases$rf_cases) && "model" %in% names(pretrained_cases$rf_cases)) {
        pretrained_cases$rf_cases$model
      } else {
        pretrained_cases$rf_cases
      }
      rf_predictions <- estiMINT::predict_annual_cases(
        rf_model, new_data_cases, pretrained_cases$feature_cols
      )

      new_data_cases$ensemble_cases_per_1000 <- (xgb_predictions + rf_predictions) / 2
      results$cases <- new_data_cases
      results$cases$scenario <- scenario_ids[i]       # NEW
    }

    results
  }

  # Run all scenarios
  runs <- lapply(seq_len(n_scenarios), run_single_scenario)

  # Combine results
  out <- list()
  if ("prevalence" %in% predictor) {
    out$prevalence <- do.call(rbind, lapply(runs, `[[`, "prevalence")) |>
      dplyr::group_by(scenario) |>
      dplyr::mutate(index = dplyr::cur_group_id()) |>
      dplyr::ungroup()
  }
  if ("cases" %in% predictor) {
    out$cases <- do.call(rbind, lapply(runs, `[[`, "cases")) |>
      dplyr::group_by(scenario) |>
      dplyr::mutate(index = dplyr::cur_group_id()) |>
      dplyr::ungroup()
  }

  out
}
