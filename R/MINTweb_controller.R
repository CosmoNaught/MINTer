#' Run MINT Scenarios for Malaria Intervention Analysis
#'
#' @param res_use Numeric vector of current resistance levels
#' @param res_future Numeric vector of future resistance levels after next campaign
#' @param py_only Numeric vector of pyrethroid-only net proportions
#' @param py_pbo Numeric vector of pyrethroid-PBO net proportions
#' @param py_pyrrole Numeric vector of pyrethroid-pyrrole net proportions
#' @param py_ppf Numeric vector of pyrethroid-PPF net proportions
#' @param net_type_future Character vector of future net choices per scenario
#'   (allowed: "py_only","py_pbo","py_pyrrole","py_ppf"); use NA for baseline.
#' @param itn_future Numeric vector (0–1) of future ITN coverage per scenario; use NA for baseline.
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
#' @param cull_prevalence Optional integer vector to remove the first X years worth of simulation and aggregate 
#'   it must have length equal to the number of scenarios. Defaults to "Scenario1", "Scenario2", ...
#'
#' @return List of prevalence and cases predictions
#' @export
run_mint_scenarios <- function(
  res_use,
  res_future = NULL,
  py_only,
  py_pbo,
  py_pyrrole,
  py_ppf,
  net_type_future = NULL,
  itn_future = NULL,
  prev,
  Q0,
  phi,
  season,
  routine,
  irs,
  irs_future,
  lsm,
  eir_models = "xgboost",
  prevalence_models = "LSTM",
  predictor = c("prevalence", "cases"),
  year_start = 2,
  year_end = 5,
  scenario_tag = NULL,
  cull_prevalence = NULL
  ) {

  # ---- Validate scenario vector lengths and defaults for future resistance and ITN/net choices ----
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
  allowed_net_types <- c("py_only", "py_pbo", "py_pyrrole", "py_ppf")
  if (is.null(net_type_future) && is.null(itn_future)) {
    net_type_future <- rep(NA_character_, n_scenarios)
    itn_future      <- rep(NA_real_,      n_scenarios)
  } else {
    if (is.null(net_type_future)) net_type_future <- rep(NA_character_, n_scenarios)
    if (is.null(itn_future))      itn_future      <- rep(NA_real_,      n_scenarios)
    if (length(net_type_future) != n_scenarios || length(itn_future) != n_scenarios) {
      stop("`net_type_future` and `itn_future` must each have length ", n_scenarios, " (or be NULL).")
    }
    bad_type_idx <- which(!is.na(net_type_future) & !net_type_future %in% allowed_net_types)
    if (length(bad_type_idx)) {
      stop("Unknown `net_type_future` at positions: ",
           paste(bad_type_idx, collapse = ", "),
           ". Allowed: ", paste(allowed_net_types, collapse = ", "), ".")
    }
    bad_itn_idx <- which(!is.na(itn_future) & (itn_future < 0 | itn_future > 1))
    if (length(bad_itn_idx)) {
      stop("`itn_future` must be between 0 and 1 where provided. Bad positions: ",
           paste(bad_itn_idx, collapse = ", "), ".")
    }
  }
  if (is.null(res_future) || length(res_future) == 0) {
    res_future <- res_use
  }

  # ---- Validate malaria-environment vectors and predictors, assign scenario IDs ----
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
  predictor <- match.arg(predictor, c("prevalence", "cases"), several.ok = TRUE)
  scenario_ids <- if (is.null(scenario_tag)) {
    paste0("Scenario", seq_len(n_scenarios))
  } else {
    if (length(scenario_tag) != n_scenarios) {
      stop("`scenario_tag` must have length ", n_scenarios, " to match the scenario inputs.")
    }
    as.character(scenario_tag)
  }

  # ---- Load pretrained models needed for EIR prediction ----
  pretrained <- estiMINT::load_pretrained_eir_models()

  # ---- Core per-scenario execution: compute net effects, predict EIR, build scenarios, run emulators ----
  run_single_scenario <- function(i) {

    # ---- Compute current and future net effectiveness for this scenario ----
    net_now <- calculate_overall_dn0(
      resistance_level = res_use[i],
      py_only    = py_only[i],
      py_pbo     = py_pbo[i],
      py_pyrrole = py_pyrrole[i],
      py_ppf     = py_ppf[i]
    )
    if (is.na(net_type_future[i]) || is.na(itn_future[i])) {
      net_next <- calculate_overall_dn0(
        resistance_level = res_future[i],
        py_only    = py_only[i],
        py_pbo     = py_pbo[i],
        py_pyrrole = py_pyrrole[i],
        py_ppf     = py_ppf[i]
      )
    } else {
      pyo <- pypbo <- pypyr <- pppf <- 0
      switch(net_type_future[i],
        py_only    = { pyo   <- itn_future[i] },
        py_pbo     = { pypbo <- itn_future[i] },
        py_pyrrole = { pypyr <- itn_future[i] },
        py_ppf     = { pppf  <- itn_future[i] },
        { stop("Unknown net_type_future '", net_type_future[i], "' at index ", i, ".") }
      )
      net_next <- calculate_overall_dn0(
        resistance_level = res_future[i],
        py_only    = pyo,
        py_pbo     = pypbo,
        py_pyrrole = pypyr,
        py_ppf     = pppf
      )
    }

    # ---- Assemble runtime features, predict initial EIR using selected models, average across models ----
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

    # ---- Build full intervention scenarios for emulator inputs ----
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

    # ---- Run emulators for requested predictors; post-process prevalence (cull first 2 years, optional window-avg) ----
    results <- list()


    if ("prevalence" %in% predictor) {
      results$prevalence <- run_malaria_emulator(
        scenarios   = scen,
        predictor   = "prevalence",
        model_types = prevalence_models
      )

    if (!is.null(cull_prevalence)) {
      drop_n <- pmax(0L, as.integer(cull_prevalence[1])) * 365L
      k      <- pmax(1L, as.integer(cull_prevalence[2]))
      results$prevalence <- results$prevalence |>
        dplyr::slice(-(seq_len(pmin(dplyr::n(), drop_n)))) |>
        dplyr::group_by(bin = (dplyr::row_number() - 1L) %/% k) |>
        dplyr::summarise(
          timestep   = dplyr::first(timestep),
          prevalence = mean(prevalence, na.rm = TRUE),
          .groups    = "drop"
        ) |>
        dplyr::select(timestep, prevalence)  # <— removes the bin column
    }
    results$prevalence$scenario <- scenario_ids[i]

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

      new_data_cases$cases_per_1000 <- (xgb_predictions + rf_predictions) / 2
      new_data_cases$scenario <- scenario_ids[i]
      results$cases <- new_data_cases
    }

    results
  }

  # ---- Execute all scenarios and bind results with scenario indexing ----
  runs <- lapply(seq_len(n_scenarios), run_single_scenario)

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
      dplyr::ungroup() |>
      dplyr::select("year", "cases_per_1000", "scenario")
  }

  # ---- Return combined prevalence and/or cases as requested ----
  out
}
