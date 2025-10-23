#' Optimized Run MINT Scenarios for Malaria Intervention Analysis
#'
#' Runs vectorized scenario builds, predicts EIR, and batches neural-network
#' inference for prevalence and/or cases.
#'
#' @param res_use Numeric vector of current resistance levels
#' @param res_future Numeric vector of future resistance levels after next campaign
#' @param py_only Numeric vector of pyrethroid-only net proportions
#' @param py_pbo Numeric vector of pyrethroid-PBO net proportions
#' @param py_pyrrole Numeric vector of pyrethroid-pyrrole net proportions
#' @param py_ppf Numeric vector of pyrethroid-PPF net proportions
#' @param net_type_future Character vector of future net choices per scenario
#' @param itn_future Numeric vector (0–1) of future ITN coverage per scenario
#' @param prev Numeric vector of malaria prevalence levels
#' @param Q0 Numeric vector of Q0 values
#' @param phi Numeric vector of phi bednet values
#' @param season Numeric vector of seasonality indicators
#' @param routine Numeric vector of routine treatment indicators
#' @param irs Numeric vector of current IRS coverage
#' @param irs_future Numeric vector of future IRS coverage
#' @param lsm Numeric vector of LSM coverage
#' @param eir_models Character vector of models to use for EIR prediction (e.g., \code{"xgboost"})
#' @param prevalence_models Character vector of models for prevalence prediction (e.g., \code{"LSTM"})
#' @param cases_models Character vector of models for cases prediction (e.g., \code{"LSTM"})
#' @param predictor Character vector of predictors to run; one or both of \code{c("prevalence","cases")}
#' @param year_start,year_end Integers for case-year range
#' @param scenario_tag Optional character vector of scenario identifiers
#' @param benchmark Logical; if \code{TRUE}, record/print timing information
#' @param preload_models Logical; preload all models at start (default \code{TRUE})
#' @param use_cache Logical; use cached models (default \code{TRUE})
#'
#' @return List of prevalence and cases predictions, plus benchmark times if benchmark=TRUE
#' @export
run_minter_scenarios <- function(
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
  cases_models = "LSTM",
  predictor = c("prevalence", "cases"),
  year_start = 2,
  year_end = 5,
  scenario_tag = NULL,
  benchmark = TRUE,
  preload_models = TRUE,
  use_cache = TRUE
) {
  
  # Initialize total timer
  if (benchmark) {
    t_total_start <- Sys.time()
    bench_times <- list()
  }
  
  # Preload all models if requested
  if (preload_models && use_cache) {
    if (benchmark) t_start <- Sys.time()
    
    # Check if models are already cached
    models_loaded <- !is.null(.minter_cache$eir_models) || 
                    !is.null(.minter_cache$case_models) ||
                    !is.null(.minter_cache$nn_prevalence)
    
    if (!models_loaded) {
      message("[INFO] Pre-loading all models into cache...")
      preload_all_models(verbose = FALSE)
    }
    
    if (benchmark) {
      bench_times$preload_models <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    }
  }
  
  # ---- Validate inputs (same as before) ----
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
    
    net_type_future[!is.na(itn_future) & itn_future == 0] <- NA_character_
    
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
  
  # ---- Load models (using cache) ----
  if (benchmark) t_start <- Sys.time()
  
  if (use_cache) {
    pretrained <- get_cached_model("eir_models")
  } else {
    pretrained <- estiMINT::load_xgb_model()
  }
  
  if (benchmark) {
    bench_times$load_eir_models <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  }
    
  # ---- Build all scenarios first (vectorized) ----
  if (benchmark) t_start <- Sys.time()
  
  all_scenarios <- list()
  eir_values <- numeric(n_scenarios)
  
  for (i in seq_len(n_scenarios)) {
    # Calculate net effectiveness
    net_now <- calculate_overall_dn0(
      resistance_level = res_use[i],
      py_only    = py_only[i],
      py_pbo     = py_pbo[i],
      py_pyrrole = py_pyrrole[i],
      py_ppf     = py_ppf[i]
    )
    
    if (all(c(py_only[i], py_pbo[i], py_pyrrole[i], py_ppf[i]) == 0)) {
      net_now$itn_use <- 0
      net_now$dn0     <- 0
    }
    
    if (!is.na(itn_future[i]) && itn_future[i] == 0) {
      net_next <- list(dn0 = 0, itn_use = 0)
    } else if (is.na(net_type_future[i]) || is.na(itn_future[i])) {
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
        py_ppf     = { pppf  <- itn_future[i] }
      )
      net_next <- calculate_overall_dn0(
        resistance_level = res_future[i],
        py_only    = pyo,
        py_pbo     = pypbo,
        py_pyrrole = pypyr,
        py_ppf     = pppf
      )
    }
    
    # Predict EIR
    runtime <- data.frame(
      prevalence  = prev[i],
      dn0_use     = net_now$dn0,
      Q0          = Q0[i],
      phi_bednets = phi[i],
      seasonal    = season[i],
      itn_use     = net_now$itn_use,
      irs_use     = irs[i]
    )
    
    eir_predictions <- list()
    if ("xgboost" %in% eir_models) {
      eir_predictions$xgboost <- estiMINT::run_xgb_model(runtime, pretrained)
    }
    
    eir <- Reduce(`+`, eir_predictions) / length(eir_predictions)
    eir_values[i] <- eir


    # Store scenario
    all_scenarios[[i]] <- list(
      eir = eir,
      dn0_use = net_now$dn0,
      dn0_future = net_next$dn0,
      Q0 = Q0[i],
      phi_bednets = phi[i],
      seasonal = season[i],
      routine = routine[i],
      itn_use = net_now$itn_use,
      irs_use = irs[i],
      itn_future = net_next$itn_use,
      irs_future = irs_future[i],
      lsm = lsm[i],
      scenario_id = scenario_ids[i]
    )
  }
  
  if (benchmark) {
    bench_times$run_eir_models <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  }
  
  # ---- Compute EIR validity + create scenario data frames for batch processing ----
  eir_valid <- eir_values >= 0.68 & eir_values <= 371.0
  scenario_meta <- data.frame(
    scenario_tag = scenario_ids,
    eir_valid    = eir_valid,
    stringsAsFactors = FALSE
  )

  scenarios_df <- do.call(rbind, lapply(all_scenarios, function(s) {
    data.frame(
      eir = s$eir,
      dn0_use = s$dn0_use,
      dn0_future = s$dn0_future,
      Q0 = s$Q0,
      phi_bednets = s$phi_bednets,
      seasonal = s$seasonal,
      routine = s$routine,
      itn_use = s$itn_use,
      irs_use = s$irs_use,
      itn_future = s$itn_future,
      irs_future = s$irs_future,
      lsm = s$lsm,
      scenario_tag = s$scenario_id,
      stringsAsFactors = FALSE
    )
  }))
  
  # ---- Run neural network predictions (batched) ----
  results <- list()
  
  if ("prevalence" %in% predictor) {
    if (benchmark) t_start <- Sys.time()
    
    # Run batched predictions
    prevalence_results <- run_malaria_emulator(
      scenarios = scenarios_df,
      predictor = "prevalence",
      model_types = prevalence_models,
      use_cache = use_cache,
      benchmark = benchmark
    )
    
    # Add scenario IDs
    rows_per_scn <- nrow(prevalence_results) / n_scenarios
    prevalence_results$scenario      <- rep(scenario_ids, each = rows_per_scn)
    prevalence_results$scenario_tag  <- prevalence_results$scenario
    prevalence_results$eir_valid     <- rep(eir_valid,    each = rows_per_scn)

    results$prevalence <- prevalence_results
    
    if (benchmark) {
      bench_times$run_neural_network <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
      if (!is.null(attr(prevalence_results, "benchmark"))) {
        bench_times$nn_details <- attr(prevalence_results, "benchmark")
      }
    }
  }
  
  # ---- Run cases predictions ----
  if ("cases" %in% predictor) {
    if (benchmark) t_start <- Sys.time()
    
    # Run batched predictions
    cases_results <- run_malaria_emulator(
      scenarios = scenarios_df,
      predictor = "cases",
      model_types = cases_models,
      use_cache = use_cache,
      benchmark = benchmark
    )
    
    # Add scenario IDs
    rows_per_scn <- nrow(cases_results) / n_scenarios
    cases_results$scenario      <- rep(scenario_ids, each = rows_per_scn)
    cases_results$scenario_tag  <- cases_results$scenario
    cases_results$eir_valid     <- rep(eir_valid,    each = rows_per_scn)
    results$cases <- cases_results
    
    if (benchmark) {
      bench_times$run_neural_network <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
      if (!is.null(attr(cases_results, "benchmark"))) {
        bench_times$nn_details <- attr(cases_results, "benchmark")
      }
    }
  }
  
  # ---- Add benchmark results ----
  if (benchmark) {
    bench_times$total <- as.numeric(difftime(Sys.time(), t_total_start, units = "secs"))
    bench_times$total_scenarios <- n_scenarios
    results$benchmarks <- bench_times
    
    # Print summary
    cat("\n=== Optimized Benchmark Results ===\n")
    if (!is.null(bench_times$preload_models)) {
      cat(sprintf("Pre-load models to cache: %.3f seconds\n", bench_times$preload_models))
    }
    cat(sprintf("Load EIR models: %.3f seconds%s\n", 
               bench_times$load_eir_models,
               ifelse(use_cache && bench_times$load_eir_models < 0.01, " (cached)", "")))
    
    cat(sprintf("Run EIR predictions (%d scenarios): %.3f seconds\n", 
               n_scenarios, bench_times$run_eir_models))
    
    if ("prevalence" %in% predictor) {
      cat(sprintf("Run Neural Network (%d scenarios): %.3f seconds\n", 
                 n_scenarios, bench_times$run_neural_network))
      
      if (!is.null(bench_times$nn_details)) {
        cat("\n--- Neural Network Performance ---\n")
        if (!is.null(bench_times$nn_details$model_loading)) {
          cat(sprintf("  Model loading: %.3f seconds%s\n", 
                     bench_times$nn_details$model_loading,
                     ifelse(use_cache && bench_times$nn_details$model_loading < 0.01, 
                           " (cached)", "")))
        }
        if (!is.null(bench_times$nn_details$data_prep)) {
          cat(sprintf("  Data preparation: %.3f seconds\n", 
                     bench_times$nn_details$data_prep))
        }
        if (!is.null(bench_times$nn_details$python_inference)) {
          cat(sprintf("  Python inference: %.3f seconds\n", 
                     bench_times$nn_details$python_inference))
          cat(sprintf("    - Target (10ms x %d): %.3f seconds\n", 
                     n_scenarios, 0.010 * n_scenarios))
          
          overhead <- bench_times$nn_details$python_inference - (0.010 * n_scenarios)
          if (overhead > 0) {
            cat(sprintf("    - Overhead: %.3f seconds (%.1fx slower)\n",
                       overhead,
                       bench_times$nn_details$python_inference / (0.010 * n_scenarios)))
          } else {
            cat(sprintf("    - ACHIEVED TARGET! %.1fx faster than expected\n",
                       (0.010 * n_scenarios) / bench_times$nn_details$python_inference))
          }
        }
        if (!is.null(bench_times$nn_details$r_conversion)) {
          cat(sprintf("  R data conversion: %.3f seconds\n", 
                     bench_times$nn_details$r_conversion))
        }
      }
    }
    
    if ("cases" %in% predictor) {
      cat(sprintf("Run Neural Network (%d scenarios): %.3f seconds\n", 
                 n_scenarios, bench_times$run_neural_network))
      
      if (!is.null(bench_times$nn_details)) {
        cat("\n--- Neural Network Performance ---\n")
        if (!is.null(bench_times$nn_details$model_loading)) {
          cat(sprintf("  Model loading: %.3f seconds%s\n", 
                     bench_times$nn_details$model_loading,
                     ifelse(use_cache && bench_times$nn_details$model_loading < 0.01, 
                           " (cached)", "")))
        }
        if (!is.null(bench_times$nn_details$data_prep)) {
          cat(sprintf("  Data preparation: %.3f seconds\n", 
                     bench_times$nn_details$data_prep))
        }
        if (!is.null(bench_times$nn_details$python_inference)) {
          cat(sprintf("  Python inference: %.3f seconds\n", 
                     bench_times$nn_details$python_inference))
          cat(sprintf("    - Target (10ms x %d): %.3f seconds\n", 
                     n_scenarios, 0.010 * n_scenarios))
          
          overhead <- bench_times$nn_details$python_inference - (0.010 * n_scenarios)
          if (overhead > 0) {
            cat(sprintf("    - Overhead: %.3f seconds (%.1fx slower)\n",
                       overhead,
                       bench_times$nn_details$python_inference / (0.010 * n_scenarios)))
          } else {
            cat(sprintf("    - ACHIEVED TARGET! %.1fx faster than expected\n",
                       (0.010 * n_scenarios) / bench_times$nn_details$python_inference))
          }
        }
        if (!is.null(bench_times$nn_details$r_conversion)) {
          cat(sprintf("  R data conversion: %.3f seconds\n", 
                     bench_times$nn_details$r_conversion))
        }
      }
    }
    
    cat(sprintf("\nTotal time: %.3f seconds\n", bench_times$total))
    
    cat("====================================\n\n")
  }

  results$scenario_meta <- scenario_meta
  results$eir_valid <- any(eir_valid)
  return(results)
}