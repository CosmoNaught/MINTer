#' Lightweight Mintweb Controller for MINT Scenarios
#'
#' Simple wrapper function that calls run_minter_scenarios with default parameters
#' and returns the raw output without any processing.
#'
#' @param res_use Numeric vector of current resistance levels
#' @param res_future Numeric vector of future resistance levels after next campaign (default: NULL)
#' @param py_only Numeric vector of pyrethroid-only net proportions
#' @param py_pbo Numeric vector of pyrethroid-PBO net proportions
#' @param py_pyrrole Numeric vector of pyrethroid-pyrrole net proportions
#' @param py_ppf Numeric vector of pyrethroid-PPF net proportions
#' @param net_type_future Character vector of future net choices per scenario (default: NULL)
#' @param itn_future Numeric vector (0 - 1) of future ITN coverage per scenario (default: NULL)
#' @param prev Numeric vector of malaria prevalence levels
#' @param Q0 Numeric vector of Q0 values
#' @param phi Numeric vector of phi bednet values
#' @param season Numeric vector of seasonality indicators
#' @param routine Numeric vector of routine treatment indicators
#' @param irs Numeric vector of current IRS coverage
#' @param irs_future Numeric vector of future IRS coverage
#' @param lsm Numeric vector of LSM coverage
#' @param scenario_tag Optional character vector of scenario identifiers (default: \code{NULL})
#' @param clean_output Logical; if \code{TRUE}, remove helper columns and filter timesteps (default \code{TRUE})
#' @param tabulate Logical; if \code{TRUE}, aggregate \code{cases} into 4 quantile-like bins per scenario (default \code{TRUE})
#'
#' @return List containing prevalence and cases predictions as returned by run_minter_scenarios
#' @export
run_mintweb_controller <- function(
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
  scenario_tag = NULL,
  clean_output = TRUE,
  tabulate = TRUE
) {
  # Call the main minter function with default values for technical parameters
  results <- run_minter_scenarios(
    res_use = res_use,
    res_future = res_future,
    py_only = py_only,
    py_pbo = py_pbo,
    py_pyrrole = py_pyrrole,
    py_ppf = py_ppf,
    net_type_future = net_type_future,
    itn_future = itn_future,
    prev = prev,
    Q0 = Q0,
    phi = phi,
    season = season,
    routine = routine,
    irs = irs,
    irs_future = irs_future,
    lsm = lsm,
    # Default technical parameters
    eir_models = "xgboost",
    prevalence_models = "LSTM",
    predictor = c("prevalence", "cases"),
    year_start = 2,
    year_end = 5,
    scenario_tag = scenario_tag,
    benchmark = FALSE,
    preload_models = TRUE,
    use_cache = TRUE
  )
  
  # Clean output if flag is set
  if (clean_output) {
    # Process prevalence data
    if (!is.null(results$prevalence)) {
      results$prevalence <- results$prevalence[ave(seq_len(nrow(results$prevalence)), results$prevalence$scenario, FUN = seq_along) > 52, ]
      row.names(results$prevalence) <- NULL
      columns_to_remove <- c("index", "timestep", "model_type")
      cols_to_keep <- !names(results$prevalence) %in% columns_to_remove
      results$prevalence <- results$prevalence[, cols_to_keep, drop = FALSE]
    }
    
    # Process cases data
    if (!is.null(results$cases)) {
      columns_to_remove <- c("index", "timestep","model_type")
      cols_to_keep <- !names(results$cases) %in% columns_to_remove
      results$cases <- results$cases[, cols_to_keep, drop = FALSE]
        if (tabulate) {
          df <- results$cases
          # preserve original scenario order
          scenarios <- as.character(df$scenario)
          split_list <- split(df, factor(df$scenario, levels = unique(scenarios)))

          out <- lapply(split_list, function(d) {
            x <- as.numeric(d$cases)
            valid <- is.finite(x)
            n <- sum(valid)

            totals <- numeric(4)
            if (n > 0) {
              o <- order(x[valid], na.last = NA)
              ranks <- integer(n); ranks[o] <- seq_len(n)
              bins <- floor((ranks - 1) * 4 / n) + 1  # 1..4
              sums <- tapply(x[valid], bins, sum)
              totals[as.integer(names(sums))] <- as.numeric(sums)
            }

            data.frame(
              cases = totals,
              scenario = d$scenario[1],
              row.names = NULL
            )
          })
          results$cases <- do.call(rbind, out)
          rownames(results$cases) <- NULL
          names(results$cases)[names(results$cases) == "cases"] <- "cases_per_1000"
        }

    }
  }
  
  # Return the results
  return(results)
}