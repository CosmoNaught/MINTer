#' MINTer: Malaria INTervention Emulator Runner
#'
#' A comprehensive package for malaria intervention simulation and neural network-based
#' emulation. MINTer combines the power of malariasimulation with trained GRU/LSTM 
#' models to provide rapid predictions of intervention outcomes.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{create_lhs_scenarios}}: Create Latin Hypercube Sampling scenarios
#'   \item \code{\link{run_malaria_emulator}}: Run emulator on database parameters
#'   \item \code{\link{run_malaria_emulator_v2}}: Run emulator on scenarios or database
#'   \item \code{\link{local_cluster_malariasim_controller}}: Run simulations in parallel
#' }
#'
#' @section Workflow:
#' 1. Create scenarios using \code{create_lhs_scenarios()}
#' 2. Run simulations using \code{local_cluster_malariasim_controller()}
#' 3. Create database from results (if needed)
#' 4. Run emulator for rapid predictions using \code{run_malaria_emulator_v2()}
#'
#' @section Python Dependencies:
#' This package requires Python with numpy, torch, and pandas installed.
#' Use \code{initialize_python()} to set up Python dependencies.
#'
#' @keywords internal
"_PACKAGE"
#' @name MINTer
#' @importFrom reticulate import py_run_string py_eval source_python py_module_available
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom ggplot2 ggplot aes geom_line geom_vline scale_x_continuous labs theme_minimal theme_bw theme element_rect ylim ggsave
#' @importFrom dplyr filter bind_rows
#' @importFrom jsonlite fromJSON
#' @importFrom malariasimulation get_parameters set_species set_equilibrium set_bednets set_spraying set_carrying_capacity run_simulation peak_season_offset gamb_params
#' @importFrom data.table fread
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' @importFrom progressr progressor handlers with_progress
NULL