#' @keywords internal
"_PACKAGE"

## usethis namespace: start
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
#' @importFrom rlang .data
## usethis namespace: end
NULL