#' Fetch Rolling Data from DuckDB
#'
#' @param db_path Path to DuckDB database
#' @param table_name Name of table in database
#' @param window_size Window size for rolling average (days)
#' @param param_index Parameter index to fetch (NULL for list of available)
#' @param predictor "prevalence" or "cases"
#'
#' @return Data frame with results or list of available parameters
#' @export
fetch_rolling_data <- function(db_path, table_name = "simulation_results", 
                              window_size = 14, param_index = NULL, 
                              predictor = "prevalence") {
  
  message(sprintf("[INFO] Connecting to DuckDB at %s", db_path))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con))
  
  DBI::dbExecute(con, "PRAGMA threads=8;")
  
  if (is.null(param_index)) {
    query <- sprintf("SELECT DISTINCT parameter_index, global_index FROM %s ORDER BY parameter_index", table_name)
    result <- DBI::dbGetQuery(con, query)
    return(result)
  }
  
  param_where_clause <- sprintf("WHERE parameter_index = %d", param_index)
  
  distinct_sims_subquery <- sprintf("
    SELECT DISTINCT parameter_index, simulation_index, global_index
    FROM %s
    %s
  ", table_name, param_where_clause)
  
  if (predictor == "prevalence") {
    cte_subquery <- sprintf("
      SELECT
        t.parameter_index,
        t.simulation_index,
        t.global_index,
        t.timesteps,
        CASE WHEN t.n_age_0_1825 = 0 THEN NULL
             ELSE CAST(t.n_detect_lm_0_1825 AS DOUBLE) / t.n_age_0_1825
        END AS raw_prevalence,
        t.eir, t.dn0_use, t.dn0_future, t.Q0, t.phi_bednets,
        t.seasonal, t.routine, t.itn_use, t.irs_use,
        t.itn_future, t.irs_future, t.lsm
      FROM %s t
      JOIN (%s) rs
      USING (parameter_index, simulation_index)
    ", table_name, distinct_sims_subquery)
    
    preceding <- window_size - 1
    last_6_years_day <- 6 * 365
    
    final_query <- sprintf("
      WITH cte AS (%s)
      SELECT
        parameter_index,
        simulation_index,
        global_index,
        ROW_NUMBER() OVER (
          PARTITION BY parameter_index, simulation_index
          ORDER BY timesteps
        ) AS timesteps,
        AVG(raw_prevalence) OVER (
          PARTITION BY parameter_index, simulation_index
          ORDER BY timesteps
          ROWS BETWEEN %d PRECEDING AND CURRENT ROW
        ) AS prevalence,
        eir, dn0_use, dn0_future, Q0, phi_bednets,
        seasonal, routine, itn_use, irs_use,
        itn_future, irs_future, lsm
      FROM cte
      WHERE cte.timesteps >= %d
        AND (cte.timesteps %% %d) = 0
      ORDER BY parameter_index, simulation_index, timesteps
    ", cte_subquery, preceding, last_6_years_day, window_size)
    
  } else {  # cases
    cte_subquery <- sprintf("
      SELECT
        t.parameter_index,
        t.simulation_index,
        t.global_index,
        t.timesteps,
        t.n_inc_clinical_0_36500,
        t.n_age_0_36500,
        CASE WHEN t.n_age_0_36500 = 0 THEN NULL
            ELSE 1000.0 * CAST(t.n_inc_clinical_0_36500 AS DOUBLE) / t.n_age_0_36500
        END AS raw_cases,
        t.eir, t.dn0_use, t.dn0_future, t.Q0, t.phi_bednets,
        t.seasonal, t.routine, t.itn_use, t.irs_use,
        t.itn_future, t.irs_future, t.lsm
      FROM %s t
      JOIN (%s) rs
      USING (parameter_index, simulation_index)
    ", table_name, distinct_sims_subquery)
    
    last_6_years_day <- 6 * 365
    
    final_query <- sprintf("
      WITH cte AS (%s),
      timestep_groups AS (
        SELECT
          parameter_index,
          simulation_index, 
          global_index,
          FLOOR((timesteps - %d) / %d) AS group_id,
          1000.0 * SUM(n_inc_clinical_0_36500) / SUM(n_age_0_36500) AS cases,
          MAX(eir) AS eir,
          MAX(dn0_use) AS dn0_use,
          MAX(dn0_future) AS dn0_future,
          MAX(Q0) AS Q0,
          MAX(phi_bednets) AS phi_bednets,
          MAX(seasonal) AS seasonal,
          MAX(routine) AS routine,
          MAX(itn_use) AS itn_use,
          MAX(irs_use) AS irs_use,
          MAX(itn_future) AS itn_future,
          MAX(irs_future) AS irs_future,
          MAX(lsm) AS lsm
        FROM cte
        WHERE timesteps >= %d
        GROUP BY parameter_index, simulation_index, global_index, group_id
      )
      SELECT
        parameter_index,
        simulation_index,
        global_index,
        ROW_NUMBER() OVER (
          PARTITION BY parameter_index, simulation_index
          ORDER BY group_id
        ) AS timesteps,
        cases,
        eir, dn0_use, dn0_future, Q0, phi_bednets,
        seasonal, routine, itn_use, irs_use,
        itn_future, irs_future, lsm
      FROM timestep_groups
      ORDER BY parameter_index, simulation_index, group_id
    ", cte_subquery, last_6_years_day, window_size, last_6_years_day)
  }
  
  result <- DBI::dbGetQuery(con, final_query)
  return(result)
}

#' List Available Parameters in Database
#'
#' @param db_path Path to DuckDB database
#' @param table_name Name of table in database
#'
#' @return Data frame with parameter_index and global_index
#' @export
list_available_parameters <- function(db_path, table_name = "simulation_results") {
  params <- fetch_rolling_data(db_path, table_name, window_size = 14, 
                              param_index = NULL, predictor = "prevalence")
  return(params)
}