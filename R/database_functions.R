#' Fetch windowed (non-overlapping) data from DuckDB (1:1 with Python)
#'
#' Bins the last 6 years of each simulation into non-overlapping windows
#' of size `window_size` days, anchored at day `6*365` of the simulation.
#' For prevalence, it computes ratio-of-sums within each bin:
#'   SUM(n_detect_lm_0_1825) / SUM(n_age_0_1825)
#' For cases, it computes:
#'   1000 * SUM(n_inc_clinical_0_36500) / NULLIF(SUM(n_age_0_36500), 0)
#'
#' @param db_path Path to DuckDB database
#' @param table_name Name of table in database (default "simulation_results")
#' @param window_size Window size (days) for binning (default 14)
#' @param param_index Parameter index to fetch (NULL returns list of available)
#' @param predictor "prevalence" or "cases"
#'
#' @return Data frame with columns:
#'   parameter_index, simulation_index, global_index (numeric if parsable),
#'   timesteps (1..T window index), abs_timesteps (start day of bin),
#'   prevalence/cases, and static covariates.
#' @export
fetch_rolling_data <- function(db_path,
                               table_name   = "simulation_results",
                               window_size  = 14,
                               param_index  = NULL,
                               predictor    = "prevalence") {

  message(sprintf("[INFO] Connecting to DuckDB at %s", db_path))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # match Python defaults
  DBI::dbExecute(con, "PRAGMA memory_limit='32GB';")
  DBI::dbExecute(con, "PRAGMA threads=8;")

  if (is.null(param_index)) {
    # Return available parameter/global pairs; parse numeric global_index too
    q <- sprintf("
      SELECT DISTINCT
        parameter_index,
        -- keep original string and a parsed numeric for convenience
        global_index                                         AS global_index_str,
        TRY_CAST(REGEXP_EXTRACT(CAST(global_index AS VARCHAR), '([0-9]+)', 1) AS BIGINT)
          AS global_index
      FROM %s
      ORDER BY parameter_index
    ", table_name)
    return(DBI::dbGetQuery(con, q))
  }

  last_6_years_day <- 6 * 365
  W <- as.integer(window_size)

  if (predictor == "prevalence") {
    final_query <- sprintf("
      WITH raw AS (
        SELECT
          parameter_index,
          simulation_index,
          -- Parse digits from strings like 'simulation_results_2502.rds'
          TRY_CAST(REGEXP_EXTRACT(CAST(global_index AS VARCHAR), '([0-9]+)', 1) AS BIGINT)
            AS global_index,
          CAST(timesteps AS INTEGER)   AS abs_timesteps,
          CAST(n_detect_lm_0_1825 AS DOUBLE) AS n_detect,
          CAST(n_age_0_1825        AS DOUBLE) AS n_age,
          eir, dn0_use, dn0_future, Q0, phi_bednets,
          seasonal, routine, itn_use, irs_use,
          itn_future, irs_future, lsm
        FROM %1$s
        WHERE parameter_index = %2$d
          AND timesteps >= %3$d
      ),
      groups AS (
        SELECT
          parameter_index,
          simulation_index,
          FLOOR( (abs_timesteps - %3$d) / %4$d ) AS gid,
          -- ratio-of-sums prevalence (no rolling mean)
          SUM(n_detect) / NULLIF(SUM(n_age), 0) AS prevalence,
          MIN(abs_timesteps) AS abs_timesteps,
          MAX(global_index)  AS global_index,
          MAX(eir)           AS eir,
          MAX(dn0_use)       AS dn0_use,
          MAX(dn0_future)    AS dn0_future,
          MAX(Q0)            AS Q0,
          MAX(phi_bednets)   AS phi_bednets,
          MAX(seasonal)      AS seasonal,
          MAX(routine)       AS routine,
          MAX(itn_use)       AS itn_use,
          MAX(irs_use)       AS irs_use,
          MAX(itn_future)    AS itn_future,
          MAX(irs_future)    AS irs_future,
          MAX(lsm)           AS lsm
        FROM raw
        GROUP BY parameter_index, simulation_index, gid
      )
      SELECT
        parameter_index,
        simulation_index,
        global_index,
        ROW_NUMBER() OVER (
          PARTITION BY parameter_index, simulation_index
          ORDER BY gid
        ) AS timesteps,
        abs_timesteps,
        prevalence,
        eir, dn0_use, dn0_future, Q0, phi_bednets,
        seasonal, routine, itn_use, irs_use,
        itn_future, irs_future, lsm
      FROM groups
      ORDER BY parameter_index, simulation_index, timesteps
    ", table_name, as.integer(param_index), last_6_years_day, W)

  } else {
    final_query <- sprintf("
      WITH raw AS (
        SELECT
          parameter_index,
          simulation_index,
          TRY_CAST(REGEXP_EXTRACT(CAST(global_index AS VARCHAR), '([0-9]+)', 1) AS BIGINT)
            AS global_index,
          CAST(timesteps AS INTEGER)   AS abs_timesteps,
          CAST(n_inc_clinical_0_36500 AS DOUBLE) AS inc,
          CAST(n_age_0_36500           AS DOUBLE) AS pop,
          eir, dn0_use, dn0_future, Q0, phi_bednets,
          seasonal, routine, itn_use, irs_use,
          itn_future, irs_future, lsm
        FROM %1$s
        WHERE parameter_index = %2$d
          AND timesteps >= %3$d
      ),
      groups AS (
        SELECT
          parameter_index,
          simulation_index,
          FLOOR( (abs_timesteps - %3$d) / %4$d ) AS gid,
          1000.0 * SUM(inc) / NULLIF(SUM(pop), 0) AS cases,
          MIN(abs_timesteps) AS abs_timesteps,
          MAX(global_index)  AS global_index,
          MAX(eir)           AS eir,
          MAX(dn0_use)       AS dn0_use,
          MAX(dn0_future)    AS dn0_future,
          MAX(Q0)            AS Q0,
          MAX(phi_bednets)   AS phi_bednets,
          MAX(seasonal)      AS seasonal,
          MAX(routine)       AS routine,
          MAX(itn_use)       AS itn_use,
          MAX(irs_use)       AS irs_use,
          MAX(itn_future)    AS itn_future,
          MAX(irs_future)    AS irs_future,
          MAX(lsm)           AS lsm
        FROM raw
        GROUP BY parameter_index, simulation_index, gid
      )
      SELECT
        parameter_index,
        simulation_index,
        global_index,
        ROW_NUMBER() OVER (
          PARTITION BY parameter_index, simulation_index
          ORDER BY gid
        ) AS timesteps,
        abs_timesteps,
        cases,
        eir, dn0_use, dn0_future, Q0, phi_bednets,
        seasonal, routine, itn_use, irs_use,
        itn_future, irs_future, lsm
      FROM groups
      ORDER BY parameter_index, simulation_index, timesteps
    ", table_name, as.integer(param_index), last_6_years_day, W)
  }

  DBI::dbGetQuery(con, final_query)
}

#' List Available Parameters in Database (with parsed global_index)
#' @export
list_available_parameters <- function(db_path, table_name = "simulation_results") {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA threads=8;")
  DBI::dbGetQuery(con, sprintf("
    SELECT DISTINCT
      parameter_index,
      global_index                                         AS global_index_str,
      TRY_CAST(REGEXP_EXTRACT(CAST(global_index AS VARCHAR), '([0-9]+)', 1) AS BIGINT)
        AS global_index
    FROM %s
    ORDER BY parameter_index
  ", table_name))
}
