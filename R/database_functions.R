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

#' Run Emulator for a Parameter
#'
#' @param db_path Path to DuckDB database
#' @param param_index Parameter index to analyze
#' @param models List from load_emulator_models()
#' @param window_size Window size for rolling average
#' @param counterfactual Named list with parameter name and values (optional)
#' @param output_dir Directory to save outputs
#' @param plot_tight Use tight y-axis scaling
#' @param model_types Vector of model types to use
#'
#' @return List with predictions and plot
#' @export
run_emulator <- function(db_path, 
                        param_index,
                        models,
                        window_size = 14,
                        counterfactual = NULL,
                        output_dir = NULL,
                        plot_tight = FALSE,
                        model_types = c("GRU", "LSTM")) {
  
  # Validate model types
  model_types <- match.arg(model_types, c("GRU", "LSTM"), several.ok = TRUE)
  
  # Fetch data
  df <- fetch_rolling_data(db_path, "simulation_results", window_size, 
                          param_index, models$predictor)
  
  df[models$static_covars] <- 
    lapply(df[models$static_covars], function(x) as.numeric(x))
  
  if (nrow(df) == 0) {
    stop(sprintf("No data found for parameter index %d", param_index))
  }
  
  # Get global index
  global_index <- as.numeric(unique(df$global_index)[1])
  
  message(sprintf("\n[INFO] Parameter Information:"))
  message(sprintf("  - Parameter Index: %d", param_index))
  message(sprintf("  - Global Index: %d", global_index))
  message(sprintf("  - Corresponding RDS file: simulation_results_%d.rds", global_index))
  
  # Group by simulation
  sim_groups <- split(df, df$simulation_index)
  num_sims <- length(sim_groups)
  message(sprintf("[INFO] Parameter %d has %d simulations", param_index, num_sims))
  
  # Print parameter values
  first_sim <- sim_groups[[1]]
  param_values <- first_sim[1, models$static_covars]
  message("\n[INFO] Input Parameter Values:")
  for (param_name in names(param_values)) {
    message(sprintf("  - %s: %g", param_name, param_values[[param_name]]))
  }
  
  # Import numpy
  np <- reticulate::import("numpy")
  
  # Prepare for plotting
  target_column <- ifelse(models$predictor == "prevalence", "prevalence", "cases")
  all_predictions <- list()
  cf_predictions <- list()
  
  # Create plot
  plot_data <- list()
  
  # Process each simulation
  for (sim_idx in names(sim_groups)) {
    sim_df <- sim_groups[[sim_idx]]
    sim_df <- sim_df[order(sim_df$timesteps), ]
    
    # Get data
    t <- as.numeric(sim_df$timesteps)
    y_true <- as.numeric(sim_df[[target_column]])
    
    # For consistency, always use actual timesteps (not scaled)
    t_plot <- t
    
    # Add actual data to plot
    plot_data[[length(plot_data) + 1]] <- data.frame(
      timestep = t_plot,
      value = y_true,
      type = "Actual",
      simulation = sim_idx
    )
    
    # Prepare inputs
    static_vals <- as.numeric(sim_df[1, models$static_covars])
    static_vals_scaled <- models$static_scaler$transform(matrix(static_vals, nrow = 1))
    
    # Build input features
    T_len <- nrow(sim_df)
    
    if (models$use_cyclical_time) {
      if (models$predictor == "cases") {
        day_of_year <- (t * window_size) %% 365
      } else {
        day_of_year <- t %% 365
      }
      
      sin_t <- sin(2 * pi * day_of_year / 365)
      cos_t <- cos(2 * pi * day_of_year / 365)
      
      X_full <- matrix(0, nrow = T_len, ncol = 2 + length(models$static_covars))
      X_full[, 1] <- sin_t
      X_full[, 2] <- cos_t
      X_full[, 3:ncol(X_full)] <- rep(static_vals_scaled, each = T_len)
    } else {
      t_norm <- (t - min(t)) / (max(t) - min(t))
      X_full <- matrix(0, nrow = T_len, ncol = 1 + length(models$static_covars))
      X_full[, 1] <- t_norm
      X_full[, 2:ncol(X_full)] <- rep(static_vals_scaled, each = T_len)
    }
    
    # Get predictions using Python
    X_full_np <- np$array(X_full, dtype = np$float32)
    
    if ("GRU" %in% model_types) {
      y_gru <- reticulate::py$predict_full_sequence(models$gru_model, X_full_np, models$device)
    }
    if ("LSTM" %in% model_types) {
      y_lstm <- reticulate::py$predict_full_sequence(models$lstm_model, X_full_np, models$device)
    }
    
    # Store predictions
    for (i in 1:length(t)) {
      pred_item <- list(
        parameter_index = param_index,
        simulation_index = as.numeric(sim_idx),
        global_index = as.numeric(global_index),
        timestep = t_plot[i],
        true_value = y_true[i]
      )
      
      if ("GRU" %in% model_types) {
        pred_item$gru_prediction <- y_gru[i]
      }
      if ("LSTM" %in% model_types) {
        pred_item$lstm_prediction <- y_lstm[i]
      }
      
      all_predictions[[length(all_predictions) + 1]] <- pred_item
    }
    
    # Only plot first simulation's predictions
    if (sim_idx == names(sim_groups)[1]) {
      if ("GRU" %in% model_types) {
        plot_data[[length(plot_data) + 1]] <- data.frame(
          timestep = t_plot,
          value = y_gru,
          type = "GRU",
          simulation = sim_idx
        )
      }
      
      if ("LSTM" %in% model_types) {
        plot_data[[length(plot_data) + 1]] <- data.frame(
          timestep = t_plot,
          value = y_lstm,
          type = "LSTM", 
          simulation = sim_idx
        )
      }
      
      # Run counterfactual if requested
      if (!is.null(counterfactual)) {
        cf_param_name <- names(counterfactual)[1]
        cf_values <- counterfactual[[1]]
        
        for (cf_val in cf_values) {
          # Modify parameter value
          cf_static_vals <- static_vals
          param_idx <- which(models$static_covars == cf_param_name)
          cf_static_vals[param_idx] <- cf_val
          
          # Scale and build features
          cf_static_scaled <- models$static_scaler$transform(matrix(cf_static_vals, nrow = 1))
          
          if (models$use_cyclical_time) {
            X_cf <- X_full
            X_cf[, 3:ncol(X_cf)] <- rep(cf_static_scaled, each = T_len)
          } else {
            X_cf <- X_full
            X_cf[, 2:ncol(X_cf)] <- rep(cf_static_scaled, each = T_len)
          }
          
          # Get counterfactual predictions
          X_cf_np <- np$array(X_cf, dtype = np$float32)
          
          if ("GRU" %in% model_types) {
            cf_gru <- reticulate::py$predict_full_sequence(models$gru_model, X_cf_np, models$device)
            plot_data[[length(plot_data) + 1]] <- data.frame(
              timestep = t_plot,
              value = cf_gru,
              type = sprintf("GRU %s=%g", cf_param_name, cf_val),
              simulation = sim_idx
            )
          }
          
          if ("LSTM" %in% model_types) {
            cf_lstm <- reticulate::py$predict_full_sequence(models$lstm_model, X_cf_np, models$device)
            plot_data[[length(plot_data) + 1]] <- data.frame(
              timestep = t_plot,
              value = cf_lstm,
              type = sprintf("LSTM %s=%g", cf_param_name, cf_val),
              simulation = sim_idx
            )
          }
        }
      }
    }
  }
  
  # Combine plot data
  plot_df <- dplyr::bind_rows(plot_data)
  
  # Create unified plot
  title_text <- sprintf("%s - Parameter Index = %d | Global Index = %d\n(%d Simulations)",
                       ifelse(models$predictor == "prevalence", "Prevalence", "Cases per 1000"),
                       param_index, global_index, num_sims)
  
  p <- create_unified_plot(plot_df, models$predictor, title_text, window_size, plot_tight)
  
  # Save outputs if directory specified
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Save plot
    plot_filename <- sprintf("%s_parameter_%d_global_%d_predictions%s.png",
                           models$predictor, param_index, global_index,
                           ifelse(plot_tight, "_tight", ""))
    ggplot2::ggsave(file.path(output_dir, plot_filename), p, width = 10, height = 6)
    
    # Save predictions
    pred_df <- dplyr::bind_rows(all_predictions)
    write.csv(pred_df, file.path(output_dir, 
                                sprintf("%s_parameter_%d_global_%d_predictions.csv",
                                       models$predictor, param_index, global_index)))
    
    message(sprintf("[INFO] Saved outputs to %s", output_dir))
  }
  
  return(list(
    plot = p,
    predictions = dplyr::bind_rows(all_predictions),
    param_index = param_index,
    global_index = global_index
  ))
}