#' Run Malaria Emulator (Original Version)
#'
#' @param db_path Path to DuckDB database
#' @param param_index Parameter index (NULL for random selection)
#' @param predictor "prevalence" or "cases"
#' @param models_base_dir Base directory with trained models
#' @param output_dir Directory to save outputs
#' @param counterfactual Named list, e.g. list(eir = c(1, 10, 100))
#' @param window_size Window size for rolling average
#' @param plot_tight Use tight y-axis scaling
#' @param device "cpu" or "cuda"
#' @param model_types Vector of model types to use
#'
#' @return List with results
#' @export
#' @examples
#' \dontrun{
#' results <- run_malaria_emulator(
#'   db_path = "/path/to/database.duckdb",
#'   param_index = 10,
#'   predictor = "prevalence",
#'   counterfactual = list(eir = c(1, 10, 100))
#' )
#' }
run_malaria_emulator <- function(db_path,
                                param_index = NULL,
                                predictor = "prevalence",
                                models_base_dir = ".",
                                output_dir = NULL,
                                counterfactual = NULL,
                                window_size = 14,
                                plot_tight = FALSE,
                                device = NULL,
                                model_types = c("GRU", "LSTM")) {
  
  # Load models
  message("[INFO] Loading emulator models...")
  models <- load_emulator_models(models_base_dir, predictor, device)
  
  # Select parameter if not specified
  if (is.null(param_index)) {
    params <- list_available_parameters(db_path)
    param_index <- sample(params$parameter_index, 1)
    message(sprintf("[INFO] Randomly selected parameter index: %d", param_index))
  }
  
  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(models_base_dir, predictor, "emulator_predictions")
  }
  
  # Run emulator with model types
  results <- run_emulator(
    db_path = db_path,
    param_index = param_index,
    models = models,
    window_size = window_size,
    counterfactual = counterfactual,
    output_dir = output_dir,
    plot_tight = plot_tight,
    model_types = model_types
  )
  
  # Display plot
  print(results$plot)
  
  message("\n[INFO] Summary:")
  message(sprintf("  - Predictor type: %s", predictor))
  message(sprintf("  - Parameter Index: %d", results$param_index))
  message(sprintf("  - Global Index: %d", results$global_index))
  message(sprintf("  - Output saved to: %s", output_dir))
  
  return(results)
}

#' Run Malaria Emulator V2 (Scenarios and Database)
#'
#' @param db_path Path to DuckDB database (NULL for scenario mode)
#' @param param_index Parameter index (NULL for random or scenario mode)
#' @param scenarios Data frame with scenario parameters (used when db_path is NULL)
#' @param predictor "prevalence" or "cases"
#' @param models_base_dir Base directory with trained models
#' @param output_dir Directory to save outputs
#' @param counterfactual Named list for counterfactual analysis (only with db_path)
#' @param window_size Window size for rolling average
#' @param plot_tight Use tight y-axis scaling
#' @param device "cpu" or "cuda"
#' @param model_types Vector of model types to use ("GRU", "LSTM", or both)
#' @param time_steps Number of time steps for scenario predictions (in days)
#'
#' @return List with results
#' @export
#' @examples
#' \dontrun{
#' # Scenario mode
#' scenarios <- create_scenarios(
#'   eir = c(5.2, 35.8),
#'   dn0_use = c(0.15, 0.35),
#'   dn0_future = c(0.20, 0.45),
#'   Q0 = c(0.65, 0.75),
#'   phi_bednets = c(0.45, 0.65),
#'   seasonal = c(0, 1),
#'   routine = c(0, 0),
#'   itn_use = c(0.25, 0.55),
#'   irs_use = c(0.10, 0.35),
#'   itn_future = c(0.30, 0.60),
#'   irs_future = c(0.15, 0.40),
#'   lsm = c(0.05, 0.45)
#' )
#' 
#' results <- run_malaria_emulator_v2(
#'   scenarios = scenarios,
#'   predictor = 'prevalence',
#'   model_types = c('GRU', 'LSTM')
#' )
#' }
run_malaria_emulator_v2 <- function(db_path = NULL,
                                   param_index = NULL,
                                   scenarios = NULL,
                                   predictor = "prevalence",
                                   models_base_dir = ".",
                                   output_dir = NULL,
                                   counterfactual = NULL,
                                   window_size = 14,
                                   plot_tight = FALSE,
                                   device = NULL,
                                   model_types = c("GRU", "LSTM"),
                                   time_steps = 2190) {  # 6 years
  
  # Load models
  message("[INFO] Loading emulator models...")
  models <- load_emulator_models(models_base_dir, predictor, device)
  
  # Determine mode: database or scenario
  if (!is.null(db_path)) {
    # Database mode - use original functionality
    message("[INFO] Running in database mode")
    
    # Select parameter if not specified
    if (is.null(param_index)) {
      params <- list_available_parameters(db_path)
      param_index <- sample(params$parameter_index, 1)
      message(sprintf("[INFO] Randomly selected parameter index: %d", param_index))
    }
    
    # Set output directory
    if (is.null(output_dir)) {
      output_dir <- file.path(models_base_dir, predictor, "emulator_predictions")
    }
    
    # Run emulator with model types support
    results <- run_emulator(
      db_path = db_path,
      param_index = param_index,
      models = models,
      window_size = window_size,
      counterfactual = counterfactual,
      output_dir = output_dir,
      plot_tight = plot_tight,
      model_types = model_types
    )
    
    # Display plot
    print(results$plot)
    
    message("\n[INFO] Summary:")
    message(sprintf("  - Mode: Database"))
    message(sprintf("  - Predictor type: %s", predictor))
    message(sprintf("  - Parameter Index: %d", results$param_index))
    message(sprintf("  - Global Index: %d", results$global_index))
    message(sprintf("  - Model types: %s", paste(model_types, collapse = ", ")))
    message(sprintf("  - Output saved to: %s", output_dir))
    
    return(results)
    
  } else if (!is.null(scenarios)) {
    # Scenario mode
    message("[INFO] Running in scenario mode")
    
    # Validate scenarios
    if (!is.data.frame(scenarios)) {
      stop("Scenarios must be a data frame")
    }
    
    required_cols <- models$static_covars
    missing_cols <- setdiff(required_cols, names(scenarios))
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing required columns in scenarios: %s", 
                  paste(missing_cols, collapse = ", ")))
    }
    
    message(sprintf("[INFO] Processing %d scenarios", nrow(scenarios)))
    message(sprintf("[INFO] Using model types: %s", paste(model_types, collapse = ", ")))
    message(sprintf("[INFO] Generating predictions for %d years", time_steps/365))
    
    # Set output directory
    if (is.null(output_dir)) {
      output_dir <- file.path(models_base_dir, predictor, "scenario_predictions")
    }
    
    # Generate predictions
    predictions <- generate_scenario_predictions(
      scenarios = scenarios,
      models = models,
      model_types = model_types,
      time_steps = time_steps
    )
    
    # Create and save plots
    plots <- create_scenario_plots(
      predictions = predictions,
      models = models,
      model_types = model_types,
      window_size = window_size,
      output_dir = output_dir
    )
    
    # Display plots
    for (i in 1:length(plots)) {
      print(plots[[i]])
    }
    
    message("\n[INFO] Summary:")
    message(sprintf("  - Mode: Scenario"))
    message(sprintf("  - Predictor type: %s", predictor))
    message(sprintf("  - Number of scenarios: %d", nrow(scenarios)))
    message(sprintf("  - Model types: %s", paste(model_types, collapse = ", ")))
    message(sprintf("  - Time period: %d years", time_steps/365))
    message(sprintf("  - Output saved to: %s", output_dir))
    
    return(list(
      plots = plots,
      predictions = predictions,
      scenarios = scenarios,
      model_types = model_types
    ))
    
  } else {
    stop("Either db_path or scenarios must be provided")
  }
}

#' Create Scenario Plots
#'
#' @param predictions List of predictions from generate_scenario_predictions
#' @param models List from load_emulator_models()
#' @param model_types Vector of model types that were used
#' @param window_size Window size for time scaling
#' @param output_dir Directory to save plots
#'
#' @return List of ggplot objects
#' @export
create_scenario_plots <- function(predictions, models, model_types, 
                                window_size = 14, output_dir = NULL) {
  
  plots <- list()
  
  for (i in 1:length(predictions)) {
    pred <- predictions[[i]]
    
    # Create plot data
    plot_data <- list()
    
    if ("GRU" %in% model_types) {
      plot_data[[length(plot_data) + 1]] <- data.frame(
        timestep = pred$timesteps,
        value = pred$gru,
        type = "GRU",
        simulation = "1"  # Dummy value for consistency
      )
    }
    
    if ("LSTM" %in% model_types) {
      plot_data[[length(plot_data) + 1]] <- data.frame(
        timestep = pred$timesteps,
        value = pred$lstm,
        type = "LSTM",
        simulation = "1"  # Dummy value for consistency
      )
    }
    
    plot_df <- dplyr::bind_rows(plot_data)
    
    # Create parameter label
    param_labels <- sapply(names(pred$parameters), function(param) {
      sprintf("%s=%.2g", param, pred$parameters[[param]])
    })
    param_string <- paste(param_labels[1:3], collapse = ", ")
    if (length(param_labels) > 3) {
      param_string <- paste0(param_string, ", ...")
    }
    
    # Create unified plot
    title_text <- sprintf("%s Prediction - Scenario %d\n%s",
                         ifelse(models$predictor == "prevalence", "Prevalence", "Cases per 1000"),
                         i, param_string)
    
    p <- create_unified_plot(plot_df, models$predictor, title_text, window_size, FALSE)
    
    plots[[i]] <- p
    
    # Save plot if output directory specified
    if (!is.null(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      
      filename <- sprintf("%s_scenario_%d_%s.png",
                         models$predictor, i,
                         paste(tolower(model_types), collapse = "_"))
      
      ggplot2::ggsave(file.path(output_dir, filename), p, width = 10, height = 6)
      message(sprintf("[INFO] Saved scenario %d plot to %s", i, file.path(output_dir, filename)))
    }
  }
  
  return(plots)
}