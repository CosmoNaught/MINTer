#' Run Malaria Emulator
#'
#' Runs malaria emulator in either database mode or scenario mode.
#' 
#' @param db_path Path to DuckDB database (for database mode)
#' @param param_index Parameter index for database mode (NULL for random selection)
#' @param scenarios Data frame with scenario parameters (for scenario mode)
#' @param predictor "prevalence" or "cases"
#' @param models_base_dir Base directory with trained models. If NULL (default),
#'   uses models bundled with the package.
#' @param output_dir Directory to save outputs (auto-generated if NULL)
#' @param counterfactual Named list for counterfactual analysis (database mode only)
#' @param window_size Window size for rolling average
#' @param plot_tight Use tight y-axis scaling
#' @param device "cpu" or "cuda"
#' @param model_types Vector of model types to use ("GRU", "LSTM", or both)
#' @param time_steps Number of time steps for scenario predictions (in days)
#'
#' @return List with results
#' @export
run_malaria_emulator <- function(db_path = NULL,
                                param_index = NULL,
                                scenarios = NULL,
                                predictor = "prevalence",
                                models_base_dir = NULL,
                                output_dir = NULL,
                                counterfactual = NULL,
                                window_size = 14,
                                plot_tight = FALSE,
                                device = NULL,
                                model_types = c("GRU", "LSTM"),
                                time_steps = 2190) {  # 6 years default DO NOT CHANGE!

  # Validate inputs - must have either db_path or scenarios
  if (is.null(db_path) && is.null(scenarios)) {
    stop("Either 'db_path' or 'scenarios' must be provided")
  }

  if (!is.null(db_path) && !is.null(scenarios)) {
    warning("Both 'db_path' and 'scenarios' provided. Using database mode.")
    scenarios <- NULL
  }

  if (!predictor %in% c("prevalence", "cases")) {
    stop("Predictor must be either 'prevalence' or 'cases'")
  }

  valid_models <- c("GRU", "LSTM")
  if (!all(model_types %in% valid_models)) {
    stop(sprintf("Invalid model types. Must be one or more of: %s", 
                paste(valid_models, collapse = ", ")))
  }

  message("[INFO] Loading emulator models...")
  models <- load_emulator_models(models_base_dir, predictor, device)

  mode <- ifelse(!is.null(db_path), "database", "scenario")
 
  # Set default output directory based on mode
  if (is.null(output_dir)) {
    # If using bundled models, create output directory in current working directory
    if (is.null(models_base_dir)) {
      output_dir <- file.path(getwd(), predictor, 
                             ifelse(mode == "database", "emulator_predictions", "scenario_predictions"))
    } else {
      # If using custom models, create output directory relative to models directory
      output_dir <- file.path(models_base_dir, predictor, 
                             ifelse(mode == "database", "emulator_predictions", "scenario_predictions"))
    }
  }
 
  # Execute based on mode
  if (mode == "database") {
    message("[INFO] Running in database mode")
    
    # Validate counterfactual if provided
    if (!is.null(counterfactual) && !is.list(counterfactual)) {
      stop("Counterfactual must be a named list (e.g., list(eir = c(1, 10, 100)))")
    }
    
    # Select parameter if not specified
    if (is.null(param_index)) {
      params <- list_available_parameters(db_path)
      if (nrow(params) == 0) {
        stop("No parameters found in database")
      }
      param_index <- sample(params$parameter_index, 1)
      message(sprintf("[INFO] Randomly selected parameter index: %d", param_index))
    }
    
    # Run emulator in database mode
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
    
    # Add summary information
    message("\n[INFO] Summary:")
    message(sprintf("  - Mode: Database"))
    message(sprintf("  - Predictor type: %s", predictor))
    message(sprintf("  - Parameter Index: %d", results$param_index))
    message(sprintf("  - Global Index: %d", results$global_index))
    message(sprintf("  - Model types: %s", paste(model_types, collapse = ", ")))
    if (!is.null(counterfactual)) {
      message(sprintf("  - Counterfactual: %s", 
                     paste(names(counterfactual), collapse = ", ")))
    }
    message(sprintf("  - Models source: %s", 
                   ifelse(is.null(models_base_dir), "Bundled with package", models_base_dir)))
    message(sprintf("  - Output saved to: %s", output_dir))
    
    return(results)
    
  } else {
    message("[INFO] Running in scenario mode")
    
    # Validate scenarios
    if (!is.data.frame(scenarios)) {
      stop("Scenarios must be a data frame")
    }
    
    if (nrow(scenarios) == 0) {
      stop("Scenarios data frame is empty")
    }
    
    # Check for required columns
    required_cols <- models$static_covars
    missing_cols <- setdiff(required_cols, names(scenarios))
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing required columns in scenarios: %s", 
                  paste(missing_cols, collapse = ", ")))
    }
    
    # Warn if counterfactual provided in scenario mode
    if (!is.null(counterfactual)) {
      warning("Counterfactual analysis is only available in database mode. Ignoring.")
    }
    
    message(sprintf("[INFO] Processing %d scenarios", nrow(scenarios)))
    message(sprintf("[INFO] Using model types: %s", paste(model_types, collapse = ", ")))
    message(sprintf("[INFO] Generating predictions for %.1f years", time_steps/365))
    
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
    for (i in seq_along(plots)) {
      print(plots[[i]])
      if (i < length(plots) && interactive()) {
        readline(prompt = "Press [enter] to see next plot")
      }
    }

    # Create summary results
    results <- list(
      mode = "scenario",
      plots = plots,
      predictions = predictions,
      scenarios = scenarios,
      model_types = model_types,
      time_steps = time_steps,
      output_dir = output_dir
    )

    # Add summary information
    message("\n[INFO] Summary:")
    message(sprintf("  - Mode: Scenario"))
    message(sprintf("  - Predictor type: %s", predictor))
    message(sprintf("  - Number of scenarios: %d", nrow(scenarios)))
    message(sprintf("  - Model types: %s", paste(model_types, collapse = ", ")))
    message(sprintf("  - Time period: %.1f years", time_steps/365))
    message(sprintf("  - Models source: %s", 
                   ifelse(is.null(models_base_dir), "Bundled with package", models_base_dir)))
    message(sprintf("  - Output saved to: %s", output_dir))

    return(results)
  }
}

#' Create Scenario Plots (Helper Function)
#'
#' @param predictions List of predictions from generate_scenario_predictions
#' @param models List from load_emulator_models()
#' @param model_types Vector of model types that were used
#' @param window_size Window size for time scaling
#' @param output_dir Directory to save plots
#'
#' @return List of ggplot objects
#' @keywords internal
create_scenario_plots <- function(predictions, models, model_types, 
                                window_size = 14, output_dir = NULL) {
  
  # Create output directory if needed
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  plots <- list()
  
  for (i in seq_along(predictions)) {
    pred <- predictions[[i]]
    
    # Create plot data
    plot_data <- list()
    
    if ("GRU" %in% model_types && !is.null(pred$gru)) {
      plot_data[[length(plot_data) + 1]] <- data.frame(
        timestep = pred$timesteps,
        value = pred$gru,
        type = "GRU",
        simulation = "1"
      )
    }
    
    if ("LSTM" %in% model_types && !is.null(pred$lstm)) {
      plot_data[[length(plot_data) + 1]] <- data.frame(
        timestep = pred$timesteps,
        value = pred$lstm,
        type = "LSTM",
        simulation = "1"
      )
    }
    
    if (length(plot_data) == 0) {
      warning(sprintf("No predictions available for scenario %d", i))
      next
    }
    
    plot_df <- dplyr::bind_rows(plot_data)
    
    # Create parameter label (show first 3 parameters + indication if more)
    param_labels <- sapply(names(pred$parameters), function(param) {
      sprintf("%s=%.2g", param, pred$parameters[[param]])
    })
    
    param_string <- paste(param_labels[1:min(3, length(param_labels))], collapse = ", ")
    if (length(param_labels) > 3) {
      param_string <- paste0(param_string, sprintf(" (+%d more)", length(param_labels) - 3))
    }
    
    # Create plot title
    title_text <- sprintf("%s Prediction - Scenario %d\n%s",
                         ifelse(models$predictor == "prevalence", "Prevalence", "Cases per 1000"),
                         i, param_string)
    
    # Create the plot
    p <- create_unified_plot(plot_df, models$predictor, title_text, window_size, FALSE)
    
    plots[[i]] <- p
    
    # Save plot if output directory specified
    if (!is.null(output_dir)) {
      filename <- sprintf("%s_scenario_%03d_%s.png",
                         models$predictor, i,
                         paste(tolower(model_types), collapse = "_"))
      
      filepath <- file.path(output_dir, filename)
      ggplot2::ggsave(filepath, p, width = 10, height = 6, dpi = 300)
      message(sprintf("[INFO] Saved scenario %d plot to %s", i, filename))
    }
  }
  
  return(plots)
}