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