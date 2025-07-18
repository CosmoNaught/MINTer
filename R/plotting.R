
#' Create Scenario Plots from Emulator Results
#'
#' Creates plots for each scenario from emulator results dataframe.
#'
#' @param results Data frame from run_malaria_emulator
#' @param output_dir Directory to save plots (optional)
#' @param plot_tight Use tight y-axis scaling (default FALSE)
#'
#' @return List of ggplot objects
#' @export
create_scenario_plots <- function(results, output_dir = NULL, plot_tight = FALSE) {
  
  # Extract metadata from attributes
  predictor <- attr(results, "predictor")
  scenarios <- attr(results, "scenarios")
  model_types <- attr(results, "model_types")
  window_size <- attr(results, "window_size")
  mode <- attr(results, "mode")
  param_index <- attr(results, "param_index")
  global_index <- attr(results, "global_index")
  parameters <- attr(results, "parameters")
  counterfactual <- attr(results, "counterfactual")
  
  # If attributes are missing, try to infer
  if (is.null(predictor)) {
    if ("prevalence" %in% names(results)) {
      predictor <- "prevalence"
    } else if ("cases" %in% names(results)) {
      predictor <- "cases"
    } else {
      stop("Cannot determine predictor type from results")
    }
  }
  
  if (is.null(window_size)) {
    window_size <- 14  # default
  }
  
  # Get unique scenarios
  unique_indices <- unique(results$index)
  
  # Create output directory if needed
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  plots <- list()
  
  # Handle database mode differently
  if (!is.null(mode) && mode == "database") {
    # Filter to reasonable timesteps
    MAX_STEPS <- 156
    results <- dplyr::filter(results, timestep <= MAX_STEPS)
    
    # Prepare plot data
    plot_df <- data.frame(
      timestep = results$timestep,
      value = results[[predictor]],
      type = results$model_type,
      simulation = "1"
    )
    
    # Create title
    title_text <- sprintf("%s - Parameter Index = %d | Global Index = %d",
                         ifelse(predictor == "prevalence", "Prevalence", "Cases per 1000"),
                         param_index, global_index)
    
    if (!is.null(parameters)) {
      param_string <- paste(sprintf("%s=%.2g", names(parameters)[1:min(3, length(parameters))], 
                                   parameters[1:min(3, length(parameters))]), collapse = ", ")
      if (length(parameters) > 3) {
        param_string <- paste0(param_string, sprintf(" (+%d more)", length(parameters) - 3))
      }
      title_text <- paste0(title_text, "\n", param_string)
    }
    
    # Create the plot
    p <- create_unified_plot(plot_df, predictor, title_text, window_size, plot_tight)
    
    plots[[1]] <- p
    
    # Save plot if output directory specified
    if (!is.null(output_dir)) {
      filename <- sprintf("%s_parameter_%d_global_%d_%s%s.png",
                         predictor, param_index, global_index,
                         paste(tolower(model_types), collapse = "_"),
                         ifelse(!is.null(counterfactual), "_counterfactual", ""))
      filepath <- file.path(output_dir, filename)
      ggplot2::ggsave(filepath, p, width = 10, height = 6, dpi = 300)
      message(sprintf("[INFO] Saved plot to %s", filename))
    }
    
  } else {
    # Handle scenario mode
    for (idx in unique_indices) {
      # Filter data for this scenario
      scenario_data <- results[results$index == idx, ]
      
      # Prepare plot data
      plot_df <- data.frame(
        timestep = scenario_data$timestep,
        value = scenario_data[[predictor]],
        type = scenario_data$model_type,
        simulation = "1"
      )
      
      # Create parameter label if scenarios available
      if (!is.null(scenarios) && idx <= nrow(scenarios)) {
        scenario_params <- scenarios[idx, ]
        param_labels <- sapply(names(scenario_params), function(param) {
          sprintf("%s=%.2g", param, scenario_params[[param]])
        })
        param_string <- paste(param_labels[1:min(3, length(param_labels))], collapse = ", ")
        if (length(param_labels) > 3) {
          param_string <- paste0(param_string, sprintf(" (+%d more)", length(param_labels) - 3))
        }
      } else {
        param_string <- sprintf("Scenario %d", idx)
      }
      
      # Create plot title
      title_text <- sprintf("%s Prediction - %s",
                           ifelse(predictor == "prevalence", "Prevalence", "Cases per 1000"),
                           param_string)
      
      # Create the plot using the existing function
      p <- create_unified_plot(plot_df, predictor, title_text, window_size, plot_tight)
      
      plots[[idx]] <- p
      
      # Save plot if output directory specified
      if (!is.null(output_dir)) {
        # Get model types for this scenario
        scenario_models <- unique(scenario_data$model_type)
        
        filename <- sprintf("%s_scenario_%03d_%s.png",
                           predictor, idx,
                           paste(tolower(scenario_models), collapse = "_"))
        filepath <- file.path(output_dir, filename)
        ggplot2::ggsave(filepath, p, width = 10, height = 6, dpi = 300)
        message(sprintf("[INFO] Saved scenario %d plot to %s", idx, filename))
      }
    }
  }
  
  return(plots)
}


#' Create Unified Plot for Emulator Results
#'
#' @param plot_data Data frame with timestep, value, type columns
#' @param predictor "prevalence" or "cases"
#' @param title_text Title for the plot
#' @param window_size Window size for time scaling
#' @param plot_tight Use tight y-axis scaling
#'
#' @return ggplot object
#' @export
create_unified_plot <- function(plot_data, predictor, title_text, 
                               window_size = 14, plot_tight = FALSE) {
  MAX_STEPS <- 156
  # Convert timesteps to years
  plot_data <- dplyr::filter(plot_data, timestep <= MAX_STEPS)
  
  # Every timestep represents `window_size` days, no matter which predictor
  plot_data$years <- (plot_data$timestep * window_size) / 365
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = years, y = value, color = type)) +
    ggplot2::geom_vline(xintercept = 3, linetype = "dashed", color = "black", alpha = 0.5) +
    ggplot2::geom_line(data = dplyr::filter(plot_data, type == "Actual"), 
              ggplot2::aes(group = simulation), alpha = 0.5, linetype = "dashed") +
    ggplot2::geom_line(data = dplyr::filter(plot_data, type != "Actual"), 
              linewidth = 1.2) +
    ggplot2::scale_x_continuous(
      breaks = 0:6,
      limits = c(0, 6),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = title_text,
      x = "Years",
      y = ifelse(predictor == "prevalence", "Prevalence", "Cases per 1000"),
      color = "Model"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  # Set y-axis limits
  if (!plot_tight && predictor == "prevalence") {
    p <- p + ggplot2::ylim(0, 1)
  }
  
  return(p)
}