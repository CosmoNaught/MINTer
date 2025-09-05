#' Create Scenario Plots from Emulator Results
#'
#' Creates plots for scenarios from emulator results dataframe.
#' Can create either individual plots per scenario or combined plot.
#'
#' @param results Data frame with columns: index, timestep, prevalence/cases, model_type, scenario
#' @param output_dir Directory to save plots (optional)
#' @param plot_type "individual", "combined", or "both" (default "both")
#' @param predictor "prevalence" or "cases" (auto-detected if NULL)
#' @param window_size Days per timestep (default 14)
#' @param plot_tight Use tight y-axis scaling (default FALSE)
#'
#' @return List of ggplot objects
#' @export
create_scenario_plots <- function(results, 
                                  output_dir = NULL, 
                                  plot_type = "both",
                                  predictor = NULL,
                                  window_size = 14,
                                  plot_tight = FALSE) {
  
  library(ggplot2)
  library(dplyr)
  
  # Auto-detect predictor if not specified
  if (is.null(predictor)) {
    if ("prevalence" %in% names(results)) {
      predictor <- "prevalence"
    } else if ("cases" %in% names(results)) {
      predictor <- "cases"
    } else {
      stop("Cannot determine predictor type. Please specify 'prevalence' or 'cases'.")
    }
  }
  
  # Create output directory if needed
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Calculate timestep range for years 2-6
  # Year 2 starts at day 730, which is timestep 730/14 ≈ 52.14
  # Year 6 ends at day 2190, which is timestep 2190/14 ≈ 156.43
  start_timestep <- ceiling(730 / window_size)  # 53
  end_timestep <- floor(2190 / window_size)     # 156
  
  # Filter to years 2-6 and prepare data
  plot_data <- results %>%
    filter(timestep >= start_timestep & timestep <= end_timestep) %>%
    mutate(
      # Convert timesteps to years, then shift so year 2 becomes year 0
      years_raw = (timestep * window_size) / 365,
      years = years_raw - 2  # Shift so year 2 becomes 0
    )
  
  # Check if scenario column exists
  has_scenarios <- "scenario" %in% names(plot_data)
  
  if (!has_scenarios) {
    # If no scenario column, treat each index as a scenario
    plot_data$scenario <- paste0("Scenario_", plot_data$index)
  }
  
  plots <- list()
  
  # Create combined plot if requested
  if (plot_type %in% c("combined", "both")) {
    p_combined <- create_combined_scenario_plot(
      plot_data, 
      predictor, 
      plot_tight
    )
    
    plots[["combined"]] <- p_combined
    
    # Save combined plot
    if (!is.null(output_dir)) {
      filename <- sprintf("%s_all_scenarios_combined.png", predictor)
      filepath <- file.path(output_dir, filename)
      ggsave(filepath, p_combined, width = 12, height = 8, dpi = 300)
      message(sprintf("[INFO] Saved combined plot to %s", filename))
    }
  }
  
  # Create individual plots if requested
  if (plot_type %in% c("individual", "both")) {
    unique_scenarios <- unique(plot_data$scenario)
    
    for (scen in unique_scenarios) {
      scenario_data <- filter(plot_data, scenario == scen)
      
      p_individual <- create_individual_scenario_plot(
        scenario_data,
        scen,
        predictor,
        plot_tight
      )
      
      plots[[scen]] <- p_individual
      
      # Save individual plot
      if (!is.null(output_dir)) {
        # Clean scenario name for filename
        clean_name <- gsub("[^A-Za-z0-9_-]", "_", scen)
        filename <- sprintf("%s_scenario_%s.png", predictor, clean_name)
        filepath <- file.path(output_dir, filename)
        ggsave(filepath, p_individual, width = 10, height = 6, dpi = 300)
        message(sprintf("[INFO] Saved %s plot to %s", scen, filename))
      }
    }
  }
  
  return(plots)
}


#' Create Combined Plot with All Scenarios
#'
#' @param plot_data Filtered data frame with years column
#' @param predictor "prevalence" or "cases"
#' @param plot_tight Use tight y-axis scaling
#'
#' @return ggplot object
create_combined_scenario_plot <- function(plot_data, predictor, plot_tight = FALSE) {
  
  # Define color palette for different scenarios
  n_scenarios <- length(unique(plot_data$scenario))
  colors <- scales::hue_pal()(n_scenarios)
  
  # Create y-axis label
  y_label <- ifelse(predictor == "prevalence", "Prevalence", "Cases per 1000")
  
  # Base plot
  p <- ggplot(plot_data, aes(x = years, y = .data[[predictor]], 
                             color = scenario, linetype = model_type)) +
    geom_line(size = 1.2, alpha = 0.8) +
    scale_x_continuous(
      name = "Years",
      breaks = 0:4,
      labels = 0:4,
      limits = c(0, 4),
      expand = c(0.02, 0.02)
    ) +
    labs(
      title = sprintf("All Scenarios - %s Predictions", y_label),
      subtitle = "Years 2-6 of simulation (displayed as 0-4)",
      y = y_label,
      color = "Scenario",
      linetype = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      axis.title = element_text(size = 11),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9)
    )
  
  # Add vertical line at year 1 (year 3 in original timeline) if it exists
  if (1 >= 0 && 1 <= 4) {
    p <- p + geom_vline(xintercept = 1, linetype = "dotted", 
                       color = "gray50", alpha = 0.5)
  }
  
  # Set y-axis limits based on predictor
  if (!plot_tight) {
    if (predictor == "prevalence") {
      p <- p + scale_y_continuous(limits = c(0, 1), labels = scales::percent_format())
    } else {
      # For cases, use data-driven limits
      y_max <- max(plot_data[[predictor]], na.rm = TRUE) * 1.1
      p <- p + scale_y_continuous(limits = c(0, y_max))
    }
  } else {
    # Tight scaling
    if (predictor == "prevalence") {
      p <- p + scale_y_continuous(labels = scales::percent_format())
    }
  }
  
  # Adjust legend based on number of scenarios
  if (n_scenarios > 6) {
    p <- p + guides(color = guide_legend(ncol = 2))
  }
  
  return(p)
}


#' Create Individual Scenario Plot
#'
#' @param scenario_data Filtered data for one scenario
#' @param scenario_name Name of the scenario
#' @param predictor "prevalence" or "cases"
#' @param plot_tight Use tight y-axis scaling
#'
#' @return ggplot object
create_individual_scenario_plot <- function(scenario_data, scenario_name, 
                                           predictor, plot_tight = FALSE) {
  
  # Create y-axis label
  y_label <- ifelse(predictor == "prevalence", "Prevalence", "Cases per 1000")
  
  # Define colors for model types
  model_colors <- c("GRU" = "#E41A1C", "LSTM" = "#377EB8", "Actual" = "black")
  
  p <- ggplot(scenario_data, aes(x = years, y = .data[[predictor]], 
                                 color = model_type)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = model_colors, breaks = names(model_colors)) +
    scale_x_continuous(
      name = "Years",
      breaks = 0:4,
      labels = 0:4,
      limits = c(0, 4),
      expand = c(0.02, 0.02)
    ) +
    labs(
      title = sprintf("%s - %s Prediction", scenario_name, y_label),
      subtitle = "Years 2-6 of simulation (displayed as 0-4)",
      y = y_label,
      color = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      axis.title = element_text(size = 11),
      legend.title = element_text(size = 10, face = "bold")
    )
  
  # Add vertical line at year 1
  if (1 >= 0 && 1 <= 4) {
    p <- p + geom_vline(xintercept = 1, linetype = "dotted", 
                       color = "gray50", alpha = 0.5)
  }
  
  # Set y-axis limits
  if (!plot_tight) {
    if (predictor == "prevalence") {
      p <- p + scale_y_continuous(limits = c(0, 1), labels = scales::percent_format())
    } else {
      y_max <- max(scenario_data[[predictor]], na.rm = TRUE) * 1.1
      p <- p + scale_y_continuous(limits = c(0, y_max))
    }
  } else {
    if (predictor == "prevalence") {
      p <- p + scale_y_continuous(labels = scales::percent_format())
    }
  }
  
  return(p)
}


#' Quick plot function for interactive use
#'
#' @param csv_file Path to CSV file with emulator results
#' @param output_dir Directory to save plots (optional)
#' @param plot_type "individual", "combined", or "both" (default "both")
#'
#' @export
plot_emulator_results <- function(csv_file, output_dir = NULL, plot_type = "both") {
  
  # Read CSV
  results <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  # Create plots
  plots <- create_scenario_plots(
    results = results,
    output_dir = output_dir,
    plot_type = plot_type
  )
  
  # Return plots for further manipulation if needed
  invisible(plots)
}