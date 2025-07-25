#' Create scenarios for Malaria Simulation
#'
#' @param eir Entomological Inoculation Rate vector
#' @param dn0_use Current bednet effectiveness vector
#' @param dn0_future Future bednet effectiveness vector
#' @param Q0 Human blood index vector
#' @param phi_bednets Proportion of bites on humans in bed vector
#' @param seasonal Seasonal transmission indicator (0/1) vector
#' @param routine Routine distribution indicator (0/1) vector
#' @param itn_use Current ITN usage vector
#' @param irs_use Current IRS coverage vector
#' @param itn_future Future ITN usage vector
#' @param irs_future Future IRS coverage vector
#' @param lsm Larval source management coverage vector
#'
#' @return A data frame of scenarios
#' @export
create_malariasim_scenarios <- function(eir, dn0_use, dn0_future, Q0, phi_bednets,
                                        seasonal, routine, itn_use, irs_use,
                                        itn_future, irs_future, lsm) {
  
  # Check that all vectors have the same length
  vector_lengths <- c(
    length(eir), length(dn0_use), length(dn0_future),
    length(Q0), length(phi_bednets), length(seasonal),
    length(routine), length(itn_use), length(irs_use),
    length(itn_future), length(irs_future), length(lsm)
  )
  
  if (length(unique(vector_lengths)) > 1) {
    stop("All input vectors must have the same length. Current lengths: ",
         paste(vector_lengths, collapse = ", "))
  }
  
  # Create scenarios by combining vectors element-wise
  scenarios <- data.frame(
    eir = eir,
    dn0_use = dn0_use,
    dn0_future = dn0_future,
    Q0 = Q0,
    phi_bednets = phi_bednets,
    seasonal = seasonal,
    routine = routine,
    itn_use = itn_use,
    irs_use = irs_use,
    itn_future = itn_future,
    irs_future = irs_future,
    lsm = lsm,
    stringsAsFactors = FALSE
  )
  
  # Create Data directory if it doesn't exist
  if (!dir.exists("Data")) {
    dir.create("Data")
  }
  
  # Write to CSV file
  utils::write.csv(scenarios, file = "Data/malariasim_scenarios.csv", row.names = FALSE)
  
  # Print summary information
  message("Created ", nrow(scenarios), " scenarios in Data/malariasim_scenarios.csv")
  message("Columns: ", ncol(scenarios))
  
  # Return the dataframe
  return(scenarios)
}

#' Run Malaria Simulations
#'
#' Run malaria simulations for all parameter sets in the LHS scenarios file.
#' This function handles dependency checking, parallel processing, and progress tracking.
#'
#' @param max_threads Maximum number of parallel workers to use
#' @param lhs_scenario Path to LHS scenarios CSV file (default: "Data/malariasim_scenarios.csv")
#' @param bednet_params_path Path to bednet parameters RDS file. If NULL (default),
#'   uses the bednet parameters bundled with the package.
#' @param output_dir Directory to save simulation outputs (default: "Data")
#' @param reps Number of replicates per parameter set (default: 8)
#' @param human_population Human population size (default: 100000)
#' @param sim_years Number of years to simulate (default: 12)
#'
#' @return A list with simulation results and metadata
#' @export
run_malariasim <- function(max_threads = 12,
                           lhs_scenario = "Data/malariasim_scenarios.csv",
                           bednet_params_path = NULL,
                           output_dir = NULL, #"Data"
                           reps = 8,
                           human_population = 100000, # DO NOT CHANGE!
                           sim_years = 12) { # DO NOT CHANGE!
  
  # Set random seed for reproducibility
  set.seed(123)
  
  # Constants
  YEAR <- 365
  SIM_LENGTH <- sim_years * YEAR
  HUMAN_POPULATION <- human_population
  
  # Enable progress bars
  progressr::handlers(global = TRUE)
  
  # Helper function for safe core count
  safe_ncores <- function(requested) {
    avail <- parallel::detectCores(logical = FALSE)
    max(1L, min(requested, max_threads, avail))
  }
  
  # Handle bednet parameters path
  if (is.null(bednet_params_path)) {
    # Use bundled bednet parameters
    bednet_params_path <- system.file("extdata", "bednet_params_raw.RDS", package = "MINTer")
    
    # If not found in installed package, check development directory
    if (bednet_params_path == "" || !file.exists(bednet_params_path)) {
      if (file.exists("inst/extdata/bednet_params_raw.RDS")) {
        bednet_params_path <- "inst/extdata/bednet_params_raw.RDS"
        message("[INFO] Using bednet parameters from development directory")
      } else {
        stop("Could not find bundled bednet parameters. Please ensure MINTer is properly installed ",
             "with bednet_params_raw.RDS, or specify a custom path with bednet_params_path parameter.")
      }
    } else {
      message("[INFO] Using bundled bednet parameters")
    }
  }
  
  # Load input data
  message("Loading bednet parameters...\n")
  if (!file.exists(bednet_params_path)) {
    stop("Bednet parameters file not found: ", bednet_params_path)
  }
  bednet_params <- readRDS(bednet_params_path)
  
  message("Loading malariasim scenarios...\n")
  if (!file.exists(lhs_scenario)) {
    stop("LHS scenarios file not found: ", lhs_scenario)
  }
  lhs_data <- fread(lhs_scenario)
  
  param_index <- nrow(lhs_data)
  message(sprintf("Parameter sets: %d | Replicates: %d\n", param_index, reps))
  
  # Build parameter list
  message("Generating parameters...\n")
  progress_param <- utils::txtProgressBar(min = 0, max = param_index, style = 3)
  param_list <- lapply(seq_len(param_index), function(ps) {
    utils::setTxtProgressBar(progress_param, ps)
    get_runtime_parameters(ps, lhs_data, HUMAN_POPULATION,
                           bednet_params, SIM_LENGTH)
  })
  close(progress_param)
  message("\n")
  
  # Set up parallel processing
  workers_total <- safe_ncores(max_threads)
  future::plan(future::multisession, workers = workers_total)
  
  # Create job grid
  job_grid <- expand.grid(set = seq_len(param_index),
                          rep = seq_len(reps))
  num_jobs <- nrow(job_grid)
  total_ticks <- num_jobs * 2 
  
  # Main parallel execution with progress
  message("Running simulations...\n")
  with_progress({
    p <- progressor(steps = total_ticks)
    
    job_results <- future_lapply(seq_len(num_jobs), function(k) {
      job <- job_grid[k, ]
      i   <- job$set
      j   <- job$rep
      
      param_item <- param_list[[i]]
      
      p(message = sprintf("set %d / rep %d - started", i, j))
      
      # Run a single replicate
      res <- tryCatch({
        result <- malariasimulation::run_simulation(
          param_item$timesteps,
          param_item$parameters
        )
        result
      }, error = function(e) {
        structure(list(message = e$message), class = "simulation_error")
      })
      
      
      p(message = sprintf("set %d / rep %d - done", i, j))
      
      list(set = i, rep = j, result = res,
           success = !inherits(res, "simulation_error"))
    }, future.seed = TRUE)
  })
  
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  results <- vector("list", param_index)
  failed  <- data.frame(parameter_set = numeric(0), rep = numeric(0))
  
  for (i in seq_len(param_index)) {
    subset_rows <- vapply(job_results, function(x) x$set == i, logical(1))
    replica_objs <- job_results[subset_rows]
    
    # Extract just the results
    repl_results <- lapply(replica_objs, `[[`, "result")
    failed_reps  <- which(!vapply(replica_objs, `[[`, logical(1), "success"))
    
    file_path <- file.path(output_dir, sprintf("simulation_results_%d.rds", i))
    saveRDS(list(input = param_list[[i]], outputs = repl_results), file = file_path)
    
    results[[i]] <- list(
      status        = if (length(failed_reps)) "partial" else "success",
      parameter_set = i,
      failed_reps   = failed_reps,
      filename      = file_path
    )
    
    if (length(failed_reps)) {
      failed <- rbind(
        failed,
        data.frame(parameter_set = i,
                   rep = failed_reps)
      )
    }
  }
  
  # Summary output
  for (res in results) {
    if (res$status == "success") {
      cli::cli_alert_success("Set {res$parameter_set} finished: {res$filename}")
    } else {
      cli::cli_alert_warning(
        "Set {res$parameter_set} finished with {length(res$failed_reps)} failed reps: {res$filename}"
      )
    }
  }
  
  # Save failed runs info
  if (nrow(failed) > 0) {
    utils::write.csv(failed, file.path(output_dir, "failed.csv"), row.names = FALSE)
  }
  
  message("\nSimulation complete!")
  message(sprintf("Results saved in %d file(s)", param_index))
  if (nrow(failed)) {
    message(sprintf("Failed runs: %d (see failed.csv)", nrow(failed)))
  } else {
    message("All runs completed successfully!")
  }
  
  # Return summary
  invisible(list(
    results = results,
    failed = failed,
    param_list = param_list,
    output_dir = output_dir,
    workers_used = workers_total
  ))
}

#' Run Malaria Simulation with Local Cluster
#'
#' @param input Input parameters list
#' @param reps Number of replicates
#'
#' @return List of simulation results
#' @export
local_cluster_malariasim_controller <- function(input, reps) {
  cl <- parallel::makeCluster(max(1, reps - 1))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, {
    library(malariasimulation)
  })
  
  parallel::clusterExport(cl, c("input"), envir = environment())
  
  task_fun <- function(k, input) {
    result <- malariasimulation::run_simulation(
      input$timesteps,
      input$parameters
    )
    return(result)
  }
  
  results <- parallel::parLapply(cl, seq_len(reps), task_fun, input = input)
  
  output <- list()
  output$input <- input
  output$input$parameters <- NULL
  output$outputs <- results
  
  return(output)
}

#' Run Malaria Simulation Locally
#'
#' @param input Input parameters list
#' @param reps Number of replicates
#'
#' @return List of simulation results
#' @export
local_malariasim_controller <- function(input, reps) {
  results <- lapply(1:reps, function(k) {
    tryCatch({
      result <- malariasimulation::run_simulation(
        input$timesteps,
        input$parameters
      )
      return(result)
    }, error = function(e) {
      return(structure(
        list(
          message = e$message,
          trace = as.character(e),
          error = e
        ),
        class = c("rrq_task_error", "error", "condition")
      ))
    })
  })
  
  
  output <- list()
  output$input <- input
  output$input$parameters <- NULL
  output$outputs <- results  # List of results directly
  
  return(output)
}