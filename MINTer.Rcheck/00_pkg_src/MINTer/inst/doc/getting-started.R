## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
# # Install devtools if you haven't already
# if (!requireNamespace("devtools", quietly = TRUE)) {
#   install.packages("devtools")
# }
# 
# # Install MINTer
# devtools::install_github("CosmoNaught/MINTer")

## ----eval=FALSE---------------------------------------------------------------
# library(MINTer)

## ----eval=FALSE---------------------------------------------------------------
# # Install segMINT for database functionality
# devtools::install_github("CosmoNaught/segMINT")

## ----eval=FALSE---------------------------------------------------------------
# library(MINTer)
# 
# # Create scenarios
# 
# # Create intervention scenarios
# intervention_scenarios <- create_scenarios(
#   eir = c(5.2, 35.8, 180.5),
#   dn0_use = c(0.15, 0.35, 0.55),
#   dn0_future = c(0.20, 0.45, 0.65),
#   Q0 = c(0.65, 0.75, 0.85),
#   phi_bednets = c(0.45, 0.65, 0.75),
#   seasonal = c(0, 1, 1),
#   routine = c(0, 0, 1),
#   itn_use = c(0.25, 0.55, 0.85),
#   irs_use = c(0.10, 0.35, 0.70),
#   itn_future = c(0.30, 0.60, 0.90),
#   irs_future = c(0.15, 0.40, 0.75),
#   lsm = c(0.05, 0.45, 0.85)
# )
# 
# # Run emulator
# results <- MINTer::run_malaria_emulator(
#   scenarios = scenarios,
#   predictor = 'cases',
#   output_dir = 'scenario_output',
#   model_types = c('GRU', 'LSTM')
# )

## ----eval=FALSE---------------------------------------------------------------
# library(MINTer)
# library(segMINT)  # Required for database creation
# 
# # Step 1: Create scenarios for simulation
# scenarios <- MINTer::create_malariasim_scenarios(
#   eir = c(5.2, 35.8, 180.5),
#   dn0_use = c(0.15, 0.35, 0.55),
#   dn0_future = c(0.20, 0.45, 0.65),
#   Q0 = c(0.65, 0.75, 0.85),
#   phi_bednets = c(0.45, 0.65, 0.75),
#   seasonal = c(0, 1, 1),
#   routine = c(0, 0, 1),
#   itn_use = c(0.25, 0.55, 0.85),
#   irs_use = c(0.10, 0.35, 0.70),
#   itn_future = c(0.30, 0.60, 0.90),
#   irs_future = c(0.15, 0.40, 0.75),
#   lsm = c(0.05, 0.45, 0.85)
# )
# 
# # Step 2: Run malaria simulations
# MINTer::run_malariasim(
#   max_threads = 12,
#   lhs_scenario = "Data/malariasim_scenarios.csv",
#   output_dir = "Data/Simout"
# )
# 
# # Step 3: Create database from simulation results
# segMINT::create_database(
#   dir = "/path/to/your/working/dir/Data/Database",
#   file_name = "malariasim_database.duckdb", # custom name
#   table_name = "simulation_results", # custom name
#   data_dir = "/path/to/your/working/dir/Data/Simout",
#   N = "all",
#   randomize = FALSE
# )
# 
# # Step 4: Run emulator with counterfactual analysis for your 3rd malariasimulation scenario to explore prevalence changes across a range of EIR values
# MINTer::run_malaria_emulator(
#   db_path = "/path/to/your/working/dir/Data/Database/malariasim_database.duckdb",
#   param_index = 3,
#   predictor = "prevalence",
#   counterfactual = list(eir = c(0.5, 60, 300)),
#   output_dir = "counterfactuals_output"
# )
# 

## ----eval=FALSE---------------------------------------------------------------
# # Results include:
# # - Timesteps in days (converted to years in plots)
# # - Predicted prevalence values (0-1)
# # - Comparison between GRU and LSTM models
# # - Y-axis: Proportion infected (0 = 0%, 1 = 100%)

## ----eval=FALSE---------------------------------------------------------------
# # Results include:
# # - Timesteps in 14-day intervals
# # - Cases per 1000 population per 14 days
# # - Model comparisons
# # - Y-axis: Clinical incidence rate

## ----eval=FALSE---------------------------------------------------------------
# # Check Python configuration
# reticulate::py_config()
# 
# # The package automatically initializes Python when loaded
# # If you need to reinitialize:
# MINTer::initialize_python(verbose = TRUE)

