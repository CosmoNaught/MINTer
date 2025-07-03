#' Get Runtime Parameters for Simulation
#'
#' @param i Index of parameter set
#' @param lhs_data Data frame of LHS scenarios
#' @param HUMAN_POPULATION Human population size
#' @param bednet_params Bednet parameter data
#' @param SIM_LENGTH Simulation length in days
#'
#' @return List of input parameters
#' @export
get_runtime_parameters <- function(i, lhs_data, HUMAN_POPULATION, bednet_params, SIM_LENGTH) {
  # Extract lhs_sample for the given index
  lhs_sample <- lhs_data[i, ]
  
  # Initialize simulation parameters for each row independently
  selected_seasonality <- set_seasonality(lhs_sample)
  simparams <- initialize_simulation_parameters(lhs_sample, HUMAN_POPULATION, selected_seasonality)
  
  # Assign the simulation parameters for treatment
  treatment_simparams <- simparams
  
  # Initialize parameters and lists to store timesteps
  bednet_treatment_result <- set_bednet_parameters(treatment_simparams, lhs_sample, bednet_params)
  treatment_simparams <- bednet_treatment_result$simparams
  bednet_treatment_timesteps <- bednet_treatment_result$timesteps
  
  irs_treatment_result <- set_irs_parameters(treatment_simparams, lhs_sample)
  treatment_simparams <- irs_treatment_result$simparams
  irs_treatment_timesteps <- irs_treatment_result$timesteps
  
  lsm_treatment_result <- set_lsm_parameters(treatment_simparams, lhs_sample)
  treatment_simparams <- lsm_treatment_result$simparams
  lsm_treatment_timesteps <- lsm_treatment_result$timesteps
  
  unique_bednet_timesteps <- unique(bednet_treatment_timesteps)
  unique_irs_timesteps <- unique(irs_treatment_timesteps)
  unique_lsm_timesteps <- unique(lsm_treatment_timesteps)
  
  input_entry <- list(
    MINT_parameters = lhs_sample,
    timesteps = SIM_LENGTH,
    parameters = treatment_simparams,
    treatment_timesteps = list(
      mass_bednet = c(0, 3, 6, 9) * 365,
      irs = unique_irs_timesteps,
      lsm = unique_lsm_timesteps
    )
  )
  
  return(input_entry)
}

#' Initialize Simulation Parameters
#'
#' @param lhs_sample Single row from LHS data
#' @param HUMAN_POPULATION Human population size
#' @param selected_seasonality Seasonality parameters
#'
#' @return malariasimulation parameters object
#' @export
initialize_simulation_parameters <- function(lhs_sample, HUMAN_POPULATION, selected_seasonality) {
  simparams <- malariasimulation::get_parameters(
    list(
      human_population = HUMAN_POPULATION,
      prevalence_rendering_min_ages = c(0) * 365,
      prevalence_rendering_max_ages = c(5) * 365,
      clinical_incidence_rendering_min_ages = c(0) * 365,
      clinical_incidence_rendering_max_ages = c(100) * 365,
      model_seasonality = TRUE,
      g0 = selected_seasonality$g0,
      g = selected_seasonality$g,
      h = selected_seasonality$h
    )
  )
  
  Anopheles_mosquito_params <- set_mosquito_parameters(lhs_sample)
  
  simparams <- malariasimulation::set_species(
    simparams,
    species = list(Anopheles_mosquito_params),
    proportions = c(1)
  )
  
  simparams <- malariasimulation::set_equilibrium(
    parameters = simparams,
    init_EIR = lhs_sample$eir
  )
  
  return(simparams)
}

#' Set IRS Parameters
#'
#' @param simparams Simulation parameters object
#' @param lhs_sample Single row from LHS data
#'
#' @return List with updated simparams and timesteps
#' @export
set_irs_parameters <- function(simparams, lhs_sample) {
  YEAR <- 365
  peak <- malariasimulation::peak_season_offset(simparams)
  month <- 30
  peak_season_offset_override <- TRUE
  
  if (peak_season_offset_override) {
    peak_season_time <- 0
  } else {
    peak_season_time <- peak - 3 * month
  }
  
  sprayingtimesteps <- seq(0, 12) * YEAR + peak_season_time
  
  simparams <- malariasimulation::set_spraying(
    simparams,
    timesteps = sprayingtimesteps,
    coverages = c(rep(lhs_sample$irs_use, 9), rep(lhs_sample$irs_future, 4)),
    ls_theta = matrix(2.025, nrow = length(sprayingtimesteps), ncol = 1),
    ls_gamma = matrix(-0.009, nrow = length(sprayingtimesteps), ncol = 1),
    ks_theta = matrix(-2.222, nrow = length(sprayingtimesteps), ncol = 1),
    ks_gamma = matrix(0.008, nrow = length(sprayingtimesteps), ncol = 1),
    ms_theta = matrix(-1.232, nrow = length(sprayingtimesteps), ncol = 1),
    ms_gamma = matrix(-0.009, nrow = length(sprayingtimesteps), ncol = 1)
  )
  
  list(simparams = simparams, timesteps = sprayingtimesteps)
}

#' Set LSM Parameters
#'
#' @param simparams Simulation parameters object
#' @param lhs_sample Single row from LHS data
#'
#' @return List with updated simparams and timesteps
#' @export
set_lsm_parameters <- function(simparams, lhs_sample) {
  YEAR <- 365
  lsm_coverage <- lhs_sample$lsm
  lsmtimesteps <- 9 * YEAR
  
  simparams <- simparams |> malariasimulation::set_carrying_capacity(
    carrying_capacity_scalers = matrix((1 - lsm_coverage), ncol = 1),
    timesteps = lsmtimesteps
  )
  
  list(simparams = simparams, timesteps = lsmtimesteps)
}

#' Set Seasonality Parameters
#'
#' @param lhs_sample Single row from LHS data
#'
#' @return List with g0, g, and h parameters
#' @export
set_seasonality <- function(lhs_sample) {
  seasonal <- list(
    seasonality = c(0.285505, -0.1328150, 0.0109352, 0.0139190, 0.3253520, -0.1046750, 0.0779865),
    seas_name = 'seasonal'
  )
  
  perennial <- list(
    seasonality = c(0.285277, -0.0216681, 0.0529426, 0.0073646, 0.0248801, 0.0242904, -0.0168910),
    seas_name = 'perennial'
  )
  
  if (lhs_sample$seasonal == 1) {
    selected_seasonality <- seasonal$seasonality
  } else {
    selected_seasonality <- perennial$seasonality
  }
  
  list(
    g0 = selected_seasonality[1],
    g = selected_seasonality[2:4],
    h = selected_seasonality[5:7]
  )
}

#' Set Mosquito Parameters
#'
#' @param lhs_sample Single row from LHS data
#'
#' @return List of mosquito parameters
#' @export
set_mosquito_parameters <- function(lhs_sample) {
  # Get default parameters from malariasimulation
  fun_params <- malariasimulation::gamb_params
  
  Anopheles_mosquito_params <- fun_params
  Anopheles_mosquito_params$blood_meal_rates <- 1/3
  Anopheles_mosquito_params$foraging_time <- 0.69
  Anopheles_mosquito_params$mum <- 0.132
  Anopheles_mosquito_params$phi_bednets <- lhs_sample$phi_bednets
  Anopheles_mosquito_params$phi_indoors <- lhs_sample$phi_bednets + 0.05
  Anopheles_mosquito_params$Q0 <- lhs_sample$Q0
  Anopheles_mosquito_params$species <- 'Anopheles'
  
  return(Anopheles_mosquito_params)
}