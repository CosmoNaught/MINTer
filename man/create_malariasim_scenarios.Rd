% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation_functions.R
\name{create_malariasim_scenarios}
\alias{create_malariasim_scenarios}
\title{Create scenarios for Malaria Simulation}
\usage{
create_malariasim_scenarios(
  eir,
  dn0_use,
  dn0_future,
  Q0,
  phi_bednets,
  seasonal,
  routine,
  itn_use,
  irs_use,
  itn_future,
  irs_future,
  lsm
)
}
\arguments{
\item{eir}{Entomological Inoculation Rate vector}

\item{dn0_use}{Current bednet effectiveness vector}

\item{dn0_future}{Future bednet effectiveness vector}

\item{Q0}{Human blood index vector}

\item{phi_bednets}{Proportion of bites on humans in bed vector}

\item{seasonal}{Seasonal transmission indicator (0/1) vector}

\item{routine}{Routine distribution indicator (0/1) vector}

\item{itn_use}{Current ITN usage vector}

\item{irs_use}{Current IRS coverage vector}

\item{itn_future}{Future ITN usage vector}

\item{irs_future}{Future IRS coverage vector}

\item{lsm}{Larval source management coverage vector}
}
\value{
A data frame of scenarios
}
\description{
Create scenarios for Malaria Simulation
}
