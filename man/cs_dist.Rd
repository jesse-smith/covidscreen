% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_dist.R
\name{cs_dist}
\alias{cs_dist}
\title{Calculate the Steady-State Joint Distribution for a Population}
\usage{
cs_dist(
  vac = list(p_comm = 0.5, p_org = 0.5, eff = 0.5),
  inf = list(p_incid = 0.005, t_symp = 5, t_presymp = 5),
  symp = list(p_inf_vac = 0.5, p_inf_unvac = 0.5, p_uninf = 0),
  test = list(p_symp = 1, p_asymp_vac = 0, p_asymp_unvac = 1/7),
  detect = list(sens = 0.85, spec = 1)
)
}
\arguments{
\item{vac}{\code{[list(3)]} A named list containing vaccination parameters:
\describe{
  \item{p_comm \code{[numeric(1)]}}{Proportion vaccinated in the community}
  \item{p_org \code{[numeric(1)]}}{Proportion vaccinated in the organization of interest}
  \item{eff \code{[numeric(1)]}}{Vaccine efficacy}
}}

\item{inf}{\code{[list(3)]} A named list containing infection parameters:
\describe{
  \item{p_incid \code{[numeric(1)]}}{Proportion of community newly infected each day}
  \item{t_symp \code{[numeric(1)]}}{Duration of symptomatic period}
  \item{t_presymp \code{[numeric(1)]}}{Duration of presymptomatic period}
}}

\item{symp}{\code{[list(3)]} A named lust containing symptom parameters:
\describe{
  \item{p_inf_vac \code{[numeric(1)]}}{Proportion of vaccinated infections who are symptomatic}
  \item{p_inf_unvac \code{[numeric(1)]}}{Proportion of unvaccinated infections who are symptomatic}
  \item{p_uninf \code{[numeric(1)]}}{Proportion of uninfected people who are symptomatic}
}}

\item{test}{\code{[list(3)]} A named list containing testing parameters:
\describe{
  \item{p_symp \code{[numeric(1)]}}{Probability of being tested if symptomatic}
  \item{p_asymp_vac \code{[numeric(1)]}}{Probability of being tested if asymptomatic and vaccinated}
  \item{p_asymp_unvac \code{[numeric(1)]}}{Probability of being tested if asymptomatic and unvaccinated}
}}

\item{detect}{\code{[list(2)]} A named list containing detection parameters:
\describe{
  \item{sens \code{[numeric(1)]}}{Test sensitivity}
  \item{spec \code{[numeric(1)]}}{Test specificity}
}}
}
\value{
A \code{data.table}
}
\description{
\code{cs_dist()} calculates the discrete joint distribution of vaccination,
infection, symptoms, tests, and detections in a population.
}
