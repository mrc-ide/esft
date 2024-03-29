% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commodities_forecast.R
\name{commodities_weekly}
\alias{commodities_weekly}
\title{Commodities by week}
\usage{
commodities_weekly(
  equipment,
  patients,
  cases,
  tests,
  screening_hcws,
  hcws,
  lab_params,
  test_ratios,
  total_tests,
  params
)
}
\arguments{
\item{equipment}{From the equipment.rda file}

\item{patients}{From patients_weekly}

\item{cases}{From cases_weekly}

\item{tests}{diagnostics_weekly}

\item{screening_hcws}{screening_hcws_weekly}

\item{hcws}{hcws_weekly}

\item{lab_params}{From lab parameters}

\item{test_ratios}{This should be from test ratios}

\item{total_tests}{total_tests from that function}

\item{params}{from get_parameters}
}
\value{
Dataframe of commodities weekly
\describe{
  \item{category}{Category of item: Diagnostics, PPE, Hygiene, or Case
  management (and there are several types of case management categories)}
  \item{week_begins}{Date the week begins}
  \item{week_ends}{Date the week ends}
  \item{unit}{Unit items supplied in}
  \item{item}{Item name}
  \item{total_amount}{Total amount of that item}
}
}
\description{
Combination of diagnostics, ppe, hygiene, and case management
requirements by week. Returns total amount forecast.
}
