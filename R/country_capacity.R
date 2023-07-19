#' @title Gets country capacity.
#'
#' @param iso3c Country code, in iso3c format (in all capital letters)
#' @param overrides a named list of parameter values to use instead of defaults
#'
#' @return List of country capacity parameters.
#' \describe{
#'   \item{country}{Country name}
#'   \item{iso3c}{Country code, iso3c format}
#'   \item{population}{Population estimates from the United Nations Development
#'   Programme}
#'   \item{yoy_growth}{Year over year growth of country population}
#'   \item{income_group}{Income group}
#'   \item{n_hcws}{Number of HCWs (sum nurses + doctors, from HWFE data)}
#'   \item{n_labs}{Number of lab staff (from HWFE data)}
#'   \item{n_hosp_beds}{Est beds per country, from World Bank}
#'   \item{perc_beds_crit_covid}{Percent beds for ICU patients per country,
#'   estimated from World Bank/Imperial College Report}
#'   \item{perc_beds_not_covid}{Percent beds not allocated to COVID, default
#'   = 0.4}
#'   \item{perc_beds_sev_covid}{Percent beds allocated to severe COVID- cases,
#'   1 - (perc_beds_crit_covid + perc_beds_not_covid)}
#'   \item{beds_covid}{Number of hospital beds allocated to COVID (num hospital
#'   beds * perc allocated to covid)}
#'   \item{severe_beds_covid}{Number of severe beds allocated to COVID}
#'   \item{crit_beds_covid}{Number of critical beds allocated to COVID}
#' }
#'
#' @source \url{https://population.un.org/wup/Download/}
#' @source \url{https://www.who.int/publications/i/item/WHO-2019-nCoV-Tools-Essential_forecasting-2022.1}
#' @source \url{https://data.worldbank.org/indicator/SH.MED.BEDS.ZS}
#' @source Imperial College, Report 12: The Global Impact of COVID-19 and
#' Strategies for Mitigation and Suppression
#' @source \url{https://www.researchgate.net/figure/Health-Workforce-Estimator-HWFE-tool-applies-the-WISN-approach-to-caring-for-COVID_fig3_358174845}
#' @source \url{https://apps.who.int/gho/data/node.main.HWFGRP?lang=en}
#' @export
get_country_capacity <- function(iso3c = NULL,
                                 overrides = list()) {
  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(esft::who$country_code)) {
      stop("Iso3c not found")
    }
    country <- countrycode::countrycode(iso3c,
      origin = "iso3c",
      destination = "country.name"
    )
  }

  pop <- esft::who$population[esft::who$country_code == iso3c]
  yoy_growth <- esft::population$yoy[esft::population$country_code == iso3c]
  income_group <- esft::who$income_group[esft::who$country_code == iso3c]

  n_hcws <- esft::who$doctors[esft::who$country_code == iso3c] +
    esft::who$nurses[esft::who$country_code == iso3c]
  # theres an if clause here that will if the above doesnt exist, take the
  # capped numbers in back calculations
  # which are the parameters dedicated to covid * sum of doctors and nurses,
  # and params screening * sum docs and nurses
  n_labs <- esft::who$labs[esft::who$country_code == iso3c]

  n_hosp_beds <- esft::who$beds_total[esft::who$country_code == iso3c]
  perc_beds_crit_covid <- esft::who$perc_icu_beds[
    esft::who$country_code == iso3c
  ]

  perc_beds_crit_covid <- perc_beds_crit_covid / 100
  perc_beds_not_covid <- 0.4
  perc_beds_sev_covid <- 1 - perc_beds_not_covid - perc_beds_crit_covid

  beds_covid <- round(n_hosp_beds * (1 - perc_beds_not_covid))
  severe_beds_covid <- round(n_hosp_beds * perc_beds_sev_covid)
  crit_beds_covid <- round(n_hosp_beds * perc_beds_crit_covid)

  country_capacity <- list(
    country = country,
    iso3c = iso3c,
    population = pop,
    yoy_growth = yoy_growth,
    income_group = income_group,
    n_hcws = n_hcws,
    n_labs = n_labs,
    n_hosp_beds = n_hosp_beds,
    perc_beds_crit_covid = perc_beds_crit_covid,
    perc_beds_not_covid = perc_beds_not_covid,
    perc_beds_sev_covid = perc_beds_sev_covid,
    beds_covid = beds_covid,
    severe_beds_covid = severe_beds_covid,
    crit_beds_covid = crit_beds_covid
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(country_capacity))) {
      stop(paste("unknown parameter", name, sep = " "))
    }
    country_capacity[[name]] <- overrides[[name]]
  }

  perc_beds <- c(
    country_capacity$perc_beds_crit_covid,
    country_capacity$perc_beds_not_covid,
    country_capacity$perc_beds_sev_covid
  )

  if (!approx_sum(perc_beds, 1)) {
    stop("Bed allocation percentages do not sum to 1")
  }

  return(country_capacity)
}
