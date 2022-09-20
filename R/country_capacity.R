#' @title Gets country capacity.
#'
#' @param country Country name
#' @param iso3c Country code, in iso3c format
#' @param overrides a named list of parameter values to use instead of defaults
#'
#' @return List of country name, iso3c code, and population.
#' @export
get_country_capacity <- function(country = NULL,
                                 iso3c = NULL,
                                 overrides = list()) {

  if (!is.null(country) && !is.null(iso3c)) {
    # check they are the same one using the countrycodes
    iso3c_check <- countrycode::countrycode(country,
                                            origin = "country.name",
                                            destination = "iso3c"
    )
    country_check <- countrycode::countrycode(iso3c,
                                              origin = "iso3c",
                                              destination = "country.name"
    )
    if (iso3c_check != iso3c & country_check != country) {
      stop("Iso3c country code and country name do not match. Please check
           input/spelling and try again.")
    }
  }

  ## country route
  if (!is.null(country)) {
    country <- as.character(country)
    if (!country %in% unique(esft::who$country_name)) {
      stop("Country not found")
    }

    pop <- esft::who$population[esft::who$country_name == country]
    yoy_growth <- esft::population$yoy[esft::population$country_wb == country]
    income_group <- esft::who$income_group[esft::who$country_name == country]

    n_hcws <- esft::who$doctors[esft::who$country_name == country] +
      esft::who$nurses[esft::who$country_name == country]
    n_labs <- esft::who$labs[esft::who$country_name == iso3c]

    n_hosp_beds <- esft::who$beds_total[esft::who$country_name == country]
    perc_beds_crit_covid <- esft::who$perc_icu_beds[esft::who$country_name == country]

    iso3c <- countrycode::countrycode(country,
                                      origin = "country.name",
                                      destination = "iso3c"
    )
  }

  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(esft::who$country_code)) {
      stop("Iso3c not found")
    }
    pop <- esft::who$population[esft::who$country_code == iso3c]
    yoy_growth <- esft::population$yoy[esft::population$country_code == iso3c]
    income_group <- esft::who$income_group[esft::who$country_code == iso3c]

    n_hcws <- esft::who$doctors[esft::who$country_code == iso3c] +
      esft::who$nurses[esft::who$country_code == iso3c]
    n_labs <- esft::who$labs[esft::who$country_code == iso3c]

    n_hosp_beds <- esft::who$beds_total[esft::who$country_code == iso3c]
    perc_beds_crit_covid <- esft::who$perc_icu_beds[esft::who$country_code == iso3c]

    country <- countrycode::countrycode(iso3c,
                                        origin = "iso3c",
                                        destination = "country.name"
    )
  }

  perc_beds_crit_covid <- perc_beds_crit_covid/100
  perc_beds_not_covid <- 0.4
  perc_beds_sev_covid <- 1 - perc_beds_not_covid - perc_beds_crit_covid

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
    perc_beds_sev_covid = perc_beds_sev_covid
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(country_capacity))) {
      stop(paste('unknown parameter', name, sep=' '))
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

#' @title Gets beds
#'
#' @param country_capacity a named list produced by get_country_capacity
#' @param overrides a named list of parameter values to use instead of defaults
#'
#' @return List of bed counts by category
#' @export
get_beds <- function(country_capacity, overrides = list()) {

  n_hosp_beds <- country_capacity$n_hosp_beds
  perc_beds_not_covid <- country_capacity$perc_beds_not_covid
  perc_beds_sev_covid <- country_capacity$perc_beds_sev_covid
  perc_beds_crit_covid <- country_capacity$perc_beds_crit_covid

  beds <- list(
    beds_covid = round(n_hosp_beds*(1-perc_beds_not_covid)),
    severe_beds_covid = round(n_hosp_beds*perc_beds_sev_covid),
    crit_beds_covid = round(n_hosp_beds*perc_beds_crit_covid))

  for (name in names(overrides)) {
    if (!(name %in% names(beds))) {
      stop(paste('unknown parameter', name, sep=' '))
    }
    beds[[name]] <- overrides[[name]]
  }

  return(beds)
}
