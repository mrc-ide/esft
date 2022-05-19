#' Gets country capacity.
#'
#' Maybe add labs here?
#'
#' @param country
#' @param iso3c
#' @param overrides
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

    population <- esft::who$population[who$country_name == country]
    yoy_growth <- esft::population$yoy_growth[population$country_wb == country]
    income_group <- esft::who$income_group[who$country_name == country]

    n_hcws <- esft::who$doctors[who$country_name == country] +
      esft::who$nurses[who$country_name == country]

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
    population <- esft::who$population[who$country_code == iso3c]
    yoy_growth <- esft::population$yoy_growth[population$country_code == iso3c]
    income_group <- esft::who$income_group[who$country_code == iso3c]

    n_hcws <- esft::who$doctors[who$country_code == iso3c] +
      esft::who$nurses[who$country_code == iso3c]

    n_hosp_beds <- esft::who$beds_total[esft::who$country_code == iso3c]
    perc_beds_crit_covid <- esft::who$perc_icu_beds[esft::who$country_code == iso3c]

    country <- countrycode::countrycode(iso3c,
                                        origin = "iso3c",
                                        destination = "country.name"
    )
  }

  if (is.null(perc_beds_not_covid)) {
    perc_beds_not_covid <- 0.4
  }
  if (is.null(perc_beds_sev_covid)) {
    perc_beds_sev_covid <- 1 - perc_beds_not_covid - perc_beds_crit_covid
  }

  country_capacity <- list(
    country = country,
    iso3c = iso3c,
    population = population,
    yoy_growth = yoy_growth,
    income_group = income_group,
    n_hcws = n_hcws,
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
