#' Produce Weekly Summary
#'
#' @param
#' @return dataframe of weekly summary
#' \describe{
#'   \item{starting_data}{first date of the week}
#'   \item{current_week}{Week number}
#'   \item{total_uncapped_new_cases}{Total new cases presenting each week - uncapped}
#'   \item{new_sev_cases}{Subset of total new cases presenting each week that are severe}
#'   \item{new_crit_cases}{Subset of total new cases presenting each week that are critical}
#'   \item{new_uncapped_sus_cases}{Subset of total new cases presenting each week that are suspected but negative}
#'
#'   moderate/mild: would need to call get_parameters beforehand
#'   \item{new_mild_cases}{Subset of total new cases presenting each week that are mild}
#'   \item{new_mod_cases}{Subset of total new cases presenting each week that are moderate}

#'
#'   # patients that get admitted with caps
#'   # would need to call country_capacity first
#'
#'   # uncapped sick patients per week excluding those who have been discharged/removed
#'
#'   # sick patients in beds per week (so the above with bedcaps)

#'   # patients recovering or dying from illness, per week
#'   # patients discharged from beds last week
#'   \item{year}{Starting year of 5 year group, i.e. 1960 for 1960-64, etc. From
#'   1950-2100.}
#'   \item{gender}{"both", "female", or "male"}
#'   \item{value}{Age specific mortality rate, or the central rate of
#'   mortality, mx.}
#'   \item{income_group}{World Bank country income group.}
#' draft:
#' would have the data passed in
#' when we have the excess fits, I will implement them
#'
#' @import dplyr
#' @importFrom data.table first
#' @import countrycode
#'
#' @export
weekly_summary<-function(country=NULL, iso3c = NULL, params = params,
                         data=data,
                         starting_date = NULL)
                         {
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
    iso3c <- countrycode::countrycode(country,
                                      origin = "country.name",
                                      destination = "iso3c"
    )
    data <- subset(data, data$country == country)
  } else if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(esft::who$country_code)) {
      stop("Country not found")
    }
    country <- countrycode::countrycode(iso3c,
                                      origin = "iso3c",
                                      destination = "country.name"
    )
    data <- subset(data, data$iso3c == iso3c)
  } else {
    stop("Must specify country or Iso3c country code.")
  }


  data$date <- as.Date(data$date)

  if (!is.null(starting_date)) {
    starting_date <- as.Date(starting_date)
    data <- subset(data, data$date >= starting_date)
  }

  if (is.null(params)){
    stop("Parameters must be called using get_parameters before calculating
         the weekly summary.")
  }

  data <- data %>%
    dplyr::select(-c(death_calibrated))

  data <- data %>%
    tidyr::pivot_wider(names_from=compartment,
                values_from=y_mean)

  data <- data %>%
    dplyr::group_by(week = cut(date, breaks="week")) %>%
    dplyr::summarise(date = data.table::last(date),
              hospital_demand = sum(hospital_demand, na.rm=TRUE),
              ICU_demand = sum(ICU_demand, na.rm=TRUE),
              hospital_incidence = sum(hospital_incidence, na.rm=TRUE),
              ICU_incidence = sum(ICU_incidence, na.rm=TRUE),
              # prevalence = data.table::last(prevalence),
              # Rt = data.table::last(Rt),
              # Reff = data.table::last(Reff),
              infections = data.table::last(infections),
              # deaths= sum(deaths, na.rm=TRUE),
              cumulative_infections = data.table::last(cumulative_infections))
              # cumulative_deaths = data.table::last(cumulative_deaths)) # do i take

  data <- data %>%
    dplyr::mutate(cum_severe_cases = cumsum(hospital_incidence),
           new_severe_cases = hospital_incidence,
           cum_critical_cases = cumsum(ICU_incidence),
           new_critical_cases = ICU_incidence,
           adm_severe_cases_nocap = hospital_demand,
           adm_critical_cases_nocap = ICU_demand)

  data <- data %>%
    dplyr::mutate(adm_severe_cases_cap = ifelse(hospital_incidence<params$severe_beds_covid,
                                                hospital_incidence, params$severe_beds_covid),
           adm_critical_cases_cap = ifelse(ICU_incidence<params$crit_beds_covid,
                                           ICU_incidence, params$crit_beds_covid),

           # moderate and mild cases, method in patient calcs:
           # why only severe and critical here, and not moderate?
           new_mild_cases = (new_severe_cases+new_critical_cases)*
             params$mildI_proportion/(params$sevI_proportion+params$critI_proportion),
           new_mod_cases = (new_severe_cases+new_critical_cases)*
             params$modI_proportion/(params$sevI_proportion+params$critI_proportion),

           # second method also in patient calcs:
           # what is difference between prevalence and infections here? - need to talk to greg
           # second possible method - needs review
           # this results in MUCH fewer cases estimated btw
           new_mild_cases_2 = infections*params$mildI_proportion,
           new_mod_cases_2 = infections*params$modI_proportion,
           new_severe_cases_2 = infections*params$sevI_proportion,
           new_critical_cases_2 = infections*params$critI_proportion)


  data <- data %>%
    dplyr::mutate(cum_mild_cases = cumsum(new_mild_cases),
           cum_mod_cases = cumsum(new_mod_cases))


  return(data)

}
