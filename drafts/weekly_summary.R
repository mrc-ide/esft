#' Produce Weekly Summary
#' @param
#' @return dataframe of weekly summary
#' \describe{
#'   \item{starting_data}{first date of the week}
#'   \item{current_week}{Week number}
#'   \item{total_uncapped_new_cases}{Total new cases presenting each week - uncapped}
#'   \item{new_uncapped_sev_cases}{Subset of total new cases presenting each week that are severe}
#'   \item{new_uncapped_crit_cases}{Subset of total new cases presenting each week that are critical}
#'   \item{new_uncapped_sus_cases}{Subset of total new cases presenting each week that are suspected but negative}
#'
#'   moderate/mild: would need to call get_parameters beforehand
#'   \item{new_uncapped_mild_cases}{Subset of total new cases presenting each week that are mild}
#'   \item{new_uncapped_mod_cases}{Subset of total new cases presenting each week that are moderate}

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
#' @export
weekly_summary<-function(country=NULL, iso3c = NULL, params = params,
                         capacity = country_capacity, starting_date = NULL,
                         forecast_length = NULL, delivery_leadtime = NULL,
                         data=data)
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


    starting_date <- as.Date("2022-01-02")
    afg_data$date <- as.Date(afg_data$date)

    afg_data <- afg_data %>%
      select(c(date, compartment, y_mean, scenario, iso3c, country))

    afg_data <- afg_data %>%
      pivot_wider(names_from=compartment,
                  values_from=y_mean)

    afg_data_test<-afg_data %>%
      group_by(week = cut(date, breaks="week")) %>%
      summarise(date = last(date),
                hospital_demand = sum(hospital_demand, na.rm=TRUE),
                ICU_demand = sum(ICU_demand, na.rm=TRUE),
                hospital_incidence = sum(hospital_incidence, na.rm=TRUE),
                ICU_incidence = sum(ICU_incidence, na.rm=TRUE),
                prevalence = last(prevalence),
                Rt = last(Rt),
                Reff = last(Reff),
                infections = last(infections),
                deaths= sum(deaths, na.rm=TRUE),
                cumulative_infections = last(cumulative_infections),
                cumulative_deaths = last(cumulative_deaths)) # do i take

    afg_data_test <- afg_data_test %>%
      mutate(cum_severe_cases = cumsum(hospital_incidence),
             new_severe_cases = hospital_incidence,
             cum_critical_cases = cumsum(ICU_incidence),
             new_critical_cases = ICU_incidence,
             adm_severe_cases_nocap = hospital_demand,
             adm_critical_cases_nocap = ICU_demand)
  }
}
