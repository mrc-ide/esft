#' Produce Weekly Summary of Cases
#'
#' @description
#' Note: since proportion of mild and moderate are the same, the estimates of
#' these categories will be the same.
#'
#' @param params
#' @param data
#' @param starting_date
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{hospital_demand}{xyz}
#'   \item{ICU_demand}{xyz}
#'   \item{hospital_incidence}{xyz}
#'   \item{ICU_incidence}{xyz}
#'   \item{infections}{xyz}
#'   \item{cumulative_infections}{xyz}
#'   \item{cum_severe_cases}{xyz}
#'   \item{new_severe_cases}{xyz}
#'   \item{cum_critical_cases}{xyz}
#'   \item{new_critical_cases}{xyz}
#'   \item{adm_severe_cases_nocap}{xyz}
#'   \item{adm_critical_cases_nocap}{xyz}
#'   \item{adm_severe_cases_cap}{xyz}
#'   \item{adm_critical_cases_cap}{xyz}
#'   \item{new_mild_cases}{xyz}
#'   \item{new_mod_cases}{xyz}
#'   \item{new_mild_cases_2}{xyz}
#'   \item{new_mod_cases_2}{xyz}
#'   \item{new_severe_cases_2}{xyz}
#'   \item{new_critical_cases_2}{xyz}
#'   \item{cum_mild_cases}{xyz}
#'   \item{cum_mod_cases}{xyz}
#'   \item{rem_mild_cases}{xyz}
#'   \item{rem_mod_cases}{xyz}
#'   \item{rem_severe_cases}{xyz}
#'   \item{rem_critical_cases}{xyz}
#'   \item{cum_rem_mild_cases}{xyz}
#'   \item{cum_rem_mod_cases}{xyz}
#'   \item{cum_rem_severe_cases}{xyz}
#'   \item{cum_rem_critical_cases}{xyz}
#'}
#' @import dplyr
#' @import data.table
#' @import countrycode
#'
#' @export
cases_weekly<-function(params,
                         data,
                         starting_date = "2019-11-29")
                         {
# add exists part here
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
    dplyr::group_by(week_begins = cut(date, breaks="week")) %>%
    dplyr::summarise(week_ends = data.table::last(date),
              hospital_demand = max(hospital_demand, na.rm=TRUE),
              ICU_demand = max(ICU_demand, na.rm=TRUE),
              hospital_incidence = sum(hospital_incidence, na.rm=TRUE),
              ICU_incidence = sum(ICU_incidence, na.rm=TRUE),
              infections = data.table::last(infections),
              cumulative_infections = data.table::last(cumulative_infections))

  data$week_begins <- as.Date(as.character(data$week_begins))

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
           cum_mod_cases = cumsum(new_mod_cases),
           rem_mild_cases = data.table::shift(new_mild_cases,
                                              n=params$stay_mild),
           rem_mod_cases = data.table::shift(new_mod_cases,
                                             n=params$stay_mod),
           rem_severe_cases = data.table::shift(new_severe_cases,
                                                n=params$stay_sev),
           rem_critical_cases = data.table::shift(new_critical_cases,
                                                  n=params$stay_crit))

  data <- data %>%
    dplyr::mutate(cum_rem_mild_cases = data.table::shift(cum_mild_cases,
                                                         n=params$stay_mild),
                  cum_rem_mod_cases = data.table::shift(cum_mod_cases,
                                                        n=params$stay_mod),
                  cum_rem_severe_cases = cum_severe_cases - adm_severe_cases_nocap,
                  cum_rem_critical_cases = cum_critical_cases - adm_critical_cases_nocap)

  data[is.na(data)] <- 0

  return(data)

}
