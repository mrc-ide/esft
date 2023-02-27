#' @title Commodities by week - combination of diagnostics, ppe, hygiene, treatment by week
#' plus gives total
#' maybe i should add in total cost here?
#'
#' I want to add in leadtime some other way. I think
#'
#' @description
#' Theres the issue now that I think the amount left depends on reusability
#' So i think by row
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
commodities_weekly <- function(params, equipment, weekly_summary) {

  # maybe add in leadtime here?
  # and get period for which forecasted for
  # like subset weekly summary here ?
  hygiene <- hygiene_weekly(equipment, hcws, patients, cases, tests,
                            screening_hcws)

  case_management <- case_management_weekly(params, equipment, weekly_summary)

  ppe <- ppe_weekly(params, equipment, weekly_summary)
  diagnostic_supplies <- diagnostic_supplies_weekly(params, equipment, weekly_summary)

  # maybe, get totals?
  commodities <- rbind(case_management, hygiene, ppe, diagnostics_supplies)
}

#' @title Hygiene weekly
#'
#' @description
#'
#' @param equipment This should be the data frame of equipment need
#' @param hcws HCWs_weekly
#' @param patients patients_weekly
#' @param cases cases_weekly
#' @param tests diagnostics_weekly
#' @param screening_hcws screening_hcws_weekly
#'
#' @import dplyr
#' @return Dataframe of Hygiene Commodities Forecast
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{screening_hcw_uncapped}{Number of HCWs required for screening/triage,
#'   based on tests needed for outpatients (i.e. suspected negatives, mild and
#'   moderate cases) per day, and number of cases one HCW can screen a day}
#'   \item{screening_hcw_capped}{Number of HCWs allocated to screening/triage:
#'   minimum of uncapped HCWs as calculated above and number of HCWs available/
#'   reported per country and the percentage of HCWs allocated to screening,
#'   which is a parameter that can be modified in get_parameters}
#' }
#' @export
hygiene_weekly <- function(equipment, hcws, patients, cases, tests,
                           screening_hcws) {

  equipment <- equipment %>%
    dplyr::mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    )
  # unnecessary, but helps me think
  hygiene <- subset(equipment, equipment$group == "Hygiene")

  reusable_multiplier <- ifelse(hygiene$reusable == TRUE, 1, 7)


  hygiene[, c(8:24)] <- hygiene[, c(8:24)] * reusable_multiplier

  amounts <- merge(hcws, hygiene)
  amounts <- merge(amounts, tests)
  amounts <- merge(amounts, patients)
  amounts <- merge(amounts, screening_hcws)

  amounts <- amounts %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(
      amount_inpatient_hcw = hcws_inpatient_capped * amount_per_inpatient_hcw_per_day +
        cleaners_inpatient_capped * amount_per_inpatient_cleaner_per_day +
        inf_caregivers_hosp_uncapped * amount_per_inpatient_inf_caregiver_per_day +
        amb_personnel_inpatient_capped * amount_per_inpatient_ambworker_per_day +
        bio_eng_inpatient_capped * amount_per_inpatient_biomed_eng_per_day,
      amount_inpatient_patient = total_beds_inuse * amount_per_inpatient_sev_crit_patient_per_day +
        sev_beds_inuse * amount_per_inpatient_sev_patient_per_day +
        crit_beds_inuse * amount_per_inpatient_crit_patient_per_day,
      amount_isolation = ifelse(
        reusable == TRUE,
        inf_caregivers_isol_uncapped * params$stay_mild +
          tests_mild * params$stay_mild +
          tests_mod * params$stay_mod,
        inf_caregivers_isol_uncapped * amount_per_isolation_inf_caregiver_per_day * params$stay_mild +
          tests_mild * params$stay_mild * amount_per_isolation_patient_per_day +
          tests_mod * params$stay_mod * amount_per_isolation_patient_per_day
      ),
      amount_screening_hcw = ifelse(
        reusable == TRUE,
        (ifelse(amount_per_screening_hcw_per_day > 0,
          screening_hcw_capped, 0
        ) +
          ifelse(amount_per_screening_patient_per_day > 0,
            tests_mild + tests_mod
          )),
        (screening_hcw_capped * amount_per_screening_hcw_per_day +
          tests_mod * amount_per_screening_patient_per_day * params$stay_mod +
          tests_mild * amount_per_screening_patient_per_day * params$stay_mild
        )
      ),
      amount_lab = ifelse(
        reusable == TRUE,
        (ifelse(amount_per_lab_tech_per_day > 0,
                lab_staff_capped, 0) +
           ifelse(amount_per_lab_cleaner_per_day > 0,
                  cleaners_lab, 0)),
        (lab_staff_capped*amount_per_lab_tech_per_day +
           cleaners_lab*amount_per_lab_cleaner_per_day)
      )
      ) %>%
    dplyr::select(c(item, week_begins, week_ends, amount_inpatient_hcw,
             amount_inpatient_patient,amount_isolation, amount_screening_hcw,
             amount_lab
             ))

  amounts$total_amount <- rowSums(amounts[,c(4:8)])
  amounts$category <- "hygiene"

  return(amounts)
}

#' @title Case management weekly: accessories, consumables, and biomedical equipment
#'
#' @description
#'
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param cases I'm not sure whether to use the patients or cases dataframe
#'
#'
#' @export
case_management_forecast <- function(params, equipment, patients) {

  equipment <- equipment %>%
    dplyr::mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    )
  # unnecessary, but helps me think
  case <- subset(equipment, startsWith(equipment$category,"Case management"))

  reusable_multiplier <- ifelse(case$reusable == TRUE, 1, 7)


  case[, c(8:24)] <- case[, c(8:24)] * reusable_multiplier

  amounts <- merge(case, patients[,c("week_begins", "week_ends", "sev_beds_inuse",
                                    "crit_beds_inuse", "total_beds_inuse")])

  # although this looks like it will double count, in actuality,
  # there are only parameters for either patients or beds, not both
  amounts <- amounts %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(
      demand_sev_patient = sev_beds_inuse*amount_per_inpatient_sev_patient_per_day +
          sev_beds_inuse*amount_per_inpatient_sev_bed_per_day,
      demand_crit_patient = crit_beds_inuse*amount_per_inpatient_crit_patient_per_day +
        crit_beds_inuse*amount_per_inpatient_crit_bed_per_day,
      demand_sev_crit_patient = total_beds_inuse*amount_per_inpatient_sev_crit_patient_per_day +
        total_beds_inuse*amount_per_inpatient_sev_crit_bed_per_day
      )

  # order by item and week begins, for the cumulative function
  # amounts <- amounts[
  #   with(amounts, order(item, week_begins)),
  # ]

  # need to add part about first week here -
  first_amounts <- amounts %>%
    arrange(item, week_begins) %>%
    group_by(item) %>%
    slice(which.min(week_begins))

  first_amounts <- first_amounts %>%
    mutate(amount_sev_patient = ceiling(demand_sev_patient),
           amount_crit_patient = ceiling(demand_crit_patient),
           amount_sev_crit_patient = ceiling(demand_sev_crit_patient))

  amounts <- subset(amounts, amounts$week_begins != min(first_amounts$week_begins))
  amounts <- full_join(first_amounts, amounts)
  amounts <- amounts[
      with(amounts, order(item, week_begins)),
    ]

  # this has to be a row wise operation
  # of if reusable, this weeks amount = demand - previous demand
  amounts$cum_demand_sev_patient <- ave(amounts$demand_sev_patient, amounts$item, FUN=cumsum)
  amounts$cum_demand_crit_patient <- ave(amounts$demand_crit_patient, amounts$item, FUN=cumsum)
  amounts$cum_demand_sev_crit_patient <- ave(amounts$demand_sev_crit_patient, amounts$item, FUN=cumsum)

   amounts <- amounts %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(
      amount_sev_patient = ifelse(
        reusable == TRUE & week_begins != min(week_begins),
        max((demand_sev_patient - cum_demand_sev_patient), 0),
        demand_sev_patient),
      amount_crit_patient = ifelse(
        reusable == TRUE & week_begins != min(week_begins),
        max((demand_crit_patient - cum_demand_crit_patient), 0),
        demand_crit_patient),
      amount_sev_crit_patient = ifelse(
        reusable == TRUE & week_begins != min(week_begins),
        max((demand_sev_crit_patient - cum_demand_sev_crit_patient),0),
        demand_sev_crit_patient)
    )# %>%

   # there is a max term in here, a max of
  # if reusable:
  # and then find the max of 0 and the demand - sum of demand so far
  # if not reusable: just take the sumsof the above
}

#' @title PPE need weekly
#'
#' @description
#'
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
ppe_forecast <- function(params, equipment, weekly_summary) {
  ppe <- subset(equipment, equipment$group == "PPE")
}
#' @title Diagnostics week
#'
#' @description
#'
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
diagnostics_forecast <- function(params, equipment, weekly_summary) {

}
