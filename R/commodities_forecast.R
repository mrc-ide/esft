#' @title Commodities by week - combination of diagnostics, ppe, hygiene, treatment by week
#' plus gives total
#'
#' @description
#'
#' @param equipment From the equipment.rda file
#' @param patients From patients_weekly
#' @param cases From cases_weekly
#' @param tests diagnostics_weekly
#' @param screening_hcws screening_hcws_weekly
#' @param hcws hcws_weekly
#' @param lab_params From lab parameters
#' @param test_ratios This should be from test ratios
#' @param total_tests total_tests from that function
#' @param params from get_parameters
#'
#'
#' @export
commodities_weekly <- function(equipment, patients, cases, tests,
                               screening_hcws, hcws, lab_params, test_ratios,
                               total_tests, params) {
  hygiene <- hygiene_forecast(
    equipment, hcws, patients, cases, tests,
    screening_hcws
  )

  case_management <- case_management_forecast(params, equipment, patients)

  ppe <- ppe_forecast(
    equipment, hcws, patients, cases, tests,
    screening_hcws
  )
  diagnostic_supplies <- diagnostics_forecast(
    lab_params, equipment, test_ratios,
    total_tests, patients
  )

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
#' @export
hygiene_forecast <- function(equipment, hcws, patients, cases, tests,
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
      amount_inpatient_hcw = hcws_inpatient_capped *
        amount_per_inpatient_hcw_per_day +
        cleaners_inpatient_capped * amount_per_inpatient_cleaner_per_day +
        inf_caregivers_hosp_uncapped *
        amount_per_inpatient_inf_caregiver_per_day +
        amb_personnel_inpatient_capped *
        amount_per_inpatient_ambworker_per_day +
        bio_eng_inpatient_capped * amount_per_inpatient_biomed_eng_per_day,
      amount_inpatient_patient = total_beds_inuse *
        amount_per_inpatient_sev_crit_patient_per_day +
        sev_beds_inuse * amount_per_inpatient_sev_patient_per_day +
        crit_beds_inuse * amount_per_inpatient_crit_patient_per_day,
      amount_isolation = ifelse(
        reusable == TRUE,
        inf_caregivers_isol_uncapped * params$stay_mild +
          tests_mild * params$stay_mild +
          tests_mod * params$stay_mod,
        inf_caregivers_isol_uncapped *
          amount_per_isolation_inf_caregiver_per_day * params$stay_mild +
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
          lab_staff_capped, 0
        ) +
          ifelse(amount_per_lab_cleaner_per_day > 0,
            cleaners_lab, 0
          )),
        (lab_staff_capped * amount_per_lab_tech_per_day +
          cleaners_lab * amount_per_lab_cleaner_per_day)
      )
    ) %>%
    dplyr::select(c(
      item, week_begins, week_ends, amount_inpatient_hcw,
      amount_inpatient_patient, amount_isolation, amount_screening_hcw,
      amount_lab
    ))

  amounts$total_amount <- rowSums(amounts[, c(4:8)])
  amounts$category <- "hygiene"

  return(amounts)
}

#' @title Case management weekly: accessories, consumables, and biomedical equipment
#'
#' @description in O(N^2) time. Next iteration should get rid of for loops.
#' To do: get rid of for loop for non reusable items.
#' Can also experiment with Reduce.
#'
#' @param params Classic parameter function (?)
#' @param equipment This should be the data frame of equipment need
#' @param patients From patients_weekly
#'
#'
#' @export
case_management_forecast <- function(params, equipment, patients) {
  equipment <- equipment %>%
    dplyr::mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    )

  case <- subset(equipment, startsWith(equipment$category, "Case management"))

  reusable_multiplier <- ifelse(case$reusable == TRUE, 1, 7)

  case[, c(8:24)] <- case[, c(8:24)] * reusable_multiplier

  amounts <- merge(case, patients[, c(
    "week_begins", "week_ends", "sev_beds_inuse",
    "crit_beds_inuse", "total_beds_inuse"
  )])

  # although this looks like it will double count, in actuality,
  # there are only parameters for either patients or beds, not both
  amounts <- amounts %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(
      demand_sev_patient = sev_beds_inuse *
        amount_per_inpatient_sev_patient_per_day +
        sev_beds_inuse * amount_per_inpatient_sev_bed_per_day,
      demand_crit_patient = crit_beds_inuse *
        amount_per_inpatient_crit_patient_per_day +
        crit_beds_inuse * amount_per_inpatient_crit_bed_per_day,
      demand_sev_crit_patient = total_beds_inuse *
        amount_per_inpatient_sev_crit_patient_per_day +
        total_beds_inuse * amount_per_inpatient_sev_crit_bed_per_day
    )

  # calculate the amounts for the first week with supplies only
  first_amounts <- amounts %>%
    arrange(item, week_begins) %>%
    group_by(item) %>%
    slice(which.min(week_begins))

  # round up supplies
  first_amounts <- first_amounts %>%
    mutate(
      amount_sev_patient = ceiling(demand_sev_patient),
      amount_crit_patient = ceiling(demand_crit_patient),
      amount_sev_crit_patient = ceiling(demand_sev_crit_patient)
    )

  # get rid of these rows within the overall amounts dataframe
  amounts <- subset(amounts,
                    amounts$week_begins != min(first_amounts$week_begins))
  # substitute these rows back in
  amounts <- full_join(first_amounts, amounts)
  # order by date and item
  amounts <- amounts[
    with(amounts, order(item, week_begins)),
  ]

  # benchmark this
  # df <- subset(amounts, amounts$reusable == T)
  items <- unique(amounts$item)

  #
  # split and run this only on reusable items
  # if you split - 1360 FALSE, 2641 TRUE - so reduce time by a third
  for (i in 1:length(items)) {
    df <- subset(amounts, amounts$item == items[i])
    for (n in 1:nrow(df)) {
      amount_sev <- df$amount_sev_patient[n]
      demand_sev <- df$demand_sev_patient[n]
      amount_crit <- df$amount_crit_patient[n]
      demand_crit <- df$demand_crit_patient[n]
      amount_sev_crit <- df$amount_sev_crit_patient[n]
      demand_sev_crit <- df$demand_sev_crit_patient[n]
      if (is.na(amount_crit)) {
        if (df$reusable[n] == T) {
          sum_sev <- sum(df$amount_sev_patient[1:n - 1])
          amount_sev_replace <- max(ceiling(demand_sev -
            sum_sev), 0)
          sum_crit <- sum(df$amount_crit_patient[1:n - 1])
          amount_crit_replace <- max(ceiling(demand_crit -
            sum_crit), 0)
          sum_sev_crit <- sum(df$amount_sev_crit_patient[1:n - 1])
          amount_sev_crit_replace <- max(ceiling(demand_sev_crit -
            sum_sev_crit), 0)
        } else {
          amount_sev_replace <- ceiling(demand_sev)
          amount_crit_replace <- ceiling(demand_crit)
          amount_sev_crit_replace <- ceiling(demand_sev_crit)
        }
        df$amount_sev_patient[n] <- amount_sev_replace
        df$amount_crit_patient[n] <- amount_crit_replace
        df$amount_sev_crit_patient[n] <- amount_sev_crit_replace
      }
    }
    res <- rbind(res, df) # this likely takes most time
  }

  res <- res %>% dplyr::select(c(
    item, category, week_begins, week_ends, amount_sev_patient,
    amount_crit_patient, amount_sev_crit_patient
  ))

  res$total_amount <- rowSums(res[, c(
    "amount_sev_patient",
    "amount_crit_patient",
    "amount_sev_crit_patient"
  )])

  return(res)
}

#' @title PPE need weekly
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
#' @export
ppe_forecast <- function(equipment, hcws, patients, cases, tests,
                         screening_hcws) {
  equipment <- equipment %>%
    dplyr::mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    )
  # unnecessary, but helps me think
  ppe <- subset(equipment, equipment$group == "PPE")

  reusable_multiplier <- ifelse(ppe$reusable == TRUE, 1, 7)

  ppe[, c(8:24)] <- ppe[, c(8:24)] * reusable_multiplier

  amounts <- merge(hcws, ppe)
  amounts <- merge(amounts, tests)
  amounts <- merge(amounts, patients)
  amounts <- merge(amounts, screening_hcws)

  amounts <- amounts %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(
      amount_inpatient_hcw = hcws_inpatient_capped *
        amount_per_inpatient_hcw_per_day +
        cleaners_inpatient_capped * amount_per_inpatient_cleaner_per_day +
        inf_caregivers_hosp_uncapped *
        amount_per_inpatient_inf_caregiver_per_day +
        amb_personnel_inpatient_capped *
        amount_per_inpatient_ambworker_per_day +
        bio_eng_inpatient_capped * amount_per_inpatient_biomed_eng_per_day,
      amount_inpatient_patient = total_beds_inuse *
        amount_per_inpatient_sev_crit_patient_per_day +
        sev_beds_inuse * amount_per_inpatient_sev_patient_per_day +
        crit_beds_inuse * amount_per_inpatient_crit_patient_per_day,
      amount_isolation = ifelse(
        reusable == TRUE,
        inf_caregivers_isol_uncapped * params$stay_mild +
          tests_mild * params$stay_mild +
          tests_mod * params$stay_mod,
        inf_caregivers_isol_uncapped *
          amount_per_isolation_inf_caregiver_per_day * params$stay_mild +
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
          lab_staff_capped, 0
        ) +
          ifelse(amount_per_lab_cleaner_per_day > 0,
            cleaners_lab, 0
          )),
        (lab_staff_capped * amount_per_lab_tech_per_day +
          cleaners_lab * amount_per_lab_cleaner_per_day)
      )
    ) %>%
    dplyr::select(c(
      item, week_begins, week_ends, amount_inpatient_hcw,
      amount_inpatient_patient, amount_isolation, amount_screening_hcw,
      amount_lab
    ))

  amounts$total_amount <- rowSums(amounts[, c(4:8)])
  amounts$category <- "ppe"

  return(amounts)
}

#' @title Diagnostics weekly
#'
#' @description
#'
#' @param lab_params From lab parameters
#' @param equipment This should be the data frame of equipment need
#' @param test_ratios This should be from test ratios
#' @param total_tests total_tests from that function
#' @param patients patients_weekly, this has the number of hospital facilities in use
#'
#'
#' @export
diagnostics_forecast <- function(lab_params, equipment, test_ratios,
                                 total_tests, patients) {
  equipment <- equipment %>%
    dplyr::mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    )

  dx <- subset(equipment, equipment$group == "Diagnostics")
  dx <- merge(dx, n_tests)
  dx <- merge(dx, patients)
  dx$total_amount <- NA

  # ignore warnings
  dx$total_amount[grep("manual PCR", dx$item)] <- dx$total_tests_capped *
    test_ratios$ratio[test_ratios$type == "manual"] / (
      lab_params$perc_wastage_manual_test_kits *
        lab_params$num_tests_manual_test_kits)
  dx$total_amount[grep("Triple packaging", dx$item)] <-
    dx$hosp_facilities_inuse *
    lab_params$triple_packaging_per_unit
  dx$total_amount[grep("Swab and Viral", dx$item)] <- dx$total_tests_capped
  dx$total_amount[grep("high-throughput", dx$item)] <- dx$total_tests_capped *
    test_ratios$ratio[test_ratios$type == "high_throughput"]
  dx$total_amount[grep("RT-PCR cartridge", dx$item)] <- dx$total_tests_capped *
    test_ratios$ratio[test_ratios$type == "near_patient"]
  dx$total_amount[grep("Antigen Rapid Diagnostic Tests", dx$item)] <-
    dx$total_tests_capped *
    test_ratios$ratio[test_ratios$type == "antigen"]

  # remove NAs (keep in mind, for some equipment items we did not have calculations to copy)
  dx <- dx[complete.cases(dx), ]
  dx <- dx %>% select(c(item, week_begins, week_ends, total_amount))
  dx$category <- "diagnostics"

  return(dx)
}
