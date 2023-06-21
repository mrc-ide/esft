#' @title Commodities by week
#'
#' @description Combination of diagnostics, ppe, hygiene, and case management
#' requirements by week. Returns total amount forecast.
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
#' @return Dataframe of commodities weekly
#' \describe{
#'   \item{category}{Category of item: Diagnostics, PPE, Hygiene, or Case
#'   management (and there are several types of case management categories)}
#'   \item{week_begins}{Date the week begins}
#'   \item{week_ends}{Date the week ends}
#'   \item{unit}{Unit items supplied in}
#'   \item{item}{Item name}
#'   \item{total_amount}{Total amount of that item}
#' }
#' @export
commodities_weekly <- function(equipment, patients, cases, tests,
                               screening_hcws, hcws, lab_params, test_ratios,
                               total_tests, params) {
  hygiene <- hygiene_forecast(
    equipment, hcws, patients, cases, tests,
    screening_hcws, params
  )

  case_management <- case_management_forecast(equipment, patients)

  ppe <- ppe_forecast(
    equipment, hcws, patients, cases, tests,
    screening_hcws, params
  )
  diagnostic_supplies <- diagnostics_forecast(
    lab_params, equipment, test_ratios,
    n_tests = total_tests, patients
  )

  # maybe, get totals?
  commodities <- rbind(
    case_management[, c(
      "category", "week_begins", "week_ends",
      "unit", "item", "total_amount"
    )],
    hygiene[, c(
      "category", "week_begins", "week_ends", "unit",
      "item", "total_amount"
    )],
    ppe[, c(
      "category", "week_begins", "week_ends", "unit",
      "item", "total_amount"
    )],
    diagnostic_supplies[, c(
      "category", "week_begins",
      "week_ends", "unit", "item",
      "total_amount"
    )]
  )
}

#' @title Hygiene weekly
#'
#' @description The amounts per hygiene item come from the equipment sheet:
#' this is a formula that feeds into the overall commodity forecast, but is
#' independent and provides more details (including needs per cadre).
#'
#' @param equipment This should be the data frame of equipment need
#' @param hcws HCWs_weekly
#' @param patients patients_weekly
#' @param cases cases_weekly
#' @param tests diagnostics_weekly
#' @param screening_hcws screening_hcws_weekly
#' @param params From get_parameters()
#'
#'
#' @return Dataframe of weekly hygiene forecast
#' \describe{
#'   \item{category}{Category of item: Hygiene}
#'   \item{week_begins}{Date the week begins}
#'   \item{week_ends}{Date the week ends}
#'   \item{unit}{Unit items supplied in}
#'   \item{item}{Item name}
#'   \item{total_amount}{Total amount of that item (sum of all columns
#'   following)}
#'   \item{amount_inpatient_hcw}{Total amount for all inpatient HCWs, cleaners,
#'   informal caregivers, ambulance personnel, and boimedical engineers}
#'   \item{amount_inpatient_patient}{Total amount for all inpatient patients}
#'   \item{amount_isolation}{Total amount for all informal caregivers for
#'   isolating patients as well as the isolating patients}
#'   \item{amount_screening}{Total amount for screening HCWs and patients}
#'   \item{amount_lab}{Total amount for all lab technicians and cleaners}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
hygiene_forecast <- function(equipment, hcws, patients, cases, tests,
                             screening_hcws, params) {
  equipment <- equipment %>%
    dplyr::mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    )

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
      amount_screening = ifelse(
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
      amount_inpatient_patient, amount_isolation, amount_screening,
      amount_lab, unit
    ))

  amounts$total_amount <- rowSums(amounts[, c(4:8)])
  amounts$category <- "Hygiene"

  return(amounts[, c(
    "category", "week_begins", "week_ends",
    "item", "unit", "total_amount", "amount_inpatient_hcw",
    "amount_inpatient_patient", "amount_isolation",
    "amount_screening", "amount_lab"
  )])
}

#' @title Case management weekly: accessories, consumables, and biomedical
#' equipment
#'
#' @description in O(N^2) time. Next iteration should get rid of for loops.
#' To do: get rid of for loop for non reusable items.
#' Can also experiment with Reduce. Also note - the last three columns do not
#' double count the item amounts, they are structured this way due to the way
#' that the amount per patient is structured in the input data sheet. There is
#' never a case where all three final columns have entries >0.
#'
#' @param equipment This should be the data frame of equipment need
#' @param patients From patients_weekly
#'
#' @return Dataframe of commodities weekly
#' \describe{
#'   \item{category}{Category of item: either Case management - accessories &
#'   consumables or Case management - biomedical equipment}
#'   \item{week_begins}{Date the week begins}
#'   \item{week_ends}{Date the week ends}
#'   \item{unit}{Unit items supplied in}
#'   \item{item}{Item name}
#'   \item{total_amount}{Total amount of that item (sum of all columns
#'   following)}
#'   \item{amount_sev_patient}{Total amount for severe patients}
#'   \item{amount_crit_patient}{Total amount for critical patients}
#'   \item{amount_sev_crit_patient}{Total amount for severe and critical
#'   patients}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
case_management_forecast <- function(equipment, patients) {
  equipment <- equipment %>%
    dplyr::mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    )

  case <- subset(equipment, startsWith(equipment$category, "Case management"))

  amounts <- merge(case, patients[, c(
    "week_begins", "week_ends", "sev_patients_admitted_cap",
    "crit_patients_admitted_cap", "sev_beds_inuse", "crit_beds_inuse",
    "total_beds_inuse"
  )])

  # although this looks like it will double count, in actuality,
  # there are only parameters for either patients or beds, not both
  amounts <- amounts %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(
      demand_sev_patient = sev_patients_admitted_cap *
        amount_per_inpatient_sev_patient_per_day +
        sev_beds_inuse * amount_per_inpatient_sev_bed_per_day,
      demand_crit_patient = crit_patients_admitted_cap *
        amount_per_inpatient_crit_patient_per_day +
        crit_beds_inuse * amount_per_inpatient_crit_bed_per_day,
      demand_sev_crit_patient = (sev_patients_admitted_cap +
        crit_patients_admitted_cap) *
        amount_per_inpatient_sev_crit_patient_per_day +
        total_beds_inuse * amount_per_inpatient_sev_crit_bed_per_day
    )

  # calculate the amounts for the first week with supplies only
  first_amounts <- amounts %>%
    dplyr::arrange(item, week_begins) %>%
    dplyr::group_by(item) %>%
    dplyr::slice(which.min(week_begins))

  # round up supplies
  first_amounts <- first_amounts %>%
    dplyr::mutate(
      amount_sev_patient = ceiling(demand_sev_patient),
      amount_crit_patient = ceiling(demand_crit_patient),
      amount_sev_crit_patient = ceiling(demand_sev_crit_patient)
    )

  # get rid of these rows within the overall amounts dataframe
  amounts <- subset(
    amounts,
    amounts$week_begins != min(first_amounts$week_begins)
  )
  # substitute these rows back in - generates a lot of text, maybe fix?
  amounts <- full_join(first_amounts, amounts)
  # order by date and item
  amounts <- amounts[
    with(amounts, order(item, week_begins)),
  ]

  items <- unique(amounts$item)

  res <- data.frame()
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
    amount_crit_patient, amount_sev_crit_patient, unit
  ))

  res$total_amount <- rowSums(res[, c(
    "amount_sev_patient",
    "amount_crit_patient",
    "amount_sev_crit_patient"
  )])

  return(res[, c(
    "category", "week_begins", "week_ends",
    "item", "unit", "total_amount", "amount_sev_patient",
    "amount_crit_patient", "amount_sev_crit_patient"
  )])
}

#' @title PPE need weekly
#'
#' @description Calculates PPE need forecasts for different cadres of HCWs for
#' the forecast period.
#'
#' @param equipment This should be the data frame of equipment need
#' @param hcws HCWs_weekly
#' @param patients patients_weekly
#' @param cases cases_weekly
#' @param tests diagnostics_weekly
#' @param screening_hcws screening_hcws_weekly
#' @param params From get_parameters()
#'
#' @return Dataframe of PPE needs weekly
#' \describe{
#'   \item{category}{Category of item: either Case management - accessories &
#'   consumables or Case management - biomedical equipment}
#'   \item{week_begins}{Date the week begins}
#'   \item{week_ends}{Date the week ends}
#'   \item{unit}{Unit items supplied in}
#'   \item{item}{Item name}
#'   \item{total_amount}{Total amount of that item (sum of all columns
#'   following)}
#'   \item{amount_inpatient_hcw}{Total amount for inpatient HCWs - including
#'   HCWs, cleaners, ambulance workers, informal caregivers, and biomedical
#'   engineers}
#'   \item{amount_inpatient_patient}{Total amount for inpatient patients}
#'   \item{amount_isolation}{Total amount for informal caregivers and patients
#'   in isolation}
#'   \item{amount_screening}{Total amount for screening HCWs and screened
#'   patients}
#'   \item{amount_lab}{Total amount for lab technicians and cleaners}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
ppe_forecast <- function(equipment, hcws, patients, cases, tests,
                         screening_hcws, params) {
  equipment <- equipment %>%
    dplyr::mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    )
  # unnecessary, but helps me think
  ppe <- subset(equipment, equipment$group == "PPE")

  reusable_multiplier <- ifelse(ppe$reusable == TRUE, 1, 7)

  ppe[, c(8:24)] <- ppe[, c(8:24)] * reusable_multiplier

  # amounts <- merge(hcws, ppe) I think the issue is that there are duplicate
  # rows per type of item
  amounts <- merge(hcws, tests)
  amounts <- merge(amounts, patients)
  amounts <- merge(amounts, screening_hcws)
  amounts <- merge(amounts, ppe)
  # i should test the if reusable is true condution
  # need to go over hcws bit by bit
  res <- data.frame()
  items <- unique(amounts$item)
  # split and run this only on reusable items
  # if you split - 1360 FALSE, 2641 TRUE - so reduce time by a third
  for (i in 1:length(items)) {
    df <- subset(amounts, amounts$item == items[i])
    for (n in 1:nrow(df)) {
      if (df$reusable[n] == TRUE) {
        amount_isolation <- ifelse(
          df$amount_per_isolation_inf_caregiver_per_day[n] > 0,
          df$inf_caregivers_isol_uncapped[n], 0
        ) +
          ifelse(
            df$amount_per_isolation_patient_per_day[n] > 0, df$tests_mild[n] +
            df$tests_mod[n], 0)
        amount_screening <- ifelse(df$amount_per_screening_hcw_per_day[n] > 0,
          df$screening_hcw_capped[n], 0
        ) +
          ifelse(
            df$amount_per_screening_patient_per_day[n] > 0, df$tests_mild[n] +
            df$tests_mod[n], 0)
        amount_lab <-
          (ifelse(df$amount_per_lab_tech_per_day[n] > 0,
            df$lab_staff_capped[n], 0
          ) +
            ifelse(df$amount_per_lab_cleaner_per_day[n] > 0,
              df$cleaners_lab[n], 0
            ))

        amount_inpatient_hcw <- (
          ifelse(df$amount_per_inpatient_hcw_per_day[n] > 0,
            df$hcws_inpatient_capped[n], 0
          ) +
            ifelse(df$amount_per_inpatient_cleaner_per_day[n] > 0,
              df$cleaners_inpatient_capped[n], 0
            ) +
            ifelse(df$amount_per_inpatient_inf_caregiver_per_day[n] > 0,
              df$inf_caregivers_hosp_uncapped[n], 0
            ) +
            ifelse(df$amount_per_inpatient_ambworker_per_day[n] > 0,
              df$amb_personnel_inpatient_capped[n], 0
            ) +
            ifelse(df$amount_per_inpatient_biomed_eng_per_day[n] > 0,
              df$bio_eng_inpatient_capped[n], 0
            )
        )
      } else {
        amount_isolation <- (df$inf_caregivers_isol_uncapped[n] *
          df$amount_per_isolation_inf_caregiver_per_day[n] * params$stay_mild) +
          (df$tests_mild[n] * params$stay_mild *
             df$amount_per_isolation_patient_per_day[n]) +
          (df$tests_mod[n] * params$stay_mod *
             df$amount_per_isolation_patient_per_day[n])
        amount_screening <- (df$screening_hcw_capped[n] *
                               df$amount_per_screening_hcw_per_day[n]) +
          (df$tests_mod[n] * df$amount_per_screening_patient_per_day[n] *
             params$stay_mod) +
          (df$tests_mild[n] * df$amount_per_screening_patient_per_day[n] *
             params$stay_mild
          )
        amount_lab <-
          (df$lab_staff_capped[n] * df$amount_per_lab_tech_per_day[n]) +
          (df$cleaners_lab[n] * df$amount_per_lab_cleaner_per_day[n])

        amount_inpatient_hcw <- (
          (df$amount_per_inpatient_hcw_per_day[n] *
            df$hcws_inpatient_capped[n]) +
            (df$cleaners_inpatient_capped[n] *
               df$amount_per_inpatient_cleaner_per_day[n]) +
            (df$inf_caregivers_hosp_uncapped[n] *
              df$amount_per_inpatient_inf_caregiver_per_day[n]) +
            (df$amb_personnel_inpatient_capped[n] *
              df$amount_per_inpatient_ambworker_per_day[n]) +
            (df$bio_eng_inpatient_capped[n] *
               df$amount_per_inpatient_biomed_eng_per_day[n])
        )
      }
      amount_inpatient_patient <- (df$total_beds_inuse[n] *
        df$amount_per_inpatient_sev_crit_patient_per_day[n]) +
        (df$sev_beds_inuse[n] *
           df$amount_per_inpatient_sev_patient_per_day[n]) +
        (df$crit_beds_inuse[n] *
           df$amount_per_inpatient_crit_patient_per_day[n])

      df$amount_isolation[n] <- amount_isolation
      df$amount_screening[n] <- amount_screening
      df$amount_lab[n] <- amount_lab
      df$amount_inpatient_patient[n] <- amount_inpatient_patient
      df$amount_inpatient_hcw[n] <- amount_inpatient_hcw
    }
    res <- rbind(res, df) # this likely takes most time
  }
  res <- res %>% dplyr::select(c(
    item, week_begins, week_ends, amount_isolation,
    amount_lab, amount_screening,
    amount_inpatient_patient, amount_inpatient_hcw,
    unit
  ))

  res$total_amount <- res$amount_isolation + res$amount_lab +
    res$amount_screening + res$amount_inpatient_patient +
    res$amount_inpatient_hcw
  res$category <- "PPE"

  return(res[, c(
    "category", "week_begins", "week_ends",
    "item", "unit", "total_amount", "amount_inpatient_hcw",
    "amount_inpatient_patient", "amount_isolation",
    "amount_screening", "amount_lab"
  )])
}

#' @title Diagnostics weekly
#'
#' @description Calculates a forecast of diagnostic needs by week.
#'
#' @param lab_params From lab parameters
#' @param equipment This should be the data frame of equipment need
#' @param test_ratios This should be from test ratios
#' @param n_tests n_tests from total_tests
#' @param patients patients_weekly, this has num of hospital facilities in use
#'
#' @return Dataframe of diagnostics weekly
#' \describe{
#'   \item{category}{Category of item: diagnostics}
#'   \item{week_begins}{Date the week begins}
#'   \item{week_ends}{Date the week ends}
#'   \item{unit}{Unit items supplied in}
#'   \item{item}{Item name}
#'   \item{total_amount}{Total amount of that item}
#' }
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
diagnostics_forecast <- function(lab_params, equipment, test_ratios,
                                 n_tests, patients) {
  equipment <- equipment %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))
    )

  dx <- subset(equipment, equipment$group == "Diagnostics")
  dx <- merge(dx, n_tests)
  dx <- merge(dx, patients)
  dx$total_amount <- NA

  # the issue here is that i only did it for the first week,
  dx$total_amount[grep("manual PCR", dx$item)] <-
    dx$total_tests_capped[grep("manual PCR", dx$item)] *
    test_ratios$ratio[test_ratios$type == "manual"] / (100 -
      (lab_params$perc_wastage_manual_test_kits *
        lab_params$num_tests_manual_test_kits))
  dx$total_amount[grep("Triple packaging", dx$item)] <-
    dx$hosp_facilities_inuse[grep("Triple packaging", dx$item)] *
      lab_params$triple_packaging_per_unit
  dx$total_amount[grep("Swab and Viral", dx$item)] <-
    dx$total_tests_capped[grep("Swab and Viral", dx$item)]
  dx$total_amount[grep("high-throughput", dx$item)] <-
    dx$total_tests_capped[grep("high-throughput", dx$item)] *
    test_ratios$ratio[test_ratios$type == "high_throughput"]
  dx$total_amount[grep("RT-PCR cartridge", dx$item)] <-
    dx$total_tests_capped[grep("RT-PCR cartridge", dx$item)] *
    test_ratios$ratio[test_ratios$type == "near_patient"]
  dx$total_amount[grep("Antigen Rapid Diagnostic Tests", dx$item)] <-
    dx$total_tests_capped[grep("Antigen Rapid Diagnostic Tests", dx$item)] *
      test_ratios$ratio[test_ratios$type == "antigen"]

  # order by date and item
  dx <- dx[
    with(dx, order(item, week_begins)),
  ]

  res <- data.frame()
  nas <- dx[!complete.cases(dx), ]
  dx <- dx[complete.cases(dx), ]
  items <- unique(dx$item)

  for (i in 1:length(items)) {
    df <- subset(dx, dx$item == items[i])
    for (n in 1:nrow(df)) {
      # this is the amount if it is not reusable
      amount_nonreusable <- df$total_amount[n]
      # if its not one of the NA values
      if (df$reusable[n] == TRUE) { # if the item is reusable
        # find sum of what has been given so far
        sum_sofar <- sum(df$total_amount[1:n - 1])
        amount_replace <- max(ceiling(amount_nonreusable -
          sum_sofar), 0)
      } else {
        amount_replace <- amount_nonreusable
      }
      df$total_amount[n] <- amount_replace
    }
    res <- rbind(res, df) # this likely takes most time
  }
  res <- rbind(res, nas)
  # remove NAs (for some equipment items we did not have calculations to copy)
  res <- res %>% dplyr::select(c(
    item, week_begins, week_ends, total_amount,
    unit
  ))
  res$category <- "Diagnostics"

  return(res[, c(
    "category", "week_begins", "week_ends",
    "item", "unit", "total_amount"
  )])
}
