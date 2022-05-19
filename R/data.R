#' Who Summary Data
#'
#' Summary of estimates of healthcare workforce and hospital bed indicators
#' provided by the WHO. Compiled from the GHO, WB, and UNDP datasets.
#'
#' For healthcare workforce estimates, the average YoY population growth rate
#' was applied to either the latest known number of healthcare workers within
#' a specific category, or the product of the income group average and the
#' population. Similar calculations were performed to estimate the number of
#' beds and percentage of beds allocated to the ICU.
#'
#' To do: translate the compilation process into an r function, for up to date
#' estimates.
#'
#' GHO data comes from here: \url{https://apps.who.int/gho/data/node.main.HWFGRP?lang=en}
#'
#'@format A data frame with 265 rows and 18 variables:
#' \describe{
#'   \item{country_name}{Country name}
#'   \item{country_code}{Iso3c codes}
#'   \item{who_region}{WHO region}
#'   \item{income_group}{Income group}
#'   \item{income_class}{Income class}
#'   \item{population}{Country population}
#'   \item{nurses}{Absolute number of estimated nurses.}
#'   \item{midwives}{Absolute number of estimated midwives.}
#'   \item{labs}{Absolute number of total estimated lab staff.}
#'   \item{doctors}{Absolute number of estimated doctors.}
#'   \item{trad_comp_med}{Number of estimated traditional and complementary
#'   medical personnel.}
#'   \item{chws}{Number of estimated community healthcare workers.}
#'   \item{pharmacists}{Estimated number of pharmacists.}
#'   \item{physiotherapists}{Estimated number of physiotherapists.}
#'   \item{dentists}{Estimated absolute number of dentists.}
#'   \item{beds_per_1000}{Estimated beds per 1,000 people in a country.}
#'   \item{perc_icu_beds}{Estimated percentage of reported beds allocated to ICU
#'   use.}
#'   \item{beds_total}{Total estimated number of beds in a country.}
#' }
#' @source WHO ESFT
"who"

#' WB Beds
#'
#' Number of hospital beds per 1,000 people by country by year.The most recent
#' year of data varies across countries. Taken from: \url{https://data.worldbank.org/indicator/SH.MED.BEDS.ZS}.
#'
#'@format A data frame with 264 rows and 65 variables:
#' \describe{
#'   \item{country_name}{Country name}
#'   \item{country_code}{Iso3c codes}
#'   \item{indicator_name}{Indicator name, in this case, hospital beds per 1,000
#'   people.}
#'   \item{indicator_code}{Indicator code,in this case, SH.MED.BEDS.ZS.}
#'   \item{1960...2019}{Years}
#'   \item{most_recent}{Most recent bed value: either reported, or income group
#'   average if not available.}
#' }
#' @source The world bank.
"wb_beds"

#' Income group averages of hospital beds per 1,000
#'
#'
#'@format A data frame with 5 rows and 3 variables:
#' \describe{
#'   \item{income_group}{Income group}
#'   \item{beds_1000}{Hospital beds per 1,000 people}
#'   \item{source}{Source of estimate}
#' }
#' @source WHO ESFT
"bed_nr_proxy"

#' Income group averages of percentage beds allocated to critical care
#'
#'
#'@format A data frame with 5 rows and 3 variables:
#' \describe{
#'   \item{income_group}{Income group}
#'   \item{perc_crit_proxy}{Average percentage of beds allocated to critical care}
#'   \item{source}{Source of estimate}
#' }
#' @source WHO ESFT
"perc_crit_proxy"

#' Health care work force data
#'
#' HWFE tool methodology, combined with LMIC-specific inputs from consultations,
#' e.g., with Ethiopian clinical leads. Shows patient time per 24 hours by severity type.
#' \url{https://www.researchgate.net/figure/Health-Workforce-Estimator-HWFE-tool-applies-the-WISN-approach-to-caring-for-COVID_fig3_358174845}.
#' Also uses adappt tool: \url{https://www.euro.who.int/en/health-topics/Health-systems/pages/strengthening-the-health-system-response-to-covid-19/surge-planning-tools/adaptt-surge-planning-support-tool}
#' These tools yield different outputs based on different focus + models.
#' Note ESFT has SEIR models, other tools use simplified projections/epi models.
#'
#'@format A data frame with 21 rows and 7 variables:
#' \describe{
#'   \item{occupational_title}{Occupational Title. }
#'   \item{patient_t24_mild}{Iso3c codes}
#'   \item{patient_t24_mod}{WHO region}
#'   \item{patient_t24_sev}{WHO region}
#'   \item{patient_t24_crit}{WHO region}
#'   \item{patient_t24_screen}{WHO region}
#'   \item{esft_group}{WHO region}
#' }
#' @source WHO ESFT
"hwfe"

#' Diagnostics
#'
#' From the WHO ESFT Manual:
#' The reference values for the module were taken from an assessment of
#' available equipment and are based on a number of factors including population
#' size, HIV burden (as many machines were initially purchased for HIV testing),
#' and testing platforms known to the WHO. Please note that the reference values
#' are estimates and may not exactly match the number of platforms in each
#' country.
#' \url{https://apps.who.int/iris/bitstream/handle/10665/333983/WHO-2019-nCoV-Tools-Essential_forecasting-Overview-2020.1-eng.pdf?sequence=1&isAllowed=y}
#'
#'@format A data frame with 220 rows and 9 variables:
#' \describe{
#'   \item{country_name}{Country name}
#'   \item{who_region}{WHO region}
#'   \item{income_group}{Income group}
#'   \item{roche_6800}{}
#'   \item{roche_8800}{}
#'   \item{abbott_m2000}{}
#'   \item{hologic_panther}{}
#'   \item{genexpert}{}
#'   \item{manual}{}
#' }
#' @source WHO ESFT
"diagnostics"

#' Pharmaceuticals
#'
#'@format A data frame with 148 rows and 44 variables:
#' \describe{
#'   \item{covid_specific}{}
#'   \item{item_nr}{}
#'   \item{drug}{}
#'   \item{classification}{}
#'   \item{concentration}{}
#'   \item{formulation}{}
#'   \item{units}{}
#'   \item{drug_form}{}
#'   \item{price_usd}{}
#'   \item{crit_days_per_treatment_course}{}
#'   \item{crit_daily_amount}{}
#'   \item{crit_drug_form_per_treatment_course}{}
#'   \item{crit_vol_per_treatment_course}{}
#'   \item{perc_crit_patients_receiving_treatment}{}
#'   \item{sev_days_per_treatment_course}{}
#'   \item{sev_daily_amount}{}
#'   \item{sev_drug_form_per_treatment_course}{}
#'   \item{sev_vol_per_treatment_course}{}
#'   \item{perc_sev_patients_receiving_treatment}{}
#'   \item{mod_days_per_treatment_course}{}
#'   \item{mod_daily_amount}{}
#'   \item{mod_drug_form_per_treatment_course}{}
#'   \item{mod_vol_per_treatment_course}{}
#'   \item{perc_mod_patients_receiving_treatment}{}
#'   \item{mild_days_per_treatment_course}{}
#'   \item{mild_daily_amount}{}
#'   \item{mild_drug_form_per_treatment_course}{}
#'   \item{mild_vol_per_treatment_course}{}
#'   \item{perc_mild_patients_receiving_treatment}{}
#'   \item{total_drug_form_crit_patients}{}
#'   \item{total_vol_crit_patients}{}
#'   \item{total_cost_usd_crit}{}
#'   \item{total_drug_form_sev_patients}{}
#'   \item{total_vol_sev_patients}{}
#'   \item{total_cost_usd_sev}{}
#'   \item{total_drug_form_mod_patients}{}
#'   \item{total_vol_mod_patients}{}
#'   \item{total_cost_usd_mod}{}
#'   \item{total_drug_form_mild_patients}{}
#'   \item{total_vol_mild_patients}{}
#'   \item{total_cost_usd_mild}{}
#'   \item{total_drug_form_all}{}
#'   \item{total_volume_all}{}
#'   \item{total_cost_usd}{}
#' }
#' @source WHO ESFT
"pharmaceuticals"

#' Equipment Ratios
#'
#'@format A data frame with 53 rows and 41 variables:
#' \describe{
#'   \item{category}{}
#'   \item{grouping}{}
#'   \item{item}{}
#'   \item{unit}{}
#'   \item{reusable}{}
#'   \item{supplied_with}{}
#'   \item{price}{}
#'   \item{consumable}{}
#'   \item{amount_per_inpatient_hcw_per_day}{}
#'   \item{amount_per_inpatient_cleaner_per_day}{}
#'   \item{amount_per_inpatient_inf_caregiver_per_day}{}
#'   \item{amount_per_inpatient_ambworker_per_day}{}
#'   \item{amount_per_inpatient_biomed_eng_per_day}{}
#'   \item{amount_per_inpatient_sev_patient_per_day}{}
#'   \item{amount_per_inpatient_crit_patient_per_day}{}
#'   \item{amount_per_inpatient_sev&crit_patient_per_day}{}
#'   \item{amount_per_inpatient_sev_bed_per_day}{}
#'   \item{amount_per_inpatient_crit_bed_per_day}{}
#'   \item{amount_per_inpatient_sev&crit_bed_per_day}{}
#'   \item{amount_per_mild&mod_inf_caregiver_per_day}{}
#'   \item{amount_per_mild&mod_patient_per_day}{}
#'   \item{amount_per_screening_hcw_per_day}{}
#'   \item{amount_per_screening_patient_per_day}{}
#'   \item{amount_per_lab_tech_per_day}{}
#'   \item{amount_per_lab_cleaner_per_day}{}
#'   \item{amount_per_mod_com_care_hcw_per_day}{}
#'   \item{amount_per_mod_com_care_hygienist_per_day}{}
#'   \item{amount_per_mod_com_care_caretaker_per_day}{}
#'   \item{amount_per_mod_com_care_patient_per_day}{}
#'   \item{amount_per_mod_com_care_bed_per_day}{}
#'   \item{amount_per_pointofentry_hcw_per_day}{}
#'   \item{amount_per_pointofentry_hygienist_per_day}{}
#'   \item{amount_per_pointofentry_caretaker_per_day}{}
#'   \item{amount_per_pointofentry_patient_per_day}{}
#'   \item{amount_per_pointofentry_bed_per_day}{}
#'   \item{amount_per_mild_qcenter_hcw_per_day}{}
#'   \item{amount_per_mild_qcenter_hygienist_per_day}{}
#'   \item{amount_per_mild_qcenter_caretakers_per_day}{}
#'   \item{amount_per_mild_qcenter_ambulancier_per_day}{}
#'   \item{amount_per_mild_qcenter_patient_per_day}{}
#'   \item{amount_per_mild_qcenter_bed_per_day}{}
#' }
#' @source WHO ESFT
"equipment"

#' Transmission scenarios
#'
#'@format A data frame with 3 rows and 5 variables:
#' \describe{
#'   \item{imperial_scenario}{}
#'   \item{label_death_calibrated}{}
#'   \item{label_not_death_calibrated}{}
#'   \item{R}{}
#'   \item{imperial_category_labels}{}
#' }
#' @source WHO ESFT
"transmission_scenarios"
