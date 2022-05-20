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
#' These are simplified groups from HWC Need worksheet (external). Patient time
#' is given across different settings, according to staff skills and scope of
#' practice. Note that only hours/day is used in the calculations.
#'
#'@format A data frame with 21 rows and 7 variables:
#' \describe{
#'   \item{occupational_title}{Occupational title}
#'   \item{patient_t24_mild}{Patient time per 24 hrs per mild case}
#'   \item{patient_t24_mod}{Patient time per 24 hrs per moderate case}
#'   \item{patient_t24_sev}{Patient time per 24 hrs per severe case}
#'   \item{patient_t24_crit}{Patient time per 24 hrs per critical case}
#'   \item{patient_t24_screen}{Patient time per 24 hrs for screening/triage}
#'   \item{esft_group}{Category of worker in ESFT}
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
#' The high-throughput conventional machines are the Roche 6800, Roche 8800,
#' Abbott m2000, and the Hologic Panthers. These are also used for HIV testing.
#' GeneXpert machines are near-patient PCR processing machines, that provide on
#' the spot diagnostics for patients. Manual real time PCR platforms are slower
#' but also provide PCR processing.
#'
#'
#'@format A data frame with 220 rows and 9 variables:
#' \describe{
#'   \item{country_name}{Country name}
#'   \item{who_region}{WHO region}
#'   \item{income_group}{Income group}
#'   \item{roche_6800}{Number of Roche 6800 machines estimated to be in country}
#'   \item{roche_8800}{Number of Roche 8800 machines estimated to be in country}
#'   \item{abbott_m2000}{Number of Abbott m2000 machines estimated to be in country}
#'   \item{hologic_panther}{Number of Hologic Panther machines estimated to be in country}
#'   \item{genexpert}{Number of GeneXpert machines estimated to be in country}
#'   \item{manual}{Number of manual machines estimated to be in country}
#' }
#' @source WHO ESFT
"diagnostics"

#' Pharmaceuticals
#'
#' Data frame of all pharmaceutical commodities that are included in the
#' forecast. The items included are based on bundles recommended by the WHO and
#' estimates are generated from scenario patient volumes by patient type.
#'
#' Am not sure why I have volume in this dataframe. I can not find it in the original ESFTs or later ones.
#'
#'@format A data frame with 148 rows and 43 variables:
#' \describe{
#'   \item{covid_specific}{Whether or not the drug is specific to COVID-19 treatment - TRUE if COVID-19 specific, FALSE if not.}
#'   \item{drug}{Drug product}
#'   \item{classification}{Type of drug}
#'   \item{concentration}{Concentration and formulation of drug}
#'   \item{formulation}{Formulation of drug, numeric}
#'   \item{units}{Units of formulation of drug}
#'   \item{drug_form}{Form of drug administration}
#'   \item{price_usd}{Price per unit of drug product}
#'   \item{crit_days_per_treatment_course}{Number of days per treatment course for critical patients}
#'   \item{crit_daily_amount}{Daily amount for critical patients}
#'   \item{crit_drug_form_per_treatment_course}{Total amount per treatment course per critical patients}
#'   \item{crit_vol_per_treatment_course}{Total volume of drug per treatment course}
#'   \item{perc_crit_patients_receiving_treatment}{Percentage of critical patients receiving this treatment}
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

#' Population info
#'
#'@format A data frame with 259 rows and 6 variables:
#' \describe{
#'   \item{imperial_scenario}{}
#'   \item{label_death_calibrated}{}
#'   \item{label_not_death_calibrated}{}
#'   \item{R}{}
#'   \item{imperial_category_labels}{}
#' }
#' @source WHO ESFT
"population"
