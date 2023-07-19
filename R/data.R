#' @title Who Summary Data
#'
#' @description Summary of estimates of healthcare workforce and hospital bed
#' indicators provided by the WHO. Compiled from the GHO, WB, and UNDP datasets.
#'
#' @details For healthcare workforce estimates, the average YoY population
#' growth rate was applied to either the latest known number of healthcare
#' workers within a specific category, or the product of the income group
#' average and the population. Similar calculations were performed to estimate
#' the number of beds and percentage of beds allocated to the ICU.
#'
#'
#' @format A data frame with 265 rows and 18 variables:
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
#' @source \url{https://www.who.int/publications/i/item/WHO-2019-nCoV-Tools-Essential_forecasting-2022.1}
"who"

#' @title Imperial Model Fits to Excess Death Data
#'
#' @format A data frame with 3,298,878 rows and 6 variables:
#' \describe{
#'   \item{scenario}{Transmission scenario, as given in the ESFT tool.}
#'   \item{compartment}{Projection compartment.}
#'   \item{date}{Date.}
#'   \item{death_calibrated}{Logical: Calibrated to reported deaths (TRUE) or
#'   to excess deaths (FALSE).}
#'   \item{y_mean}{Mean value of compartment specified.}
#'   \item{iso3c}{Iso3c country codes.}
#' }
#' @source \url{https://github.com/mrc-ide/global_lmic_projections_esft}
"icl_data"

#' @title Health care work force data
#'
#' @description HWFE tool methodology, combined with LMIC-specific inputs from
#' consultations, e.g., with Ethiopian clinical leads. Shows patient time per
#' 24 hours by severity type.
#'
#' @details Also uses adaptt tool:
#' \url{https://www.euro.who.int/en/health-topics/Health-systems/pages/strengthening-the-health-system-response-to-covid-19/surge-planning-tools/adaptt-surge-planning-support-tool}
#' These tools yield different outputs based on different focus + models. Note
#' ESFT has SEIR models, other tools use simplified projections/epi models.
#' These are simplified groups from HWC Need worksheet (external). Patient time
#' is given across different settings, according to staff skills and scope of
#' practice. Note that only hours/day is used in the calculations.
#'
#' @format A data frame with 21 rows and 7 variables:
#' \describe{
#'   \item{occupational_title}{Occupational title}
#'   \item{patient_t24_mild}{Patient time per 24 hrs per mild case}
#'   \item{patient_t24_mod}{Patient time per 24 hrs per moderate case}
#'   \item{patient_t24_sev}{Patient time per 24 hrs per severe case}
#'   \item{patient_t24_crit}{Patient time per 24 hrs per critical case}
#'   \item{patient_t24_screen}{Patient time per 24 hrs for screening/triage}
#'   \item{esft_group}{Category of worker in ESFT}
#' }
#' @source \url{https://www.researchgate.net/figure/Health-Workforce-Estimator-HWFE-tool-applies-the-WISN-approach-to-caring-for-COVID_fig3_358174845}
"hwfe"

#' @title Diagnostics
#'
#' @description According to the WHo ESFT: "the reference values for the module
#' were taken from an assessment of available equipment and are based on a
#' number of factors including population size, HIV burden (as many machines
#' were initially purchased for HIV testing), and testing platforms known to the
#' WHO. Please note that the reference values are estimates and may not exactly
#' match the number of platforms in each country."
#'
#' @details The high-throughput conventional machines are the Roche 6800, Roche
#' 8800, Abbott m2000, and the Hologic Panthers. These are also used for HIV
#' testing. GeneXpert machines are near-patient PCR processing machines, that
#' provide on the spot diagnostics for patients. Manual real time PCR platforms
#' are slower but also provide PCR processing.
#'
#'
#' @format A data frame with 220 rows and 10 variables:
#' \describe{
#'   \item{country_name}{Country name}
#'   \item{region}{WHO region}
#'   \item{income_group}{Income group}
#'   \item{roche_6800}{Number of Roche 6800 machines estimated to be in country}
#'   \item{roche_8800}{Number of Roche 8800 machines estimated to be in country}
#'   \item{abbott_m2000}{Number of Abbott m2000 machines estimated to be in
#'   country}
#'   \item{hologic_panther}{Number of Hologic Panther machines estimated to be
#'   in country}
#'   \item{genexpert}{Number of GeneXpert machines estimated to be in country}
#'   \item{manual}{Number of manual machines estimated to be in country}
#'   \item{country_code}{ISO3C code per country}
#' }
#' @source \url{https://apps.who.int/iris/bitstream/handle/10665/333983/WHO-2019-nCoV-Tools-Essential_forecasting-Overview-2020.1-eng.pdf?sequence=1&isAllowed=y}
"diagnostics"

#' @title Pharmaceuticals
#'
#' @description Data frame of all pharmaceutical commodities that are included
#' in the forecast. The items included are based on bundles recommended by the
#' WHO and estimates are generated from scenario patient volumes by patient
#' type.
#'
#'
#' @format A data frame with 147 rows and 23 variables:
#' \describe{
#'   \item{covid_specific}{Whether or not the drug is specific to COVID-19
#'   treatment - TRUE if COVID-19 specific, FALSE if not.}
#'   \item{drug}{Drug product}
#'   \item{classification}{Drug family to which the drug product belongs}
#'   \item{concentration}{Dosage and drug form}
#'   \item{formulation}{Dosage only}
#'   \item{units}{Unit in which the dosage is measured}
#'   \item{drug_form}{Formulation in which the drug is packages (i.e. tablet)}
#'   \item{price_usd}{Price per unit of drug product}
#'   \item{num_days_per_crit_treatment}{Number of days per treatment course for
#'   critical patients}
#'   \item{daily_amount_crit}{Daily amount for critical patients}
#'   \item{form_per_crit_course}{Total drug form per treatment course per
#'   critical patients}
#'   \item{perc_crit_treated}{Percentage of critical patients receiving this
#'   treatment}
#'   \item{num_days_per_sev_treatment}{Number of days per treatment course for
#'   severe patients}
#'   \item{daily_amount_sev}{Daily amount for severe patients}
#'   \item{form_per_sev_course}{Total drug form per treatment course per severe
#'   patients}
#'   \item{perc_sev_treated}{Percentage of severe patients receiving this
#'   treatment}
#'   \item{num_days_per_mod_treatment}{Number of days per treatment course for
#'   moderate patients}
#'   \item{daily_amount_mod}{Daily amount for moderate patients}
#'   \item{form_per_mod_course}{Total drug form per treatment course per
#'   moderate patients}
#'   \item{perc_mod_treated}{Percentage of moderate patients receiving this
#'   treatment}
#'   \item{num_days_per_mild_treatment}{Number of days per treatment course for
#'   mild patients}
#'   \item{daily_amount_mild}{Daily amount for mild patients}
#'   \item{form_per_mild_course}{Total drug form per treatment course per mild
#'   patients}
#'   \item{perc_mild_treated}{Percentage of mild patients receiving this
#'   treatment}
#' }
#' @source \url{https://www.who.int/publications/i/item/WHO-2019-nCoV-Tools-Essential_forecasting-2022.1}
"pharmaceuticals"

#' @title Equipment Ratios
#'
#' @description This dataframe contains inputs/assumptions for the commodities
#' forecasted across care settings (e.g., inpatient, screening/triage, and
#' isolation) and end users (e.g., HCW, patients).
#'
#' @details
#' Here is the description of care settings and personnel as outlined in the
#' ESFT tool:
#'
#' \bold{Care Setting Descriptions:}
#' \itemize{
#'  \item{Inpatient:}{ Inpatient care for severe and critical patients}
#'  \item{Screening/triage:}{ Screening and triaging all suspected COVID-19
#'  patients at first point of contact with the health care system (including
#'  those who ultimately test negative)}
#'  \item{Isolation:}{ Typically care provided at home or in a community
#'  facility (e.g., stadiums, gymnasiums, hotels) for mild/moderate patients}
#'  \item{Laboratories:}{ Labs where testing is conducted/processed}
#'}
#' \bold{Personnel Descriptions}
#' (these definitions and ILO ISCO codes are in use in all tabs of the tool):
#' \itemize{
#'  \item{HCW:}{ medical practitioners, including physicians, nursing
#'  professionals, and paramedical practitioners (ILO ISCO codes 2240, 2211,
#'  2212, 2221, 3221, 5321, 3256)}
#'  \item{Cleaners:}{ cleaners and helpers (ILO ISCO code 9112)}
#'  \item{Caregiver:}{ patient carers such as parent, spouse, partner, etc.}
#'  \item{Ambulancier:}{ emergency paramedics and drivers (ILO ISCO codes 8322,
#'  3258)}
#'  \item{Patient:}{ suspected case or person diagnosed with COVID-19 (can be
#'  further defined as a severe or critical or applicable to both)}
#'  \item{Bed:}{ equipment needed per patient bed (can be further defined as a
#'  severe or critical or applicable to both)}
#'}
#' Note: It may appear as if there is double counting, especially when it comes
#' to inpatient patients and beds. This is not the case - if there is a value
#' for one (either bed or patient), there is no value for the other.
#' Furthermore, if there are singular values for severe and critical, there will
#' not be values in the both severe and critical cell.
#'
#' @format A data frame with 53 rows and 24 variables:
#' \describe{
#'   \item{category}{Equipment type category: testing, infection prevention &
#'   control (IPC), case management - biomedical equipment, or case management
#'   - accessories & consumables.}
#'   \item{group}{Sub-categories of groups within each category.}
#'   \item{item}{Item name, with details}
#'   \item{unit}{Unit of measurement of the item}
#'   \item{reusable}{TRUE/FALSE: Is this item reusable or not.}
#'   \item{supplied_with}{Other equipment this item is supplied with.}
#'   \item{price}{Price per unit of the item, in USD. Updated with each
#'   iteration of the ESFT, most recently in April 2022.}
#'   \item{amount_per_inpatient_hcw_per_day}{Amount per healthcare worker per
#'   day in an inpatient setting.}
#'   \item{amount_per_inpatient_cleaner_per_day}{Amount per cleaner per day in
#'   an inpatient setting.}
#'   \item{amount_per_inpatient_inf_caregiver_per_day}{Amount per informal
#'   caregiver per day in an inpatient setting.}
#'   \item{amount_per_inpatient_ambworker_per_day}{Amount per ambulance
#'   personnel per day in an inpatient setting.}
#'   \item{amount_per_inpatient_biomed_eng_per_day}{Amount per biomedical
#'   engineer per day in an inpatient setting.}
#'   \item{amount_per_inpatient_sev_patient_per_day}{Amount per severe patient
#'   per day in an inpatient setting.}
#'   \item{amount_per_inpatient_crit_patient_per_day}{Amount per critical
#'   patient per day in an inpatient setting.}
#'   \item{amount_per_inpatient_sev_crit_patient_per_day}{Amount per severe and
#'   critical patient per day in an inpatient setting.}
#'   \item{amount_per_inpatient_sev_bed_per_day}{Amount per severe bed per day
#'   in an inpatient setting.}
#'   \item{amount_per_inpatient_crit_bed_per_day}{Amount per critical bed per
#'   day in an inpatient setting.}
#'   \item{amount_per_inpatient_sev_crit_bed_per_day}{Amount per severe and
#'   critical bed per day in an inpatient setting.}
#'   \item{amount_per_isolation_inf_caregiver_per_day}{Amount per informal
#'   caregiver per day in an isolation setting.}
#'   \item{amount_per_isolation_patient_per_day}{Amount per mild or moderate
#'   patient per day in an isolation setting.}
#'   \item{amount_per_screening_hcw_per_day}{Amount per healthcare worker per
#'   day in a screening/triage setting.}
#'   \item{amount_per_screening_patient_per_day}{Amount per patient per day in
#'   a screening/triage setting.}
#'   \item{amount_per_lab_tech_per_day}{Amount per cleaner per day in a
#'   laboratory setting.}
#'   \item{amount_per_lab_cleaner_per_day}{Amount per cleaner per day in a
#'   laboratory setting.}
#' }
#' @source \url{https://www.who.int/publications/i/item/WHO-2019-nCoV-Tools-Essential_forecasting-2022.1}
"equipment"

#' @title UNDP Population data
#'
#' @format A data frame with 259 rows and 6 variables:
#' \describe{
#'   \item{country_wb}{Country name in World Bank data.}
#'   \item{country_undp}{Country name in UNDP data.}
#'   \item{country_code}{Country code, iso3c format.}
#'   \item{pop_2020}{Population of country in 2020.}
#'   \item{pop_2015}{Population of country in 2015.}
#'   \item{yoy}{Year over year growth of country population.}
#' }
#' @source \url{https://population.un.org/wup/Download/}
"population"

#' @title Transmission scenarios
#'
#' @format A data frame with 3 rows and 5 variables:
#' \describe{
#'   \item{imperial_scenario}{Scenario as given in the ESFT tool.}
#'   \item{label_death_calibrated}{Label of the scenario in the death calibrated
#'   fits.}
#'   \item{label_not_death_calibrated}{Label of the scenario in the non death
#'   calibrated fits.}
#'   \item{R}{R number associated with transmission scenarios.}
#'   \item{imperial_category_labels}{Scenario labels as used in imperial model
#'   fits.}
#' }
#' @source \url{https://www.who.int/publications/i/item/WHO-2019-nCoV-Tools-Essential_forecasting-2022.1}
"transmission_scenarios"

#' @title Hours per shift number for diagnostic machines
#'
#' @format A data frame with 3 rows and 2 variables:
#' \describe{
#'   \item{shifts}{Number of shifts per day}
#'   \item{hours}{Total hours working with this number of shifts}
#' }
#' @source \url{https://www.who.int/publications/i/item/WHO-2019-nCoV-Tools-Essential_forecasting-2022.1}
"hours_per_shift"

#' @title Throughput data for diagnostic machinery
#'
#' @format A data frame with 7 rows and 4 variables:
#' \describe{
#'   \item{platform}{Diagnostic platform}
#'   \item{platform_key}{Platform key number that matches internal column names}
#'   \item{throughput_8hrs}{Number of tests that could be processed in an 8 hr
#'   shift}
#'   \item{throughput_16hrs}{Number of tests that could be processed in a 16 hr
#'   shift}
#'   \item{throughput_24hrs}{Number of tests that could be processed in a 24 hr
#'   shift}
#'   \item{type}{One of three types of machine: high throughput, near patient,
#'   or manual}
#'   \item{shifts_day}{Shifts the item was intended to work for per day}
#'   \item{days_week}{Days per week the machine would work per week}
#'   \item{covid_capacity}{Capacity per machine for covid cases}
#' }
"throughput"

#' @title Non-COVID Essentials
#'
#' @format A data frame with 6 rows and 23 variables.
#' \describe{
#'   \item{category}{Equipment type category: testing, infection prevention &
#'   control (IPC), case management - biomedical equipment, or case management
#'   - accessories & consumables.}
#'   \item{group}{Sub-categories of groups within each category.}
#'   \item{item}{Item name, with details}
#'   \item{unit}{Unit of measurement of the item}
#'   \item{reusable}{TRUE/FALSE: Is this item reusable or not.}
#'   \item{amount_per_noncovid_doctor_per_day}{Amount required per medical
#'   doctor not associated with the COVID-19 response per day.}
#'   \item{amount_per_noncovid_nurse_per_day}{Amount required per nurse
#'   not associated with the COVID-19 response per day.}
#'   \item{amount_per_noncovid_lab_tech_per_day}{Amount required per laboratory
#'   scientist and/or technician not associated with the COVID-19 response per
#'   day.}
#'   \item{amount_per_noncovid_midwife_per_day}{Amount required per midwife per
#'   day.}
#'   \item{amount_per_noncovid_dentist_per_day}{Amount required per dentist per
#'   day.}
#'   \item{amount_per_noncovid_physio_per_day}{Amount required per
#'   physiotherapist per day.}
#'   \item{amount_per_noncovid_traditional_compl_per_day}{Amount required per
#'   traditional and complementary medical personnel per day.}
#'   \item{amount_per_noncovid_chw_per_day}{Amount required per community health
#'   worker (CHW) per day.}
#'   \item{amount_per_noncovid_pharmacist_per_day}{Amount required per
#'   pharmacist per day.}
#' }
"noncovid"

#' @title Capacity as translated to percent
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{capacity}{Capacity is string form}
#'   \item{percent}{What the string translated to as percent}
#' }
"capacity_perc"

#' @title WHO Coronavirus (COVID-19) Dashboard
#'
#' @format A data frame with 305,019 rows and 8 variables:
#' \describe{
#'   \item{Date_reported}{Date of reporting to WHO}
#'   \item{Country_code}{ISO Alpha-2 country code}
#'   \item{Country}{Country, territory, area}
#'   \item{WHO_region}{	WHO regional offices: WHO Member States are grouped into
#'   six WHO regions -- Regional Office for Africa (AFRO), Regional Office for
#'   the Americas (AMRO), Regional Office for South-East Asia (SEARO), Regional
#'   Office for Europe (EURO), Regional Office for the Eastern Mediterranean
#'   (EMRO), and Regional Office for the Western Pacific (WPRO).}
#'   \item{New_cases}{New confirmed cases. Calculated by subtracting previous
#'   cumulative case count from current cumulative cases count.}
#'   \item{Cumulative_cases}{	Cumulative confirmed cases reported to WHO to
#'   date.}
#'   \item{New_deaths}{New confirmed deaths. Calculated by subtracting previous
#'   cumulative deaths from current cumulative deaths}
#'   \item{Cumulative_deaths}{Cumulative confirmed deaths reported to WHO to
#'   date.}
#' }
#' @source \url{https://covid19.who.int/data}
"who_data"
