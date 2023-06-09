hcws_weekly <- function(params, # from get_parameters
                        capacity, # from get_country_capacity
                        lab_params, # get_lab_parameters
                        tests, # from diagnostics_weekly
                        patients, # patients_weekly
                        t_labs, # total_labs
                        hcw_caps
) {
  data <- merge(patients, tests)
  # params <- merge(params, capacity)
  # params <- merge(params, hcw_caps)


  data <- data %>%
    dplyr::mutate(
      hcws_inpatient_uncapped = .data$total_beds_inuse *
        hcw_caps$hcws_per_bed,
      hcws_inpatient_capped = min(
        .data$total_beds_inuse * hcw_caps$hcws_per_bed,
        hcw_caps$hcws_inpatients_cap
      ),
      inf_caregivers_hosp_uncapped = .data$total_beds_inuse *
        params$n_inf_caregivers_hosp,
      cleaners_inpatient_capped = min(
        .data$total_beds_inuse * hcw_caps$hygienists_per_bed,
        hcw_caps$cleaners_inpatient_cap
      ),
      # double check if i want to add the HCW caps to data or params - then rewrite
      amb_personnel_inpatient_capped = .data$total_beds_inuse*params$ambulancews_per_bed,
      bio_eng_inpatient_capped = .data$total_beds_inuse*params$bioengs_per_bed,
      # it's basically calculated this way i think to accommodate for mild
      # + mod testing in outpatient setting
      inf_caregivers_isol_uncapped = (
        .data$tests_mild + .data$tests_mod) * params$n_inf_caregivers_isol,
      # for the all testing strategy, capped by testing capacity
      # (confusing - needs simplification)
      lab_staff_capped = min(
        t_labs * lab_params$lab_staff_per_lab, hcw_caps$lab_staff_cap
      ),
      cleaners_lab = t_labs * lab_params$hygienists_per_lab
    ) %>%
    dplyr::select(c(
      week_ends, week_begins, hcws_inpatient_capped,
      hcws_inpatient_uncapped, inf_caregivers_hosp_uncapped,
      cleaners_inpatient_capped, amb_personnel_inpatient_capped,
      bio_eng_inpatient_capped, inf_caregivers_isol_uncapped, lab_staff_capped,
      cleaners_lab
    ))

  return(data)
}

diagnostic_capacity <- calc_diagnostic_capacity(country_diagnostic_capacity =
                                                  country_test_capacity,
                                                throughput, hours_per_shift =
                                                  hours_per_shift,
                                                shifts_per_day = 1)

# also saving the name code
sink("mylist.txt")
cat(paste0("#'   \\item{",names(hcw_caps), "}{}\n"))
sink()

#' * crit_patients_nocap - ICU_demand
#' * sev_patients_nocap - hospital_demand
sink("mylist.txt")
cat(paste0("#' * ",names(hcw_caps), "-\n"))
sink()
# i'm saving my microbenchmark code cuz i'm proud of it
mbm = microbenchmark(
  base = replace(ref_hcws, is.na(ref_hcws),
                 overrides[match(names(ref_hcws),
                                 names(overrides))][is.na(ref_hcws)]),
  utils = modifyList(ref_hcws, overrides[intersect(names(overrides),
                                                   names(which(is.na(ref_hcws))))]),
  times=100
)
mbm

modifyList(ref_hcws, overrides[intersect(names(overrides), names(which(is.na(ref_hcws))))])
l1 <- replace(ref_hcws, is.na(ref_hcws),
              overrides[match(names(ref_hcws),
                              names(overrides))][is.na(ref_hcws)])
a=names(ref_hcws)%in%names(overrides)&is.na(ref_hcws)
replace(ref_hcws, a, overrides[names(which(a))])
