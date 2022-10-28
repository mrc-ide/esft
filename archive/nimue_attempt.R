library(apothecary)
library(nimue)
library(tidyverse)
library(squire)


# alexandra's nimue run
# Run the model with an example population and no vaccination
no_vaccine <- nimue::run(country = "United Kingdom",
                         max_vaccine = 0,
                         R0 = 2)

# Format the output selecting infection and deaths
out1 <-
  format(no_vaccine,
         compartments = NULL,
         summaries = c("infections", "deaths")) %>%
  mutate(Name = "No vaccine")

# this is a function specific to the nimue runs - it extracts the infections and deaths
# by essentially removing all other compartments and getting rid of "cumu" in the variable titles
# but is specific - nimue simulations require specific functions to format them

# let's try to do the exact same thing with apothecary
none <- apothecary::run_apothecary(country = "United Kingdom",
                       R0 = 2)

index <- squire:::odin_index(none$model)
none_deaths <- sum(apply(none$output[, index$D], 2, max))

R <- "low"
if (R == "high") {
  R0 <- 2
} else {
  R0 <- 1.35
}
type_1_eff <- 0.70
none <- run_apothecary(country = "United Kingdom", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
index <- squire:::odin_index(none$model)
none_deaths <- sum(apply(none$output[, index$D], 2, max))
type1 <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                        time_period = time, seeding_cases = 20, day_return = TRUE,
                        hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                        prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                        drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                        drug_11_prop_treat = 1, drug_11_GetOx_effect_size = type_1_eff, drug_11_NoOx_effect_size = type_1_eff + 0.5 * (1 - type_1_eff),
                        drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                        drug_12_prop_treat = 1, drug_12_GetOx_effect_size = type_1_eff, drug_12_NoOx_effect_size = type_1_eff + 0.5 * (1 - type_1_eff),
                        drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                        drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = type_1_eff, drug_13_GetOx_NoMV_effect_size = type_1_eff + 0.5 * (1 - type_1_eff), drug_13_NoOx_NoMV_effect_size = 1)
type1_deaths <- sum(apply(type1$output[, index$D], 2, max))
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(type1$output[, index$D], 2, max))
direct_deaths_averted <- total_deaths_averted
