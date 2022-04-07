#' @title Preset parameters for the low transmission scenario
#' @details Use a vector of preset parameters for the low transmission scenario
#' @details Default parameters are Death Calibrated Label = Decrease Transmission 50%,
#' Not Death Calibrated Label = Low Transmission, R(eff) or R(0) = 0.47, Imperial Category Label = Additional 50% Reduction
#' @export
low_transmission <- c("Decrease Transmission 50%", "Low Transmission", 0.47, "Additional 50% Reduction")

#' @title Preset parameters for the medium transmission scenario
#' @details Use a vector of preset parameters for the medium transmission scenario
#' @details Default parameters are Death Calibrated Label = Maintain Current Transmission,
#' Not Death Calibrated Label = Medium Transmission, R(eff) or R(0) = 0.94, Imperial Category Label = Maintain Status Quo
#' @export
med_transmission <- c("Maintain Current Transmission", "Medium Transmission", 0.94, "Maintain Status Quo")

#' @title Preset parameters for the high transmission scenario
#' @details Use a vector of preset parameters for the high transmission scenario
#' @details Default parameters are Death Calibrated Label = Increase Transmission 50%,
#' Not Death Calibrated Label = High Transmission, R(eff) or R(0) = 1.41, Imperial Category Label = Relax Interventions 50%
#' @export
high_transmission <- c("Increase Transmission 50%", "High Transmission", 1.41, "Relax Interventions 50%")


#' @title Parameterise drugs to use in the model
#'
#' @param parameters the model parameters
#' @param scenarios a list of scenario parameters, can be set using presets
#' @export
set_scenarios <- function(parameters, scenarios) {
  keys <- c(
    'death_calibrated',
    'not_death_calibrated',
    'R_eff',
    'imperial_label'
  )
  for (scenario in seq_along(scenarios)) {
    for (i in seq_along(scenarios[[scenario]])) {
      parameters[[keys[[i]]]] <- c(parameters[[keys[[i]]]], scenarios[[scenario]][[i]])
    }
  }
  parameters
}
