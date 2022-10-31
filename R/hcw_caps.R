#' Static HCW Caps
#'
#' @description This function calculates the static HCW caps found in the
#' `Weekly Summary` in the ESFT.
#'
#' @param params Country capacity params, get_country_capacity
#' @param throughput Throughput dataframe, from r data file - can be altered
#' @param cap_lab_staff TRUE/FALSE. Option to set a cap on lab staff based on
#' percent of diagnostic machinery allocated to COVID
#'
#' @return List of caps
#' \describe{
#'   \item{hcws_inpatients_cap}{Date the week summarized begins}
#'   \item{hcws_screening_cap}{Date the week summarized ends}
#'   \item{lab_staff}{Lab staff available}
#'   \item{cap_lab_staff}{TRUE/FALSE, whether you want to cap lab staff by
#'   machines allocated to COVID or not}
#' }
#'
#' @export
hcw_caps_static <- function(params,
                            throughput,
                            cap_lab_staff = FALSE
                            # option to cap lab staff by percent allocated to covid
                            # percent allocated to covid is the average of the percent
                            # machine type allocation to covid testing
) {
  # add exists part here

  hcws_inpatients_cap <- params$perc_hcws_treat_covid * params$n_hcws
  # n_hcws = num nurses + num doctors
  hcws_screening_cap <- params$perc_hcws_screen_covid * params$n_hcws

  # calculating average covid capacity
  covid_capacity_high_throughput <- mean(
    throughput$covid_capacity[throughput$type == "high_throughput"]
  )
  covid_capacity_near_patient <- mean(
    throughput$covid_capacity[throughput$type == "near_patient"]
  )
  covid_capacity_manual <- mean(
    throughput$covid_capacity[throughput$type == "manual"]
  )

  if (cap_lab_staff == FALSE) {
    lab_staff <- params$n_labs
  } else {
    lab_cap <- mean(
      covid_capacity_high_throughput,
      covid_capacity_near_patient,
      covid_capacity_manual
    )
    lab_staff <- params$n_labs * lab_cap
  }
  hcw_lab_caps <- list(
    hcws_inpatients_cap = hcws_inpatients_cap,
    hcws_screening_cap = hcws_screening_cap,
    lab_staff = lab_staff,
    cap_lab_staff = cap_lab_staff
  )
  return(hcw_lab_caps)
}

