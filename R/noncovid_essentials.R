#' @title Reference cadres of HCWs
#'
#' @description Provides reference values of HCWs taken from the WHO ESFT, and
#' populates the list with standard values if estimates are not available.
#' One question: why aren't the reference values proportional to the country
#' population size?
#'
#' @param iso3c ISO3C country code.
#' @param params From get_parameters
#' @param who WHO summary data from the esft package, who.rda
#' @param throughput Either diagnostic_capacity or throughput
#' @param default These are the base values supplied by the ESFT worksheet -
#' and are replaced by calculated values if not supplied.
#'
#' @return Dataframe of reference HCWs
#' \describe{
#'   \item{n_docs}{Number of medical doctors (non-COVID-19)}
#'   \item{n_nurses}{Number of nurses (non-COVID-19)}
#'   \item{n_labs}{Number of laboratory scientists and techs (non-COVID-19)}
#'   \item{n_midwives}{Number of midwives}
#'   \item{n_dentists}{Number of dentists}
#'   \item{n_physiotherapists}{Number of physiotherapists}
#'   \item{n_trad_comp_med}{Number of traditional and complementary medical
#'   personnel}
#'   \item{n_chws}{Number of community health workers}
#'   \item{n_pharmacists}{Number of pharmacists}
#' }
#'
#' @importFrom countrycode countrycode
#'
#' @export
reference_hcw <- function(iso3c = NULL, params, who, throughput,
                          default = list(
                            n_docs = 8000,
                            n_nurses = 5000,
                            n_labs = 300,
                            n_midwives = 500,
                            n_dentists = 10,
                            n_physiotherapists = 50,
                            n_trad_comp_med = 4000,
                            n_chws = 245,
                            n_pharmacists = 818
                          )) {
  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(who$country_code)) {
      stop("Iso3c not found")
    }
    country <- countrycode::countrycode(iso3c,
      origin = "iso3c",
      destination = "country.name"
    )
  }

  throughput <- throughput[, c("type", "covid_capacity")]
  throughput <- throughput[!duplicated(throughput), ]
  avg_lab <- mean(throughput$covid_capacity)

  # this is calculated as the backup if the default values are not available
  overrides <- list(
    n_docs = who$doctors[who$country_code == iso3c] *
      params$perc_hcws_not_covid,
    n_nurses = who$nurses[who$country_code == iso3c] *
      params$perc_hcws_not_covid,
    n_labs = who$labs[who$country_code == iso3c] * (1 - avg_lab),
    n_midwives = who$midwives[who$country_code == iso3c],
    n_dentists = who$dentists[who$country_code == iso3c],
    n_physiotherapists = who$physiotherapists[who$country_code == iso3c],
    n_trad_comp_med = who$trad_comp_med[who$country_code == iso3c],
    n_chws = who$chws[who$country_code == iso3c],
    n_pharmacists = who$pharmacists[who$country_code == iso3c]
  )

  default <- modifyList(default, overrides[intersect(
    names(overrides),
    names(which(is.na(default)))
  )])

  return(default)
}

#' @title Non COVID essentials
#'
#' @description Calculates PPE and other commodities needed for non-COVID-19
#' essential health services that need to continue during the pandemic.
#'
#' @param noncovid Data frame of equipment need, noncovid.rda
#' @param ref_hcws Returned by reference_hcw
#' @param forecast_length Length of forecast, in number of weeks (default=12)
#' @param days_week Average days worked per week, default = 5
#'
#' @return Dataframe of commodities weekly
#' \describe{
#'   \item{item}{Item name}
#'   \item{amount_noncovid_doctors}{Total commodity needs for the non-COVID-19
#'   doctors}
#'   \item{amount_noncovid_nurse}{Total commodity needs for the non-COVID-19
#'   nurses}
#'   \item{amount_noncovid_lab_techs}{Total commodity needs for the non-COVID-19
#'   lab scientists and technicians}
#'   \item{amount_midwives}{Total commodity needs for the midwives}
#'   \item{amount_dentists}{Total commodity needs for the dentists}
#'   \item{amount_physios}{Total commodity needs for the physiotherapists}
#'   \item{amount_trad_comp_meds}{Total commodity needs for the traditional
#'   and complementary medical personnel}
#'   \item{amount_chws}{Total commodity needs for the community healthcare
#'   workers}
#'   \item{amount_pharmacists}{Total commodity needs for the pharmacists}
#' }
#'
#' @import dplyr
#'
#' @export
noncovid_essentials <- function(noncovid, ref_hcws,
                                forecast_length = 12,
                                days_week = 5) {
  noncovid[is.na(noncovid)] <- 0
  noncovid <- noncovid %>%
    dplyr::mutate(
      amount_noncovid_doctors = ifelse(reusable == TRUE,
        ref_hcws$n_docs,
        ref_hcws$n_docs * amount_per_noncovid_doctor_per_day * days_week *
          forecast_length
      ),
      amount_noncovid_nurses = ifelse(reusable == TRUE,
        ref_hcws$n_nurses,
        ref_hcws$n_nurses * amount_per_noncovid_nurse_per_day * days_week *
          forecast_length
      ),
      amount_noncovid_lab_techs = ifelse(reusable == TRUE,
        ref_hcws$n_labs,
        ref_hcws$n_labs * amount_per_noncovid_lab_tech_per_day * days_week *
          forecast_length
      ),
      amount_midwives = ifelse(reusable == TRUE,
        ref_hcws$n_midwives,
        ref_hcws$n_midwives * amount_per_noncovid_midwife_per_day * days_week *
          forecast_length
      ),
      amount_dentists = ifelse(reusable == TRUE,
        ref_hcws$n_dentists,
        ref_hcws$n_dentists * amount_per_noncovid_dentist_per_day * days_week *
          forecast_length
      ),
      amount_physios = ifelse(reusable == TRUE,
        ref_hcws$n_physiotherapists,
        ref_hcws$n_physiotherapists * amount_per_noncovid_physio_per_day *
          days_week * forecast_length
      ),
      amount_trad_comp_meds = ifelse(reusable == TRUE,
        ref_hcws$n_trad_comp_med,
        ref_hcws$n_trad_comp_med *
          amount_per_noncovid_traditional_compl_per_day *
          days_week * forecast_length
      ),
      amount_chws = ifelse(reusable == TRUE,
        ref_hcws$n_chws,
        ref_hcws$n_chws * amount_per_noncovid_chw_per_day * days_week *
          forecast_length
      ),
      amount_pharmacists = ifelse(reusable == TRUE,
        ref_hcws$n_pharmacists,
        ref_hcws$n_pharmacists * amount_per_noncovid_pharmacist_per_day *
          days_week * forecast_length
      )
    )

  noncovid <- noncovid[, c(3, 15:23)]

  return(noncovid)
}
