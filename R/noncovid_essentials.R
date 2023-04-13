#' @title Reference cadres of HCWs
#'
#' @description
#'
#' @param iso3c ISO3C country code.
#' @param params From get_parameters
#' @param who WHO summary data from the esft package, who.rda
#' @param throughput Either diagnostic_capacity or throughput
#' @param overrides These are the base values supplied by the ESFT worksheet -
#' replacements for if there are no reference cadre values
#'
#'
#' @return
#' @export
reference_hcw <- function(iso3c = NULL, params, who, throughput,
                          overrides = list(
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
    if (!iso3c %in% unique(esft::who$country_code)) {
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

  ref_hcws <- list(
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

  ref_hcws <- modifyList(ref_hcws, overrides[intersect(
    names(overrides),
    names(which(is.na(ref_hcws)))
  )])

  return(ref_hcws)
}

#' @title Non COVID essentials
#'
#' @description
#' @param noncovid Data frame of equipment need, noncovid.rda
#' @param ref_hcws Returned by reference_hcw
#' @param forecast_length Length of forecast, in number of weeks (default=12)
#' @param days_week Average days worked per week, default = 5
#'
#'
#' @export
noncovid_essentials <- function(noncovid, ref_hcws,
                                forecast_length = 12,
                                days_week = 5) {
  noncovid[is.na(noncovid)] <- 0
  noncovid <- noncovid %>%
    dplyr::mutate(
      amount_per_noncovid_doctor = ifelse(reusable == TRUE,
        ref_hcws$n_docs,
        ref_hcws$n_docs * amount_per_noncovid_doctor_per_day * days_week *
          forecast_length
      ),
      amount_per_noncovid_nurse = ifelse(reusable == TRUE,
        ref_hcws$n_nurses,
        ref_hcws$n_nurses * amount_per_noncovid_nurse_per_day * days_week *
          forecast_length
      ),
      amount_per_noncovid_lab_tech = ifelse(reusable == TRUE,
        ref_hcws$n_labs,
        ref_hcws$n_labs * amount_per_noncovid_lab_tech_per_day * days_week *
          forecast_length
      ),
      amount_per_noncovid_midwife = ifelse(reusable == TRUE,
        ref_hcws$n_midwives,
        ref_hcws$n_midwives * amount_per_noncovid_midwife_per_day * days_week *
          forecast_length
      ),
      amount_per_noncovid_dentist = ifelse(reusable == TRUE,
        ref_hcws$n_dentists,
        ref_hcws$n_dentists * amount_per_noncovid_dentist_per_day * days_week *
          forecast_length
      ),
      amount_per_noncovid_physio = ifelse(reusable == TRUE,
        ref_hcws$n_physiotherapists,
        ref_hcws$n_physiotherapists * amount_per_noncovid_physio_per_day *
          days_week * forecast_length
      ),
      amount_per_noncovid_trad_comp_med = ifelse(reusable == TRUE,
        ref_hcws$n_trad_comp_med,
        ref_hcws$n_trad_comp_med * amount_per_noncovid_traditional_compl_per_day *
          days_week * forecast_length
      ),
      amount_per_noncovid_chws = ifelse(reusable == TRUE,
        ref_hcws$n_chws,
        ref_hcws$n_chws * amount_per_noncovid_chw_per_day * days_week *
          forecast_length
      ),
      amount_per_noncovid_pharmacists = ifelse(reusable == TRUE,
        ref_hcws$n_pharmacists,
        ref_hcws$n_pharmacists * amount_per_noncovid_pharmacist_per_day *
          days_week * forecast_length
      )
    )
  return(noncovid)
}
