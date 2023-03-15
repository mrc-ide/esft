#' @title Reference cadre
#'
#' @description Should I include days worked per week?
#'
#' @param iso3c
#' @param params
#' @param who This might have to be directly called
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

  throughput <- throughput[,c("type", "covid_capacity")]
  throughput <- throughput[!duplicated(throughput),]
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

  ref_hcws <- modifyList(ref_hcws, overrides[intersect(names(overrides),
                                           names(which(is.na(ref_hcws))))])

  return(ref_hcws)
}

#' @title Non COVID essentials
#'
#' @description
#' @param params I think this needs to be the user dashboard/input activity
#' @param noncovid This should be the data frame of equipment need
#' @param ref_hcws This should be a weekly summary data.frame, containing
#' the requisite columns
#' @param days_week Either take from throughput/diagnostic_capacity or set manual
#'
#'
#' @export
noncovid_essentials_weekly <- function(params, noncovid, ref_hcws, days_week = 5) {

  hygiene <- hygiene_weekly(equipment, hcws, patients, cases, tests,
                            screening_hcws)

  case_management <- case_management_weekly(params, equipment, weekly_summary)

  ppe <- ppe_weekly(params, equipment, weekly_summary)
  diagnostic_supplies <- diagnostic_supplies_weekly(params, equipment, weekly_summary)

  # maybe, get totals?
  commodities <- rbind(case_management, hygiene, ppe, diagnostics_supplies)
}
