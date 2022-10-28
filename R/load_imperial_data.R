#' Load imperial forecasts
#'
#' @description Downloads the raw imperial data from the imperial fits repo.
#' Do not use this function to mass download data (only one url at a time
#' please!), unless you have set up a Personal Access Token (PAT).
#'
#'
#' @param warnings Default is false. Whether to give warnings.
#' @param country_code Country code of country/countries to load. Else will
#' yield random sample of 5 countries.
#' @param scenario Default is medium transmission, with an R(0) number or R(eff)
#' number of 0.94. Other options include "Low", with an R(0) or R(eff) of 0.47,
#' which simulates a 50 percent decrease in transmission, or "High", with an
#' R(0) or R(eff) of 1.41, which simulates a 50 percent increase in
#' transmission.
#'
#' @import countrycode
#' @import gh
#' @importFrom utils URLencode
#' @importFrom utils read.csv
#'
#' @return All of the imperial SEIR projections.
#' @export
load_imperial_data <- function(warnings = FALSE, country_code = NULL,
                               scenario = "Medium") {
  iso3 <- utils::read.csv("data-raw/countries.csv")
  colnames(iso3)[1] <- "country_code"
  iso3 <- iso3[!is.na(iso3)]
  if (!(is.null(country_code))) {
    iso3 <- subset(iso3, iso3 == country_code)
  } else {
    iso3 <- sample(iso3, size = 5)
  }
  # get scenario translation
  scenarios <- esft::transmission_scenarios
  scenario_label <-
    scenarios$imperial_category_labels[scenarios$imperial_scenario == scenario]
  scenario_label <- utils::URLencode(scenario_label)

  urls_to_try <- list()
  for (c in iso3) {
    qurl <- paste0(
      "https://raw.githubusercontent.com/mrc-ide/global_lmic_projections_esft/main/",
      c, "/", scenario_label, ".Rds?raw=true"
    )
    urls_to_try <- append(urls_to_try, qurl)
  }

  if (warnings == TRUE) {
    df_list <- lapply(urls_to_try, read_url)
  } else {
    df_list <- suppressMessages(lapply(urls_to_try, read_url))
  }

  df_list <- df_list[!sapply(df_list, is.null)]
  # rename list elements to country codes
  names(df_list) <- iso3
  # add column with country codes to each data frame within the list
  df_list <- mapply(cbind, df_list, "country_code" = iso3, SIMPLIFY = FALSE)
  # merge all elements of list into one dataframe
  all_data <- Reduce(function(x, y) merge(x, y, all = TRUE), df_list)

  return(all_data)
}

#' Reads RDS files from urls
#'
#' @param url The url of the Rds file of the country fit from github.
#'
#' @export
#' @source \url{https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r}
#' @return Whatever url is read.
read_url <- function(url) {
  out <- tryCatch(readRDS(url(url, method = "libcurl")),
    error = function(cond) {
      message(paste("URL does not exist:", url))
      message(cond)
      return(NA)
    },
    warning = function(cond) {
      message(paste("URL caused a warning:", url))
      message(cond)
      return(NULL)
    },
    finally = {
      message(paste("Processed URL:", url))
    }
  )
  return(out)
}
