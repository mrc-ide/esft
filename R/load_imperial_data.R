#' Load imperial forecasts
#'
#' Takes a long time (a few hours) to download all the data.
#' Data results in about 5 GBs, 15 million obs.
#' Best in test: download.file, with a tryCatch with read.csv in case of server
#' error. Also extending timeout.
#' Needs additional work for benchmarking and making sure we are downloading
#' the same thing every time.
#'
#' This function loads the imperial forecasts from their github repo.
#'
#' @param warnings Default is false. Whether to give warnings.
#' @import countrycode
#' @importFrom RCurl curlSetOpt
#'
#' @return All of the imperial SEIR projections.
#' @export
load_imperial_data <- function(warnings = FALSE, timeout = 200) {
  iso3 <- read.csv("data-raw/countries.csv")
  colnames(iso3)[1] <- "country_code"
  iso3 <- iso3[!is.na(iso3)]

  urls.to.try <- list()
  for (country in iso3) {
    url <- paste0(
      "https://raw.githubusercontent.com/mrc-ide/global-lmic-reports/master/",
      country, "/projections.csv"
    )
    urls.to.try <- append(urls.to.try, url)
  }
  RCurl::curlSetOpt(timeout = timeout)
  if (warnings == TRUE) {
    df_list <- lapply(urls.to.try[1], readUrl)
  } else {
    df_list <- suppressMessages(lapply(urls.to.try[1], readUrl))
  }

  df_list <- df_list[!sapply(df_list, is.null)]
  all.data <- Reduce(function(x, y) merge(x, y, all = TRUE), df_list)

  return(all.data)
}

#' Reads urls
#' Note: taken from a stack overflow thread.
#' Modify somewhat please.
#'
#' @param url
#'
#' @return Whatever url is read.
readUrl <- function(url) {
  tmp <- tempfile()
  tryCatch(download.file(url, tmp),
    error = function(cond) {
      message(paste("URL does not exist:", url))
      message(cond)
      tryCatch(read.csv(url),
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
      return(NA)
    },
    warning = function(cond) {
      message(paste("URL caused a warning:", url))
      message(cond)
      tryCatch(read.csv(url),
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
      return(NULL)
    },
    finally = {
      message(paste("Processed URL:", url))
    }
  )
  out <- read.csv(tmp)
  return(out)
}
