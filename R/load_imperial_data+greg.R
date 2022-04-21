#' Load imperial forecasts
#'
#' Downloads the raw imperial data from the imperial fits repo.
#' Before use, run through the github setup code.
#' Do not use this function to mass download data.
#'
#'
#' @param warnings Default is false. Whether to give warnings.
#' @param country.code Country code of country/countries to load. Else will
#' yield random sample of 5 countries.
#' @param scenario Default is medium transmission, with an R(0) number or R(eff)
#' number of 0.94. Other options include "Low", with an R(0) or R(eff) of 0.47,
#' which simulates a 50% decrease in transmission, or "High", with an R(0) or
#' R(eff) of 1.41, which simulates a 50% increase in transmission.
#'
#' @import countrycode
#' @import gh
#'
#' @return All of the imperial SEIR projections.
#' @export
load_imperial_data <- function(warnings=FALSE, country.code=NULL,
                               scenario = "Medium"){

  iso3 <-read.csv("data-raw/countries.csv")
  colnames(iso3)[1]<-"country_code"
  iso3<-iso3[!is.na(iso3)]
  if (!(is.null(country.code))){
    iso3<-subset(iso3, iso3==country.code)
  } else {
    iso3<-sample(iso3, size=5)
  }

  scenarios <- esft::transmission_scenarios
  scenario_label<-
    scenarios$imperial_category_labels[scenarios$imperial_scenario == scenario]
  scenario_label<-gsub(" ", "%20", scenario_label)
  urls.to.try<-list()
  for(c in iso3){
    qurl<-paste0("https://raw.githubusercontent.com/mrc-ide/global_lmic_projections_esft/main/",
                c, "/",scenario_label, ".rds")
    urls.to.try<-append(urls.to.try, qurl)
  }

  if(warnings==TRUE){
    df_list <-lapply(urls.to.try, readUrl)
  } else {
    df_list <-suppressMessages(lapply(urls.to.try, readUrl)) # error here
  }

  df_list<-df_list[!sapply(df_list,is.null)]
  all.data <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

  return(all.data)
}

#' Reads urls
#' Note: taken from a stack overflow thread.
#' Modify somewhat please.
#'
#' @param url
#' @importFrom gh gh
#' @export
#' @return Whatever url is read.
readUrl <- function(url) {
  out<-tryCatch(readRDS(url(url, method="libcurl")),
                    error=function(cond) {
                      message(paste("URL does not exist:", url_name))
                      message(cond)
                      return(NA)
                    },
                    warning=function(cond) {
                      message(paste("URL caused a warning:", url_name))
                      message(cond)
                      return(NULL)
                    },
                    finally={
                      message(paste("Processed URL:", url_name))
                    }
    )
  }
  return(out)
}
