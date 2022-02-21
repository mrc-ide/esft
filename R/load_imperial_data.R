#' Load imperial forecasts
#'
#' Takes a long time (a few hours) to download all the data.
#' Data results in about 5 GBs, 15 million obs.
#' Maybe filter out before?
#'
#' This function loads the imperial forecasts from their github repo.
#'
#' @param warnings Default is false. Whether to give warnings.
#' @import countrycode
#' @return All of the imperial SEIR projections.
#' @export
load_imperial_data <- function(warnings=FALSE){

  iso3 <-read.csv("data-raw/countries.csv")
  colnames(iso3)[1]<-"country_code"
  iso3<-iso3[!is.na(iso3)]

  urls.to.try<-list()
  for(country in iso3){
    url<-paste0("https://raw.githubusercontent.com/mrc-ide/global-lmic-reports/master/",
                country, "/projections.csv")
    urls.to.try<-append(urls.to.try, url)
  }

  if(warnings==TRUE){
    df_list <-lapply(urls.to.try, readUrl)
  } else {
    df_list <-suppressMessages(lapply(urls.to.try, readUrl))
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
#'
#' @return Whatever url is read.
readUrl <- function(url) {

  out <- tryCatch(read.csv(url),
                  error=function(cond) {
                    message(paste("URL does not exist:", url))
                    message(cond)
                    return(NA)
                  },
                  warning=function(cond) {
                    message(paste("URL caused a warning:", url))
                    message(cond)
                    return(NULL)
                  },
                  finally={
                    message(paste("Processed URL:", url))
                  }
  )

  return(out)
}
