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
load_imperial_data3 <- function(warnings=FALSE){

  iso3 <-read.csv("data-raw/countries.csv")
  colnames(iso3)[1]<-"country_code"
  iso3<-iso3[!is.na(iso3)]

  urls.to.try<-list()
  for(country in iso3){
    qurl<-paste0("https://raw.githubusercontent.com/mrc-ide/global-lmic-reports/master/",
                country, "/projections.csv")

    urls.to.try<-append(urls.to.try, qurl)
  }

  if(warnings==TRUE){
    df_list <-lapply(urls.to.try[1:5], readUrl4)
  } else {
    df_list <-suppressMessages(lapply(urls.to.try[1:5], readUrl4))
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
readUrl4 <- function(url) {
  # download
  tmp<-tempfile()
  gh::gh(paste0('GET ', url), .destfile = tmp, .overwrite = TRUE)
  # read
  out <- tryCatch(read.csv(tmp),
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
  unlink(tmp)
  return(out)
}
