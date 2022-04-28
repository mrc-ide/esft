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
#'
#' @import countrycode
#' @import gh
#'
#' @return All of the imperial SEIR projections.
#' @export
load_imperial_data <- function(warnings=FALSE, country.code=NULL){

  iso3 <-read.csv("data-raw/countries.csv")
  colnames(iso3)[1]<-"country_code"
  iso3<-iso3[!is.na(iso3)]
  if (!(is.null(country.code))){
    iso3<-subset(iso3, iso3==country.code)
  } else {
    iso3<-sample(iso3, size=5)
  }

  urls.to.try<-list()
  for(c in iso3){
    qurl<-paste0("https://raw.githubusercontent.com/mrc-ide/global-lmic-reports/master/",
                c, "/projections.csv")
    urls.to.try<-append(urls.to.try, qurl)
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
#' @importFrom gh gh
#' @export
#' @return Whatever url is read.
readUrl <- function(url) {
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
