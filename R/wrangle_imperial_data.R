#' Filter and process the imperial data
#'
#' This function loads the imperial forecasts from their github repo.
#'
#' @import countrycode
#' @return All of the imperial SEIR projections.
#' @export process imperial data like how esft did
wrangle_imperial_data <- function(date = "2022-01-01", scenario = "all",
                                  iso3c = "all", cumulative_infections = TRUE,
                                  imperial_data = NULL){

  # load data if required
  if(is.null(imperial_data)){
    imperial_data<-load_imperial_data()
  }

  # filter by date
  date <- as.Date(date)
  imperial_data$date <- as.Date(imperial_data$date)
  imperial_data<-imperial_data[imperial_data$date >= date,]

  # subset by scenario type
  if(tolower(scenario) == "all") {
    imperial_data<-subset(imperial_data, imperial_data$scenario %in%
                            c("Maintain Status Quo", "Optimistic", "Pessimistic"))
  } else if(tolower(scenario) == "maintain status quo"){
    imperial_data<-subset(imperial_data,
                          imperial_data$scenario == "Maintain Status Quo")
  } else if(tolower(scenario) == "optimistic"){
    imperial_data<-subset(imperial_data,
                          imperial_data$scenario =="Optimistic")
  } else if(tolower(scenario) == "pessimistic"){
    imperial_data<-subset(imperial_data,
                          imperial_data$scenario == "Pessimistic")
  }
  # subset by compartment
  if(cumulative_infections == TRUE){
    imperial_data<-subset(imperial_data,
                          imperial_data$compartment == "cumulative_infections")
  }
  # subset by country code
  if(iso3c != "all"){
    iso3c<-as.character(iso3c)
    if(!iso3c %in% unique(imperial_data$iso3c)){
      stop("Iso3c not found")
    }
    imperial_data <- subset(imperial_data, imperial_data$iso3c == iso3c)
  }

  # cumulative case types
  # new cases by week

  # weekly summary csv
}
