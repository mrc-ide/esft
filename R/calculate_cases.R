#' Patient Calculations
#'
#' For now, this works with imperial model outputs.
#'
#' @param scenario
#' @return
#' @export
calculate_cases<-function(country_data, starting_date = "2022-01-02",
                          forecast_length = 10, # delivery_leadtime = 1,
                          model = "Imperial SEIR", transmission_scenario = "Medium"){
  # subset by starting date
  starting_date <- as.Date(starting_date)
  country_data$date <- as.Date(country_data$date)
  country_data<-subset(country_data, country_data$date >= starting_date)

   # subset by scenario
  # add in parameters for transmission here
  if (tolower(transmission_scenario) == "medium"){
    country_data<-subset(country_data, country_data$scenario == "Maintain Status Quo")
  } else if (tolower(transmission_scenario) == "high"){
    country_data<-subset(country_data, country_data$scenario == "Relax Interventions 50%")
  } else if (tolower(transmission_scenario) == "low"){
    country_data<-subset(country_data, country_data$scenario == "Additional 50% Reduction")
  }

  cases<-data.frame(week = seq(0,forecast_length, by=1))
  for (week in cases$week){
    cases$cumulative_infections[week] <-
      country_data$y_mean[country_data$date == starting_date + (week-1)*7
                          & country_data$compartment == "cumulative_infections"]
    cases$new_infections_total[week]<-
      sum(country_data$y_mean[(country_data$date <= starting_date + (week)*7) &
                              (country_data$date > starting_date + (week)*7-7) &
                              (country_data$compartment == "infections")])
  }

}
