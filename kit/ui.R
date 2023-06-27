library(shiny)
library(markdown)
library(rstudioapi)
library(esft)
# devtools::install_github("mrc-ide/esft")

navbarPage("ESFT",
           tabPanel("Inputs and Parameters",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("country_code",
                                    label = "Choose a country:",
                                    choices = unique(who$country_code)),
                        h5("Country Selected:"),
                        verbatimTextOutput("country_name"),
                        numericInput("forecast_period",
                                     "Forecast Period (in weeks)",
                                     min = 1, value = 12, max = 52, step=1),
                        dateInput('week1',
                                  label = 'First date of forecast: yyyy-mm-dd',
                                  value = '2022-01-01'
                        ),
                        # option to put in delivery lead time
                      ),
                      mainPanel(
                        h4("Set Parameters"),
                        tabsetPanel(
                          tabPanel("Main",
                            h5("Proportions of Severity"),
                            numericInput("mild_i_proportion",
                                         "Proportion of cases that are mild",
                                         min = 0, value = 0.4, max=1, step=0.1),
                            numericInput("mod_i_proportion",
                                         "Proportion of cases that are moderate",
                                         min = 0, value = 0.4, max=1, step=0.1),
                            numericInput("sev_i_proportion",
                                         "Proportion of cases that are severe",
                                         min = 0, value = 0.15, max=1, step=0.1),
                            numericInput("crit_i_proportion",
                                         "Proportion of cases that are critical",
                                         min = 0, value = 0.05, max=1, step=0.1),
                            h5("Length of Stay in Weeks"),
                            numericInput("stay_mild",
                                         "Weeks of illness/isolation for mild cases",
                                         min = 0, value = 2),
                            numericInput("stay_mod",
                                         "Weeks of illness/isolation for moderate cases",
                                         min = 0, value = 2),
                            numericInput("stay_sev",
                                         "Weeks of illness/hospitalisation for severe cases",
                                         min = 0, value = 1),
                            numericInput("stay_crit",
                                         "Weeks of illness/hospitalisation for critical cases",
                                         min = 0, value = 2),
                            h5("Infection Fatality Rates"),
                            numericInput("ifr_sev",
                                         "Infection fatality rate of severe infections",
                                         min = 0, value = 0.134, max=1, step=0.01),
                            numericInput("ifr_crit",
                                         "Infection fatality rate of critical infections",
                                         min = 0, value = 0.5, max=1, step=0.01),
                            h5("Proportion of Health-Care Workers Allocated to COVID Response"),
                            numericInput("perc_hcws_not_covid",
                                         "Percent of HCWS not allocated to COVID-19 response",
                                         min = 0, value = 0.4, max=1, step=0.01),
                            numericInput("perc_hcws_treat_covid",
                                         "Percent of HCWS allocated to COVID-19 response",
                                         min = 0, value = 0.53, max=1, step=0.01),
                            numericInput("perc_hcws_screen_covid",
                                         "Percent of HCWS allocated to COVID-19 screening",
                                         min = 0, value = 0.07, max=1, step=0.01),
                            h5("Health-Care Worker and Bed Parameters"),
                            numericInput("n_hosp_beds_per_care_unit",
                                         "Number of hospital beds per care unit",
                                         min = 0, value = 40, step=1),
                            numericInput("ambulancews_per_bed",
                                         "Ambulance workers per hospital bed",
                                         min = 0, value = 0.06),
                            numericInput("bioengs_per_bed",
                                         "Bioengineers per hospital bed",
                                         min = 0, value = 0.02),
                            numericInput("n_inf_caregivers_hosp",
                                         "Number of informal caregivers per hospital bed",
                                         min = 0, value = 0),
                            numericInput("n_inf_caregivers_isol",
                                         "Number of informal caregivers per isolating case",
                                         min = 0, value = 1),
                            numericInput("cases_screened_per_hcw_per_day",
                                         "Cases screened per HCW per day",
                                         min = 0, value = 10),
                            h5("Percent ventilated"),
                            numericInput("perc_crit_inv_mv",
                                         "Percent of critical patients invasively ventilated",
                                         min = 0, value = 0.66, max=1,step=0.01),
                            numericInput("perc_crit_noninv_mv",
                                         "Percent of critical patients noninvasively ventilated",
                                         min = 0, value = 0.33, max=1,step=0.01),
                            h5("Oxygen Flow"),
                            numericInput("o2_flow_sev",
                                         "O2 flow of severe patients in LPM",
                                         min = 0, value = 10, step=1),
                            numericInput("o2_flow_crit_inv_mv",
                                         "O2 flow of critical patients (invasive) in LPM",
                                         min = 0, value = 30, step=1),
                            numericInput("o2_flow_crit_noninv_mv",
                                         "O2 flow of critical patients (noninvasive) in LPM",
                                         min = 0, value = 30, step=1),
                          ),
                          tabPanel("Diagnostics",
                                   "Left blank for now"),
                          tabPanel("Lab",
                                   "Left blank, from get_lab")


                        ),

                        # Diagnostic parameters, get_diagnostic_parameters
                        # Lab parameters, get_lab_parameters
                      ),

                    )
           ),
           tabPanel("Capacity",
                    uiOutput("capacity")
           ),
           tabPanel("Country Model Fits",
                    verbatimTextOutput("Imperial SEIR fit data")
           ),
           tabPanel("Weekly Summaries",
                    verbatimTextOutput("summary")
           ),
           tabPanel("Commodities Forecast",
                    verbatimTextOutput("summary")
           ),
           navbarMenu("More",
                      tabPanel("Reference Data",
                               verbatimTextOutput("table")
                      ),
                      tabPanel("Sources",
                               fluidRow(
                                 column(3,
                                        img(class="img-polaroid",
                                            src=paste0("http://upload.wikimedia.org/",
                                                       "wikipedia/commons/9/92/",
                                                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                        tags$small(
                                          "Source: Photographed at the Bay State Antique ",
                                          "Automobile Club's July 10, 2005 show at the ",
                                          "Endicott Estate in Dedham, MA by ",
                                          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                            "User:Sfoskett")
                                        )
                                 )
                               )
                      )
           )
)
