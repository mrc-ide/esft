# Create the Shiny app
shinyApp(
  ui = navbarPage("ESFT",
                  tabPanel("Inputs and Parameters", ui_input),
                  # i think also i can use source (ui_input.R)
                  tabPanel("Capacity", ui_capacity),
                  tabPanel("Weekly Summaries", ui_summaries),
                  tabPanel("Equipment Forecasts", ui_forecasts)
  ),
  server = server
)
