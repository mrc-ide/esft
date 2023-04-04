#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
load("../data/who.rda")

function(input, output, session) {
  output$parameters <- renderUI({
    (esft::get_parameters())
  })
  output$country_name <- renderPrint({
    countrycode::countrycode(sourcevar=input$country_code,
                             origin="iso3c",
                             destination = "country.name")})

  output$capacity <- renderUI({
    esft::get_country_capacity(iso3c=input$country_code)
  })

  # output$table <- DT::renderDataTable({
  #   DT::datatable(cars)
  # })
}
