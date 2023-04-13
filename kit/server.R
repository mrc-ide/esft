#
# Current error: no package called esft
# maybe have to build package first ?
# changed the visibility that helps
# but i would like to get rid of the dependencies, to make it actulaly easier
# setwd(dirname(getActiveDocumentContext()$path))
# load("../data/who.rda")

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
