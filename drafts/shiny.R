library(shiny)
library(forecast)

ui <- fluidPage(
  titlePanel("Forecast Parameter Sensitivity Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("training_period", "Training Period:",
                  min = 30, max = 90, value = 60, step = 1),
      sliderInput("test_period", "Test Period:",
                  min = 10, max = 30, value = 20, step = 1),
      selectInput("model", "Forecast Model:",
                  choices = c("ets", "arima", "auto.arima"))
    ),
    mainPanel(
      plotOutput("forecast_plot")
    )
  )
)

server <- function(input, output) {
  data("airline")
  air <- window(airline, end=c(2008,12))

  observeEvent(input$model, {
    model_fun <- switch(input$model,
                        "ets" = ets,
                        "arima" = arima,
                        "auto.arima" = auto.arima)
    fit <- model_fun(air)
    fcast <- forecast(fit, h=input$test_period)
    output$forecast_plot <- renderPlot({
      plot(fcast)
    })
  })
}

shinyApp(ui, server)
