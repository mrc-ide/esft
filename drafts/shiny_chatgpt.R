library(shiny)

# UI for the "Input" page
ui_input <- fluidPage(
  titlePanel("Input Parameters"),
  sidebarLayout(
    sidebarPanel(
      numericInput("param1", "Parameter 1:", value = 0),
      numericInput("param2", "Parameter 2:", value = 0),
      numericInput("param3", "Parameter 3:", value = 0),
      actionButton("submit_btn", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("error_output")
    )
  )
)

# UI for the "Process" page
ui_process <- fluidPage(
  titlePanel("Process Parameters"),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("params_output"),
      actionButton("process_btn", "Process")
    ),
    mainPanel(
      verbatimTextOutput("function_output")
    )
  )
)

# UI for the "Output" page
ui_output <- fluidPage(
  titlePanel("Output"),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("combined_params_output"),
      actionButton("combine_btn", "Combine")
    ),
    mainPanel(
      verbatimTextOutput("combined_output")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # "Input" page server logic
  observeEvent(input$submit_btn, {
    # Check if parameters add up to zero
    if (input$param1 + input$param2 + input$param3 != 0) {
      output$error_output <- renderText({
        "Error: Parameters do not add up to zero!"
      })
    } else {
      # No error, proceed to "Process" page
      output$error_output <- renderText("")
      updateTabsetPanel(session, "main_tabs", selected = "process")
    }
  })

  # "Process" page server logic
  observeEvent(input$process_btn, {
    # Store parameters in a list
    params <- list(param1 = input$param1,
                   param2 = input$param2,
                   param3 = input$param3)

    output$params_output <- renderPrint({
      params
    })

    # Perform some processing on parameters
    processed_output <- processFunction(params)

    output$function_output <- renderPrint({
      processed_output
    })

    # Proceed to "Output" page
    updateTabsetPanel(session, "main_tabs", selected = "output")
  })

  # "Output" page server logic
  observeEvent(input$combine_btn, {
    # Combine parameters and processed output
    combined_params <- list(param1 = input$param1,
                            param2 = input$param2,
                            param3 = input$param3)
    combined_output <- combineFunction(combined_params, processed_output)

    output$combined_params_output <- renderPrint({
      combined_params
    })

    output$combined_output <- renderPrint({
      combined_output
    })
  })

  # Function to process parameters
  processFunction <- function(params) {
    # Perform some processing here
    # Example: Squaring each parameter
    processed_params <- lapply(params, function(x) x^2)
    return(processed_params)
  }

  # Function to combine parameters and processed output
  combineFunction <- function(params, processed_output) {
    # Combine parameters and processed output
    # Example: Concatenate parameters and processed output
    combined_output <- c(params, processed_output)
    return(combined_output)
  }
}

# Create the Shiny app
shinyApp(
  ui = navbarPage("Package Replication",
                  tabPanel("Input", ui_input),
                  tabPanel("Process", ui_process),
                  tabPanel("Output", ui_output)
  ),
  server = server
)

