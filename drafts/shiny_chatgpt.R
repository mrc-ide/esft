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
ui_capacity <- fluidPage(
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
ui_summaries <- fluidPage(
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




