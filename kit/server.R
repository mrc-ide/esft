# Server logic
function(input, output, session) {

  # "Input" page server logic
  observeEvent(input$check_inputs_btn, {
     if (input$week1 > "2020-01-01" && input$week1 < "2023-01-01") {
      output$error_output <- renderText({
        "Error: The dates are not between 2020 and 2023!"
      })
      } else {
        output$error_output <- renderText("")
      }
  })
  observeEvent(input$check_props_btn, {
    if (input$mild_i_proportion + input$mod_i_proportion + input$sev_i_proportion +
        input$crit_i_proportion != 1) {
      output$error_output <- renderText({
        "Error: Proportions of infection severity level do not add up to one!"
      })
      } else {
      output$error_output <- renderText("")
      }
  })
  observeEvent(input$check_vent_btn, {
    if (input$perc_crit_inv_mv + input$perc_crit_noninv_mv != 1) {
      output$error_output <- renderText({
        "Error: Mechanical ventilation proportions do not add up to one!"
      })
      } else {
      output$error_output <- renderText("")
      }
  })
  # "Process" page server logic
  observeEvent(input$process_btn, {
    # Store parameters in a list
    parameters <- list(param1 = input$param1,
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
