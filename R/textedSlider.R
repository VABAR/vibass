## A shiny module with a slider and and alternative input via text
## https://mastering-shiny.org/scaling-modules.html

textedSliderInput <- function(id, label, min, max, value, step, size){
  ns = NS(id)
  # tagList(
  splitLayout(
    cellWidths = c("80%", "20%"),
    sliderInput(
      ns("slider"),
      label = label,
      min = min,
      max = max,
      value = value,
      step = step
    )
    ,
    textInput(ns("text"), label = NULL, value = value, size)
  )
}

textedSliderServer <- function(id, value) {
  moduleServer(id, function(input, output, session) {
    ans <- reactiveValues(value = value)
    updateSliderInput(session, "slider", value = value)

    ## observe slider
    observeEvent(input$slider,{
      ans$value <- input$slider
    }, ignoreInit = TRUE)

    ## observe text
    observeEvent(input$text,{
      ans$value <- as.numeric(input$text)
    })

    ## observe reactive
    observeEvent(ans$value,{
      updateSliderInput(session, "slider", value = ans$value)
      updateTextInput(session, "text", value = ans$value)
    })

    return(ans)
  })
}

textedSliderApp <- function() {
  ui <- fluidRow(
    # div(
    #   style = "display:inline-block;vertical-align:sub;",
    textedSliderInput("alpha", "alpha", 0, 110, 0.5, .2, "70px")
    # )
    ,
    verbatimTextOutput("text")
  )
  server <- function(input, output, session) {
    a0 <- textedSliderServer("alpha", 0.5)
    output$text <- renderPrint({
      print(a0$value)
    })
  }
  shinyApp(ui, server)
}
