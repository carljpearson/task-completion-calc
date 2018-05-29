#total testing bed

library(shiny)


#test away

if (interactive()) {
  
  ui <- fluidPage(
    checkboxInput("somevalue", "Some value", FALSE),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({ input$somevalue })
  }
  shinyApp(ui, server)
}