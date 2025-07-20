
ui <- shiny::fluidPage(
  shiny::titlePanel("wordfindr"),
  shiny::mainPanel(
    shiny::textInput("text", "pattern", "", "33%", "Enter pattern..."),
    shiny::textOutput("value")
  )
)

server <- function(input, output) {
  output$value <- shiny::renderText({ input$text })
}

shiny::shinyApp(ui, server)
