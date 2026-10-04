library(shiny)

ui <- fluidPage(
  titlePanel("New Hope Birds"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel()
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
