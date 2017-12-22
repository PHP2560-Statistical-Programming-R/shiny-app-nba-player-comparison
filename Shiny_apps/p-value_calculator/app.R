# simple p-value calculator

# check for requiered packages and get them if needed
names <- c(
  "shiny",
  "stats"
)

for(name in names) {
  if (!(name %in% installed.packages()))
    install.packages(name, repos="http://cran.us.r-project.org")
  library(name, character.only=TRUE)
}

ui <- fluidPage(
  titlePanel("p-value calculator"),
  br(),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "z", label = "z-score",  step = 0.1, value = 0)
                 ),
    mainPanel(
      h1(textOutput("p"))
              )
  )
)

server <- function(input, output) {
  output$p <- renderText({
      paste("p-value: ", pnorm(- input$z))
                              })
}

shinyApp(ui = ui, server = server)

