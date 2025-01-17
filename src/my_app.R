library(shiny)
# Define the UI ----

ui <- fluidPage(
  titlePanel(h1("My First Web App")),
  sidebarLayout(
    sidebarPanel("sidebar panel"),
    mainPanel(h3("Wow, I'm creating a webpage and web server!"),
              h4("This is really cool.")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
