library(shiny)
# Define the UI ----

ui <- fluidPage(titlePanel(h4("My First Web App")),
       sidebarLayout(sidebarPanel(img(src = "batnet.jpg", height = 72, width = 72), selectInput(
         "favorite_monster",
         label = "Choose one of the following...",
         choices = c("Zombie", "Vampire", "Alien", "Werewolf"),
         selected = "Zombie"
       ), style="text-align:center"),
        mainPanel(h3("Wow, I'm creating a webpage and web server!"),
        h4("This is really cool.", style="color:blue; text-align:center"), textOutput("favorite_monster"))))

# Define server logic ----
server <- function(input, output) {
  output$favorite_monster <- renderText({paste0("You have selected... ",input$favorite_monster)})
}

# Run the app ----
shinyApp(ui = ui, server = server)
