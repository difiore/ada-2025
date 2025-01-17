library(shiny)
library(DT)
library(tidyverse)
library(broom)
f <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)
d <- select(d, height, weight, age, gender, major, zombies_killed, years_of_education)
d$gender <- factor(d$gender)
d$major <- factor(d$major)
r <- c("height", "weight", "age", "zombies_killed", "years_of_education", "gender")
p <- names(d)
# Define the UI ----
ui <- fluidPage(
  titlePanel(h1("Simple LM Visualizer")),
  sidebarLayout(
    sidebarPanel(width = 6,
      selectInput(
        "response",
        label = "Choose a response variable...",
        choices = c("", r)
      ),
      br(),
      selectInput(
        "predictors",
        label = "Choose one or more predictor variables...",
        choices = p,
        multiple = TRUE
      ),
      br(),
      selectInput(
        "reg_type",
        label = "Choose type of regression...",
        choices = c("lm", "glm", "binomial")
      ),
      br(),
      textOutput("model"),
      br(),
      tableOutput("modelresults"),
      style="text-align:center"
    ),
    mainPanel(width = 6,
      dataTableOutput("datatable"),
      plotOutput("plot")
    )
  )
)

# Define server logic ----
server <- function(input, output) {

  output$datatable <-
    renderDataTable(d, options = list(
      paging = TRUE,
      lengthMenu = list(c(5, 10, 25, -1), c('5', '10', '25', 'All')),
      pageLength = 5
    ))
  
  m <- reactive({
    mod <- NULL
    if (input$response == "" |
        length(input$predictors) == 0) {
      return(mod)
    }
    mod <- paste0(input$response, " ~ ", input$predictors[1])
    if (length(input$predictors) > 1) {
      for (i in 2:length(input$predictors)) {
        mod <- paste0(mod, " + ", input$predictors[i])
      }
    }
    return(mod)
  })
  
  output$model <- renderText({paste0("Model: ", print(paste0(input$reg_type, "(", m(), ")")))})
  
  output$modelresults <- renderTable({
    if (!is.null(m())) {
      if (input$reg_type == "lm"){
      res <- lm(data = d, formula = m())
      } else if (input$reg_type == "glm") {
        res <- glm(data = d, formula = m(), family = poisson(link = "log"))
      } else {
        res <- glm(data = d, formula = m(), family = binomial(link = "logit"))
      }
      tidy(res) |> select(term, estimate, p.value)
    }
  }, width = "100%", rownames = TRUE, striped = TRUE, spacing = "s", bordered =
    TRUE, align = "c", digits = 3)

  output$plot <- renderPlot({
    if (!is.null(m()) & length(input$predictors) == 1) {
      y <- input$response
      x <- input$predictors
      #if (input$reg_type == "glm") {
      #    d <- d |> mutate(resp = log(.data[[y]]))
      #  } else {
          d <- d |> mutate(resp = .data[[y]])
      #  }
      if (class(d[[x]]) != "factor") {
        p <- ggplot(data = d,
                    aes(x = .data[[x]],
                        y = resp)) +
          geom_point() +
          geom_smooth(method = "glm")
      } else {
        p <- ggplot(data = d,
                    aes(x = .data[[x]],
                        y = resp)) +
          geom_violin() +
          geom_jitter(width = 0.2, alpha = 0.5)
      }
      p <- p + xlab(x) + ylab(y) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      p
    } else if (!is.null(m()) & length(input$predictors) == 2) {
      y <- input$response
      x <- input$predictors
    #  if (input$reg_type == "glm") {
    #    d <- d |> mutate(resp = log(.data[[y]]))
    #  } else {
        d <- d |> mutate(resp = .data[[y]])
    #  }
      if (class(d[[x[1]]]) == "factor" & class(d[[x[2]]]) == "factor") {
        p <- ggplot(data = d, aes(x = .data[[x[1]]],
                                  y = resp)) +
          geom_violin() +
          geom_jitter(width = 0.2, alpha = 0.5) +
          facet_wrap(~ d[[x[2]]])
        p <- p + xlab(x[1]) + ylab(y)
      } else if (class(d[[x[1]]]) != "factor" & class(d[[x[2]]]) == "factor"){
        p <- ggplot(data = d, aes(x = .data[[x[1]]],
                                  y = resp)) +
          geom_point() +
          geom_smooth(method = input$reg_type) +
          facet_wrap(~ d[[x[2]]])
        p <- p + xlab(x[1]) + ylab(y)
      } else if (class(d[[x[1]]]) == "factor" & class(d[[x[2]]]) != "factor"){
        p <- ggplot(data = d, aes(x = .data[[x[2]]],
                                  y = resp)) +
          geom_point() +
          geom_smooth(method = input$reg_type) +
          facet_wrap(~ d[[x[1]]])
        p <- p + xlab(x[2]) + ylab(y)
      } else {
        p <- NULL
      }
      p <- p + theme(
        axis.text.x = element_text(angle = 90,
                                   hjust = 1)) 
      p
    }
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
