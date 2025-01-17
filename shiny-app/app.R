library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(tidyverse)
library(broom)
f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)
# d <- select(d, height, weight, age, gender, major, zombies_killed, years_of_education)
d <- d |> select(
  Species1, Family1, Order1, Beak.Length_Culmen,
  Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length,
  Tail.Length, Mass, Habitat, Migration, Trophic.Level,
  Trophic.Niche, Min.Latitude, Max.Latitude, Centroid.Latitude,
  Range.Size, Primary.Lifestyle) |>
  mutate(Habitat = factor(Habitat),
         Migration = factor(Migration),
         Trophic.Level = factor(Trophic.Level),
         Trophic.Niche = factor(Trophic.Niche),
         Primary.Lifestyle = factor(Primary.Lifestyle))
#d$gender <- factor(d$gender)
#d$major <- factor(d$major)
#r <- c("height", "weight", "age", "zombies_killed", "years_of_education", "gender")
r <- names(d)
p <- names(d)

ui <- grid_page(
  layout = c(
    "header  header    header   ",
    "sidebar datatable datatable",
    "table   plotly    plotly   ",
    "table   plotly    plotly   "
  ),
  row_sizes = c(
    "80px",
    "2fr",
    "0.27fr",
    "1fr"
  ),
  col_sizes = c(
    "400px",
    "0.27fr",
    "1.73fr"
  ),
  gap_size = "0.5rem",
  grid_card(
    area = "sidebar",
    card_header(
      "",
      selectInput(
        inputId = "response",
        label = "Select response variable",
        choices = c("", r)
      ),
      selectInput(
        inputId = "predictors",
        label = "Select predictor variable(s)",
        choices = p,
        multiple = TRUE
      ),
      selectInput(
        inputId = "reg_type",
        label = "Select regression type",
        choices = c("lm", "glm", "binomial")
      ),
      br(),
      textOutput(
        outputId = "model"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Simple LM Visualizer",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "table",
    card_header("Table"),
    card_body(
      div(tableOutput(outputId = "modelresults"), style = "font-size:80%")
    )
  ),
  grid_card(
    area = "plotly",
    card_body(
      plotOutput(
        outputId = "plot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "datatable",
    card_body(
      div(DTOutput(outputId = "datatable", width = "100%"), style = "font-size:80%")
    )
  )
)


server <- function(input, output) {

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
  
  output$datatable <-
    renderDataTable(d, options = list(
      paging = TRUE,
      lengthMenu = list(c(5, 10, 25, -1), c('5', '10', '25', 'All')),
      pageLength = 5
    ))

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
      if (class(d[[x]]) == "numeric") {
        p <- ggplot(data = d,
                    aes(x = .data[[x]],
                        y = resp)) +
          geom_point() +
          geom_smooth(method = input$reg_type)
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
      } else if (class(d[[x[1]]]) == "numeric" & class(d[[x[2]]]) != "numeric"){
        p <- ggplot(data = d, aes(x = .data[[x[1]]],
                                  y = resp)) +
          geom_point() +
          geom_smooth(method = input$reg_type) +
          facet_wrap(~ d[[x[2]]])
        p <- p + xlab(x[1]) + ylab(y)
      } else if (class(d[[x[1]]]) != "numeric" & class(d[[x[2]]]) == "numeric"){
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

shinyApp(ui, server)
  

