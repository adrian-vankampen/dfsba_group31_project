# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data

# trend_data <- read_csv("data/trend_data.csv")
# trend_description <- read_csv("data/trend_description.csv")

# ==============================================================================

# Define UI

ui <- fluidPage(
  # sliderInput(inputId = "date_range",
  #             label = "Choose a date range",
  #             min = as.Date("2000-01-01","%Y-%m-%d"),
  #             max = as.Date("2020-11-30","%Y-%m-%d"),
  #             value=c(as.Date("2000-01-01"),
  #                     as.Date("2020-11-30")),
  #             timeFormat="%Y-%m-%d",
  sliderInput(inputId = "n",
              label = "Choose N",
              min = 1,
              max = 1000000,
              step = 1,
              value = 100),#             step = 30),
  sliderInput(inputId = "mean",
              label = "Choose mean",
              min = -10,
              max = 10,
              step = 0.1,
              value = 0),
  sliderInput(inputId = "sd",
              label = "Choose standard deviation",
              min = 0,
              max = 5,
              step = 0.1,
              value = 1),
  plotOutput("hist")
)

# ==============================================================================

# Define server function
server <- function(input, output) {
  output$hist <- renderPlot({
    title <- "Normal distribution"
    hist(rnorm(n = input$n,
               mean = input$mean,
               sd = input$sd),
         breaks = max(10, input$n/10000),
         xlim = c(-20, 20))
    })
  
}

# ==============================================================================

# Create Shiny object
shinyApp(ui = ui, server = server)
