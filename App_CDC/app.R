library(shiny)
library(bslib)
library(tidyverse)


# loading data
cdc <- read_delim("data/cdc.txt", delim = "|")

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  title = "CDC BRFSS: Histogram of Weight Grouped by Gender",
  sidebar = sidebar(
    position = "right",
    width = "3in",
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 5,
      max = 50,
      value = 30,
      animate = TRUE        # sneaky play button
    )
  ),
  plotOutput(outputId = "distPlot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    x_variable    <- "weight"
    x_axis_label  <- "Weight in Pounds"
    fill_variable <- "gender"
    fill_label    <- "Gender"
    
    # bin shiftability
    x <- cdc |> pull(x_variable)
    bin_breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
                      length.out = input$bins + 1)
    
    ggplot(data = cdc, aes(x = !!sym(x_variable))) +
      geom_histogram(
        breaks = bin_breaks,
        aes(fill = !!sym(fill_variable)),
        position = "identity",
        alpha = 0.5
      ) +
      scale_fill_discrete(name = fill_label, labels = c("f" = "Female", "m" = "Male")) +
      theme_minimal(base_size = 20) +
      theme(legend.position = "inside") +
      xlab(x_axis_label) +
      ylab("Count")
  })
}

shinyApp(ui = ui, server = server)
