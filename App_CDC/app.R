library(shiny)
library(bslib)
library(tidyverse)
library(ggthemes)

# loading data
cdc <- read_delim("data/cdc.txt", delim = "|") |> mutate(
  exerany  = factor(exerany,  levels = c(1, 0), labels = c("Yes", "No")),
  hlthplan = factor(hlthplan, levels = c(1, 0), labels = c("Yes", "No")),
  smoke100 = factor(smoke100, levels = c(1, 0), labels = c("Yes", "No")),
  gender   = factor(gender, levels = c("f", "m"), labels = c("Female", "Male")),
  genhlth  = factor(genhlth,
                    levels = c("excellent", "very good", "good", "fair", "poor"),
                    labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"))
)

# Define UI ----
ui <- page_sidebar(
  title = "CDC BRFSS Histograms",
  sidebar = sidebar(
    position = "right",
    width = "3in",
    selectInput(
      inputId = "var",
      label = "Select Variable:",
      choices = list("Actual Weight", "Desired Weight", "Height"),
      selected = "Actual Weight"
    ),
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 5,
      max = 50,
      value = 30
    ),
    radioButtons(
      inputId = "var_leg",
      label = "Select Fill/Legend Variable:",
      choices = list(
        "General Health",
        "Health Coverage",
        "Exercised in Past Month",
        "Smoked 100 Cigarettes",
        "Gender"
      ),
      selected = "General Health"
    )
  ),
  plotOutput(outputId = "distPlot")
)

# Define server logic ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    x_variable <- case_when(
      input$var == "Actual Weight"  ~ "weight",
      input$var == "Desired Weight" ~ "wtdesire",
      input$var == "Height"         ~ "height")
    
    x_axis_label <- switch(input$var,
                           "Actual Weight"  = "Actual Weight in Pounds",
                           "Desired Weight" = "Desired Weight in Pounds",
                           "Height"         = "Height in Inches")
    
    fill_variable <- switch(input$var_leg,
                            "General Health"          = "genhlth",
                            "Health Coverage"         = "hlthplan",
                            "Exercised in Past Month" = "exerany",
                            "Smoked 100 Cigarettes"   = "smoke100",
                            "Gender"                  = "gender")
    
    # bin breaks that respond to the slider
    x <- cdc |> pull(x_variable)
    bin_breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
                      length.out = input$bins + 1)
    
    ggplot(cdc, aes(x = !!sym(x_variable))) +
      geom_histogram(
        breaks = bin_breaks,
        aes(fill = !!sym(fill_variable)),
        position = "identity",
        alpha = 0.5
      ) +
      scale_fill_discrete(name = input$var_leg) +
      theme_fivethirtyeight(base_size = 20) +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        axis.title = element_text()
      ) +
      xlab(x_axis_label) +
      ylab("Count") +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
  })
}

# running app
shinyApp(ui = ui, server = server)