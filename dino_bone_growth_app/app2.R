library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(easystats)
theme_set(theme_classic())
getwd() %>% print()
dinosaur <-
  readxl::read_xlsx("./Dinosaur Age vs Femur Length.xlsx") |>
  janitor::clean_names() |>
  rename(data_set = x1) |>
  fill(data_set, .direction = "down") |>
  drop_na()

choices = c('Linear',
            'Quadratic',
            'Cubic',
            'Exponential',
            'Logarithmic',
            'Logistic',
            'Gompertz')

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Dinosaur Bone Dimension"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data_set",
        "Select Data Set",
        choices = dinosaur$data_set,
        selected = "Tyrannosaur 1"
      ),
      selectInput(
        "method",
        label = "Model",
        choices = choices,
        selected = 'Linear'
      ),
      sliderInput(
        'input_bone',
        label = 'Bone Measurement',
        min = 0,
        max = max(dinosaur$bone_dimension) + 10,
        step = 1,
        value = 10
      ),
      textOutput('age_prediction')
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotlyOutput("distPlot"),
              plotlyOutput("residPlot"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  #
  
  dinosaur_filtered <- reactive({
    filter(dinosaur, data_set == input$data_set)
  })
  
  
  
  
  model <- reactive({
    if (input$method == "Linear") {
      glm(data = dinosaur_filtered(), formula = age ~ bone_dimension)
    } else if (input$method == "Quadratic") {
      glm(data = dinosaur_filtered(),
          formula = age ~ poly(bone_dimension, 2))
    } else if (input$method == "Cubic") {
      glm(data = dinosaur_filtered(),
          formula = age ~ poly(bone_dimension, 3))
    } else if (input$method == "Exponential") {
      nls(
        age ~ a * exp(b * bone_dimension),
        data = dinosaur_filtered(),
        start = list(a = 1, b = 1),
        model = TRUE
      )
    } else if (input$method == "Logarithmic") {
      a = 1
      b = 0
      nls(age ~ (b + a * log(bone_dimension)),
          data = dinosaur_filtered(),
          start = list(a = a, b = b))
    } else if (input$method == "Logistic") {
      L = max(dinosaur_filtered()$bone_dimension) + 10
      k = 0.1
      x0 = median(dinosaur_filtered()$age)
      nls(
        age ~ -log(L / bone_dimension - 1) / k + x0,
        data = dinosaur_filtered(),
        start = list(k = k, x0 = x0, L = L)
      )
    } else if (input$method == "Gompertz") {
      c = 1
      b = median(dinosaur_filtered()$age)
      a = max(dinosaur_filtered()$bone_dimension) + 1
      nls(age ~ 1 / c * (b - log(log(
        a / (bone_dimension)
      ))),
      data = dinosaur_filtered(),
      start = list(a = a, b = b, c = c))
    }
  })
  output$distPlot <- renderPlotly({
    ggplotly(
      ggplot(dinosaur_filtered(), aes(x = bone_dimension, y = age)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_function(
          fun = function(x) {
            predict(model(), newdata = data.frame(bone_dimension = x))
          },
          colour = "red"
        ) +
        labs(y = 'Age', x = 'Bone Dimension (mm)')
    )
  })
  
  output$residPlot <- renderPlotly({
    resid_df <-
      data.frame(
        bone_dimension = dinosaur_filtered()$bone_dimension,
        resid = dinosaur_filtered()$age - predict(model()),
        newdata = dinosaur_filtered()$bone_dimension
      )
    rmse <- sqrt(mean(resid_df$resid ^ 2))
    ggplotly(
      ggplot(data = resid_df, aes(x = bone_dimension , y = resid)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        annotate(
          'text',
          x = .75 * max(resid_df$bone_dimension),
          y = 4,
          label = paste0('RMSE = ', round(rmse, 3)),
          size = 5
        )
    )
    
  })
  output$age_prediction = renderText({
    pred = round(predict(model(), newdata = data.frame(bone_dimension = input$input_bone)), 2)
    paste0('Predicted Age = ', pred)
             
              
  })
}

# Run the application
shinyApp(ui = ui, server = server)



#### for more models
# try ordered betea regression models
# and growth curver


# our 4 models could be
# linear
# 2nd orde poly
# logistic
# beta

# let users put in y value and predict age.
# that means you want to switch age and bone_dimension
