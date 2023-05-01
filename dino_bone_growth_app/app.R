library(shiny)
library(tidyverse)
library(plotly)
getwd() %>% print()
dinosaur <- readxl::read_xlsx("./Dinosaur Age vs Femur Length.xlsx") |> 
  janitor::clean_names() |> 
  rename(data_set = x1) |> 
  fill(data_set, .direction = "down") |> 
  drop_na()
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Dinosaur Bone Dimension"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("data_set", "Select data_set", choices = dinosaur$data_set, selected = "Tyrannosaur 1"),
      selectInput("method", label = "Model", choices = c("lm", 'quadratic', 'logistic', 'beta'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      plotlyOutput("residPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    ggplotly(dinosaur |> 
               filter(data_set == input$data_set) |> 
               ggplot(aes(y = age, x = bone_dimension)) + 
               geom_point() +
               geom_smooth(method = input$method))
  })
  
  output$residPlot <- renderPlotly({
    dino_filtered <- 
      dinosaur |> 
      filter(data_set == input$data_set)
    
    if (input$method == "lm") {
      model <- lm(age~bone_dimension, data = dino_filtered)
      plt <- ggplot(model, aes(x = .fitted, y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0)
    }
    if (input$method == 'quadratic'){
      model <- lm(age ~ poly(bone_dimension, degree = 2, raw = TRUE))
      pit <- ggplot(model, aes(x = .fitted, y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0)
    }
    ggplotly(plt)
        
    
    
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

