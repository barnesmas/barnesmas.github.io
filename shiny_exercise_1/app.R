
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("What is your name?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput('name', label = NULL)),
        

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("greeting")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$greeting <- renderText({
      paste0('Hello, ', input$name, '.')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


### use prophet for time series.
