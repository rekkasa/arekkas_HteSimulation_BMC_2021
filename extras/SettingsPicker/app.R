#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Non-monotonic settings"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(
                inputId =  "b1",
                label = "First intersection with the diagonal",
                value = -5,
                min = -7,
                max = 10,
                step = .01
            ),
            numericInput(
                inputId =  "b2",
                label = "Second intersection with the diagonal (bigger!!)",
                value = -5,
                min = -7,
                max = 10,
                step = .01
            ),
            numericInput(
                inputId =  "m",
                label = "Maximum distance from the diagonal within (b1, b2)",
                value = -5,
                min = -7,
                max = 10,
                step = .01
            )
        ),
        
    
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
