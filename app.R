#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)

cdc <- read.csv("cleaned-cdc-mortality-1999-2010-2.csv", stringsAsFactors = FALSE)
cdc2010 <- cdc[cdc$Year == 2010,] #subset for 2010

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Mortality rates for U.S. states in 2010"),
   
   # Sidebar with a slider input for number of bins 
   sidebarPanel(
     selectInput("cause", "Cause of death:",
                 width = "auto",
                 choices=cdc2010$ICD.Chapter, 1
     )
   ),
   plotOutput("plt")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$plt <- renderPlot({
    ggplot(data=cdc2010[cdc2010$ICD.Chapter == input$cause,] , aes(x=reorder(State, Crude.Rate), y=Crude.Rate)) +
      labs(x="State", y="Crude Mortality Rate") +  
      geom_bar(stat="identity") +
      coord_flip() +
      theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

