#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)

data <- read_excel('data/exampledata.xlsx') %>%
  dplyr::mutate(Pct_InWork=rnorm(nrow(data), Pct_InWork, 1))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Health Inequalities In Work"),
   helpText("Use this page to discover which sectors and regions perform better when employing people with health conditions"),
   
   # 
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId="Region",
                     label="Region:",
                     choices=unique(data$Region)),
        checkboxGroupInput(inputId="Sector",
                           label="Sector:",
                           choices=unique(data$Sector))
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("byTime")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$byTime <- renderPlot({
    ggplot(data, aes(x=Year, y=Pct_InWork, colour=HealthCondition)) +
       geom_point() + geom_line() + ylab("% in work") +
       facet_wrap(~Region+Sector) + theme_bw()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

