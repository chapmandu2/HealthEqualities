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
ui <- navbarPage(title="Health Inequalities In Work",
                 
                 # Application title
                 tabPanel("Sector View",
                          helpText("Compare sectors and regions"),
                          
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
                 ),
                 tabPanel("Individual View", 
                          helpText("Which sectors have are better for someone with my health condition?"),
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(inputId="Region",
                                                 label="Region:",
                                                 choices=unique(data$Region)),
                              checkboxGroupInput(inputId="Sector",
                                                 label="Sector:",
                                                 choices=unique(data$Sector)),
                              selectInput(inputId="Year",
                                                 label="Year:",
                                                 choices=unique(data$Year))
                              
                            ),
                          mainPanel(
                            plotOutput("bySector")
                          )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$byTime <- renderPlot({
    ggplot(data, aes(x=Year, y=Pct_InWork, colour=HealthCondition)) +
       geom_point() + geom_line() + ylab("% in work") +
       facet_wrap(~Sector+Region) + theme_bw()
   })
   
   output$bySector <- renderPlot({
     ggplot(data, aes(x=Sector, y=Pct_InWork, colour=HealthCondition, fill=HealthCondition)) +
       geom_bar(stat = 'identity') + ylab("% in work") +
       facet_grid(Year~Region+HealthCondition) + theme_bw()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

