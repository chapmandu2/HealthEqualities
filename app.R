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
                          helpText("How effective are different sectors and regions at tackling health inequalities?"),
                          
                          # 
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(inputId="RegionV1",
                                                 label="Region:",
                                                 choices=unique(data$Region),
                                                 selected=unique(data$Region)),
                              checkboxGroupInput(inputId="SectorV1",
                                                 label="Sector:",
                                                 choices=unique(data$Sector),
                                                 selected=unique(data$Sector))
                              
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
                              checkboxGroupInput(inputId="RegionV2",
                                                 label="Region:",
                                                 choices=unique(data$Region),
                                                 selected=unique(data$Region)[1]),
                              selectInput(inputId="HealthConditionV2",
                                                 label="Sector:",
                                                 choices=unique(data$HealthCondition),
                                                 selected=unique(data$HealthCondition)[1]),
                              selectInput(inputId="Year",
                                                 label="Year:",
                                                 choices=unique(data$Year),
                                                  selected="2017")
                              
                            ),
                          mainPanel(
                            plotOutput("bySector")
                          )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dat_filtered1 <- reactive({
    
    data %>% 
      dplyr::filter(Region %in% input$RegionV1, Sector %in% input$SectorV1) 
    
  })

  
  dat_filtered2 <- reactive({
    
    data %>% 
      dplyr::filter(Region %in% input$RegionV2, HealthCondition %in% input$HealthConditionV2, Year == input$Year) 
    
    
  })
  
   output$byTime <- renderPlot({
    ggplot(dat_filtered1(), aes(x=Year, y=Pct_InWork, colour=HealthCondition)) +
       geom_point() + geom_line() + ylab("% in work") +
       facet_wrap(~Sector+Region) + theme_bw()
   })
   
   output$bySector <- renderPlot({
     ggplot(dat_filtered2(), aes(x=Sector, y=Pct_InWork, colour=Sector, fill=Sector)) +
       geom_bar(stat = 'identity') + ylab("% in work") +
       facet_grid(Year+Region~HealthCondition) + theme_bw()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

