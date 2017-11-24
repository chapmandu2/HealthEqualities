# Shiny app to explore simulated health inequalities data 
# Manchester DWP HackTheNorth day November 2017

library(shiny)
library(tidyverse)
library(readxl)


# Define UI for application that draws a histogram
ui <- navbarPage(title="Health Inequalities In Work",
                 
                 # Application title
                 tabPanel("Sector View",
                          helpText("How effective are different sectors and regions at tackling health inequalities?"),
                          selectInput(inputId="inputData",
                                      label="Select input data:",
                                      choices=c("Example Data"=1, "Simulated Data"=2),
                                      selected=1),
                          sidebarLayout(
                            uiOutput("ui1"),
                            mainPanel(
                              plotOutput("byTime")
                            )
                          )
                 ),
                 tabPanel("Individual View", 
                          helpText("Which sectors have are better for someone with my health condition?"),
                          sidebarLayout(
                              uiOutput("ui2"),
                          mainPanel(
                            plotOutput("bySector")
                          )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- reactive({
    if (input$inputData == 1) {
      read_excel('data/exampledata.xlsx') %>%
        dplyr::mutate(Pct_InWork=rnorm(nrow(.), Pct_InWork, 1))
    } else {
      read_tsv('data/simulated_data.tsv')  
    }
    
  })
  
  output$ui1 <- renderUI({
    sidebarPanel(
      checkboxGroupInput(inputId="RegionV1",
                         label="Region:",
                         choices=unique(data()$Region),
                         selected=unique(data()$Region)[1]),
      checkboxGroupInput(inputId="SectorV1",
                         label="Sector:",
                         choices=unique(data()$Sector),
                         selected=unique(data()$Sector))
      
    )
    
    
  })

  output$ui2 <- renderUI({
    sidebarPanel(
    checkboxGroupInput(inputId="RegionV2",
                       label="Region:",
                       choices=unique(data()$Region),
                       selected=unique(data()$Region)[1]),
    selectInput(inputId="HealthConditionV2",
                label="Sector:",
                choices=unique(data()$HealthCondition),
                selected=unique(data()$HealthCondition)[1]),
    selectInput(inputId="Year",
                label="Year:",
                choices=unique(data()$Year),
                selected="2017")
    )
  })
  
  dat_filtered1 <- reactive({
    data() %>% 
      dplyr::filter(Region %in% input$RegionV1, Sector %in% input$SectorV1) 
  })
  
  
  dat_filtered2 <- reactive({
    data() %>% 
      dplyr::filter(Region %in% input$RegionV2, HealthCondition %in% input$HealthConditionV2, Year == input$Year) 
  })
  
    
   output$byTime <- renderPlot({
    ggplot(dat_filtered1(), aes(x=Year, y=Pct_InWork, colour=HealthCondition)) +
       geom_point(size=rel(3)) + geom_line(size=rel(2)) + ylab("% in work") +
       facet_wrap(~Sector+Region) + theme_bw() + 
       theme(axis.title=element_text(size=rel(2.5)),
             axis.text=element_text(size=rel(1.7)),
             legend.title=element_text(size=0, color='white'),
             legend.text=element_text(size=rel(1.7)),
             strip.text=element_text(size=rel(1.7)),
             panel.spacing.x=unit(20, units='points'),
             panel.spacing.y=unit(5, units='points'))
   })
   
   output$bySector <- renderPlot({
     ggplot(dat_filtered2(), aes(x=Sector, y=Pct_InWork, colour=Sector, fill=Sector)) +
       geom_bar(stat = 'identity') + ylab("% in work") +
       facet_grid(Year+Region~HealthCondition) + theme_bw() +
       theme(axis.title=element_text(size=rel(2.5)),
             axis.text=element_text(size=rel(1.7)),
             legend.title=element_text(size=2.5),
             legend.text=element_text(size=rel(1.7)),
             strip.text=element_text(size=rel(1.7)),
             panel.spacing.x=unit(20, units='points'),
             panel.spacing.y=unit(5, units='points'))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

