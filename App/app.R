#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyverse)
library(lubridate)

ui<- fluidPage(
    tabsetPanel(
      tabPanel("Data Summary", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   radioButtons('categories', label=h3("Select Data Feature"),
                                choices = list('age','sex','variant','pregnant','quarantineExtended','reInfection','traveled','contactSourceCase',
                                               'condition','sampleResult1','sampleResult2','symptoms.symptomatic', 'vaccination','vaccinationcomplete','vaccineManufacturer'),
                                selected = 'age'),
                   
                   checkboxGroupInput("ages", label=h3("Reports by age over time"),
                                choices = list('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54',
                                               '55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100-104'),
                                selected = '0-4')
                 ),
                 mainPanel(
                   plotOutput('cattable'),
                   plotOutput('agetime')
                 )
               )
      ),
      
      
      tabPanel("B", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ),
      
      
      tabPanel("C", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ),
      
      
        tabPanel("Delayed Testing", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(checkboxGroupInput("ages2", label=h3("Select Age Group"),
                                                 choices = list('0-19','20-39','40-59','60-79','80+'),
                                                 selected = '0-19'),
                              checkboxGroupInput('traveled', label=h3("Previous Travel?"),
                                           choices = list('yes', 'no'),
                                           selected = 'yes'),
                              checkboxGroupInput('contact', label=h3("Known Contact?"),
                                                 choices = list('yes', 'no', 'unknown'),
                                                 selected = 'yes'), 
                              checkboxGroupInput('sex', label=h3("Select Sex"),
                                                 choices = list('male', 'female'),
                                                 selected = 'male')
                ),
                 mainPanel(
                   tabsetPanel(
                   tabPanel("By Age", plotOutput('delayed')),
                   tabPanel("Previous Travel", plotOutput('delayed1')),
                   tabPanel("Known Contact", plotOutput('delayed2')),
                   tabPanel("By Sex", plotOutput('delayed3'))
                 )
                 )
               )
      ),
      
      
      tabPanel("E", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ),
      
      
      tabPanel("F", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ),
      
      
      tabPanel("G", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ),
      
      
      tabPanel("H", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ),
      
      
      tabPanel("I", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ),
      
      
      tabPanel("J", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    #Load in data as a dataframe called CalwData
    load('../CalwData.RData')
    #Convert reportDate from string to actual date factor
    CalwData$reportDate <- as.Date(CalwData$reportDate, "%Y-%m-%d")
    
    
    
    #### Tab "Data Summary"
    
    #Plot the number of times different entries for categories appear in the data (for seeing what has enough entries to bother with)
    output$cattable <- renderPlot({
      x <- as.data.frame(table(CalwData[[input$categories]]))
      ggplot(x, aes(x=Var1, y=Freq)) + geom_bar(stat='identity')
    })
    
    #Plot the number of reports for each age each day
    output$agetime <- renderPlot({
      ggplot(CalwData[CalwData$age %in% input$ages,], aes(reportDate, fill = age))+geom_histogram(bins=70) + scale_x_date()
    })
    
    ####
    
    
    
    #### Tab B
    
    ####
    
    #### Tab C
    
    ####
    
    #### Tab "Delayed Testing" 
  
    # Density curves of days until testing
    
    # By Age Category 
    
    output$delayed <- renderPlot({
      CalwData2 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate) %>%
 
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5))))) 
      
        CalwData2$Age_Cat <- factor(CalwData2$Age_Cat,
                                  levels = c(1,2,3, 4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
        
        
   

      ggplot(CalwData2[CalwData2$Age_Cat %in% input$ages2,], aes(delayed.testing, fill = Age_Cat))+geom_density(alpha=0.4) + xlim(c(-10, 14)) + theme_minimal() + scale_fill_brewer(palette="Accent")

    })
    
     # By Traveled Status 
    
    output$delayed1 <- renderPlot({

    CalwData2 <- 
      CalwData %>% 
      mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate)
    
     
      ggplot(CalwData2[CalwData2$traveled %in% input$traveled,], aes(delayed.testing, fill = traveled))+geom_density(alpha=0.4) + xlim(c(-5, 10)) + theme_minimal() + scale_fill_brewer(palette="Dark2")
    })
    
    # By Known Contact Status 
    
    output$delayed2 <- renderPlot({
      CalwData2 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate)
      
      ggplot(CalwData2[CalwData2$contactSourceCase %in% input$contact,], aes(delayed.testing, fill = contactSourceCase))+geom_density(alpha=0.4) + xlim(c(-5, 10)) + theme_minimal() + scale_fill_brewer(palette="Dark2")
    })
    
    #By Sex 
    
    output$delayed3 <- renderPlot({
      CalwData2 <- 
        CalwData %>% 
         mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate)
        
       mu <- CalwData2 %>% 
        group_by(sex) %>% 
        summarize (grp.mean = mean(delayed.testing), na.rm = TRUE)
 
      
      ggplot(CalwData2[CalwData2$sex %in% input$sex,], aes(delayed.testing, fill = sex))+geom_density(alpha = 0.4) + geom_vline(mu[mu$sex %in% input$sex,], xintercept = mu$grp.mean) + xlim(c(-5, 10)) + theme_minimal()+ scale_fill_brewer(palette="Dark2")
    })
    
    # By Pregnancy Status 
    
    output$delayed4 <- renderPlot({
      CalwData2 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate)
      
      ggplot(CalwData2[CalwData2$pregnant %in% input$pregnant,], aes(delayed.testing, fill = pregnant))+geom_density(alpha = 0.4) + xlim(c(-5, 10)) + theme_minimal()+ scale_fill_brewer(palette="Dark2")
    })
    
    ####
    
    #### Tab E
    
    ####
    
    #### Tab F
    
    ####
    
    #### Tab G
    
    ####
    
    #### Tab H
    
    ####
    
    #### Tab I
    
    ####
    
    #### Tab J
    
    ####
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)

