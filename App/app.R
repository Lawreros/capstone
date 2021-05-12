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
                 sidebarPanel(h3("Select by:"), 
                              checkboxGroupInput("ages2", label=h4("Age Group"),
                                                 choices = list('0-19','20-39','40-59','60-79','80+'),
                                                 selected = '0-19'),
                              checkboxGroupInput('traveled', label=h4("Previous Travel"),
                                           choices = list('yes', 'no'),
                                           selected = 'yes'),
                              checkboxGroupInput('contact', label=h4("Known Contact Source"),
                                                 choices = list('yes', 'no', 'unknown'),
                                                 selected = 'yes'), 
                              checkboxGroupInput('sex', label=h4("Sex"),
                                                 choices = list('male', 'female'),
                                                 selected = 'male'), 
                              checkboxGroupInput('pregnant', label=h4("Pregnancy Status"),
                                                 choices = list('yes', 'no', 'unknown'),
                                                 selected = 'yes')
                ),
                 mainPanel(
                   h3("Density Curves of Delay in Testing"), 
                   p("The delayed testing variable is calculated by the number of days since symptom onset to an individual's first (positive result) test."),
                   p("Negative days of delayed testing can be explained if the individual was tested before symptom onset due to a known outbreak or contact, or vulnerable settings, such as nursing homes."),
                   p("The density curves below show the distribution of days until testing by age, traveled status, known contact source, sex, and pregnancy status."),
                   p("The dashed lines indicate the mean for each selected group."),
                   p("How to use: Select criteria on the left panel and click on respective tab to view results."),
                   tabsetPanel(
                   tabPanel("By Age", plotOutput('delayed')),
                   tabPanel("Previous Travel", plotOutput('delayed1')),
                   tabPanel("Known Contact", plotOutput('delayed2')),
                   tabPanel("By Sex", plotOutput('delayed3')), 
                   tabPanel("Pregnant or Not", plotOutput('delayed4'))
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
    
  # Bar chart for delayed days
    
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
        
        mu <- CalwData2 %>% 
          group_by(Age_Cat) %>% 
          summarize (grp.mean = mean(delayed.testing, na.rm = TRUE)) %>% 
          ungroup()

      ggplot()+
        geom_density(CalwData2[CalwData2$Age_Cat %in% input$ages2,], mapping = aes(delayed.testing, fill = Age_Cat), alpha=0.4) +
        geom_vline(mu[mu$Age_Cat %in% input$ages2,], mapping = aes(xintercept = grp.mean), linetype="dashed") +
        geom_label_repel(mu[mu$Age_Cat %in% input$ages2,], mapping = aes(x = grp.mean, y= 0.2, label = paste(round(grp.mean, 3)), fill = input$ages2), colour="white") +
        xlim(c(-10, 14)) + 
        theme_minimal() + 
        ggtitle ("Delayed Testing by Age") + 
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
        xlab("Number of Days") + 
        ylab("Density") + 
        guides(fill=guide_legend(title="Age Category")) + 
        scale_fill_brewer(palette="Dark2")
      
    })
    
    output$delayed1 <- renderPlot({

    CalwData2 <- 
      CalwData %>% 
      mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate)
    
    
    mu <- CalwData2 %>% 
      group_by(traveled) %>% 
      summarize (grp.mean = mean(delayed.testing, na.rm = TRUE)) %>% 
      ungroup()

     
      ggplot()+
      geom_density(CalwData2[CalwData2$traveled %in% input$traveled,], mapping = aes(delayed.testing, fill = traveled), na.rm = TRUE, alpha=0.4) + 
        geom_vline(mu[mu$traveled %in% input$traveled,], mapping = aes(xintercept = grp.mean), linetype="dashed") +
        geom_label_repel(mu[mu$traveled %in% input$traveled,], mapping = aes(x = grp.mean, y= 0.2, label = paste(round(grp.mean, 3)), fill = input$traveled), colour="white") +
        xlim(c(-5, 10))+ 
        theme_minimal() + 
        ggtitle ("Delayed Testing by Pregnancy Status") + 
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
        xlab("Number of Days") + 
        ylab("Density") + 
        guides(fill=guide_legend(title="Pregnancy Status")) + 
        scale_fill_brewer(palette="Dark2")
     
    })
    
    output$delayed2 <- renderPlot({
      CalwData2 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate)
      
      mu <- CalwData2 %>% 
        group_by(contactSourceCase) %>% 
        summarize (grp.mean = mean(delayed.testing, na.rm = TRUE)) %>% 
        ungroup()
      
      ggplot() + 
      geom_density(CalwData2[CalwData2$contactSourceCase %in% input$contact,], mapping = aes(delayed.testing, fill = contactSourceCase), na.rm = TRUE, alpha=0.4) + 
        geom_vline(mu[mu$contactSourceCase %in% input$contact,], mapping = aes(xintercept = grp.mean), linetype="dashed") +
        geom_label_repel(mu[mu$contactSourceCase %in% input$contact,], mapping = aes(x = grp.mean, y= 0.2, label = paste(round(grp.mean, 3)), fill = input$contact), colour="white") +
        xlim(c(-5, 10)) + 
        theme_minimal() + 
        ggtitle ("Delayed Testing by Known Contact Source") + 
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
        xlab("Number of Days") + 
        ylab("Density") + 
        guides(fill=guide_legend(title="Known Contact")) + 
        scale_fill_brewer(palette="Dark2")
        
    })
    
    output$delayed3 <- renderPlot({
      CalwData2 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)-symptoms.onsetDate)
      
      mu <- CalwData2 %>% 
        group_by(sex) %>% 
        summarize (grp.mean = mean(delayed.testing, na.rm = TRUE)) %>% 
        ungroup()
      
      ggplot() + 
        geom_density(CalwData2[CalwData2$sex %in% input$sex,], mapping = aes(delayed.testing, fill = sex), na.rm = TRUE, alpha = 0.4) + 
        geom_vline(mu[mu$sex %in% input$sex,], mapping = aes(xintercept = grp.mean), linetype="dashed") +
        geom_label_repel(mu[mu$sex %in% input$sex,], mapping = aes(x = grp.mean, y= 0.2, label = paste(round(grp.mean, 3)), fill = input$sex), colour="white") +
        xlim(c(-5, 10)) + 
        theme_minimal() + 
        ggtitle ("Delayed Testing by Sex") + 
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
        xlab("Number of Days") + 
        ylab("Density") + 
        guides(fill=guide_legend(title="Sex")) + 
        scale_fill_brewer(palette="Dark2") 
       

    })
    
    
    output$delayed4 <- renderPlot({
      CalwData2 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate)
      
      mu <- CalwData2 %>% 
        group_by(pregnant) %>% 
        summarize (grp.mean = mean(delayed.testing, na.rm = TRUE)) %>% 
        ungroup()
    
      
      ggplot() + 
      geom_density(CalwData2[CalwData2$pregnant %in% input$pregnant,], mapping = aes(delayed.testing, fill = pregnant), na.rm = TRUE, alpha = 0.4) + 
        geom_vline(mu[mu$pregnant %in% input$pregnant,], mapping = aes(xintercept = grp.mean), linetype="dashed") +
        geom_label_repel(mu[mu$pregnant %in% input$pregnant,], mapping = aes(x = grp.mean, y= 0.2, label = paste(round(grp.mean, 3)), fill = input$pregnant), colour="white") +
        xlim(c(-5, 10)) + 
        theme_minimal() + 
        ggtitle ("Delayed Testing by Pregnancy Status") + 
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
        xlab("Number of Days") + 
        ylab("Density") + 
        guides(fill=guide_legend(title="Pregnancy Status")) + 
        scale_fill_brewer(palette="Dark2")
      
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

