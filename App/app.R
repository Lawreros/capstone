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
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(car)

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
      
      
  
      
      tabPanel("Delayed Testing - Descriptive", fluid = TRUE,
               
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
                   tabPanel("By Age", plotOutput('delayed'), h4("Boxplot Analysis"), plotOutput('boxplot')),
                   tabPanel("Previous Travel", plotOutput('delayed1'), h4("Boxplot Analysis"), plotOutput('boxplot1')),
                   tabPanel("Known Contact", plotOutput('delayed2'), h4("Boxplot Analysis"), plotOutput('boxplot2')),
                   tabPanel("By Sex", plotOutput('delayed3'), h4("Boxplot Analysis"), plotOutput('boxplot3')), 
                   tabPanel("Pregnant or Not", plotOutput('delayed4'),h4("Boxplot Analysis"), plotOutput('boxplot4'))
                 )
                 )
               )
      ),
      
      tabPanel("Delayed Testing - MLR", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(h3("Select Features:"),
                   selectInput("ages2", label=h3("Age Category"),
                               choices = list('0-19','20-39','40-59','60-79','80+'),
                               selected = '0-19'),
                   selectInput("traveled", label=h3("Previous Travel"),
                               choices = list('yes' = "yes", 'no' = "no"),
                               selected = 'yes'),
                   selectInput("contact", label=h3("Known Contact"),
                               choices = list('yes' = "yes", 'no' = "no", 'unknown' = "unknown"),
                               selected = 'yes')
                 ),
                 mainPanel(h3("Predicted Delayed Testing using MLR"),
                           tabsetPanel(
                             tabPanel("MLR Table",
                                      p("Outcome = Number of Days to take a COVID-19 test since onset of symptoms"), 
                                      p("Codebook: "), 
                                      p("Age Category 0-19 = Reference"),
                                      p("Known Contact = Reference"),
                                      p("Traveled (yes) = Reference"), 
                                      htmlOutput('linear')), 
                                      h5("MLR interpretations"),
                                      p("The Intercept value of 2.19 is the expected mean number of days to take a test since symptoms onset if the individual is 0-19 years old, has a known Contact, and has traveled."), 
                                      p("If someone is 60-79 years they will have tested 0.84 days later than someone who is 0-19 years old with all other variables held constant (p<0.05)."), 
                                      p("If someone is over 80 years old, they will have tested 1.1 days earlier than someone who is 0-19 years old with all other variables held constant (p<0.05)."), 
                                      p("If someone had unknown or unsure contact history, they will have tested 0.64 days earlier than someone who tested due to a known contact source with all other variables held constant  (p<0.05)."),
                                      p("If someone had no previous travel history, they will have tested 0.59 days later than someone who had tested with a previous travel history with all other variables held constant (p<0.05)."),
                             tabPanel("Calculator",
                                      h4("How to Use:"), 
                                      p("Select Features for Age Category, Known Contact, and Previous Travel"),
                                      p(),
                                      h5("Predicted number of days for this individual to take test since symptoms onset:"),
                                      textOutput('linreg'))
                             
                           )
                 )
               )),
      
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
                 sidebarPanel(
                   h3('Select Age Category:'),
                   checkboxGroupInput("agetime", label=h3("Five Year Bins:"),
                                      choices = list('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54',
                                                     '55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100-104'),
                                      selected = '0-4'
                   ),
                   checkboxGroupInput("agewide", label=h3("Twenty Year Bins:"),
                                      choices = list('0-19','20-39','40-59','60-79','80+'),
                                      selected = '0-19'
                   )
                 ),
                 mainPanel(
                   h2('COVID-19 Cases over Time (Dec 2020 - April 2021'),
                   plotOutput('timetrend')
                 )
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
    
    #### Tab "Delayed Testing - MLR"
    
    output$linear <- renderUI({
      CalwData4 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate) %>%
        select(c(delayed.testing, age, pregnant, traveled, contactSourceCase, sex)) %>% 
        
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5))))) %>% 
        select(-age) 
      
      CalwData4 <- CalwData4[complete.cases(CalwData4),]
      
      CalwData4$delayed.testing <- as.numeric(CalwData4$delayed.testing)
      CalwData4$Age_Cat <- factor(CalwData4$Age_Cat,
                                  levels = c(1,2,3, 4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      
      linear.reg =lm(formula = delayed.testing ~ Age_Cat + contactSourceCase +  traveled, data = CalwData4)
      tbl <- HTML(tab_model(linear.reg, pred.labels = c("Intercept", "20-39 Years (Age_Cat)", "40-59 Years (Age_Cat)", "60-79 Years (Age_Cat)", "80+ Years (Age_Cat)", "No Contact", "Unknown Contact", "No Travel"), dv.labels = "Days to Test since Symptom Onset")$knitr)
      tbl
      
    })
    output$linreg <- renderText({
      
      CalwData4 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate) %>%
        select(c(delayed.testing, age, pregnant, traveled, contactSourceCase, sex)) %>%
        
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5))))) %>% 
        select(-age) 
      
      CalwData4 <- CalwData4[complete.cases(CalwData4),]
      
      CalwData4$delayed.testing <- as.numeric(CalwData4$delayed.testing)
      CalwData4$Age_Cat <- factor(CalwData4$Age_Cat,
                                  levels = c(1,2,3, 4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
    
    
      linear.reg =lm(formula = delayed.testing ~ Age_Cat + contactSourceCase +  traveled, data = CalwData4)
      
      mydf <- data.frame(Age_Cat = input$ages2, traveled = input$traveled, contactSourceCase = input$contact)
      
      prob <- predict(linear.reg, mydf)
      paste(round(prob,2), " days")
     
      
    })
    
    
 
    
    
    ####
    
    #### Tab "Delayed Testing - Density Curves" 
  
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
    output$boxplot <- renderPlot({
      CalwData4 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate) %>%
        select(c(delayed.testing, age, pregnant, traveled, contactSourceCase, sex)) %>% 
        
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5))))) %>% 
        select(-age)
      
      
      CalwData4$delayed.testing <- as.numeric(CalwData4$delayed.testing)
      CalwData4$Age_Cat <- factor(CalwData4$Age_Cat,
                                  levels = c(1,2,3,4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      CalwData4 <- CalwData4[complete.cases(CalwData4),]
      
      boxplot(CalwData4$delayed.testing~CalwData4$Age_Cat, col="skyblue", main='Days to take Test Since Symptoms by Age', xlab="Age", ylab="# of days of delayed testing")
      
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
    
    output$boxplot1 <- renderPlot({
      CalwData4 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate) %>%
        select(c(delayed.testing, age, pregnant, traveled, contactSourceCase, sex)) %>% 
        
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5))))) %>% 
        select(-age)
      
      
      CalwData4$delayed.testing <- as.numeric(CalwData4$delayed.testing)
      CalwData4$Age_Cat <- factor(CalwData4$Age_Cat,
                                  levels = c(1,2,3,4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      CalwData4 <- CalwData4[complete.cases(CalwData4),]
      
      boxplot(CalwData4$delayed.testing~CalwData4$traveled, col="skyblue", main='Days to take Test Since Symptoms by Age', xlab="Traveled", ylab="# of Days Delayed Testing")
      
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
    
   
    output$boxplot2 <- renderPlot({
      CalwData4 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate) %>%
        select(c(delayed.testing, age, pregnant, traveled, contactSourceCase, sex)) %>% 
        
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5))))) %>% 
        select(-age)
      
      
      CalwData4$delayed.testing <- as.numeric(CalwData4$delayed.testing)
      CalwData4$Age_Cat <- factor(CalwData4$Age_Cat,
                                  levels = c(1,2,3,4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      CalwData4 <- CalwData4[complete.cases(CalwData4),]
      
      boxplot(CalwData4$delayed.testing~CalwData4$contactSourceCase, col="skyblue", main='Delayed Testing by Known Contact', xlab="Known Contact", ylab="# of Days Delayed Testing")
      
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
    
    output$boxplot3 <- renderPlot({
      CalwData4 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate) %>%
        select(c(delayed.testing, age, pregnant, traveled, contactSourceCase, sex)) %>% 
        
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5))))) %>% 
        select(-age)
      
      
      CalwData4$delayed.testing <- as.numeric(CalwData4$delayed.testing)
      CalwData4$Age_Cat <- factor(CalwData4$Age_Cat,
                                  levels = c(1,2,3,4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      CalwData4 <- CalwData4[complete.cases(CalwData4),]
      
      boxplot(CalwData4$delayed.testing~CalwData4$sex, col="skyblue", main='Delayed Testing by Sex', xlab="Sex", ylab="# of Days Delayed Testing")
      
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
    
    output$boxplot4 <- renderPlot({
      CalwData4 <- 
        CalwData %>% 
        mutate(delayed.testing = ymd(indexcase)- symptoms.onsetDate) %>%
        select(c(delayed.testing, age, pregnant, traveled, contactSourceCase, sex)) %>% 
        
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5))))) %>% 
        select(-age)
      
      
      CalwData4$delayed.testing <- as.numeric(CalwData4$delayed.testing)
      CalwData4$Age_Cat <- factor(CalwData4$Age_Cat,
                                  levels = c(1,2,3,4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      CalwData4 <- CalwData4[complete.cases(CalwData4),]
      
      boxplot(CalwData4$delayed.testing~CalwData4$pregnant, col="skyblue", main='Delayed Testing by Pregnancy Status', xlab="Pregnacy Status", ylab="# of Days Delayed Testing")
      
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

