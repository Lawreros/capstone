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

library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggmosaic)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(car)
library(jtools)


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
                 sidebarPanel(p(strong("How to use: Select criteria on the left panel and click on respective tab to view results.")),
                              h4("Select by:"),
                              checkboxGroupInput("ages2", label=h4("Age Group"),
                                                 choices = list("0-19" = "0-19","20-39" = "20-39","40-59" = "40-59","60-79" = "60-79","80+" = "80+"),
                                                 selected = '0-19'),
                              checkboxGroupInput('traveled', label=h4("Previous Travel"),
                                           choices = list('yes', 'no'),
                                           selected = 'yes'),
                              checkboxGroupInput('contact', label=h4("Known Contact"),
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
                   p(em("Negative days of delayed testing can be explained if the individual was tested before symptom onset due to a known outbreak or contact, or vulnerable settings, such as nursing homes.")),
                   p("The density curves below show the distribution of days until testing by age, traveled status, known contact history, sex, and pregnancy status."),
                   p("The dashed lines indicate the mean for each selected group."),
        
                   tabsetPanel(
                   tabPanel("By Age", plotOutput('delayed'), h4("Boxplot Analysis"), plotOutput('boxplot')),
                   tabPanel("Previous Travel", plotOutput('delayed1'), h4("Boxplot Analysis"), plotOutput('boxplot1')),
                   tabPanel("Known Contact", plotOutput('delayed2'), h4("Boxplot Analysis"), plotOutput('boxplot2')),
                   tabPanel("By Sex", plotOutput('delayed3'), h4("Boxplot Analysis"), plotOutput('boxplot3')), 
                   tabPanel("Pregnancy Status", plotOutput('delayed4'),h4("Boxplot Analysis"), plotOutput('boxplot4'))
                 )
                 )
               )
      ),
      
      tabPanel("Delayed Testing - MLR", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(p(strong("How to use: Select criteria and click on Calculator tab to view results.")),
                              h3("Select Features:"),
                              selectInput("ages3", label=h3("Age Category"),
                              choices = list("0-19" = "0-19","20-39" = "20-39","40-59" = "40-59","60-79" = "60-79","80+" = "80+"),
                               selected = "0-19"),
                              selectInput("traveled3", label=h3("Previous Travel"),
                               choices = list("yes", "no"),
                               selected = "yes"),
                              selectInput("contact3", label=h3("Known Contact"),
                               choices = list("yes", "no","unknown"),
                               selected = "yes")
                 ),
                 mainPanel(h3("Expected Delayed Testing using MLR"),
                           tabsetPanel(
                             tabPanel("MLR Table",
                                      p("Outcome is the number of days to take a COVID-19 test since onset of symptoms"), 
                                      p(strong("Codebook:")), 
                                      p(em("Age Category 0-19 = Reference")),
                                      p(em("Known Contact = Reference")),
                                      p(em("Traveled (yes) = Reference")), 
                                      htmlOutput('linear'),
                                      h5("MLR interpretations"),
                                      p("The Intercept value of 2.19 is the expected mean number of days to take a test since symptoms onset if the individual is 0-19 years old, has a known Contact, and has traveled."), 
                                      p("If someone is 60-79 years they will have tested 0.84 days later than someone who is 0-19 years old with all other variables held constant (p<0.05)."), 
                                      p("If someone is over 80 years old, they will have tested 1.1 days earlier than someone who is 0-19 years old with all other variables held constant (p<0.05)."), 
                                      plotOutput('linregplot1'), 
                                      p("If someone had unknown (uncertain) contact history, they will have tested 0.64 days earlier than someone who tested due to a known contact history with all other variables held constant  (p<0.05)."),
                                      plotOutput('linregplot2'),
                                      p("If someone had no previous travel history, they will have tested 0.59 days later than someone who had tested with a previous travel history with all other variables held constant (p<0.05)."),
                                      plotOutput('linregplot3')
                                       ),
                             tabPanel("Calculator",
                                      h4("Expected number of days after symptoms onset individual takes COVID-19 test"),
                                      textOutput('linreg'))
                             
                           )
                 )
               )),
      
      tabPanel("E", fluid = TRUE,
               headerPanel("Association between Sex, Age and Condition (Alive/Dead)"),
               sidebarLayout(
                 sidebarPanel(h3('Summary of Analysis:'),
                              h5('In this tab, we evaluate the association between sex, age-category, and condition.'),
                              h5('Condition was defined as dead if person was killed from COVID-19 infection.'),
                              h5('Persons with unknown or missing sex, age, or condition were excluded from analysis.'),
                              h5('Chi-square tests were used to test for significant associations.')
                   ),
                 mainPanel(h1("Association Results:"),
                           h2('Sex Results'),
                           h3('Fishers Exact Test:Sex and Condition'),
                           h4('Fishers test was chosen due to chi-square test indicating approximation may be incorrect.'),
                           h4('A fisher exact test indicates condition is significantly associated with sex (p = 0.04.)'),
                      textOutput('sexfisher'),
                      h3('Mosaic Plot: Sex'),
                      plotlyOutput('sexmosaic'),
                 h2("Age Category Results"),
                      h3('Chi-Square Test: Age and Condition'),
                      h4('A chi-square test indicates condition is significantly associated with age (p<0.001).'),
                      textOutput('chisqage'),
                      h3('Mosaic Plot: Age Category'),
                      plotlyOutput('agemosaic')
                 ))
               
      ),
      
      tabPanel("F", fluid = TRUE,
               sidebarLayout(

                 sidebarPanel(
                   h3('Select Age and Sex:'),
                   radioButtons('sex', label=h4("Select Sex"),
                                c('female' = 'female','male'= 'male'),
                                selected = 'female'),
                   
                   radioButtons("age", label=h4("Select Age"),
                                choices = list('0-19','20-39','40-59','60-79','80+'),
                                selected = '0-19')
                   ),
                 mainPanel(h2('Odds of Death Given Sex and Age'),
                           h4('Outcome = Death'),
                           h4('Predictors = Age-Category and Sex'),
                           h5('Codebook:'),
                           h5('Males = reference; Age Category 0-19 = reference'),
                           htmlOutput('logvar'),
                           h3('Odds Ratio Interpretations:'),
                           h5('The odds of death for females is 0.42 times the odds of death for males, or significantly lower by 58% (p<0.01).'),
                           h5('The odds of death increases with each increasing age category and is highest in adults over 80 years old. The odds of death in adults over 80 years old is 117.51 times the odds of death in children 0-19 years old (p<0.001).'),
                           h3('Predicted Probability of Death Given Inputs (Age Category and Sex):'),
                           h5('Chance of death was calculated by converting predicted log odds of death into predicted probability of death. Predicted probabilities were transformed into percentages by multiplying by 100.'),
                           h5('Given your selected age category and sex, your percent chance of death is:'),
                           textOutput('predict'),
                           plotOutput('predictplot')
                          
                           )
                 
      )),
      

                 

      
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
                   h2('COVID-19 Cases by Age Time Trend'),
                   h3('Five Year Age Bins:'),
                   plotOutput('timetrend'),
                   h3('Twenty Year Age Bins:'),
                   plotOutput('timetrend2')
                 )
               )
      ),
   
      
      tabPanel("J", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      )
  
  ))

  


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
    
    #### Tab "Delayed Testing - Descriptive" 
    
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
      
      boxplot(CalwData4$delayed.testing~CalwData4$Age_Cat, col="skyblue", main='Days to take Test since Symptoms Onset by Age', xlab="Age", ylab="# of days of delayed testing")
      
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
      
      boxplot(CalwData4$delayed.testing~CalwData4$traveled, col="skyblue", main='Days to take Test Since Symptoms by Previous Travel', xlab="Traveled", ylab="# of Days Delayed Testing")
      
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
        ggtitle ("Delayed Testing by Known Contact History") + 
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
    
    
    output$sexmosaic <- renderPlotly({
      
      CalwDatanew <- CalwData %>%
        select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
    
      mosaic_examp <-  ggplot(data = CalwDatanew) +
        geom_mosaic(aes(x = product( condition, sex), fill = condition)) +   
        labs(y="Condition", x="Sex", title = "Mosaic Plot: Sex and COVID-19 Condition") 
      mosaic <- ggplotly(mosaic_examp)
      mosaic
    })
    
    output$sexfisher <- renderPrint({
      CalwDatanew <- CalwData %>%
        select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      
      results <- fisher.test(CalwDatanew$condition, CalwDatanew$sex)
      results
    })
    
    output$chisq <- renderPrint({
      CalwDatanew <- CalwData %>%
        select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      CalwDatanew <- CalwDatanew %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      CalwDatanew$Age_Cat <- factor(CalwDatanew$Age_Cat,
                                    levels = c(0,1,2,3, 4),
                                    labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      tbl <- table(CalwDatanew$condition, CalwDatanew$sex)
      results <- chisq.test(tbl)
      results
      
    })
    
    output$agefisher <- renderPrint({
      CalwDatanew <- CalwData %>%
        select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      
      CalwDatanew <- CalwDatanew %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      CalwDatanew$Age_Cat <- factor(CalwDatanew$Age_Cat,
                                  levels = c(0,1,2,3, 4),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      resultsage <- fisher.test(CalwDatanew$condition, CalwDatanew$Age_Cat, workspace = 400,000)
      resultsage
    })
    
    output$chisqage <- renderPrint({
      CalwDatanew <- CalwData %>%
      select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      
      CalwDatanew <- CalwDatanew %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      CalwDatanew$Age_Cat <- factor(CalwDatanew$Age_Cat,
                                    levels = c(0,1,2,3, 4),
                                    labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      tbl <- table(CalwDatanew$condition, CalwDatanew$Age_Cat)
      results <- chisq.test(tbl)
      results
      
    })
    
    output$agemosaic <- renderPlotly({
      CalwDatanew <- CalwData %>%
        select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      
      CalwDatanew <- CalwDatanew %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      CalwDatanew$Age_Cat <- factor(CalwDatanew$Age_Cat,
                                  levels = c(0,1,2,3, 4),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      mosaic_examp <-  ggplot(data = CalwDatanew) +
        geom_mosaic(aes(x = product( condition, Age_Cat), fill = condition)) +   
        labs(y="Condition", x="Age Category", title = "Mosaic Plot: Age and Condition") 
      mosaic <- ggplotly(mosaic_examp)
      mosaic
    })
    
 
  
  
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
      
      mydf <- data.frame(Age_Cat = input$ages3, contactSourceCase = input$contact3, traveled = input$traveled3)
      
      prob <- predict(linear.reg, newdata= mydf)
      paste(round(prob,2), " days")
    
      
    })
    
    
    output$linregplot1 <-renderPlot({
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
      effect_plot(linear.reg, pred=Age_Cat,x.label = "Age", y.label = "# of days to test", main.title = "By Age", colors = "darkblue", interval = TRUE)
      
    })
    
    output$linregplot2 <-renderPlot({
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
      effect_plot(linear.reg, pred= contactSourceCase, x.label = "Contact History", y.label = "# of days to test", main.title = "By Contact History", colors = "darkblue", interval = TRUE)
      
    })
    
    output$linregplot3 <-renderPlot({
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
      effect_plot(linear.reg, pred=traveled, x.label = "Travel History", y.label = "# of days to test",main.title = "By Travel History", colors = "darkblue", interval = TRUE)
      
    })
    
    
  

    
 
    
    ####
    
    #### Tab F
    
    output$logvar <- renderUI({
      CalwData2 <- CalwData %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      
      CalwData2$Age_Cat <- factor(CalwData2$Age_Cat,
                                  levels = c(0,1,2,3, 4),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      mylogit <- glm(condition ~ Age_Cat + sex , data = CalwData2, family = "binomial")
      #glmresult <- broom::tidy(mylogit, conf.int = TRUE, exponentiate=TRUE)
      tbl <- HTML(tab_model(mylogit)$knitr)
      tbl
      
    })
    
    output$predict <- renderPrint({
      CalwData2 <- CalwData %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      
      CalwData2$Age_Cat <- factor(CalwData2$Age_Cat,
                                  levels = c(0,1,2,3, 4),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      mylogit <- glm(condition ~ Age_Cat + sex , data = CalwData2, family = "binomial")
    
      newdata <- data.frame(Age_Cat = "NA",
                            sex = "NA")
      
      newdata <- newdata %>%
        mutate(Age_Cat = input$age) %>%
        mutate(sex = input$sex)
          
      prob <- predict(mylogit, newdata, type="response")
      
       percent <- prob*100
       percent
       percent %>% as.numeric() 
    })
    
    
    output$predictplot <- renderPlot({
      CalwData2 <- CalwData %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      
      CalwData2$Age_Cat <- factor(CalwData2$Age_Cat,
                                  levels = c(0,1,2,3, 4),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      mylogit <- glm(condition ~ Age_Cat, data = CalwData2, family = "binomial")
      
      #newdata <- data.frame(Age_Cat = "NA")
                            
      #newdata <- newdata %>%
        #mutate(Age_Cat = input$age) 
      
      xweight <- c(0, 1, 2, 3, 4)
      xweight <- as.factor(xweight)
      
      yweight <- predict(mylogit, list(Age_Cat = xweight ), type="response")
      yweight %>% as.numeric()
      
      plot(CalwData2$Age_Cat, CalwData2$condition, pch = 16, xlab = "Age Category", ylab = "Condition")
      lines(xweight, yweight)
 
      
    })
    
    ####
    
    #### Tab G

    
    ####
    
    #### Tab H
    
    ####
    
    #### Tab I

    
    #Plot the number of reports for each age each day
    output$timetrend <- renderPlot({
      
      Calwnew <- CalwData %>%
        select(reportDate, age) %>%
        group_by(age, reportDate) %>%
        summarise(count = n())
      
      g<- ggplot(Calwnew[Calwnew$age %in% input$agetime,], aes(x =reportDate, y= count, color = age))+ geom_point() + scale_x_date() +
        geom_smooth(method = "loess", size = 1.5) + 
        labs(title = "Number COVID-19 Cases Over Time by Age", x = "Month", y = "Number of Cases")
      g
   
    })
    
    output$timetrend2 <- renderPlot({
      
      Calwnew <- CalwData %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', '0-19',
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', '20-39',
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', '40-59',
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', '60-79', '80+')))))
      
      Calwnew <- Calwnew %>%
        select(reportDate, Age_Cat) %>%
        group_by(Age_Cat, reportDate) %>%
        summarise(count = n())
      

      g<- ggplot(Calwnew[Calwnew$Age_Cat %in% input$agewide,], aes(x =reportDate, y= count, color = Age_Cat))+ geom_point() + scale_x_date() +
        geom_smooth(method = "loess", size = 1.5) + 
        labs(title = "Number COVID-19 Cases Over Time by Age", x = "Month", y = "Number of Cases")
      g
      
    })
    

   

    ####
    
    #### Tab J
    
    ####
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)

