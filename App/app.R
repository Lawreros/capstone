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
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggsci)
library(ggrepel)
library(MASS) 
library(plotly)
library(knitr)


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
      
    
        
       tabPanel("Death, Age, Sex Association", fluid = TRUE,
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
                           h3('Chi-square Test:Sex and Condition'),
                           h4('A chi-square test indicates condition is not significantly associated with sex (p = 0.09.)'),
                      textOutput('chisq'),
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
      

        
       tabPanel("Probability of Death", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   h3('Select Age and Sex:'),
                   radioButtons('sexln', label=h4("Select Sex"),
                                c('female' = 'female','male'= 'male'),
                                selected = 'female'),
                   
                   radioButtons("ageln", label=h4("Select Age"),
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
                           h5('The odds of death for females is 0.42 times the odds of death for males, or significantly lower by 58% (p<0.001).'),
                           h5('The odds of death increases with each increasing age category and is highest in adults over 80 years old. The odds of death in adults over 80 years old is 117.51 times the odds of death in children 0-19 years old (p<0.001).'),
                           h3('Predicted Probability of Death Given Inputs (Age Category and Sex):'),
                           h5('Chance of death was calculated by converting predicted log odds of death into predicted probability of death. Predicted probabilities were transformed into percentages by multiplying by 100.'),
                           h5('Given your selected age category and sex, your percent chance of death is:'),
                           textOutput('predict')
                          
                           )
                 
      )),
      

               
      
      tabPanel("Symptoms - Deskriptive analysis", fluid = TRUE,
               
               #h3("Symptom Distribution by Age"),
               fluidRow(
                 column(4,
                        h4("Symptom Distribution in Total Group"),
                        plotlyOutput("pieallAM"),
                       
                        
                 )),
               fluidRow(
               tabsetPanel(
                 tabPanel("Symptom Distribution by Age",
                    fluidRow(
                 column(4,
                        h3("Symptom Distribution by Age"),
                        
                        selectInput('agegroupAM', "Select Age Group",
                                    choices = list('0-4','5-9','10-14','15-19','20-24','25-29',
                                                   '30-34','35-39','40-44','45-49','50-54',
                                                   '55-59','60-64','65-69','70-74','75-79',
                                                   '80-84','85-89','90-94','95-99','100-104'),
                                    selected = '0-4'
                        ),
                        br(),
                        plotlyOutput('piesubAM'),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()
                    
                 ),
                 column(1,
                       br()),
              
                 column(6,
                        plotlyOutput("bar_ageAM")
                 )),
                 fluidRow(
                   column(1,),
                   column(4,
                   h3("Age binned in 20 years interval"))),
                 fluidRow(
                 column(5,
                        plotlyOutput("bar_age_groupedAM")
                 ),
                 column(6,
                        h4("Test for group difference "),
                        br(),
                        br(),
                        p("Fishers test was chosen due small counts in some subgroups.
                        Due to multiple testing and no pre-analaysis plan with correction for multiple testing due to exploratory 
                           character of analysis, no significance level was set, but acutal p-value are displayed"),
                        br(),
                        br(),
                        selectInput('soi_ageAM', "Select Symptom",
                                    choices = list("feeling ill","fever","shivering","chills sweats","muscle pain",
                                                   "headache","difficulty breathing","cough","sore throat","runny nose",
                                                   "nausea","diarrhea","loss of smell","loss of taste"),
                                    selected = 'loss of taste'
                        ),
                        br(),
                        selectInput('ref_ageAM', "Select reference group",
                                    choices = list("0-19","20-39","40-59","60-79","80+"),
                                    selected = '0-19'),
                      
                        br(),
                        br(),
                        plotlyOutput('bar_age_grouped_fischerAM')
                        
                 )),
                 ),
               tabPanel("Symptom Distribution by Timing of Test", 
               h3("Symptom Distribution by Timing of Test"),
               br(), 
               fluidRow( 
                 column(1,
                        br()),
                 column(8,
               p("Timing of Test is defined as the time between positive COVID-19 testresult and symptom onset"),
               p("Timing of Test is binned into categories: "),
               p("Before: Participant is tested before symptom onset. E.g. due to a known outbreak or contact, or vulnerable settings, such as nursing homes."),
               p("Immediate: Participant is tested between the at day of onset until 2 days after symptom onset."),
               p("Early: Participant is tested in the intervall of 2 days to 5 days after symptom onset."),
               p("Late: Participant is after more than 5 days after symptom onset."),
                 )),
               br(),
               fluidRow(
                 column(5,
                    plotlyOutput('barAM')
                    ),
                  column(7,
                         h4("Test for group difference "),
                         br(),
                         br(),
                         p("Fishers test was chosen due small counts in some subgroups.
                        Due to multiple testing and no pre-analaysis plan with correction for multiple testing due to exploratory 
                           character of analysis, no significance level was set, but acutal p-value are displayed"),
                         br(),
                         br(),
                         br(),
                         selectInput('soiAM', "Select Symptom",
                                     choices = list("feeling ill","fever","shivering","chills sweats","muscle pain",
                                                    "headache","difficulty breathing","cough","sore throat","runny nose",
                                                    "nausea","diarrhea","loss of smell","loss of taste"),
                                     selected = 'loss of taste'
                         ),
                         br(),
                         selectInput('refAM', "Select reference group",
                                     choices = list("before","immediate","early","late"),
                                     selected = 'before'
                         ),
                         
                         
                         
                         br(),
                         br(),
                        plotlyOutput('fischerAM')
                         ),
               )
      )))),

      tabPanel("Readme File ", fluid = TRUE,
               
               mainPanel(
                 h3("Disclaimer"),
                 p("The following tool was developed for a class project. 
                 Please be aware that the predictions calculated by this tool rely soley on the available 
                regressors and DO NOT NECESSARILY IMPLY CAUSATION by said regressors. 
                 The models are not validated and should not be used for any decision making.
                 The data might differ from official records because a subset of participants were used."),
                 br(),
                 h3("The district of Calw"),
                 h4("General information about the district"),
                 p("Calw is a district (german <Landkreis>) centrally located in 
                   Baden Wuerttemberg , Germany. The district is approximately 800 square kilometers, 
                   with approximately 160.000 people living in the 25 cities and municipalities of the district.
                  These consist of 10 cities - Calw city, Altensteig, Bad Herrenalb, Bad Liebenzell,
                   Bad Teinach-Zavelstein, Bad Wildbad, Haiterbach, Nagold, Neubulach and Wildberg -
                   and 15 municipalities - Althengstett, Dobel, Ebhausen, Egenhausen, Enzkloesterle, 
                   Gechingen, Hoefen, Neuweiler, Oberreichenbach, Ostelsheim, Rohrdorf, Schoemberg,
                   Simmersfeld, Simmozheim und Unterreichenbach."),
                 h4("The health department"),
                 p("The health department is part of the district office, which is on the one hand a
                  municipal self-governing authority and on the other hand the lower 
                 state administrative authority with diverse tasks. For more
                 information please see the official homepage of the district office: lra-calw.de"),
                 h4("The health department and the COVID-19 pandemic"),
                 p("With the corona pandemic, a completely new structure was set up within the health department. 
                 Depending on the incidence, up to 80 people were / are employed full-
                 time and part-time coping with the pandemic. The military <KSK Kommando Calw> also supports the
                 tracking of contact persons with up to 20 soldiers.
                 Early on at the beginning of the second wave, the Calw District Office decided to digitize the
                 processes and use the Sormas software (see data source)"),
                 
                 h3("The Data used in this App"),
                 h4("Data source"),
                 p("The data used here is data from SORMAS (Surveillance, Outbreak Response Management and Analysis 
                 System) used by the local health department of the district of Calw. 
                 SORMAS is an e-health software developed by the Helmholtz Center for Infection Research 
                 and the German Center for Infection Research for the management of measures to combat 
                 epidemics. With the support of the federal and state governments, the software is 
                 made available to the health authorities free of charge. The program is intended to enable
                 data to be exchanged between the various bodies in the corona pandemic.
                 It was implemented at the health department in Calw in October 2020 and full data set ares 
                avaible starting from December 1st 2020. The health department is using SORMAS for 
                contact tracking, case recording and outbreak management
                 More information about SORMAS can be found on the official SORMAS 
                 homepage: https://www.sormas-oegd.de/"),
                 h4("Data extraction and annoymisation"),
                 p(" Case recording data was extraction and anonymized by Iris Brilhaus, MD in the Corona
                 task force of district office Calw. 
                 Initial data extraction was performed on April 21st 2021 and was repeated on May 13th 2021."),
                 h4("Data cleaning and translation"),
                 p("Data cleaning and translation in English was performed by Alexandra Malinovska.
                 Only the cleaned and translated data is available here. The data set might be different from 
                  official records, because some participants with missing data were excluded. Missing data
                  could be attributed 
                   to missing laboratory test, which were captured with a different software. Since analysis 
                    should include only patients with a recorded positive laboratory test, only these 
                    participants are contained in the data set. 
                   Questions about data cleaning and translation can be directed to her."),
                 h4("Data dictionary"),
                 p("The data dictionary can be found in the Readme file of this App on Github."),
                 h3("Acknowledgment"),
                 p(" We want to thank the health department Calw for providing this interesting and real-life 
                 data. We thank Dr. Frank Wiehe, first state official and Dr. Philip-Rene' Retzbach, legal 
                counsel for their support. 
                 A special thank you to Iris Brilhaus for the introduction into the data and 
                 health department processes and all fruitful discussions in her free time. "),
                 h3("Maintenance"),
                 p(" The last data extraction was performed on May 13, 2021, further data updates and 
                  maintenance of the app will be discussed with the health department Calw")
               )
               
      ),
      
      

      tabPanel("Symptoms - Ordinal Regression on testing time", fluid = TRUE,
               h3( "Ordinal Regression on testing time using symptoms"),
               p("After the descriptive analysis, we wanted to fit a model to try to answer:
                 Are symptoms associated with a specific testing time and can we model this in a regression?
                 We fit an ordinal model and compared predicted and observed probability"), 
               br(), 
               fluidRow(
                 column(6,
                   plotlyOutput("OMplotBeforeAM")),
                 column(6,
                   plotlyOutput("OMplotImmAM"))),
               fluidRow(
                 column(6,
                   plotlyOutput("OMplotEarlyAM")),
                 column(6,
                   plotlyOutput("OMplotLateAM"))),
                h3( "Model's Estimate including 95% CI"), 
               tableOutput("ordinalmodelAM")
               
                 
               ),
    
      

        
         tabPanel("Time Trend", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   h3('Select Age Category:'),
                   checkboxGroupInput("agetimeln", label=h3("Five Year Bins:"),
                                      choices = list('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54',
                                                     '55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100-104'),
                                      selected = '0-4'
                                      ),
                   checkboxGroupInput("agewideln", label=h3("Twenty Year Bins:"),
                                      choices = list('0-19','20-39','40-59','60-79','80+'),
                                      selected = '0-19'
                   )
                 ),
                 mainPanel(
                   h2('COVID-19 Cases by Age Time Trend: December 2020 - May 2021'),
                        h3('Total Cases Over Time:'),
                   plotOutput('timetrend3'),
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

  
  
  
    load('CalwData.RData')
    #Convert reportDate from string to actual date factor
    #CalwData$reportDate <- as.Date(CalwData$reportDate, "%Y-%m-%d")
  CalwData %<>%
    mutate(
      testingdelay= ymd(indexcase)- symptoms.onsetDate,
      reportingdelay = reportDate - ymd(indexcase),
      orderdelay= qOrderDate - reportDate,
      symptomaticTest=if_else(symptoms.onsetDate>indexcase, "no", "yes"),
      symptomaticatanytime = if_else(is.na(symptoms.onsetDate), "no", "yes")
    ) 


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
    
    
 #### Death, Age, Sex Association:
    
    output$sexmosaic <- renderPlotly({
      
      CalwDatanew <- CalwData %>%
        select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      
      CalwDatanew <- CalwDatanew %>%
        mutate(Sex = ifelse(sex == "female", "female",
                               ifelse(sex == "male", "male", "unknown"))) %>%
        mutate(Condition = ifelse(condition == "alive", "alive",
                                     ifelse(condition == "died", "died", "unknown"))) 
      
      CalwDatanew <- CalwDatanew[which(CalwDatanew$Condition == 'alive' | CalwDatanew$Condition == 'died'),]
      CalwDatanew <- CalwDatanew[which(CalwDatanew$Sex == 'female' | CalwDatanew$Sex == 'male'),]
    
      mosaic_examp <-  ggplot(data = CalwDatanew) +
        geom_mosaic(aes(x = product( Condition, Sex), fill = Condition)) +   
        labs(y="Condition", x="Sex", title = "Mosaic Plot: Sex and COVID-19 Condition") 
        
      
      mosaic <- ggplotly(mosaic_examp)
      mosaic
    })
    
    
    
    output$chisq <- renderPrint({
      
      CalwDatanew <- CalwData %>%
        select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      
      CalwDatanew <- CalwDatanew %>%
        mutate(Sex = ifelse(sex == "female", "female",
                            ifelse(sex == "male", "male", "unknown"))) %>%
        mutate(Condition = ifelse(condition == "alive", "alive",
                                  ifelse(condition == "died", "died", "unknown"))) 
      
      CalwDatanew <- CalwDatanew[which(CalwDatanew$Condition == 'alive' | CalwDatanew$Condition == 'died'),]
      CalwDatanew <- CalwDatanew[which(CalwDatanew$Sex == 'female' | CalwDatanew$Sex == 'male'),]
      
      tbl <- table(CalwDatanew$Condition, CalwDatanew$Sex)
      results <- chisq.test(tbl)
      results
      
    })
    
    
    output$chisqage <- renderPrint({
      CalwDatanew <- CalwData %>%
      select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      
      CalwDatanew <- CalwDatanew %>%
        mutate(Condition = ifelse(condition == "alive", "alive",
                                  ifelse(condition == "died", "died", "unknown"))) 
      
      CalwDatanew <- CalwDatanew[which(CalwDatanew$Condition == 'alive' | CalwDatanew$Condition == 'died'),]

      CalwDatanew <- CalwDatanew %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      
      CalwDatanew$Age_Cat <- factor(CalwDatanew$Age_Cat,
                                    levels = c(0,1,2,3, 4),
                                    labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      tbl <- table(CalwDatanew$Condition, CalwDatanew$Age_Cat)
      results <- chisq.test(tbl)
      results
      
    })
    
    output$agemosaic <- renderPlotly({
      
      CalwDatanew <- CalwData %>%
        select(age, sex, condition)
      CalwDatanew <- CalwDatanew[complete.cases(CalwDatanew),]
      
      CalwDatanew <- CalwDatanew %>%
        mutate(Condition = ifelse(condition == "alive", "alive",
                                  ifelse(condition == "died", "died", "unknown"))) 
      
      CalwDatanew <- CalwDatanew[which(CalwDatanew$Condition == 'alive' | CalwDatanew$Condition == 'died'),]
      
      CalwDatanew <- CalwDatanew %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 0,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 1,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 2,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 3, 4)))))
      
      CalwDatanew$Age_Cat <- factor(CalwDatanew$Age_Cat,
                                    levels = c(0,1,2,3, 4),
                                    labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
     
      
      mosaic_examp <-  ggplot(data = CalwDatanew) +
        geom_mosaic(aes(x = product( Condition, Age_Cat), fill = Condition)) +   
        labs(y="Condition", x="Age Category", title = "Mosaic Plot: Age and Condition") 
      mosaic <- ggplotly(mosaic_examp)
      mosaic
    })
    
    
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
    
   
    
     #### Probability of Death
    
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
        mutate(Age_Cat = input$ageln) %>%
        mutate(sex = input$sexln)
          
      prob <- predict(mylogit, newdata, type="response")
      
       percent <- prob*100
       percent
       percent %>% as.numeric() 
    })
    
    
    ####
    
    ####
    
    #### Tab F
    

    
    #### Tab G

    fix_symptoms=function(x){
      x%<>%factor(.,levels=c("feelingIll","fever","shivering","chillsSweats","musclePain","headache","difficultyBreathing","cough","soreThroat","runnyNose","nausea","diarrhea","lossOfSmell","lossOfTaste"),
                  labels=c("feeling ill","fever","shivering","chills sweats","muscle pain","headache","difficulty breathing","cough","sore throat","runny nose","nausea","diarrhea","loss of smell","loss of taste"))
    }
    
    CalwData %<>%
      mutate(
        testingdelay= ymd(indexcase)- symptoms.onsetDate,
        reportingdelay = reportDate - ymd(indexcase),
        orderdelay= qOrderDate - reportDate,
        symptomaticTest=if_else(symptoms.onsetDate>indexcase, "no", "yes"),
        symptomaticatanytime = if_else(is.na(symptoms.onsetDate), "no", "yes")
      ) 
    
    Symptoms= CalwData %>%
      dplyr::select(contains("symptoms."))%>%
      dplyr::select_if(is.factor)%>%
      dplyr::select(-symptoms.symptomatic) %>%
      gather(symptom, value ) %>%
      group_by(symptom,value) %>%
      dplyr::summarize(n=n())%>%
      filter(value=="yes")%>%
      mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
    
    
    
    output$pieallAM <- renderPlotly({
     plot_ly(Symptoms, labels = ~symptom, values = ~n, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(n,"cases"),
                   marker = list(colors = pal_simpsons("springfield")(14),
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE)%>%
                    layout(title = 'Symptoms',
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    })
    
    output$piesubAM <- renderPlotly({
      age_sub=input$agegroupAM
      fig_sub = CalwData %>%
        filter(age==age_sub)%>%
        dplyr::select(contains("symptoms."))%>%
        dplyr::select_if(is.factor)%>%
        dplyr::select(-symptoms.symptomatic) %>%
        gather(symptom, value ) %>%
        group_by(symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>% 
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)%>%
        plot_ly(labels = ~symptom, values = ~n, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste(n,"cases"),
                marker = list(colors = pal_simpsons("springfield")(14),
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE) %>% layout(title = paste('Symptoms in age group',age_sub),
                                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      fig_sub #%>% layout(width = 500)
      
          })
    
    output$bar_ageAM <- renderPlotly({
      Symptom_distribution_age=CalwData %>%
        dplyr::select(c(age,contains("symptoms.")))%>%
        dplyr::select_if(is.factor)%>%
        dplyr::select(-symptoms.symptomatic) %>%
        gather(symptom, value, -age ) %>%
        group_by(age,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      barplot=ggplot(Symptom_distribution_age, aes(x=age,y=n))+
        geom_col(aes(fill=symptom),position ="fill", color="black")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90), legend.title = element_blank())+
        scale_fill_simpsons()+
        xlab(paste("\n","Age Category [5 Year]")) + 
        ylab("Proportion")
      
      
      ggplotly(barplot)%>% layout(height = 700, width = 500)
      
    })
    
    
    output$bar_age_groupedAM <- renderPlotly({
    Symptom_distribution_age_g=CalwData %>%
      dplyr::select(c(age,contains("symptoms.")))%>%
      dplyr::select_if(is.factor)%>%
      mutate(age_grouped=case_when(
        age == '0-4' | age == '5-9' | age == '10-14' | age == "15-19"  ~ 0,
        age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39' ~ 1,
        age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59' ~ 2,
        age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79' ~ 3,
        TRUE ~ 4),
        age_grouped=factor(age_grouped, levels = c(0,1,2,3, 4),
                           labels = c("0-19", "20-39", "40-59", "60-79", "80+")))%>%
      dplyr::select(-c(age,symptoms.symptomatic)) %>%
      gather(symptom, value, -age_grouped ) %>%
      group_by(age_grouped,symptom,value) %>%
      dplyr::summarize(n=n())%>%
      filter(value=="yes")%>%
      mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
    
    barplot_grouped=ggplot(Symptom_distribution_age_g, aes(x=age_grouped,y=n))+
      geom_col(aes(fill=symptom),position ="fill", color="black")+
      scale_fill_simpsons()+
      theme_minimal()+
      theme(legend.title = element_blank())+
      scale_fill_simpsons()+
      xlab(paste("\n","Age Category [20 Year]")) + 
      ylab("Proportion")
    
    ggplotly(barplot_grouped)%>% layout(height = 800, width = 500)
    })
    
    output$bar_age_grouped_fischerAM= renderPlotly({
      
      Symptom_distribution_age_g=CalwData %>%
        dplyr::select(c(age,contains("symptoms.")))%>%
        dplyr::select_if(is.factor)%>%
        mutate(age_grouped=case_when(
          age == '0-4' | age == '5-9' | age == '10-14' | age == "15-19"  ~ 0,
          age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39' ~ 1,
          age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59' ~ 2,
          age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79' ~ 3,
          TRUE ~ 4),
          age_grouped=factor(age_grouped, levels = c(0,1,2,3, 4),
                             labels = c("0-19", "20-39", "40-59", "60-79", "80+")))%>%
        dplyr::select(-c(age,symptoms.symptomatic)) %>%
        gather(symptom, value, -age_grouped ) %>%
        group_by(age_grouped,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      
      symptom_of_interes=input$soi_ageAM
      bin1=input$ref_ageAM
      cof=Symptom_distribution_age_g$age_grouped
      
      fischer_test_function_gen=function(x, bin1, cof){
        plot_dataframe=NULL
        group=deparse(substitute(cof))
        data=x%>%group_by({{cof}})%>%mutate(total=sum(n))%>%ungroup
        for(b in 1:nlevels(data[[group]])){
          bin_tmp=levels(data[[group]])[b]  
          if(bin_tmp!=bin1){
            s1=data%>%filter({{cof}}==bin1)%>%filter(symptom==symptom_of_interes)%>%dplyr::select(n)%>%as.numeric()
            s2=data%>%filter({{cof}}==bin_tmp)%>%filter(symptom==symptom_of_interes)%>%dplyr::select(n)%>%as.numeric()
            ns1=data%>%filter({{cof}}==bin1)%>%filter(symptom!=symptom_of_interes)%>%summarize(sum(n))%>%as.numeric()
            ns2=data%>%filter({{cof}}==bin_tmp)%>%filter(symptom!=symptom_of_interes)%>%summarize(sum(n))%>%as.numeric()
            dat <- data.frame(
              "symptom_yes" = c(s1,s2),
              "symptom_no" = c(ns1,ns2),
              row.names = c("bin1", "bin2"),
              stringsAsFactors = FALSE)
            p_val=fisher.test(dat)$p.value
            p_val2=
              case_when(
                p_val>0.1 ~ as.character(round(p_val,1)),
                p_val>0.01 ~ as.character(round(p_val,2)),
                p_val>0.001 ~ as.character(round(p_val,3)),
                p_val<0.001 ~ "<0.001"
              )
            plot_dataframe=rbind(plot_dataframe,c("x_value"=bin_tmp, "label_value" = paste("p-value:",'\n', p_val2), "y_value"= 120 * max(c(s1/(s1+ns1)),s2/(s2+ns2))))
            
          }
        }
        return(as.data.frame(plot_dataframe))
      }
      
      colordata=as.data.frame(cbind(symptom=levels(Symptom_distribution_age_g$symptom), 
                                    level=1:nlevels(Symptom_distribution_age_g$symptom),
                                    color=pal_simpsons("springfield")(nlevels(Symptom_distribution_age_g$symptom))))
      
      farbe=colordata%>%filter(symptom==symptom_of_interes)%>%dplyr::select(color)%>%as.character()
      
      barplot_age_dis=ggplot(Symptom_distribution_age_g%>%group_by(age_grouped)%>%mutate(total=sum(n))%>%
                              filter(symptom==symptom_of_interes), aes(x=age_grouped,y=100*n/total))+
      geom_col(aes(fill=age_grouped))+
      geom_text(data=fischer_test_function_gen(Symptom_distribution_age_g,bin1, age_grouped),aes(x=x_value, y=as.numeric(y_value), label=label_value), vjust = 1.0)+
      ylim(NA, fischer_test_function_gen(Symptom_distribution_age_g,bin1, age_grouped)%>%filter(y_value==max(as.numeric(y_value)))%>%dplyr::select(y_value)%>%head(1)%>%as.numeric() +0.5)+ 
      scale_fill_manual(values = rep(farbe,5))+
      theme_minimal() + 
      theme(legend.position = "none")+ 
      xlab("Age Category [20 years]") + 
      ylab("Percent [%]")
        
      
      barplot_age_dis
      ggplotly(barplot_age_dis)
    })
    
    
    
    output$barAM <- renderPlotly({
      Symptom_distribution_delay=CalwData %>%
      dplyr::select(c(testingdelay,contains("symptoms.")))%>%
      dplyr::select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
      filter(!is.na(testingdelay))%>%
      mutate(delay_binned=case_when(
      testingdelay<0~"before",
      testingdelay%in%(0:2)~"immediate",
      testingdelay%in%(3:5)~"early",
      testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
      dplyr::select(-testingdelay)%>%
      gather(symptom, value, -delay_binned ) %>%
      group_by(delay_binned,symptom,value) %>%
      dplyr::summarize(n=n())%>%
      filter(value=="yes")%>%
      mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
    
      barplot_delay=ggplot(Symptom_distribution_delay, aes(x=delay_binned,y=n))+
      geom_col(aes(fill=symptom),position ="fill", color="black")+
      scale_fill_simpsons()+
      theme_minimal() + 
      xlab("Test Timing Category") + 
      ylab("Proportion")+
      theme(legend.title = element_blank())
    
      ggplotly(barplot_delay)%>% layout(height = 800, width = 500)
    })
    
    output$fischerAM <- renderPlotly({
      #renderPlot({
      symptom_of_interes=input$soiAM
      bin1=input$refAM
      
      fischer_test_function=function(x, bin1){
        plot_dataframe=NULL
        data=x%>%group_by(delay_binned)%>%mutate(total=sum(n))%>%ungroup
        for(b in 1:nlevels(data$delay_binned)){
          bin_tmp=levels(data$delay_binned)[b]
          if(bin_tmp!=bin1){
            s1=data%>%filter(delay_binned==bin1)%>%filter(symptom==symptom_of_interes)%>%dplyr::select(n)%>%as.numeric()
            s2=data%>%filter(delay_binned==bin_tmp)%>%filter(symptom==symptom_of_interes)%>%dplyr::select(n)%>%as.numeric()
            ns1=data%>%filter(delay_binned==bin1)%>%filter(symptom!=symptom_of_interes)%>%summarize(sum(n))%>%as.numeric()
            ns2=data%>%filter(delay_binned==bin_tmp)%>%filter(symptom!=symptom_of_interes)%>%summarize(sum(n))%>%as.numeric()
            dat <- data.frame(
              "symptom_yes" = c(s1,s2),
              "symptom_no" = c(ns1,ns2),
              row.names = c("bin1", "bin2"),
              stringsAsFactors = FALSE)
            
            p_val=fisher.test(dat)$p.value
            p_val2=
              case_when(
                p_val>0.1 ~ as.character(round(p_val,1)),
                p_val>0.01 ~ as.character(round(p_val,2)),
                p_val>0.001 ~ as.character(round(p_val,3)),
                p_val<0.001 ~ "<0.001"
              )
          
            plot_dataframe=rbind(plot_dataframe,c("x_value"=bin_tmp, "label_value" = paste("p-value:",'\n',p_val2), "y_value"= 110 * max(c(s1/(s1+ns1)),s2/(s2+ns2))))}}
        return(as.data.frame(plot_dataframe))
      }
      
      Symptom_distribution_delay=CalwData %>%
        dplyr::select(c(testingdelay,contains("symptoms.")))%>%
        dplyr::select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
        filter(!is.na(testingdelay))%>%
        mutate(delay_binned=case_when(
          testingdelay<0~"before",
          testingdelay%in%(0:2)~"immediate",
          testingdelay%in%(3:5)~"early",
          testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
        dplyr::select(-testingdelay)%>%
        gather(symptom, value, -delay_binned ) %>%
        group_by(delay_binned,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      colordata=as.data.frame(cbind(symptom=levels(Symptom_distribution_delay$symptom), 
                                    level=1:nlevels(Symptom_distribution_delay$symptom),
                                    color=pal_simpsons("springfield")(nlevels(Symptom_distribution_delay$symptom))))
      
      farbe=colordata%>%filter(symptom==symptom_of_interes)%>%dplyr::select(color)%>%as.character()
      
      barplot_delay=ggplot(Symptom_distribution_delay%>%group_by(delay_binned)%>%mutate(total=sum(n))%>%filter(symptom==symptom_of_interes), aes(x=delay_binned,y=100*n/total))+
        geom_col(aes(fill=delay_binned))+
        geom_text(data=fischer_test_function(Symptom_distribution_delay,bin1),
                  aes(x=x_value, y=as.numeric(y_value), label=label_value,vjust = 1.5))+
        ylim(NA, fischer_test_function(Symptom_distribution_delay,bin1)%>%
               filter(y_value==max(as.numeric(y_value)))%>%dplyr::select(y_value)%>%head(1)%>%as.numeric() +0.5)+
        theme_minimal() + 
        theme(legend.position = "none")+ 
        xlab("Test Timing Category") + 
        ylab("Percent [%]")+
        scale_fill_manual(values = rep(farbe,4))
    
        barplot_delay

      ggplotly(barplot_delay)%>% layout(height = 400, width = 600) 
    })
    

    
    ####
    
    #### Tab H
    
    
    output$ordinalmodelAM <- renderTable({
      
      Symptom_distribution_delay=CalwData %>%
        dplyr::select(c(testingdelay,contains("symptoms.")))%>%
        dplyr::select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
        filter(!is.na(testingdelay))%>%
        mutate(delay_binned=case_when(
          testingdelay<0~"before",
          testingdelay%in%(0:2)~"immediate",
          testingdelay%in%(3:5)~"early",
          testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
        dplyr::select(-testingdelay)%>%
        gather(symptom, value, -delay_binned ) %>%
        group_by(delay_binned,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
    Symptom_distribution_delay_expand <- Symptom_distribution_delay %>%
      uncount(n)
    
    #  Observed frequencies
    observed=Symptom_distribution_delay_expand %>%
      group_by(symptom) %>%
      dplyr::summarize(n = n(), 
                       before = sum(delay_binned == "before")/n,
                       imm = sum(delay_binned == "immediate")/n,
                       ear = sum(delay_binned == "early")/n,
                       late = sum(delay_binned == "late")/n)
    
    
    #  Predicted frequencies
    modelord <- polr(delay_binned ~ symptom, data = Symptom_distribution_delay_expand, Hess=TRUE, method="logistic")
    
    tbl= cbind(as.data.frame(exp(modelord$coefficients)), exp(confint.default(modelord))) %>%
      set_colnames(c( "estimate","Lower CI","Higher CI"))%>%
      tibble::rownames_to_column(., "symptom")
    
    tbl 
    
    })
    
    output$OMplotBeforeAM <- renderPlotly({
      
      Symptom_distribution_delay=CalwData %>%
        dplyr::select(c(testingdelay,contains("symptoms.")))%>%
        dplyr::select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
        filter(!is.na(testingdelay))%>%
        mutate(delay_binned=case_when(
          testingdelay<0~"before",
          testingdelay%in%(0:2)~"immediate",
          testingdelay%in%(3:5)~"early",
          testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
        dplyr::select(-testingdelay)%>%
        gather(symptom, value, -delay_binned ) %>%
        group_by(delay_binned,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      Symptom_distribution_delay_expand <- Symptom_distribution_delay %>%
        uncount(n)
      
      #  Observed frequencies
      observed=Symptom_distribution_delay_expand %>%
        group_by(symptom) %>%
        dplyr::summarize(n = n(), 
                         before = sum(delay_binned == "before")/n,
                         imm = sum(delay_binned == "immediate")/n,
                         ear = sum(delay_binned == "early")/n,
                         late = sum(delay_binned == "late")/n) %>%
        mutate(
          data= rep("observed",14))
      
      
      #  Predicted frequencies
      modelord <- polr(delay_binned ~ symptom, data = Symptom_distribution_delay_expand, Hess=TRUE, method="logistic")
      
     pred_data <- as_tibble(predict(modelord, type="probs")) 
    

    Symptom_distribution_delay_expand <- bind_cols(Symptom_distribution_delay_expand, pred_data)

    # Find mean probs
    predict=Symptom_distribution_delay_expand %>%
      group_by(symptom) %>%
      dplyr::summarize(
        before = mean(before),
        imm = mean(immediate),
        ear = mean(`early`),
        late = mean(`late`))%>%
      mutate(
        data= rep("predicted",14),
        n = observed$n)

    comparison= rbind(predict,observed)
    
    barplot_comparison_before=ggplot(comparison, aes(x=symptom, y=before,fill=data))+
      geom_col(position=position_dodge())+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))+
      scale_fill_simpsons()+
      xlab(paste("\n","symptoms")) + 
      ylab(paste("Propability"))+ 
      ggtitle("test before symptoms occur")
    
    
    ggplotly(barplot_comparison_before)
    })
    
    output$OMplotImmAM <- renderPlotly({
      
      Symptom_distribution_delay=CalwData %>%
        dplyr::select(c(testingdelay,contains("symptoms.")))%>%
        dplyr::select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
        filter(!is.na(testingdelay))%>%
        mutate(delay_binned=case_when(
          testingdelay<0~"before",
          testingdelay%in%(0:2)~"immediate",
          testingdelay%in%(3:5)~"early",
          testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
        dplyr::select(-testingdelay)%>%
        gather(symptom, value, -delay_binned ) %>%
        group_by(delay_binned,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      Symptom_distribution_delay_expand <- Symptom_distribution_delay %>%
        uncount(n)
      
      #  Observed frequencies
      observed=Symptom_distribution_delay_expand %>%
        group_by(symptom) %>%
        dplyr::summarize(n = n(), 
                         before = sum(delay_binned == "before")/n,
                         imm = sum(delay_binned == "immediate")/n,
                         ear = sum(delay_binned == "early")/n,
                         late = sum(delay_binned == "late")/n) %>%
        mutate(
          data= rep("observed",14))
      
      
      #  Predicted frequencies
      modelord <- polr(delay_binned ~ symptom, data = Symptom_distribution_delay_expand, Hess=TRUE, method="logistic")
      
      pred_data <- as_tibble(predict(modelord, type="probs")) 
      
      
      Symptom_distribution_delay_expand <- bind_cols(Symptom_distribution_delay_expand, pred_data)
      
      # Find mean probs
      predict=Symptom_distribution_delay_expand %>%
        group_by(symptom) %>%
        dplyr::summarize(
          before = mean(before),
          imm = mean(immediate),
          ear = mean(`early`),
          late = mean(`late`))%>%
        mutate(
          data= rep("predicted",14),
          n = observed$n)
      
      comparison= rbind(predict,observed)
      
      bbarplot_comparison_immediate= ggplot(comparison, aes(x=symptom, y=imm,fill=data))+
        geom_col( position=position_dodge())+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90))+
        theme(axis.text.x = element_text(angle = 90))+
        scale_fill_simpsons()+
        xlab(paste("\n","symptoms")) + 
        ylab(paste("Propability"))+ 
        ggtitle("test immediately when symptoms occur")
      
      ggplotly(bbarplot_comparison_immediate)
      
    })
    
    output$OMplotEarlyAM <- renderPlotly({
      
      Symptom_distribution_delay=CalwData %>%
        dplyr::select(c(testingdelay,contains("symptoms.")))%>%
        dplyr::select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
        filter(!is.na(testingdelay))%>%
        mutate(delay_binned=case_when(
          testingdelay<0~"before",
          testingdelay%in%(0:2)~"immediate",
          testingdelay%in%(3:5)~"early",
          testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
        dplyr::select(-testingdelay)%>%
        gather(symptom, value, -delay_binned ) %>%
        group_by(delay_binned,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      Symptom_distribution_delay_expand <- Symptom_distribution_delay %>%
        uncount(n)
      
      #  Observed frequencies
      observed=Symptom_distribution_delay_expand %>%
        group_by(symptom) %>%
        dplyr::summarize(n = n(), 
                         before = sum(delay_binned == "before")/n,
                         imm = sum(delay_binned == "immediate")/n,
                         ear = sum(delay_binned == "early")/n,
                         late = sum(delay_binned == "late")/n) %>%
        mutate(
          data= rep("observed",14))
      
      
      #  Predicted frequencies
      modelord <- polr(delay_binned ~ symptom, data = Symptom_distribution_delay_expand, Hess=TRUE, method="logistic")
      
      pred_data <- as_tibble(predict(modelord, type="probs")) 
      
      
      Symptom_distribution_delay_expand <- bind_cols(Symptom_distribution_delay_expand, pred_data)
      
      # Find mean probs
      predict=Symptom_distribution_delay_expand %>%
        group_by(symptom) %>%
        dplyr::summarize(
          before = mean(before),
          imm = mean(immediate),
          ear = mean(`early`),
          late = mean(`late`))%>%
        mutate(
          data= rep("predicted",14),
          n = observed$n)
      
      comparison= rbind(predict,observed)
      
      barplot_comparison_early= ggplot(comparison, aes(x=symptom, y=ear,fill=data))+
        geom_col( position=position_dodge())+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90))+
        scale_fill_simpsons()+
        xlab(paste("\n","symptoms")) + 
        ylab(paste("Propability"))+ 
        ggtitle("test early after symptoms occur")
      
      
      ggplotly(barplot_comparison_early)
      
    })
    
    output$OMplotLateAM <- renderPlotly({
      
      Symptom_distribution_delay=CalwData %>%
        dplyr::select(c(testingdelay,contains("symptoms.")))%>%
        dplyr::select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
        filter(!is.na(testingdelay))%>%
        mutate(delay_binned=case_when(
          testingdelay<0~"before",
          testingdelay%in%(0:2)~"immediate",
          testingdelay%in%(3:5)~"early",
          testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
        dplyr::select(-testingdelay)%>%
        gather(symptom, value, -delay_binned ) %>%
        group_by(delay_binned,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      Symptom_distribution_delay_expand <- Symptom_distribution_delay %>%
        uncount(n)
      
      #  Observed frequencies
      observed=Symptom_distribution_delay_expand %>%
        group_by(symptom) %>%
        dplyr::summarize(n = n(), 
                         before = sum(delay_binned == "before")/n,
                         imm = sum(delay_binned == "immediate")/n,
                         ear = sum(delay_binned == "early")/n,
                         late = sum(delay_binned == "late")/n) %>%
        mutate(
          data= rep("observed",14))
      
      
      #  Predicted frequencies
      modelord <- polr(delay_binned ~ symptom, data = Symptom_distribution_delay_expand, Hess=TRUE, method="logistic")
      
      pred_data <- as_tibble(predict(modelord, type="probs")) 
      
      
      Symptom_distribution_delay_expand <- bind_cols(Symptom_distribution_delay_expand, pred_data)
      
      # Find mean probs
      predict=Symptom_distribution_delay_expand %>%
        group_by(symptom) %>%
        dplyr::summarize(
          before = mean(before),
          imm = mean(immediate),
          ear = mean(`early`),
          late = mean(`late`))%>%
        mutate(
          data= rep("predicted",14),
          n = observed$n)
      comparison= rbind(predict,observed)
      
      barplot_comparison_late= ggplot(comparison, aes(x=symptom, y=late,fill=data))+
        geom_col( position=position_dodge())+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90))+
        scale_fill_simpsons()+
        xlab(paste("\n","symptoms")) + 
        ylab(paste("Propability"))+ 
        ggtitle("test late after symptoms occur")
      
      
      ggplotly(barplot_comparison_late)
      
    })
    
    
    ####
   
     #### Time Trend
    
    #Plot the number of reports for each age each day
    
    output$timetrend3 <- renderPlot({
      
      Calwnew <- CalwData %>%
        select(reportDate, age) %>%
        group_by(reportDate) %>%
        summarise(count = n())
      
      g<- ggplot(Calwnew, aes(x =reportDate, y= count))+ geom_point() + scale_x_date() +
        geom_smooth(method = "loess", size = 1.5) + 
        labs(title = "Total Number COVID-19 Cases Over Time", x = "Month", y = "Number of Cases")
      g
      
    })
    
    
    output$timetrend <- renderPlot({
      
      Calwnew <- CalwData %>%
        select(reportDate, age) %>%
        group_by(age, reportDate) %>%
        summarise(count = n())
      
      g<- ggplot(Calwnew[Calwnew$age %in% input$agetimeln,], aes(x =reportDate, y= count, color = age))+ geom_point() + scale_x_date() +
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
      

      g<- ggplot(Calwnew[Calwnew$Age_Cat %in% input$agewideln,], aes(x =reportDate, y= count, color = Age_Cat))+ geom_point() + scale_x_date() +
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

