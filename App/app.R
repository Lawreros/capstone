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
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggmosaic)

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
      
      
      tabPanel("D", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ),
      
      
      tabPanel("E", fluid = TRUE,
               headerPanel("Association between Sex, Age and Condition (Alive/Dead)"),
               sidebarLayout(
                 sidebarPanel(h3('Summary of Analysis:'),
                              h5('In this tab, we evaluate the association between sex, age-category, and condition.'),
                              h5('Condition was defined if died if person was killed from COVID-19 infection.'),
                              h5('Persons with unknown or missing sex, age, or condition were excluded from analysis.'),
                              h5('Chi-square tests were used to test for significant associations.')
                   ),
                 mainPanel(h1("Association Results:"),
                           h2('Sex Results'),
                           h3('Fisher Exact Test:Sex and Condition'),
                           h4('A fisher exact test indicates no significant association between condition and sex (p=0.079)'),
                      textOutput('sexfisher'),
                      textOutput('chisq'),
                      h3('Mosaic Plot: Sex'),
                      plotlyOutput('sexmosaic'),
                 h2("Age Category Results"),
                      h3('Fisher Exact Test: Age and Condition'),
                      h4('A fisher exact test indicates no significant association between COVID-19 variant and age-category (p=0.)'),
                      textOutput('agefisher'),
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
                           htmlOutput('logvar'),
                           h3('Odds Ratio Interpretations:'),
                           h5('The odds of death for females is 0.42 times the odds of death for males, or significantly lower by 58% (p<0.01.'),
                           h5('The odds of death increases with each increasing age category and is highest in adults over 80 years old. The odds of death in adults over 80 years old is 117.51 times the odds of death in children 0-19 years old (p<0.001).'),
                           h3('Predicted Percent Chance of Death Given Inputs (Age Category and Sex):'),
                           h5('Chance of death calculated by converting log odds of death into predicted probability of death.'),
                           textOutput('predict')
                           
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
    
    #### Tab C
    
    ####
    
    #### Tab D
    
    ####
    
    #### Tab E Association between travel and the variants
    
    #output$varianttraveltab <- renderTable({
      #as.data.frame.matrix(xtabs(~sex + variant, data= CalwData))
    #})
    
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
    })
    
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

