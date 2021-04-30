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
               headerPanel("Association between Travel Outside District and COVID-19 Variants"),
               sidebarLayout(
                 sidebarPanel(
                   h3('COVID-19 Variants Observed:'),
                   h4('B117'),
                   h5('Initially detected in UK'),
                   h4('B1351'),
                   h5('Initially detected in South Africa'),
                   h4('noVOC'),
                   h5('Variant of no concern')),
                 mainPanel(h2("Association Results"),
                           h3('Chi-Square Test:'),
                           h4('A chi-square test indicates no significant association between COVID-19 variant and travel history (p=0.942)'),
                 textOutput('chisq'),
                 h3('Data Frequency Table:'),
                 tableOutput('varianttraveltab'),
                 h3('Mosaic Plot:'),
                 plotOutput('varianttravelmosaic')
                 ))
               
      ),
      
      
      tabPanel("F", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   radioButtons('sex', label=h3("Select Sex"),
                                c('female' = 'female','male'= 'male'),
                                selected = 'female'),
                   
                   radioButtons("age", label=h3("Select Age"),
                                choices = list('0-19','20-39','40-59','60-79','80+'),
                                selected = '0-19')
                   ),
                 mainPanel(h2('Odds of Death Given Sex and Age'),
                           htmlOutput('logvar'),
                           textOutput(predict)
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
    
    output$varianttraveltab <- renderTable({
      as.data.frame.matrix(xtabs(~traveled + variant, data= CalwData))
    })
    
    output$varianttravelmosaic <- renderPlot({
      mosaic <- table(CalwData$variant, CalwData$traveled)
      mosaicplot(mosaic, main = "COVID-19 Variants by Travel History Mosaic Plot",
                 xlab = "COVID-19 Variants",
                 ylab = "Traveled",
                 las = 2,
                 color = "skyblue2")
    })
    
    output$chisq <- renderPrint({
      tbl <- table(CalwData$variant, CalwData$traveled)
      results <- chisq.test(tbl)
      results
    })
    
    ####
    
    #### Tab F
    
    output$logvar <- renderUI({
      CalwData2 <- CalwData %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5)))))
      
      CalwData2$Age_Cat <- factor(CalwData2$Age_Cat,
                                  levels = c(1,2,3, 4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      mylogit <- glm(condition ~ Age_Cat + sex , data = CalwData2, family = "binomial")
      #glmresult <- broom::tidy(mylogit, conf.int = TRUE, exponentiate=TRUE)
      tbl <- HTML(tab_model(mylogit)$knitr)
      tbl
      
    })
    
    output$predict <- renderPrint({
      CalwData2 <- CalwData %>%
        mutate(Age_Cat = ifelse(age == '0-4' | age == '5-9' | age == '10-14' | age == '15-19', 1,
                                ifelse(age == '20-24' | age == '25-29' | age == '30-34' | age == '35-39', 2,
                                       ifelse(age == '40-44' | age == '45-49' | age == '50-54' | age == '55-59', 3,
                                              ifelse(age == '60-64' | age == '65-69' | age == '70-74' | age == '75-79', 4, 5)))))
      
      CalwData2$Age_Cat <- factor(CalwData2$Age_Cat,
                                  levels = c(1,2,3, 4, 5),
                                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
      
      mylogit <- glm(condition ~ Age_Cat + sex , data = CalwData2, family = "binomial")
      
      newdata = data.frame()
      
      if (input$age == "0-19") {newdata$Age_Cat = 1} else if
                                      (input$age == "20-39") {newdata$Age_Cat = 2} else if
                                      (input$age == "40-59") {newdata$Age_Cat = 3} else if 
                                      (input$age == "60-79") {newdata$Age_Cat = 4}
                                      else {newdata$Age_Cat = 5}
    
      if (input$sex == "female") {newdata$sex = 1} else {newdata$sex = 0}
      
      prob <- predict(mylogit, newdata, type="response")
      prob
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

