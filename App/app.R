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





library(tidyverse)
library(magrittr)
library(lubridate)
library(ggsci)
library(ggrepel)

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
      
      
      tabPanel("G - Symptoms", fluid = TRUE,
               tabsetPanel(
                 tabPanel("Symptom Distribution by Age",
               #h3("Symptom Distribution by Age"),
               fluidRow(
                 column(4,
                        h4("Symptom Distribution in Total Group"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        plotlyOutput("pieall")
                        
                        
                 ),
                 column(2,
                        br()),
                 column(4,
                        h4("Symptom Distribution by Age"),
                        
                        selectInput('agegroup', "Select Age Group",
                                    choices = list('0-4','5-9','10-14','15-19','20-24','25-29',
                                                   '30-34','35-39','40-44','45-49','50-54',
                                                   '55-59','60-64','65-69','70-74','75-79',
                                                   '80-84','85-89','90-94','95-99','100-104'),
                                    selected = '0-4'
                        ),
                        br(),
                        plotlyOutput('piesub')
                        
                    
                 )),
               fluidRow(
                 br(),
                 br(),
                 column(4,
                        plotlyOutput("bar_age")
                 ))),
               tabPanel("Symptom Distribution by Timing of Test", 
               h3("Symptom Distribution by Timing of Test"),
               fluidRow(
                 column(4,
                        h4("Symptom Distribution in Total Group"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        plotlyOutput("piealldelay")
                        
                        
                 ),
                 column(2,
                        br()),
                 column(4,
                        h4("Symptom Distribution by Age")
            
                 )),
               
               fluidRow(
                 column(5,
                    plotlyOutput('bar')
                    ),
                  column(7,
                         br(),
                         br(),
                         br(),
                         selectInput('soi', "Select Symptom",
                                     choices = list("feeling ill","fever","shivering","chills sweats","muscle pain",
                                                    "headache","difficulty breathing","cough","sore throat","runny nose",
                                                    "nausea","diarrhea","loss of smell","loss of taste"),
                                     selected = 'loss of taste'
                         ),
                         br(),
                         br(), 
                         br(),
                         selectInput('ref', "Select reference group",
                                     choices = list("before","immediate","early","late"),
                                     selected = 'before'
                         ),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         #plotOutput ("fischer")
                        plotlyOutput('fischer')
                         ),
               )
      ))),

      
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
    #load('../CalwData.RData')
    load("~/JHU classes/Data Science in Public Health/Capstone Project/CalwData.RData")
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
    
    #### Tab C
    
    ####
    
    #### Tab D
    
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
        summarize (grp.mean = mean(delayed.testing, na.rm = TRUE))
      
      
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
      select(contains("symptoms."))%>%
      select_if(is.factor)%>%
      select(-symptoms.symptomatic) %>%
      gather(symptom, value ) %>%
      group_by(symptom,value) %>%
      dplyr::summarize(n=n())%>%
      filter(value=="yes")%>%
      mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
    
    
    
    output$pieall <- renderPlotly({
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
    
    output$piesub <- renderPlotly({
      age_sub=input$agegroup
      fig_sub = CalwData %>%
        filter(age==age_sub)%>%
        select(contains("symptoms."))%>%
        select_if(is.factor)%>%
        select(-symptoms.symptomatic) %>%
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
    
    output$bar_age <- renderPlotly({
      Symptom_distribution_age=CalwData %>%
        select(c(age,contains("symptoms.")))%>%
        select_if(is.factor)%>%
        select(-symptoms.symptomatic) %>%
        gather(symptom, value, -age ) %>%
        group_by(age,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      barplot=ggplot(Symptom_distribution_age, aes(x=age,y=n))+
        geom_col(aes(fill=symptom),position ="fill", color="black")+
        theme(axis.text.x = element_text(angle = 90))+
        scale_fill_simpsons()
      
      ggplotly(barplot)%>% layout(height = 800, width = 500)
      
    })
    #piealldelay
    
    output$bar <- renderPlotly({
      Symptom_distribution_delay=CalwData %>%
      select(c(testingdelay,contains("symptoms.")))%>%
      select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
      filter(!is.na(testingdelay))%>%
      mutate(delay_binned=case_when(
      testingdelay<0~"before",
      testingdelay%in%(0:2)~"immediate",
      testingdelay%in%(3:5)~"early",
      testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
      select(-testingdelay)%>%
      gather(symptom, value, -delay_binned ) %>%
      group_by(delay_binned,symptom,value) %>%
      dplyr::summarize(n=n())%>%
      filter(value=="yes")%>%
      mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
    
      barplot_delay=ggplot(Symptom_distribution_delay, aes(x=delay_binned,y=n))+
      geom_col(aes(fill=symptom),position ="fill", color="black")+
      scale_fill_simpsons()
    
      ggplotly(barplot_delay)%>% layout(height = 800, width = 500)
    })
    
    output$fischer <- renderPlotly({
      #renderPlot({
      symptom_of_interes=input$soi
      bin1=input$ref
      
      fischer_test_function=function(x, bin1){
        plot_dataframe=NULL
        data=x%>%group_by(delay_binned)%>%mutate(total=sum(n))%>%ungroup
        for(b in 1:nlevels(data$delay_binned)){
          bin_tmp=levels(data$delay_binned)[b]
          if(bin_tmp!=bin1){
            s1=data%>%filter(delay_binned==bin1)%>%filter(symptom==symptom_of_interes)%>%select(n)%>%as.numeric()
            s2=data%>%filter(delay_binned==bin_tmp)%>%filter(symptom==symptom_of_interes)%>%select(n)%>%as.numeric()
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
        select(c(testingdelay,contains("symptoms.")))%>%
        select(-c(symptoms.temperature,symptoms.symptomatic)) %>%
        filter(!is.na(testingdelay))%>%
        mutate(delay_binned=case_when(
          testingdelay<0~"before",
          testingdelay%in%(0:2)~"immediate",
          testingdelay%in%(3:5)~"early",
          testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
        select(-testingdelay)%>%
        gather(symptom, value, -delay_binned ) %>%
        group_by(delay_binned,symptom,value) %>%
        dplyr::summarize(n=n())%>%
        filter(value=="yes")%>%
        mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
      
      colordata=as.data.frame(cbind(symptom=levels(Symptom_distribution_delay$symptom), 
                                    level=1:nlevels(Symptom_distribution_delay$symptom),
                                    color=pal_simpsons("springfield")(nlevels(Symptom_distribution_delay$symptom))))
      
      farbe=colordata%>%filter(symptom==symptom_of_interes)%>%select(color)%>%as.character()
      
      barplot_delay=ggplot(Symptom_distribution_delay%>%group_by(delay_binned)%>%mutate(total=sum(n))%>%filter(symptom==symptom_of_interes), aes(x=delay_binned,y=100*n/total))+
        geom_col(aes(fill=delay_binned))+
        geom_text(data=fischer_test_function(Symptom_distribution_delay,bin1),
                  aes(x=x_value, y=as.numeric(y_value), label=label_value,vjust = 1.5))+
        ylim(NA, fischer_test_function(Symptom_distribution_delay,bin1)%>%
               filter(y_value==max(as.numeric(y_value)))%>%select(y_value)%>%head(1)%>%as.numeric() +0.5)+
        theme(legend.position = "none")+ 
        scale_fill_manual(values = rep(farbe,4))
     # scale_fill_manual(values=#"#FFFFFF"
      #                    farbe  
                          #values = c(symptom='#FFFFFF')
       # )#+
        #guides(fill='#FFFFFF')
    
      
      barplot_delay
      
      ggplotly(barplot_delay)%>% layout(height = 400, width = 600)
      
    })
    
    
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

