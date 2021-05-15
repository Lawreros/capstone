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
library(MASS) 

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
               
               #h3("Symptom Distribution by Age"),
               fluidRow(
                 column(4,
                        h4("Symptom Distribution in Total Group"),
                        plotlyOutput("pieall"),
                       
                        
                 )),
               fluidRow(
               tabsetPanel(
                 tabPanel("Symptom Distribution by Age",
                    fluidRow(
                 column(4,
                        h3("Symptom Distribution by Age"),
                        
                        selectInput('agegroup', "Select Age Group",
                                    choices = list('0-4','5-9','10-14','15-19','20-24','25-29',
                                                   '30-34','35-39','40-44','45-49','50-54',
                                                   '55-59','60-64','65-69','70-74','75-79',
                                                   '80-84','85-89','90-94','95-99','100-104'),
                                    selected = '0-4'
                        ),
                        br(),
                        plotlyOutput('piesub'),
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
                        plotlyOutput("bar_age")
                 )),
                 fluidRow(
                   column(1,),
                   column(4,
                   h3("Age binned in 20 years interval"))),
                 fluidRow(
                 column(5,
                        plotlyOutput("bar_age_grouped")
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
                        selectInput('soi_age', "Select Symptom",
                                    choices = list("feeling ill","fever","shivering","chills sweats","muscle pain",
                                                   "headache","difficulty breathing","cough","sore throat","runny nose",
                                                   "nausea","diarrhea","loss of smell","loss of taste"),
                                    selected = 'loss of taste'
                        ),
                        br(),
                        selectInput('ref_age', "Select reference group",
                                    choices = list("0-19","20-39","40-59","60-79","80+"),
                                    selected = '0-19'),
                      
                        br(),
                        br(),
                        plotlyOutput('bar_age_grouped_fischer')
                        
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
                    plotlyOutput('bar')
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
                         selectInput('soi', "Select Symptom",
                                     choices = list("feeling ill","fever","shivering","chills sweats","muscle pain",
                                                    "headache","difficulty breathing","cough","sore throat","runny nose",
                                                    "nausea","diarrhea","loss of smell","loss of taste"),
                                     selected = 'loss of taste'
                         ),
                         br(),
                         selectInput('ref', "Select reference group",
                                     choices = list("before","immediate","early","late"),
                                     selected = 'before'
                         ),
                         
                         
                         br(),
                         br(),
                        plotlyOutput('fischer')
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
                 p("..."),
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
      
      
      tabPanel("H - predition testing", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel(
                   plotOutput("predict")
                 )
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
    #load(url("https://github.com/Lawreros/capstone/blob/28fa412bc42ac273909cfa54354eaba101e61db4/CalwData.RData"))
    #source_data("https://github.com/Lawreros/capstone/blob/28fa412bc42ac273909cfa54354eaba101e61db4/CalwData.RData")
  
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
      dplyr::select(contains("symptoms."))%>%
      dplyr::select_if(is.factor)%>%
      dplyr::select(-symptoms.symptomatic) %>%
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
    
    output$bar_age <- renderPlotly({
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
    
    
    output$bar_age_grouped <- renderPlotly({
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
    
    output$bar_age_grouped_fischer= renderPlotly({
      
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
      
      
      symptom_of_interes=input$soi_age
      bin1=input$ref_age
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
    
    
    #piealldelay
    
    output$bar <- renderPlotly({
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
    
    
    output$predict <- renderPlot({
      
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
    #summary(modelord)
    
    pred_data <- as_tibble(predict(modelord, type="probs")) 
    #pred_data
    
    Symptom_distribution_delay_expand <- bind_cols(Symptom_distribution_delay_expand, pred_data)
    
    # Find mean probs
    predict=Symptom_distribution_delay_expand %>% 
      group_by(symptom) %>%
      dplyr::summarize(
        before = mean(before), 
        imm = mean(immediate), 
        ear = mean(`early`),
        late = mean(`late`))
    
    predict=cbind(predict, data=rep("predicted",14), n=observed$n)
    observed=cbind(observed, data=rep("observed",14))
    
    comparison= rbind(predict,observed)
    barplot_comparison_before=ggplot(comparison, aes(x=symptom, y=before,fill=data))+
      geom_col( position=position_dodge())
    
    barplot_comparison_before
    
    #barplot_comparison_immediate= ggplot(comparison, aes(x=symptom, y=imm,fill=data))+
      #geom_col( position=position_dodge())
  
    #barplot_comparison_immediate )
    })
    
    ####
    
    #### Tab I
    
    ####
    
    #### Tab J
    
    ####
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)

