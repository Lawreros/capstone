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
               sidebarLayout(
                 sidebarPanel(
                   #radioButtons('agegroup', label=h3("Select Age Group Feature"),
                    #            choices = list('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54',
                     #                          '55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100-104'),
                      #          selected = '0-4')
                 ),
                 mainPanel(
                   
                   
                   #fluidRow(
                    # column(6,
                   #plotlyOutput("pieall")),
                   #column(6,
                   #plotlyOutput('piesub')
                   #)),
                   #plotlyOutput('bar'),
                   
                 )
               )
      ),
      
      tabPanel("G 2- Symptoms", fluid = TRUE,
               
               fluidRow(
                 h3(textOutput("Symptom Distribution by Age")),
                 column(4,
                        plotlyOutput("pieall")
                 ),
                 column(2,
                        radioButtons('agegroup', label=h3("Select Age Group Feature"),
                                     choices = list('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54',
                                                    '55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100-104'),
                                     selected = '0-4',
                                     width=200
                                     )
                 ),
                 column(4,
                        plotlyOutput('piesub')
                 )),
               fluidRow(
                 plotlyOutput('bar'),
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
    #load('../CalwData.RData')
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
    
    #### Tab E
    
    ####
    
    #### Tab F
    
    ####
    
    #### Tab G
    fix_symptoms=function(x){
      x%<>%factor(.,levels=c("feelingIll","fever","shivering","chillsSweats","musclePain","headache","difficultyBreathing","cough","soreThroat","runnyNose","nausea","diarrhea","lossOfSmell","lossOfTaste"),
                  labels=c("feeling ill","fever","shivering","chills sweats","muscle pain","headache","difficulty breathing","cough","sore throat","runny nose","nausea","diarrhea","loss of smell","loss of taste"))
    }
    
    Symptoms= CalwData %>%
      select(contains("symptoms."))%>%
      select_if(is.factor)%>%
      select(-symptoms.symptomatic) %>%
      gather(symptom, value ) %>%
      group_by(symptom,value) %>%
      summarize(n=n())%>%
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
        summarize(n=n())%>%
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
      fig_sub
      
          })
    output$bar <- renderPlotly({
      Symptom_distribution_delay=CalwData %>%
      select(c(testingdelay,contains("symptoms.")))%>%
      select(-c(symptoms.fever,symptoms.symptomatic)) %>%
      filter(!is.na(testingdelay))%>%
      mutate(delay_binned=case_when(
      testingdelay<0~"before",
      testingdelay%in%(0:2)~"immediate",
      testingdelay%in%(3:5)~"early",
      testingdelay>5~"late")%>%factor(levels = c("before","immediate","early","late")))%>%
      select(-testingdelay)%>%
      gather(symptom, value, -delay_binned ) %>%
      group_by(delay_binned,symptom,value) %>%
      summarize(n=n())%>%
      filter(value=="yes")%>%
      mutate(symptom=str_replace(symptom,"symptoms.","")%>%fix_symptoms)
    
      barplot_delay=ggplot(Symptom_distribution_delay, aes(x=delay_binned,y=n))+
      geom_col(aes(fill=symptom),position ="fill", color="black")+
      scale_fill_simpsons()
    
      ggplotly(barplot_delay)
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

