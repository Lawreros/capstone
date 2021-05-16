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
library(tidyr)
library(leaflet)
library(geojsonR)

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
      tabPanel("A", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("map_date",
                               "Dates:",
                               min = as.Date("2020-03-04","%Y-%m-%d"),
                               max = as.Date("2021-05-11","%Y-%m-%d"),
                               value=as.Date("2020-03-20"),
                               timeFormat="%Y-%m-%d"),
                   radioButtons("radio", label = h3("Radio buttons"),
                                choices = list("Choice 1" = 'case', "Choice 2" = 'death'), 
                                selected = 'case')
                 ),
                 mainPanel(
                   leafletOutput('covid_map'),
                   textOutput('testthis'),
                   plotOutput('countytime')
                 )
               )
      ),
      
      
      tabPanel("B", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   
                 ),
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
    load('CalwData.RData')
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
    
    
    #### Tab A
    
    pal <- colorNumeric("viridis", NULL)
    nycounties <- rgdal::readOGR("DE-counties.geojson")
    
    c_dat <- read.csv('cases-rki-by-ags.csv')
    c_dat <- c_dat %>% rename(sum = sum_cases)
    c_dat$category <- 'case'
    d_dat <- read.csv('deaths-rki-by-ags.csv')
    d_dat$category <- 'death'
    d_dat <- d_dat %>% rename(sum = sum_deaths)
    
    dat <- rbind(c_dat,d_dat)
    dat <- dat[,c(1:325,414,338:413,416)]
    
    dat$time_iso8601 <- as.Date(dat$time_iso8601,"%Y-%m-%dT")
    new_name <- paste0(make.names(nycounties@data$GEN), "_", make.names(nycounties@data$BEZ), "")
    new_name <- append('time', c(new_name[1:400],'germany','category'))
    colnames(dat) <- new_name
    
    
    output$covid_map <- renderLeaflet({leaflet(nycounties)%>%
                          addTiles()%>%
                          addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                              fillColor = pal(unlist(with(dat,dat[(dat$time == input$map_date & dat$category == input$radio),-c(1,402,403)]),use.names = FALSE)),
                              # Highlight neighbourhoods upon mouseover
                              highlight = highlightOptions(
                              weight = 30,
                              fillOpacity = 0.9,
                              color = "red",
                              opacity = 0.1,
                              bringToFront = TRUE),
                              #sendToBack = TRUE), 
                              
                              label = ~paste0(GEN, ": ", formatC(unlist(with(dat,dat[(dat$time == input$map_date & dat$category == input$radio),-c(1,402,403)])), big.mark = ",")),layerId = seq.int(2,402)) %>%
                          addLegend(pal = pal, title='new cases per 100k in last 7 days',values = ~log10(unlist(with(dat,dat[(dat$time == input$map_date & dat$category == input$radio),-c(1,402,403)]),use.names = FALSE)+1), opacity = 1.0,
                              labFormat = labelFormat(transform = function(x) round(10^x)))
    }) #, layerId = nycounties@data$GEN
    
    observeEvent(input$covid_map_shape_click, { # update the location selectInput on map clicks
      p <- input$covid_map_shape_click
      output$testthis <- renderText(p$id)
      
      output$countytime <- renderPlot({
         ggplot(dat, aes_string("time", new_name[p$id], colour = "category")) + 
          geom_point()+
          # scale_y_continuous(
          #   trans = "log10",
          #   breaks = 1:10
          # )+
          geom_vline(xintercept = input$map_date, linetype="dotted")
      })
      
    })
    
    ####
    
    
    #### Symptom composition based on variant
    # Make a decision tree of sorts, where each node has the percentage of individuals with a given symptom combination
    # http://www.milbo.org/rpart-plot/prp.pdf
    
    
    
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

