#install.packages('shiny')
#install.packages('ggmap')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('shinythemes')
#install.packages('rsconnect')


#loading libraries
library("ggplot2")
library("ggmap")
library("dplyr")
library('shiny')
library('shinythemes')
library('geohash')
library('lubridate')
library('rsconnect')

#loading iit crime data for IIT Kent and surrounding area
load("iitK.rda")
iitK <- iit #data for iit kent area
rm(iit)

#loading data for IIT MIES area
load("IIT_new.rda")

iit<-iit3
rm(iit3)

#loading campus maps
load("maps.rda")

#load model
load('LogModel.rda')

#NOTE: uncomment to re-upload maps
# #loading maps
# miesMap <- qmap(location = "Illinois Institute of Technology", zoom=16)
# miesAreaMap <- qmap(location = "Illinois Institute of Technology", zoom=15)
# kentMap <- qmap(location = "Chicago-Kent College of Law at Illinois Institute of Technology", zoom=16)
# kentAreaMap <- qmap(location = "Chicago-Kent College of Law at Illinois Institute of Technology", zoom=15)
# save(miesMap, miesAreaMap, kentMap, kentAreaMap, file="maps.rda")

#defining variables
# min_date <- min(iit$OCCURED)
# max_date <- max(iit$OCCURED)
incidentType <- sort(unique(iit$INCIDENT_TYPE2)) #set of crime types is the same for all areas


#defining lat and lon of campuses
#campCoord <- data.frame(Point = c('IIT Mies', 'IIT Kent'), lon = c(-87.62723, -87.64221), lat = c(41.83140, 41.87914))

# Defining UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(
                    
                    wellPanel(
                      
                      h4("Predict incidents"),
                      
                      radioButtons(input='predict',
                                   label='Make prediction for:',
                                   choices=c("1 day", '3 days', "5 days"),
                                   selected='1 day',
                                   inline=F),
                      
                      # Break for visual separation
                      br(),
                      
                      # sliderInput(inputId='time', label='Time', min=0, max=24,
                      #             value=12, step=3, round, 
                      #             ticks=T, animate=T, width),
                      
                      selectInput(inputId='time', label='Time', choices=c('All','00-03','03-06', '06-09','09-12','12-15','15-18','18-21','21-00'),
                                  selected='All', multiple=F, selectize=F),
                      
                      # Break for visual separation
                      br(),
                      
                      
                      h4("Crime hotspots (hist)"),
                      
                      #Campus input
                      selectInput(inputId = "campus",
                                  label = "Select campus:",
                                  choices = c("IIT Mies", "IIT Kent"),
                                  selected = "IIT Mies"),
                      
                      
                      checkboxInput(inputId = "cpd",
                                    label = "Include CPD data:",
                                    value = TRUE),
                      
                      
                      # Date input
                      dateRangeInput(inputId = "date",
                                     label = "Select dates:",
                                     start = "2015-01-01", end = "2018-01-01",
                                     startview = "year",
                                     min = "2015-01-01", max = "2018-01-01"),
                      
                      
                      selectInput(inputId = "incidentType",
                                  label = "Select incident type:",
                                  choices = incidentType,
                                  selected = "SERIOUS INCIDENTS",
                                  multiple = TRUE,
                                  selectize = FALSE
                      ),
                      
                      
                      
                      
                      sliderInput(inputId="zoom",
                                  label= "Zoom map:",
                                  min = 0, max=1,
                                  value=0, step=1),
                      
                      # Break for visual separation
                      br()
                      
                    )#end of wellpanel
                    
                  ), #end of sidebarPanel
                  
                  # Outputs
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                tabPanel("Predict incidents", plotOutput(outputId = "predictionPlot", height = 600)),
                                tabPanel("Historical data", h3("Hotspots on map"),
                                         plotOutput(outputId = "iitDensityPlot", height = 600)))
                    
                  )
                )#end of sidebar layout
)#end of UI

# Defining server function required
server <- function(input, output) {
  
  # Create density plot IIT
  output$iitDensityPlot <- renderPlot({
    
    #no input error handling
    req(input$date)
    req(input$campus)
    req(input$incidentType)
    #req(input$datasource)
    req(input$zoom)
    
    # selecting area and map for MIES
    if (input$campus == "IIT Mies") {
      
      #selecting dataset
      area <- iit
      
      if (input$zoom == 1) {
        campusMap <- miesMap
      } else if(input$zoom == 0) {
        campusMap <- miesAreaMap
      }
    }#end of if
    
    # selecting area and map for KENT
    if (input$campus == "IIT Kent") {
      
      #selecting dataset
      area <- iitK
      
      if (input$zoom == 1) {
        campusMap <- kentMap
      } else if(input$zoom == 0) {
        campusMap <- kentAreaMap
      }
    }#end of if
    
    
    
    #plot based on selected parameters
    #filter dataset by incident type, dates and data source
    if (input$cpd == FALSE) {
      crimeDatesSel <- area %>%
        filter(TYPE_OF_DATA=="IIT-CAMPUS") %>%
        filter(INCIDENT_TYPE2 %in% input$incidentType) %>%
        filter(OCCURED >= as.POSIXct(input$date[1]) & OCCURED <= as.POSIXct(input$date[2]))
      
      #density of crimes for the selected area
      campusMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, data = crimeDatesSel, geom = "polygon") + scale_alpha(guide = 'none')
      
      
    } else {
      crimeDatesSel <- area %>%
        filter(INCIDENT_TYPE2 %in% input$incidentType) %>%
        filter(OCCURED >= as.POSIXct(input$date[1]) & OCCURED <= as.POSIXct(input$date[2]))
      
      #density of crimes for the selected area
      campusMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, data = crimeDatesSel, geom = "polygon") + scale_alpha(guide = 'none')
      
    }
    
  })#end of renderPLot
  
  
  # Create density plot Prediction
  output$predictionPlot <- renderPlot({
    
    #no input error handling
    #req(input$date)
    
    #selecting dataset
    area <- iit
    #selecting map
    campusMap <- miesAreaMap
    
    #---------------------------------    
    
    extract_month <- function(date) {
      month_val <- months(date)
      #print(month_val)
      return(substring(month_val,1,3))
    }
    
    
    extract_days <- function(date) {
      days_val <- weekdays(date)
      #print(month_val)
      return(substring(days_val,1,3))
    }
    
    
    #function that creates the crime prediction dataset (Need to add lats and lons to the set)
    crime_prediction_model <- function(date_range) {
      
      sector_datetime <- c()
      sector_range <- c()
      sector_timebucket <- c()
      sector_month <- c()
      sector_day <- c()
      sector_typeofarea <- c()
      sector_geohash <- c()
      
      
      date_value <- Sys.Date() + date_range
      
      #print("yes")
      month_val <- extract_month(date_value)
      day_val <- extract_days(date_value)
      sector_geohash <- c(sector_geohash, unlist(lapply(levels(iit$GeoHash), function(x) rep(x,8))))
      sector_month <- c(sector_month, rep(month_val, length(sector_geohash)) )
      sector_day<- c(sector_day, rep(day_val, length(sector_geohash)) )
      #sector_range <- c(sector_range,unlist(lapply(1:8, function(x) rep(x,8))))
      sector_timebucket <- c(sector_timebucket,rep(levels(iit$TIME_BUCKET), 132))
      #sector_typeofarea <- c(sector_typeofarea, rep(z, 64))
      
      start <- as.POSIXct(paste(date_value, "01:30:00", sep = " "))
      #print(start)
      interval <- 60
      end <- start + as.difftime(0.95, units="days")
      sector_datetime<- c(sector_datetime, rep(seq(from=start, by=interval*180, to=end),132))
      #print(sector_datetime)
      #print(length(sector_datetime))
      
      test_dataset <- data.frame(as.numeric(sector_datetime),as.factor(sector_timebucket),as.factor(sector_month), as.factor(sector_day), as.factor(sector_geohash))
      test_dataset[,3] <- ordered(test_dataset[,3])
      test_dataset[,4] <- ordered(test_dataset[,4])
      
      test_dataset[,'LATITUDE'] <- apply(test_dataset, 1, function(x) return(as.numeric(gh_decode(x[5])$lat)))
      test_dataset[,'LONGITUDE'] <- apply(test_dataset,1,  function(x) return(as.numeric(gh_decode(x[5])$lng)))
      colnames(test_dataset)<-c('OCCURED', 'TIME_BUCKET', 'MONTH', 'DAY', 'GeoHash', 'LATITUDE', 'LONGITUDE')
      
      return(test_dataset)  
    }#end of predict data
    
    if (input$predict == "1 day"){
      future.data<-crime_prediction_model(1)
      xVars<-c('TIME_BUCKET', 'MONTH','GEOHASH', 'DAY')
      
      future.data<-future.data[!as.character(future.data$GeoHash)=='dp3tvz5',]
      future.data<-future.data[!as.character(future.data$GeoHash)=='dp3wjbv',]
      fitted.results <- predict(finalModel
                                ,newdata = future.data
                                # Specifying response means we want the probabilities
                                ,type='response')
      
      fitted.results<-abs(1-fitted.results)
      future.data$seriousInc <- ifelse(fitted.results > 0.70,1,0)
      future.data <- future.data[!(future.data$seriousInc == 0),]
      #density of crimes for the selected area
      if (input$time!='All'){
        future.data <- future.data[future.data$TIME_BUCKET==input$time,]
      }
      
      campusMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), 
                                 bins = 4, data = future.data, geom = "polygon") + scale_alpha(guide = 'none') + 
        geom_vline(xintercept=seq(-87.6330, -87.6170, by=0.008)) +
        geom_hline(yintercept=seq(41.8276, 41.8421, by=0.003625))      
      
    } else if (input$predict == "3 days"){
      future.data<-crime_prediction_model(3)
      xVars<-c('TIME_BUCKET', 'MONTH','GEOHASH', 'DAY')
      
      future.data<-future.data[!as.character(future.data$GeoHash)=='dp3tvz5',]
      future.data<-future.data[!as.character(future.data$GeoHash)=='dp3wjbv',]
      fitted.results <- predict(finalModel
                                ,newdata = future.data
                                # Specifying response means we want the probabilities
                                ,type='response')
      
      fitted.results<-abs(1-fitted.results)
      future.data$seriousInc <- ifelse(fitted.results > 0.70,1,0)
      future.data <- future.data[!(future.data$seriousInc == 0),]
      #density of crimes for the selected area
      if (input$time!='All'){
        future.data <- future.data[future.data$TIME_BUCKET==input$time,]
      }
      campusMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 4, data = future.data, geom = "polygon") + 
        scale_alpha(guide = 'none') + 
        geom_vline(xintercept=seq(-87.6330, -87.6170, by=0.008)) +
        geom_hline(yintercept=seq(41.8276, 41.8421, by=0.003625))   
      
    } else if (input$predict == "5 days"){
      future.data<-crime_prediction_model(5)
      xVars<-c('TIME_BUCKET', 'MONTH','GEOHASH', 'DAY')
      
      future.data<-future.data[!as.character(future.data$GeoHash)=='dp3tvz5',]
      future.data<-future.data[!as.character(future.data$GeoHash)=='dp3wjbv',]
      fitted.results <- predict(finalModel
                                ,newdata = future.data
                                # Specifying response means we want the probabilities
                                ,type='response')
      
      fitted.results<-abs(1-fitted.results)
      future.data$seriousInc <- ifelse(fitted.results > 0.70,1,0)
      future.data <- future.data[!(future.data$seriousInc == 0),]
      #density of crimes for the selected area
      if (input$time!='All'){
        future.data <- future.data[future.data$TIME_BUCKET==input$time,]
      }
      campusMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 4, data = future.data, geom = "polygon") + 
        scale_alpha(guide = 'none') +
        geom_vline(xintercept=seq(-87.6330, -87.6170, by=0.008)) +
        geom_hline(yintercept=seq(41.8276, 41.8421, by=0.003625))   
      
    }
    
    #------------------------------------------
    # miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), 
    #                            bins = 3, data = future_test, geom = "polygon") + scale_alpha(guide = 'none') + 
    #   geom_vline(xintercept=seq(-87.6330, -87.6170, by=0.008)) +
    #   geom_hline(yintercept=seq(41.8276, 41.8421, by=0.003625))   
    
    #plot based on selected parameters
    #filter dataset
    # crimeDatesSel <- future.data %>%
    #   filter(OCCURED >= as.POSIXct(input$date[1]) & OCCURED <= as.POSIXct(input$date[2]))
    
    
    
  })#end of renderPLot
  
  
  
}#end of server function

# Create the Shiny app object
shinyApp(ui = ui, server = server)
