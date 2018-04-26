#install.packages('shiny')
#install.packages('ggmap')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('shinythemes')

#loading libraries
library("ggplot2")
library("ggmap")
library("dplyr")
library('shiny')
library('shinythemes')

#loading iit and uc crime data
load("iit.rda")
load("uc.rda")

#loading campus maps
load("maps.rda")


#loading maps
# miesMap <- qmap(location = "Illinois Institute of Technology", zoom=16)
# miesAreaMap <- qmap(location = "Illinois Institute of Technology", zoom=15)
# kentMap <- qmap(location = "Chicago-Kent College of Law at Illinois Institute of Technology", zoom=16)
# kentAreaMap <- qmap(location = "Chicago-Kent College of Law at Illinois Institute of Technology", zoom=15)
# ucMap <- qmap(location = "The University of Chicago", zoom=16) 
# ucAreaMap <- qmap(location = "The University of Chicago", zoom=15) 
# save(miesMap, miesAreaMap, kentMap, kentAreaMap, ucMap, ucAreaMap, file="maps.rda")

#defining variables
min_date <- min(iit$OCCURED)
max_date <- max(iit$OCCURED)
incidentType <- sort(unique(iit$INCIDENT_TYPE2)) #set of crime types is the same for all areas

#defining lat and lon of campuses
campCoord <- data.frame(Point = c('IIT Mies', 'IIT Kent', 'UChicago'), lon = c(-87.62723, -87.64221, -87.600932), lat = c(41.83140, 41.87914, 41.789576))

# Defining UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      wellPanel(
      
      h4("Crime hotspots"),       
      
      #Campus input
      selectInput(inputId = "campus", 
                  label = "Select campus:",
                  choices = c("IIT Mies", "IIT Kent", "UChicago"),
                  selected = "IIT Mies"),
        
      # # # Label
      # # HTML(paste0("Crime incidents between the following dates will be plotted. 
      # #             Pick dates between ", min_date, " and ", max_date, ".")),
      # 
      # # Break for visual separation
      # br(), br(),
      # 
      # #Source input
      # selectInput(inputId = "datasource", 
      #             label = "Select source:",
      #             choices = c("IIT-CAMPUS", "IIT-AREA", "UC-CAMPUS", "UC-AREA"),
      #             selected = "IIT-CAMPUS"),
      
      
      checkboxInput(inputId = "cpd", 
                    label = "Include CPD data:", 
                    value = TRUE),
      
      # Break for visual separation
      br(), br(),

      # Date input
      dateRangeInput(inputId = "date",
                     label = "Select dates:",
                     start = "2015-01-01", end = "2018-01-01",
                     startview = "day",
                     min = min_date, max = max_date),
      
      # Break for visual separation
      br(), br(),
      
      selectInput(inputId = "incidentType",
                     label = "Select incident type:",
                     choices = incidentType,
                     selected = "PERSON CRIME", 
                     multiple = TRUE,
                     selectize = FALSE
                     ),
      
      
      # Break for visual separation
      br(), br(),
      
      sliderInput(inputId="zoom", 
                  label= "Zoom map:", 
                  min = 0, max=1,
                  value=0, step=1),
      
      # Break for visual separation
      br(), br()
      
      )#end of wellpanel
      
    ), #end of sidebarPanel
    
    # Outputs
    mainPanel(
      h3("Hotspots on map"),    
      plotOutput(outputId = "iitDensityPlot", height = 600)
    )
  )
)

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
    
    # selecting area
    if (input$campus == "IIT Mies" | input$campus == "IIT Kent"){
      
      #selecting dataset
      area <- iit
      
      #getting map object
      if (input$campus == "IIT Mies" & input$zoom == 1) {
        campusMap <- miesMap
        #pointInd <- 1
      }
      else if (input$campus == "IIT Mies" & input$zoom == 0){
        campusMap <- miesAreaMap
        #pointInd <- 1
      }
      else if(input$campus == "IIT Kent" & input$zoom == 1){
        campusMap <- kentMap
        #pointInd <- 2
      }
      else if(input$campus == "IIT Kent" & input$zoom == 0){
        campusMap <- kentAreaMap
        #pointInd <- 2
      }#end of getting map object
    
    }#end of if
    
    else if (input$campus == "UChicago"){
      
      #selecting dataset
      area <- uc
      
      #getting map object
      if (input$zoom == 1) {
        campusMap <- ucMap
        #pointInd <- 3
      } else if (input$zoom == 0) {
        campusMap <- ucAreaMap
        #pointInd <- 3
      }
      
    }#end of else if
    
      
    #plot based on selected parameters
    #filter dataset by incident type, dates and data source
    if (input$cpd == FALSE) {
        crimeDatesSel <- area %>% 
          filter(TYPE_OF_DATA=="IIT-CAMPUS" | TYPE_OF_DATA=="UC-CAMPUS") %>%
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
  
}#end of server function

# Create the Shiny app object
shinyApp(ui = ui, server = server)