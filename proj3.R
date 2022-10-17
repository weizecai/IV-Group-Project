library("shiny")
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(mapsapi)

####################
# Prepare the data #
####################
house_price <- read.csv('House_Prices_by_Small_Area_-_Sale_Year.csv')
small_area <- read.csv('Census_of_Land_Use_and_Employment__CLUE__Suburb.csv')
colnames(small_area)[2] <- 'Small_Area'
house_price <- house_price[complete.cases(house_price$Small_Area),]
house_price <- merge(house_price, small_area, by='Small_Area', all.x=TRUE)

##################
# USER INTERFACE #
##################
# Tab 1: Introduction
ui <- navbarPage("House Price in Melbourne", id="nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              tags$head(
                                # custome CSS to let the map take up the entire page
                                includeCSS("styles.css")
                              ),
                              leafletOutput("map_house", width="100%", height="100%"),
                              # the panel includes the map, control panels, and plots
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 700, height = "auto",
                                            
                                            h3('Choose a type'),
                                            selectInput(
                                              'type',
                                              label = 'Type',
                                              choices = c('Transaction Count'='transaction',
                                                          'Median Price'='median'
                                              ),
                                              selected = 'transaction'
                                            ),
                                            
                                            h3("House Type"),
                                            radioButtons(
                                              'house_type',
                                              label = 'House Type',
                                              choices = c("All", 
                                                          "House/Townhouse", 
                                                          "Residential Apartment"
                                                          )),
                                            selected = "All",
                                            
                                            h3('Year'),
                                            radioButtons(
                                              'year',
                                              label = 'Year',
                                              choices = c("2006", 
                                                          "2007", 
                                                          "2008",
                                                          "2009", 
                                                          "2010", 
                                                          "2011",
                                                          "2012", 
                                                          "2013", 
                                                          "2014",
                                                          "2015",
                                                          "2016"
                                              )),
                                            selected = "2016"
                              ) # absolutePanel ends
                          ) # div ends
                 ),# tabPanel ends
                 tabPanel("Search Route",
                          div(class="outer",
                              
                              tags$head(
                                # custome CSS to let the map take up the entire page
                                includeCSS("styles.css")
                              ),
                              leafletOutput("search_route", width="100%", height="100%"),
                              # the panel includes the map, control panels, and plots
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 700, height = "auto",
                                            
                                            
                                            h2("Origin"),
                                            textInput('origin', 
                                                      'Origin', 
                                                      value = "", 
                                                      width = NULL, 
                                                      placeholder = 'Where you are'),
                                            h2("Destination"),
                                            textInput('dest', 
                                                      'Destination', 
                                                      value = "", 
                                                      width = NULL, 
                                                      placeholder = 'Where you want to go'),
                                            actionButton("search", "Let's Go")
                                            
                                          
                              ) # absolutePanel ends
                          ) # div ends
                 )
) # navbarPage ends

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  v <- reactiveValues(data = NULL)
  
  filterForHouseMap <- reactive({
    house_price <- st_as_sf(house_price, wkt = "the_geom")
    house_price <- house_price[house_price$Sale_Year == input$year, ]
    
    if (input$type == 'transaction') {
      data <- house_price[,c(1,2,3,5)]
      title <- "Transaction Count"
    } else {
      data <- house_price[,c(1,2,3,4)]
      title <- "Median Price"
    }
    if(input$house_type == 'House/Townhouse'){
      data <- data[data$Type=="House/Townhouse", ]
    }
    else if(input$house_type == 'Residential Apartment'){
      data <- data[data$Type == "Residential Apartment",]
    }
    else{
      data <- data
    }
    colnames(data)[4] <- 'obj'
    
    data$popup <- paste0('<font size="+2"><b>', data$Small_Area, '</b></font><br>',
                         '<b>', data$Sale_Year, '</b><br>',
                         '<font size="+1">', title, ": ", data$obj, '</font><br>'
                         )
    data[complete.cases(data$obj),]
  })
  

  
  output$map_house <- renderLeaflet({
    
    data <- filterForHouseMap()
    
    bins <- quantile(data$obj)
    pal <- colorBin("YlOrRd", domain = data$obj, bins = bins)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB)%>%
      addPolygons(
        data = data,
        fillColor = ~pal(obj),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.4,
        popup = ~popup,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE))
  }) # renderLeaflet ends
  
  observeEvent(input$map_house_shape_click, {
    click <- input$map_house_shape_click
    print(click)

  })
  
  filterForSearchRoute <- reactive({

    
    
  })
  searchRoute <- eventReactive(input$search, {
    if(length(input$origin) == 0){
      origin <- "box hill melbourne"
    }
    else{
      origin <- paste(input$origin, 'melboune')
    }
    if(length(input$dest) == 0){
      dest <- "melbourne central"
    }
    else{
      dest <- paste(input$dest, 'melbourne')
    }
    
    key=""
    doc = mp_directions(
      origin = origin,
      destination = dest,
      alternatives = TRUE,
      mode= c("transit"),
      transit_mode=c("bus", "subway", "train", "tram"),
      key = key,
      quiet = TRUE
    )
    
    mp_get_segments(doc)[1:3,]
  })
  
  
  output$search_route <- renderLeaflet({
    print(input$origin)
    print(input$dest)
    r <- searchRoute()
    
    pal = colorFactor(
      palette = sample(colors(), length(unique(r$segment_id))),
      domain = r$alternative_id
    )
    
    leaflet(r) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolylines(opacity = 1, weight = 7, color = ~pal(segment_id), popup = ~instructions)
  })
  
  
} ## server ends


#############
# RUN SHINY #
#############
shinyApp(ui, server)
