library("shiny")
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(shinyTime)
library(mapsapi)
library(DT)

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
                                            selectInput(
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
                                              ),
                                              selected = '2016'
                                            ),
                                            conditionalPanel(
                                              condition = "!is.na(v.small_area)",
                                              plotlyOutput("line_chart")
                                            ),
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
                                            draggable = FALSE, top = 60, left = 50, right ="auto", bottom = "auto",
                                            width = 1000, height = 152,
                                            
                                            
                                            
                                            fluidRow(column(6,textInput('origin',
                                                                        'Origin',
                                                                        value = "",
                                                                        width = NULL,
                                                                        placeholder = 'Where you are')),
                                                     
                                                     column(6,textInput('dest',
                                                                        'Destination',
                                                                        value = "",
                                                                        width = NULL,
                                                                        placeholder = 'Where you want to go')),
                                                     column(4,radioButtons(
                                                       'traffic',
                                                       label = 'traffic',
                                                       choices = c("bus",
                                                                   "train",
                                                                   "tram",
                                                                   "drive",
                                                                   "all public transit"
                                                                   
                                                       ),inline=T)),
                                                     column(2,timeInput("time_input", "Enter arrive time",
                                                                        value = Sys.time(), minute.steps = 1),
                                                     ),br(),br(), br(),br(),br(),
                                                     column(4,
                                                            actionButton("reset_time", "reset time")),
                                                     column(2,
                                                            actionButton("search", "Let's Go")),)
                                            
                                            
                              ),# absolutePanel ends
                              conditionalPanel( condition = "input.search",
                                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                              draggable = FALSE,  top = 60, left = "auto", right =50, bottom = "auto",
                                                              width = 600, height = 700,
                                                              
                                                              
                                                              column(width = 6,
                                                                     h4("Travel Detail"),
                                                                     dataTableOutput("detail",width = "10%"),
                                                                     actionButton("alternatives", "Alternative way"),
                                                                     textOutput("text")),
                                                ))# absolutePanel ends
                          ) # div ends
                 )
) # navbarPage ends

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  v <- reactiveValues(small_area = "Melbourne (CBD)", instructions = "")
  
  filter <- function(df){
    
    if (input$type == 'transaction') {
      data <- df[,c(1,2,3,5)]
      data$title <- "Transaction Count"
    } else {
      data <- df[,c(1,2,3,4)]
      data$title <- "Median Price"
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
    data <- data[complete.cases(data$obj),]
    
    return(data)
  }
  
  filterForHouseMap <- reactive({
    data <- st_as_sf(house_price, wkt = "the_geom")
    data <- data[data$Sale_Year == input$year, ]
    data <- filter(data)
    
    data$popup <- paste0('<font size="+2"><b>', data$Small_Area, '</b></font><br>',
                         '<b>', data$Sale_Year, '</b><br>',
                         '<font size="+1">', data$title, ": ", data$obj, '</font><br>'
    )
    data
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
        layerId = ~Small_Area,
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
  
  output$line_chart <- renderPlotly({
    data <- filter(house_price)
    data <- data[data$Small_Area == v$small_area, ]
    data <- data[order(data$Sale_Year),]
    
    fig <- plot_ly(
      data[data$Type == 'House/Townhouse',], 
      x = ~Sale_Year, y = ~obj, 
      type = 'scatter', 
      mode = 'lines', 
      showlegend = TRUE,
      name="House/Townhouse Median Price"
    )
    
    fig <- fig %>% add_trace(
      data = data[data$Type == 'Residential Apartment',], 
      x = ~Sale_Year, y = ~obj, type = 'scatter', 
      mode = 'lines', 
      showlegend = TRUE,
      name="Residential Apartment Median Price")
    
    fig
  })
  
  observeEvent(input$map_house_shape_click, {
    click <- input$map_house_shape_click
    v$small_area <- click$id
  })
  
  
  observeEvent(input$reset_time, {
    updateTimeInput(session, "time_input", value = Sys.time())})
  
  
  searchRoute <- eventReactive({input$search|input$alternatives}, {
    time=input$time_input
    if(input$origin == ""){
      origin <- "university of melbourne"
    }
    else{
      origin <- paste(input$origin, 'melbourne')
    }
    if(input$dest == ""){
      dest <- "melbourne central"
    }
    else{
      dest <- paste(input$dest, 'melbourne')
    }
    if(input$traffic %in% c("bus","tram","train")){
      mode="transit"
      transit=input$traffic
    }
    else if (input$traffic=="driving"){
      mode="driving"
      transit=NULL
    }
    else{
      mode="transit"
      transit=c("bus", "subway", "train", "tram")
    }
    
    key=""
    doc = mp_directions(
      origin = origin,
      destination = dest,
      alternatives = TRUE,
      arrival_time = time,
      mode= mode,
      transit_mode=transit,
      key = key,
      quiet = TRUE
    )
    
    mp_get_segments(doc)
  })
  
  output$search_route <- renderLeaflet({
    r <- searchRoute()
    leaflet(r) %>%
      addProviderTiles("CartoDB.DarkMatter")%>%
      setView(lat=-37.812175979147106,lng=144.96245084166554,zoom=10)
  })
  
  observeEvent(input$search,{
    
    r <- searchRoute()
    v$instructions <- ""
    idx1=0
    idx2=0
    for(i in r$alternative_id){
      if(i ==1){idx1=idx1+1}
      if(i ==2){idx2=idx2+1}
    }
    
    t=Sys.time()-180
    for(i in r[1:idx1,1:13]$duration_s){
      t=t-i
    }
    
    output$text=renderText(paste("Recommended departure time:",format(strptime(format(t, "%X"),"%H:%M:%S"),"%H:%M")))
    
    output$detail<-renderDataTable({
      dt <- DT::datatable(
        get_table(r[1:idx1,1:13]),
        rownames = FALSE, 
        options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE)
      )
      
      if(!is.na(v$instructions)){
        dt <- dt %>%
          formatStyle(
            'instructions', target = 'row', 
            backgroundColor = styleEqual(c(v$instructions), c("blue"))
          )
      }
      
      dt
    })
    
    pal <- colorNumeric(c( "#CCFBFE","#E9706C"), 1:length(unique(r$travel_mode)))
    time=format(strptime(format(input$time_input, "%X"),"%H:%M:%S"),'%H')
    map <- leaflet(r[1:idx1,1:13])
    
    if (as.integer(time) >= 7 & as.integer(time) < 17 ){
      map <- map %>% addProviderTiles("CartoDB.Positron")
    }
    else{
      map <- map %>% addProviderTiles("CartoDB.DarkMatter")
    }
    
    output$search_route <- renderLeaflet({
      map %>% 
        addPolylines(
          opacity = 1, 
          weight = 7, 
          layerId = ~instructions,
          color = ~pal(c(1:length(unique(travel_mode)))), popup = ~paste(instructions,duration_text))%>%
        addCircleMarkers(color="#A7A157",lng = unlist(st_cast(r$geometry[idx1], "POINT"))[length(unlist(st_cast(r$geometry[idx1], "POINT")))-1],
                         lat = unlist(st_cast(r$geometry[idx1], "POINT"))[length(unlist(st_cast(r$geometry[idx1], "POINT")))],
                         popup ="Destination")%>%
        addCircleMarkers(color="#43AA8B"
                         ,lng = unlist(st_cast(r$geometry[1], "POINT"))[1],
                         lat = unlist(st_cast(r$geometry[1], "POINT"))[2],
                         popup ="Start Point")
    })
  })
  
  observeEvent(input$alternatives,{
    r <- searchRoute()
    v$instructions <- ""
    
    idx1=0
    idx2=0
    for(i in r$alternative_id){
      if(i ==1){idx1=idx1+1}
      if(i ==2){idx2=idx2+1}}
    t=Sys.time()-180
    
    for(i in r[(idx1+1):(idx1+idx2),1:13]$duration_s){
      t=t-i
    }
    
    output$text=renderText(paste("Recommended departure time:",format(strptime(format(t, "%X"),"%H:%M:%S"),"%H:%M:%S")))
    
    output$detail<-renderDataTable({
      dt <- DT::datatable(
        get_table(r[(1+idx1):(idx1+idx2),1:13]),
        rownames = FALSE, 
        options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE)
      )
      
      if(!is.na(v$instructions)){
        dt <- dt %>%
          formatStyle(
            'instructions', target = 'row', 
            backgroundColor = styleEqual(c(v$instructions), c("blue"))
          )
      }
      
      dt
    })
    
    pal <- colorNumeric(c( "#CCFBFE","#E9706C"), 1:length(unique(r$travel_mode)))
    
    time=format(strptime(format(input$time_input, "%X"),"%H:%M:%S"),'%H')
    map <- leaflet(r[(idx1+1):(idx1+idx2),1:13])
    if (as.integer(time) >= 7 & as.integer(time) < 17 ){
      map <- map %>%
        addProviderTiles("CartoDB.Positron")
    }
    else{
      map <- map %>%
        addProviderTiles("CartoDB.DarkMatter")
    }
    
    output$search_route <- renderLeaflet({
      map %>%
        addPolylines(
          opacity = 1, 
          weight = 7, 
          layerId = ~instructions,
          color = ~pal(c(1:length(unique(travel_mode)))), popup = ~paste(instructions,duration_text))%>%
        addCircleMarkers(color="#A7A157",lng =~unlist(st_cast(geometry[idx2], "POINT"))[length(unlist(st_cast(geometry[idx2], "POINT")))-1],
                         lat = ~unlist(st_cast(geometry[idx2], "POINT"))[length(unlist(st_cast(geometry[idx2], "POINT")))],
                         popup ="Destination")%>%
        addCircleMarkers(color="#43AA8B"
                         ,lng = ~unlist(st_cast(geometry[1], "POINT"))[1],
                         lat = ~unlist(st_cast(geometry[1], "POINT"))[2],
                         popup ="Start Point")
    })
    
  })
  
  observeEvent(input$search_route_shape_click, {
    click <- input$search_route_shape_click
    v$instructions <- click$id
  })
  
  get_table<-function(data){
    df = data.frame(matrix(nrow = length(data$alternative_id), ncol = 4))
    colnames(df)=c("travel_mode","instructions","duration_time","distance")
    df$travel_mode=data$travel_mode
    df$instructions=data$instructions
    df$duration_time=data$duration_text
    df$distance=data$distance_text
    return(df)
  }
  
  
  
} ## server ends


#############
# RUN SHINY #
#############
shinyApp(ui, server)
