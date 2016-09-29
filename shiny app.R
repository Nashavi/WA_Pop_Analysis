require(shiny)
require(leaflet)
require(RColorBrewer)
shinyApp(
    ui = fluidPage(
      leafletOutput('myMap',width="100%",height="800px"),
      absolutePanel(top=10,right=25,
                  sliderInput("year", "Year:",min= 1990, max=2016, step=1 ,value=2002
                  #),
                  #selectInput("colors", "Color Scheme",
                  #  rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                  ),
                  checkboxInput("Majorcities", "Show Seattle & Spokane", TRUE)
                  )),
  server = function(input, output) {
    require(dplyr) #for summarise
    require(reshape) #for melt
    require(ggmap) #for geocode
    require(stringr) #for string extraction

    ####Load the data and clean up####
    wdata<-read.csv(url("https://data.wa.gov/api/views/64z8-2nqn/rows.csv"))
    wdata$SEQUENCE<-NULL
    wdata$FILTER<-NULL
    wdata<-wdata[wdata$COUNTY!="Washington",]
    wdata<-wdata[str_sub(wdata$JURISDICTION,-6,-1)!="County",]
    #wdata<-wdata[str_sub(wdata$JURISDICTION,-6,-1)!="County" | str_sub(wdata$JURISDICTION,1,5)=="Uninc" ,]
    
    ####Tranform the data frame####
    a<-melt(as.data.frame(wdata))
    
    #Extract year values and create a date
    a$yr<-substring(a$variable,5,9)
    a$variable<-NULL
    a$yr<-paste(a$yr,"-01-01",sep = "")
    a$yr<-as.Date(a$yr,format="%Y-%m-%d")
    
    county_list<-unique(a$COUNTY) #create a list of counties
    juris_list<-unique(a$JURISDICTION) #create a list of jurisdiction

    
    #Remove " (part)" from jurisdiction names
    a$JURISDICTION<-ifelse(str_sub(a$JURISDICTION,-6,-1)=="(part)",str_sub(a$JURISDICTION,1,-8),as.character(a$JURISDICTION)) 
    
    #Add " county, Washington State" to increase accuracy of the location
    a$full_add<-paste(a$JURISDICTION,", ",a$COUNTY," County, Washington State",sep="")
    
    #a<-data.frame(a,geocode(as.character(a$full_add)))
    #The above code throws an error since: google restricts requests to 2500 requests a day for non-business use. Therefore instead pulling location for each and every line in the data, we have to pull the location just once for each available city address
    
    #Pull the location for each city address in the dataframe seperately
    # full_addlist<-data.frame(as.character(unique(a$full_add)))
    # names(full_addlist)<-"add"
    # full_addlist<-data.frame(full_addlist,geocode(as.character(full_addlist$add),source="google"))
    # names(full_addlist)<-c("full_add","lon","lat")
    #save(full_addlist,file="full_addlist.Rdata") #save above file if necessary for future use
  
    load("full_addlist.Rdata")  #Load the file if already saved
    
    #Below code can be used to check on the map if the locations have been pulled correctly (i.e. f they lie in the WA state itself)
    # full_addlist%>%
    #   leaflet() %>%
    #   addTiles() %>% 
    #   addMarkers(popup=full_addlist$full_add)
    
    a<-left_join(a,full_addlist,by="full_add")
    
    a%>%
      leaflet() %>%
      addTiles() %>% 
      addMarkers(clusterOptions=markerClusterOptions(),popup=a$full_add)
  
   #pq<- renderPrint({a[a$yr==as.Date(paste(input$year,"01-01",sep=""),format="%Y-%m-%d"),]})
    
    b<-a[a$JURISDICTION %in% c("Seattle","Spokane"),]
    
    a<-a[!a$JURISDICTION %in% c("Seattle","Spokane"),]
    
    filteredData <- reactive({
      a[a$yr == as.Date(paste(as.character(input$year),"-01-01",sep = ""),format="%Y-%m-%d"),]
    })
    
    filteredData2 <- reactive({
      b[b$yr == as.Date(paste(as.character(input$year),"-01-01",sep = ""),format="%Y-%m-%d"),]
    })
    
    # colorpal <- reactive({
    #   colorNumeric(input$colors, a$value)
    # })
    
    output$myMap <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(a) %>% addTiles() %>%
        fitBounds(~min(a$lon), ~min(a$lat), ~max(a$lon), ~max(a$lat))
    })
    
    observe({
      #pal <- colorpal()

      leafletProxy("myMap",data =filteredData()) %>%
        clearShapes() %>% clearMarkerClusters() %>% clearMarkers() %>%
        addCircleMarkers(
          #clusterOptions = markerClusterOptions(),clusterId= filteredData()$COUNTY,
          weight=1,, color = "#777777",
                         #fillColor = ~pal(filteredData()$value), 
                         radius = (((filteredData()$value))/(10^4)),fillOpacity = 0.7, popup = ~paste(filteredData()$full_add,filteredData()$value)
        )
    })
    
    observe({
      proxy <- leafletProxy("myMap", data = filteredData2())
      proxy %>% clearControls()
      if (input$Majorcities) {
        #pal <- colorpal()
        proxy %>% addCircleMarkers(
          #clusterOptions = markerClusterOptions(),clusterId= filteredData()$COUNTY,
          weight=1,, color = "#777777",
          #fillColor = ~pal(filteredData()$value), 
          radius = (((filteredData2()$value))/(10^4)),fillOpacity = 0.7, popup = ~paste(filteredData2()$full_add,filteredData2()$value)
        )
      }
    })
  })
