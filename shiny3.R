require(shiny)
require(leaflet)
require(RColorBrewer)
require(plyr)
shinyApp(
  ui = fluidPage(
    leafletOutput('WaMap',width="100%",height="800px"),
    absolutePanel(top=10,right=25,
                  sliderInput("year", "Year:",min= 1991, max=2016, step=1 ,animate=animationOptions(interval=500),value=1990,sep=""),
                  checkboxInput("legend", "Display legend", TRUE),
                  tags$div(class="header", checked=NA,
                           tags$p("The Circle Markers indicate population increase"),tags$p("The Colors indicate the pop. growth in percentages"
                           #tags$a("Dataset used is publicly available here",href="https://data.wa.gov/Demographics/WAOFM-April-1-Population-by-State-County-and-City-/tecv-qzfm"
                           ))
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
    wdata$POP_1990[wdata$JURISDICTION=="Bothell (part)" & wdata$COUNTY=="Snohomish"]<-NA
    wdata$POP_1991[wdata$JURISDICTION=="Bothell (part)" & wdata$COUNTY=="Snohomish"]<-NA
    wdata$POP_1992[wdata$JURISDICTION=="Bothell (part)" & wdata$COUNTY=="Snohomish"]<-NA
    wdata<-wdata[!(wdata$JURISDICTION=="Coulee Dam (part)" & wdata$COUNTY=="Grant"),]
    wdata<-wdata[!(wdata$JURISDICTION=="Auburn (part)" & wdata$COUNTY=="Pierce"),]
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
    a$JURISDICTION<-ifelse(str_sub(a$JURISDICTION,-6,-1)=="(part)",paste(str_sub(a$JURISDICTION,1,-8),", ",a$COUNTY," County",sep = ""),as.character(a$JURISDICTION)) 
    
    #Add " county, Washington State" to increase accuracy of the location
    a$full_add<-ifelse(str_sub(a$JURISDICTION,-6,-1)=="County",paste(a$JURISDICTION,", Washington State",sep=""),paste(a$JURISDICTION,", ",a$COUNTY," County, Washington State",sep=""))
    
    #a<-data.frame(a,geocode(as.character(a$full_add)))
    #The above code throws an error since: google restricts requests to 2500 requests a day for non-business use. Therefore instead pulling location for each and every line in the data, we have to pull the location just once for each available city address
    
    #Pull the location for each city address in the dataframe seperately
    # full_addlist<-data.frame(as.character(unique(a$full_add)))
    # names(full_addlist)<-"add"
    # full_addlist<-data.frame(full_addlist,geocode(as.character(full_addlist$add),source="google"))
    # names(full_addlist)<-c("full_add","lon","lat")
    # save(full_addlist,file="full_addlist.Rdata") #save above file if necessary for future use
    # 
    load("full_addlist.Rdata")  #Load the file if already saved
    
    #Below code can be used to check on the map if the locations have been pulled correctly (i.e. f they lie in the WA state itself)
    # full_addlist%>%
    #   leaflet() %>%
    #   addTiles() %>% 
    #   addMarkers(popup=full_addlist$full_add)
    
    
    a<-ddply(a,"JURISDICTION",transform,Growth=c(0,exp(diff(log(value),recursive= F))-1))
    a<-ddply(a,"JURISDICTION",transform,Increase=c(0,diff(value),recursive= F))
    a$Increase<-ifelse(is.na(a$Increase),0,a$Increase)
    a<-a %>% group_by(JURISDICTION) %>% mutate(c_increase=cumsum(Increase))
    
    a$Growth<-ifelse(is.na(a$Growth) | is.infinite(a$Growth),0,a$Growth)
    a<-ddply(a,"JURISDICTION",transform,g2=(cumprod(1+Growth)))
    a<-left_join(a,full_addlist,by="full_add")
    colSums(is.na(a))
    
    a<-a %>% group_by(yr) %>% mutate(pratio=value/sum(value,na.rm=T))
    
    
      #ggplot(a)+geom_line(aes(x=yr,y=c_increase,color=factor(JURISDICTION)))+facet_wrap(~COUNTY,scales = "fixed")+ scale_colour_discrete(guide = FALSE) + theme_bw() +ylab("Absolute increase in residents") +xlab("Year")+ theme(axis.text.x=element_text(angle=50, vjust=0.5)) +ggtitle("Population growth in absolute numbers for WA counties")
    
    
    pal<-colorBin(palette=rainbow(12), a$g2, bins = c(0,1,2,5,7,10,12,15,18), na.color = NA, pretty=TRUE, alpha = TRUE)

    #pal<-colorNumeric(palette=rainbow(10), domain=c(0,1,2,5,7,10,12,15,18))
    
    filteredData <- reactive({
      a[a$yr == as.Date(paste(as.character(input$year),"-01-01",sep = ""),format="%Y-%m-%d"),]
    })
    
    
    output$WaMap <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(a) %>% addProviderTiles("CartoDB.Positron")%>%
        fitBounds(~min(a$lon), ~min(a$lat), ~max(a$lon), ~max(a$lat))
    })
    
    # observe({
    #   leafletProxy("WaMap",data =filteredData()) %>%
    #     clearShapes() %>% clearMarkerClusters() %>% clearMarkers() %>%
    #     addCircleMarkers(lat= ~lat,lng= ~lon,
    #                      #clusterOptions = markerClusterOptions(),clusterId= filteredData()$COUNTY,
    #                      weight=1,color = "#777777",
    #                      fillColor = ~pal(filteredData()$g2),
    #                      radius = ((1+filteredData()$pratio*10)^6),fillOpacity = 0.5, popup = ~paste(filteredData()$full_add,round((filteredData()$g2-1)*100,2),"%"))

    
    observe({
      leafletProxy("WaMap",data =filteredData()) %>%
        clearShapes() %>% clearMarkerClusters() %>% clearMarkers() %>%
        addCircleMarkers(lat= ~lat,lng= ~lon,
                         #clusterOptions = markerClusterOptions(),clusterId= filteredData()$COUNTY,
                         weight=1,color = "#777777",
                         fillColor = pal(filteredData()$g2-1),
                         radius = ((filteredData()$c_increase)^.30),fillOpacity = 0.5, popup = ~paste(filteredData()$full_add,round((filteredData()$g2-1)*100,2),"%. ","Absolute Increase of ",filteredData()$c_increase))
    })
    
    observe({
      proxy <- leafletProxy("WaMap", data = a)
      proxy %>% clearControls()
      if (input$legend) {
        proxy %>% addLegend(position = "bottomright", opacity =0.5,
                            pal = pal, values = ~g2, title = "Pop. Growth % based on 1990",labFormat = labelFormat(prefix = 'From ', suffix = '%', between = '% to ',
                              transform = function(x) 100 * round(x,2)
                            ))
      }
    })
  })
