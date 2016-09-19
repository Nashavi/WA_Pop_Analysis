require(shiny)
require(leaflet)

shinyApp(
    ui = fluidPage(
    # sidebarLayout(
    #   sidebarPanel(
    #     sliderInput("year",
    #                 "Year:",
    #                 min= 1990, max=2016, step=1 ,value=1990)
    #     ),)
      leafletOutput('myMap',width="60%",height="500px")
      ),
  server = function(input, output) {
    require(dplyr) #for summarise
    require(reshape) #for melt
    require(ggmap) #for geocode
    require(stringr) #for string extraction
    #require(rgdal)
    #require(sp)
    
    ####Load the data and clean up####
    wdata<-read.csv(url("https://data.wa.gov/api/views/64z8-2nqn/rows.csv"))
    wdata$SEQUENCE<-NULL
    wdata$FILTER<-NULL
    wdata<-wdata[wdata$COUNTY!="Washington",]
    wdata<-wdata[str_sub(wdata$JURISDICTION,-6,-1)!="County" | str_sub(wdata$JURISDICTION,1,5)=="Uninc" ,]
    
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
    # full_addlist<-data.frame(full_addlist,geocode(as.character(full_addlist$add)))
    # names(full_addlist)<-c("full_add","lon","lat")
    #save(full_addlist,file="full_addlist.Rdata") #save above file if necessary for future use
    
    load("full_addlist.Rdata")  #Load the file if already saved
    
    #Below code checks on the map if the locations have been pulled correctly (i.e. f they lie in the WA state itself)
    full_addlist%>%
      leaflet() %>%
      addTiles() %>% 
      addMarkers(popup=full_addlist$full_add)
    
    a<-left_join(a,full_addlist,by="full_add")
    
    a%>%
      leaflet() %>%
      addTiles() %>% 
      addMarkers(clusterOptions=markerClusterOptions(),popup=a$full_add)
    
    
   #pq<- reactive({a[a$yr==as.Date(paste(input$year,"01-01",sep=""),format="%Y-%m-%d"),]})
    
    pq<-a[a$yr=="1990-01-01",]
    
   output$myMap<- renderLeaflet(
     pq %>%
      leaflet() %>%
      addTiles() %>% 
      addCircles(weight=1,radius= sqrt(pq$value)*30,popup=paste(pq$full_add,pq$value))
   )
  }
)
  

