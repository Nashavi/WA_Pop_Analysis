require("ggplot2")
require("dplyr")
require("leaflet")
# require("plotly")
require("reshape")
require("ggmap") #for geocode
require("stringr")
require(rgdal)
require(sp)

wdata<-read.csv("Wa_Pop.csv")
str(wdata)
wdata$SEQUENCE<-NULL
wdata$FILTER<-NULL
wdata<-wdata[wdata$COUNTY!="Washington",]
wdata<-wdata[str_sub(wdata$JURISDICTION,-6,-1)!="County" | str_sub(wdata$JURISDICTION,1,5)=="Uninc" ,]


a<-melt(as.data.frame(wdata))
a$yr<-substring(a$variable,5,9)
a$variable<-NULL
a$yr<-paste(a$yr,"-01-01",sep = "")
a$yr<-as.Date(a$yr,format="%Y-%m-%d")

county_list<-unique(a$COUNTY)
juris_list<-unique(a$JURISDICTION)

c<-a %>% group_by(yr,COUNTY) %>% summarise(sumpop = sum(value))

ggplot() +
  geom_line(data=c,aes(y=sumpop,x=yr,color="Population"))+
  facet_wrap(~COUNTY,scales="free")+
  theme_gray()
  


#add County and Washington State to increase accuracy
# d<-data.frame(county_list,paste(county_list," County, Washington State",sep = ""))
# names(d)<-c("COUNTY","county_add")
# d<-data.frame(d,geocode(as.character(d$county_add)))
# 
# d %>%
#   leaflet() %>%
#   addTiles() %>% 
#   addMarkers(popup=d$county_list)
# 
# save(d,file = "d.Rdata")
load("d.Rdata")

c<-left_join(c,d,by="COUNTY")


a$JURISDICTION<-ifelse(str_sub(a$JURISDICTION,-6,-1)=="(part)",str_sub(a$JURISDICTION,1,-8),as.character(a$JURISDICTION)) #Removing " (part)" from jurisdiction names
a$full_add<-paste(a$JURISDICTION,", ",a$COUNTY," County, Washington State",sep="")
#Error: google restricts requests to 2500 requests a day for non-business use.
a<-data.frame(a,geocode(as.character(a$full_add)))
# #length(unique(a$full_add))
# full_addlist<-data.frame(as.character(unique(a$full_add)))
# names(full_addlist)<-"add"
# full_addlist<-data.frame(full_addlist,geocode(as.character(full_addlist$add)))
# names(full_addlist)<-c("full_add","lon","lat")
# 
# save(full_addlist,file="full_addlist.Rdata")

load("full_addlist.Rdata")

full_addlist%>%
  leaflet() %>%
  addTiles() %>% 
  addMarkers(popup=full_addlist$full_add)

a<-left_join(a,full_addlist,by="full_add")

a%>%
  leaflet() %>%
  addTiles() %>% 
  addMarkers(clusterOptions=markerClusterOptions(),popup=a$full_add)

pq<-a[a$yr=="1990-01-01",]
pq%>%
  leaflet() %>%
  addTiles() %>% 
  addCircles(weight=1,radius= sqrt(pq$value)*30,popup=paste(pq$full_add,pq$value))






#Below code from http://stackoverflow.com/questions/39399656/making-a-leaflet-layer-clickable-or-unclickable-without-reordering
url <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_5m.zip"
temp <- tempfile(fileext = '.zip')
download.file(url, temp)
unzip(temp, exdir = dirname(temp))
states <- rgdal::readOGR(file.path(dirname(temp), "cb_2015_us_state_5m.shp"),
                         layer = "cb_2015_us_state_5m", verbose = FALSE)

states <- readOGR("shp/cb_2015_us_state_20m.zip",
                  layer = "cb_2013_us_state_20m", verbose = FALSE)

neStates <- subset(states, states$STUSPS %in% "WA")

a %>%
  leaflet(neStates) %>% 
  addTiles() %>% 
  addCircles(weight=1,radius= sqrt(a$value)*30,popup=a$full_add)

a %>% leaflet(neStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>% 
  addCircles(weight=1,radius= sqrt(a$value)*30,popup=a$full_add)

