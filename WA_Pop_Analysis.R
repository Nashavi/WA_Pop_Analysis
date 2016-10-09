#We would use the following libraries for this analysis and visualization
require(dplyr) #for summarise
require(plyr) #for ddply
require(reshape) #for melt
require(ggmap) #for geocode
require(stringr) #for string extraction
require(ggplot2)


####Load the data and clean up####

wdata<-read.csv(url("https://data.wa.gov/api/views/64z8-2nqn/rows.csv")) 


####Clean the dataset ####
# 1. First I removed the Sequence and Filter columns that are meant for easy filtering in Excel.
# 2. The residents in par to Bothell in Snohomish County were in the range of 300 for the first 3 years upto 1992. From 1993 they seem to spike up above the 11,000 range. That seem to be an anomaly in the dataset and I overwrote with NAs for those first 3 years. 
# 3. Similarly the part of Coulee Dam in Grant County seem to have just 3-4 residents throughout the entire 26 year period and hence it is taken out from the analysis.
# 4. I also took out the subtotal and grand total rows from the dataset. Those rows had county name given as Washington. In addition, we remove the county wise subtotals too.

wdata$SEQUENCE<-NULL #remove unnecessary columns
wdata$FILTER<-NULL #remove unnecessary columns
wdata$POP_1990[wdata$JURISDICTION=="Bothell (part)" & wdata$COUNTY=="Snohomish"]<-NA 
wdata$POP_1991[wdata$JURISDICTION=="Bothell (part)" & wdata$COUNTY=="Snohomish"]<-NA 
wdata$POP_1992[wdata$JURISDICTION=="Bothell (part)" & wdata$COUNTY=="Snohomish"]<-NA
wdata$POP_1990[wdata$JURISDICTION=="Bainbridge Island" & wdata$COUNTY=="Kitsap"]<-NA
wdata[wdata==0]<-NA #removing 0 population and making them NAs
wdata<-wdata[!(wdata$JURISDICTION=="Coulee Dam (part)" & wdata$COUNTY=="Grant"),] #remove 
wdata<-wdata[!(wdata$JURISDICTION=="Enumclaw (part)" & wdata$COUNTY=="Pierce"),] #remove
wdata<-wdata[wdata$COUNTY!="Washington",] #remove washington subtotals and grandtotals
wdata<-wdata[str_sub(wdata$JURISDICTION,-6,-1)!="County",] #remove county subtotals


####Tranform the data frame####
#We also need to transform the dataset since it is currently appearing in wide format. Reshape library has some really cool functions that comes handy for such transformation purposes. We will use melt function to transform the dataset from a wide format to long format.

a<-melt(as.data.frame(wdata))

#Extract year values and create a date
a$yr<-substring(a$variable,5,9)
a$variable<-NULL
a$yr<-as.Date(paste(a$yr,"-01-01",sep = ""),format="%Y-%m-%d")

#### County wise summary #####

c<-a %>% group_by(yr,COUNTY) %>% dplyr::summarise(sumpop = sum(value,na.rm = T))
c<-ddply(c,"COUNTY",transform,Growth=c(0,exp(diff(log(sumpop),recursive= F))-1))
c<-ddply(c,"COUNTY",transform,g2=(cumprod(1+Growth)))




####Make the data ready for google APIs and grouping####

#In order map the cities, first we need to create the geographic coordinates for each of the cities and for this we will use geocode function from ggmap package. To increase the accuracy of the location coordinates, we create an address column with city name, county name and Washington state.

#Remove " (part)" from jurisdiction names
a$JURISDICTION<-ifelse(str_sub(a$JURISDICTION,-6,-1)=="(part)",paste(str_sub(a$JURISDICTION,1,-8),", ",a$COUNTY," County",sep = ""),as.character(a$JURISDICTION)) 

#Add " county, Washington State" to increase accuracy of the location
a$full_add<-ifelse(str_sub(a$JURISDICTION,-6,-1)=="County",paste(a$JURISDICTION,", Washington State",sep=""),paste(a$JURISDICTION,", ",a$COUNTY," County, Washington State",sep=""))

####Create and calculate plotting variables####
a<-ddply(a,"JURISDICTION",transform,Growth=c(0,exp(diff(log(value),recursive= F))-1)) #calculate growth rates
a<-ddply(a,"JURISDICTION",transform,Increase=c(0,diff(value),recursive= F)) # calculate absolute pop number increase
a$Increase<-ifelse(is.na(a$Increase),0,a$Increase) #impute NAs with 0s
a<-ddply(a,"JURISDICTION",transform,c_increase=cumsum(Increase))  #Identify cumulative increases (Could have also been derived subtracting the base year 1990 population directly)

a$Growth<-ifelse(is.na(a$Growth) | is.infinite(a$Growth),0,a$Growth) # Frow growth with NAs or inf where 0 are the denominators, we again substitute with 0
a<-ddply(a,"JURISDICTION",transform,g2=(cumprod(1+Growth))) #Calculate cumulative growth

#a<-data.frame(a,geocode(as.character(a$full_add)))
#we cant use the above code since google restricts requests to 2500 requests a day for non-business use. Therefore instead pulling location for each and every line in the data, we have to pull the location just once for each required city address.  In order to avoid the time consuming process of calling google APIs multiple times for each jurisdiction, we can instead pull location coords for each city by creating a separate data frame with with all unique addresses and joining them later to our original dataset once we have the coordinates. This also helps us in circumventing the Google imposed restriction on servicing API requests only upto 2500 requests a day for non-business use. 

#Pull the location for each city address in the dataframe seperately
add_df<-data.frame(unique(a$full_add),stringsAsFactors = FALSE)
add_df<-data.frame(add_df,geocode(add_df[,1],source="google"))
names(add_df)<-c("full_add","lon","lat")

colSums(is.na(add_df)) # Check if there are any NAs in the lng ot lat

#Below code can be used to check on the map if the locations have been pulled correctly (i.e. f they lie in the WA state itself)
add_df%>%
     leaflet() %>%
     addTiles() %>%
     addMarkers(popup=add_df$full_add)
   
save(add_df,file="add_df.Rdata") #save above file if necessary for future use

#Load the saved file 
#load("add_df.Rdata")  #Load the file if already saved

#Join the location coordinates to the dataset
a<-left_join(a,add_df,by="full_add")
#a<-ddply(a,"Jurisdiction")
#a$lon<-ifelse(str_sub(a$JURISDICTION,-6,-1)=="County",a$lon+0.0003,a$lon)


##### ggplots ######
#plot for cities that have increased/decreased their population by atleast 5000
b<-a %>% dplyr::group_by(full_add) %>% dplyr::mutate(s_values=scale(value,center=min(value),scale=F))
m_counties<-distinct(b %>% dplyr::group_by(full_add) %>% dplyr::transmute(max_inc=max(c_increase)>5000))
m2_counties<- filter(m_counties,max_inc=="TRUE")  

## ggplot #1 for scaled value center around their minimum
ggplot(b[b$full_add %in% m2_counties$full_add,])+geom_line(aes(x=yr,y=s_values,color=factor(JURISDICTION)))+facet_wrap(~COUNTY,scales = "free_y")+ scale_colour_discrete(guide = FALSE) + theme_bw() +ylab("Increase in residents") + xlab("Year")+theme(axis.text.x=element_text(angle=50, vjust=0.5)) +ggtitle("Population growth in WA counties\n(Considering only cities that have changed by atleast 5000 residents during the period)")

##ggplot #2 for growth rate for each city
ggplot(b[b$full_add %in% m2_counties$full_add,])+geom_line(aes(x=yr,y=((g2-1)*100),color=factor(JURISDICTION)))+facet_wrap(~COUNTY,scales = "free_y")+ scale_colour_discrete(guide = FALSE) + theme_bw() +ylab("Pop. growth rate increase") + xlab("Year")+theme(axis.text.x=element_text(angle=50, vjust=0.5)) +ggtitle("Population growth rate in WA counties\n(Considering only cities that have changed by atleast 5000 residents during the period)")

##ggplot #3 for growth county wise
c2<-c[!c$COUNTY %in% c("Garfield", "Wahkiakum", "Columbia","Ferry","Pend Oreille","Skamania","Pend Oreille", "San Juan","Pacific","Lincoln"),]
ggplot(c2)+geom_line(aes(x=yr,y=((g2-1)*100),color=factor(COUNTY)))+facet_wrap(~COUNTY,scales = "free_y")+ scale_colour_discrete(guide = FALSE) + theme_bw() +ylab("Pop. growth rate increase (in %)") + xlab("Year")+theme(axis.text.x=element_text(angle=50, vjust=0.5)) +ggtitle("Population growth rate in WA counties\n(Considering only cities that have changed by atleast 5000 residents during the period)")+aes(ymin=0)


#####  leaflet ######
pal<-colorBin(palette=rainbow(9), a$g2, bins = c(0,0.25,0.50,0.75,1,2,3,4,5,7,10,15), na.color = "#777777", pretty=TRUE, alpha = TRUE)

p<-a[a$yr == "2016-01-01",] #### Fill in the year in the format yyyy-01-01 

leaflet(p) %>% 
  clearShapes() %>% clearMarkerClusters() %>% clearMarkers() %>%
  addProviderTiles("CartoDB.Positron")%>%
  fitBounds(min(a$lon), min(a$lat), max(a$lon), max(a$lat)) %>%
  addCircleMarkers(lat= ~lat,lng= ~lon,
                   #clusterOptions = markerClusterOptions(),clusterId= p$COUNTY,
                   weight=1,color = "#777777",
                   fillColor = pal(p$g2-1),
                   radius = ((p$c_increase)^.30),fillOpacity = 0.5, popup = ~paste(p$full_add,round((p$g2-1)*100,2),"%. ","Absolute Increase of ",p$c_increase)) %>%
  addLegend(position = "bottomright", opacity =0.5,
            pal = pal, values = ~g2, title = "Pop. Growth % based on 1990",labFormat = labelFormat(prefix = 'From ', suffix = '%', between = '% to ',transform = function(x) 100 * round(x,2)
            ))

