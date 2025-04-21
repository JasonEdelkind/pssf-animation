library(dplyr)
library(ggmap)
library(sf)
library(lubridate)

df<-read.csv("./data/telem.total.csv")

df$Year<-year(df$Date)

removal.all<-read.csv(".\\data\\removal.all.csv")

#add a season column for both the telemetry and removal dataframes
#define a function to identify the breeding season
season<-function(x){ifelse(
  x[['Date']] >= ymd(paste0(x[['Year']],"-01-01")) & 
    x[['Date']] <= ymd(paste0(x[['Year']],"-10-14")),
  paste(as.numeric(x[['Year']])-1,"-",x[['Year']],sep=""), 
  paste(x[['Year']],"-",as.numeric(x[['Year']])+1,sep=""))
}

#apply the function
removal.all$season<-apply(removal.all, 1, season)
df$season<-apply(df, 1, season)

#read in and plot picayune boundary
#kml_boundary <- st_read(".\\PSSF_boundary.kml")
kml_boundary <- sf::read_sf(".\\shapefiles\\PSSRP_Boundary.shp")

#change crs
kml_boundary<-st_transform(kml_boundary, crs = 4269)
kml_boundary$LABEL<-"PSRP"

#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create basemap
base<-get_map(c(left=min(removal.all$Long),bottom=min(removal.all$Lat),right=max(removal.all$Long),top=max(removal.all$Lat)), source="google",maptype="satellite",zoom=10)

#Add the US and Mexico border layers
base<-ggmap(base)



base+
   geom_sf(data = kml_boundary, aes(fill = LABEL),inherit.aes=FALSE,show.legend = FALSE)+
   geom_point(data=removal.all, aes(x=Long,y=Lat,color=Sentinel))+
   scale_fill_viridis_d(option="turbo")+                              #specify viridis turbo palette 
   theme(legend.position="none")

#filtering removal to only include points within the PSSF boundary
removal_sf<-st_as_sf(removal.all, coords = c("Long", "Lat"), crs = 4269,remove=FALSE) #convert removal df to sf object
removal_sf<-st_filter(removal_sf, kml_boundary) #filter removal sf to only include points within PSSF boundary sf
removal_PSRP<-st_drop_geometry(removal_sf) #reconvert removal sf to df while creating a separate removal object to only count removals within PSSF for dynamic tally

removal_PSRP<-removal_PSRP %>% filter(season == "2024-2025")

base<-get_map(c(left=min(removal_PSRP$Long),bottom=min(removal_PSRP$Lat),right=max(removal_PSRP$Long),top=max(removal_PSRP$Lat)), source="google",maptype="satellite",zoom=12)

#Add the US and Mexico border layers
base<-ggmap(base)
#geom_path(data=country_map,aes(x=long,y=lat,group=group),size=0.4)



base+
  geom_sf(data = kml_boundary, aes(fill = LABEL),inherit.aes=FALSE,show.legend = FALSE)+
  geom_point(data=removal_PSRP, aes(x=Long,y=Lat,color=Sentinel))+
  scale_fill_viridis_d(option="turbo")+                              #specify viridis turbo palette 
  theme(legend.position="none")

method.summ<-removal_PSRP %>% group_by(Method,Sex) %>% summarise(n=n())

###telemetry summary values

#filtering telem to only include points within the PSSF boundary
telem_sf<-st_as_sf(df, coords = c("Long", "Lat"), crs = 4269,remove=FALSE) #convert telem df to sf object
telem_sf<-st_filter(telem_sf, kml_boundary) #filter telem sf to only include points within PSSF boundary sf
telem_PSRP<-st_drop_geometry(telem_sf) #reconvert telem sf to df while creating a separate telem object to only count telems within PSSF for dynamic tally

telem_PSRP<-telem_PSRP %>% filter(season == "2024-2025")

base<-get_map(c(left=min(telem_PSRP$Long),bottom=min(telem_PSRP$Lat),right=max(telem_PSRP$Long),top=max(telem_PSRP$Lat)), source="google",maptype="satellite",zoom=12)

#Add the US and Mexico border layers
base<-ggmap(base)
#geom_path(data=country_map,aes(x=long,y=lat,group=group),size=0.4)



base+
  geom_sf(data = kml_boundary, aes(fill = NA,color=LABEL),inherit.aes=FALSE,show.legend = FALSE)+
  #geom_point(data=telem_PSRP, aes(x=Long,y=Lat,color=Snake_ID))+
  geom_path(data=telem_PSRP, aes(x=Long,y=Lat,color=Snake_ID))+
   scale_fill_viridis_d(option="turbo")+                              #specify viridis turbo palette 
  theme(legend.position="none")  

ggsave(filename="tracklines.png",width = 180, height = 180, dpi=300, units = "mm",path= "./images")





telem.summ<-telem_PSRP %>%  summarise(n=n_distinct(Snake_ID))

