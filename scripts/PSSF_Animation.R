library(geodata)
library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(raster)
library(ggmap)
library(sf)
#save and load RData
#save.image(file='./RData/PSSF_Animation.RData')
load("./RData/PSSF_Animation.RData")

#read in removal data
removal<-read.csv(".\\data\\removal.scout.csv")
#removal<-read.csv(".\\data\\Cleaned_Removal.csv")
removal<-removal %>% filter(Method=="Scout")

#fixing blank scout names in removal database
removal$Sentinel<-ifelse(removal$Additional_ID == "PYBI-114", "KJ", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-06MAR24", "Vlad", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP02-06MAR24", "Vlad", removal$Sentinel)
removal$ID<-ifelse(removal$ID == "BP01- 08MAR24", "BP01-08MAR24", removal$ID)
removal$Sentinel<-ifelse(removal$ID == "BP01-08MAR24", "Eddie", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-11MAR24", "Abe", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-12MAR24", "Ronin", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-02JAN25", "Pacino", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Bobby Rubino", "Bobby Jo", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Joey", "Bobby Jo", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Harriett", "Harriet", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Darryl McLovin", "McLovin", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Timmy", "Timmy T", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Cash", "Ca$h", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Jaegar", "Jaeger", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Dos Equis", "XX", removal$Sentinel)

#changing bobby rubino to bobby jo
removal$Sentinel<-ifelse(removal$Sentinel == "Bobby Rubino","Bobby Jo",removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Joey","Bobby Jo",removal$Sentinel)

#filtering removal to only include PSSF scouts
#removal<-removal %>% filter(removal$Sentinel %in% names)

#filtering removal to only exclude points below a certain latitude
removal<-removal %>% filter(Northing>2866409)

#remove removal points south of tamiami
removal<-removal %>% filter(removal$Designation!="Curcie" & removal$GeneralLocation!="S of Old San Marco Rd")

#converting date to date format
removal$Date<-as.Date(removal$Date, format="%Y-%m-%d")

#filter data by date
removal<-removal %>% filter(removal$Date > as.Date("11/01/2018",format="%m/%d/%Y"))

#rename  columns
colnames(removal)[2] <- 'x'
colnames(removal)[3] <- 'y'


#read in and plot picayune boundary
#kml_boundary <- st_read(".\\PSSF_boundary.kml")
kml_boundary_rough <- sf::read_sf(".\\shapefiles\\DissolvedFeatures.shp")

#change crs
kml_boundary_rough<-st_transform(kml_boundary_rough, crs = 4269)
# base+
#   geom_sf(data = kml_boundary_rough, aes(fill = Label),inherit.aes=FALSE,show.legend = FALSE)+
#   geom_point(data=removal, aes(x=x,y=y))

#filtering removal to only include points within the PSSF boundary
removal_sf<-st_as_sf(removal, coords = c("x", "y"), crs = 4269,remove=FALSE) #convert removal df to sf object
removal_sf<-st_filter(removal_sf, kml_boundary_rough) #filter removal sf to only include points within PSSF boundary sf
removal_PSSF<-st_drop_geometry(removal_sf) #reconvert removal sf to df while creating a separate removal object to only count removals within PSSF for dynamic tally

#isolating scouts of interest
names<-unique(removal_PSSF$Sentinel)

#rename coords
names(removal)[names(removal) == 'x'] <- 'longitude'
names(removal)[names(removal) == 'y'] <- 'latitude'



df<-read.csv("./data/telem.total.csv")

#remove rogue 1, rogue 2, and caesar
df<-df %>% filter(df$Status !="rogue")

colnames(df)[2] <- 'lon'
colnames(df)[3] <- 'lat'
colnames(df)[4] <- 'id'

df<-df %>% filter(df$id!="PYBI_81" | df$lon > -81.75723)  #run this every time until you save new .Rdata
df<-df %>% filter(df$id!="PYBI_91" | df$lat < 26.096488)

#df<-read.csv("C:\\Users\\jason\\OneDrive\\Desktop\\Scout Removal\\Data\\telemetry.csv")

#change unknown times to 1200
df$Time<-ifelse(is.na(df$Time),1200,df$Time)

#format date column
df$Date<-as.Date(df$Date, format = "%Y-%m-%d")

#create timestamp column
df<-df %>% mutate(Timestamp=paste(Date,Time))
df$Timestamp<-as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H%M")

#filter data by date
df<-df %>% filter(df$Date > as.Date("11/01/2018",format="%m/%d/%Y"))

#save dataframe
write.csv(df,"./data/telemetry.csv", row.names = FALSE)



#create df of coord data
xy <-data.frame(df$x,df$y)

#creating a copy of df to turn into an spdf dataframe
spdf<-df

#converting point data to spatial points dataframe
coordinates(spdf) <- ~lon+lat
proj4string(spdf) <- CRS("+proj=longlat +datum=NAD83")
spdf <- st_as_sf(spdf,coords = 24:25)
spdf <- spdf %>% 
  st_transform(crs = 4269)


#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create basemap
base<-get_map(c(left=min(df$lon),bottom=min(df$lat),right=max(df$lon),top=max(df$lat)), source="google",maptype="satellite",zoom=12)

#Add the US and Mexico border layers
base<-ggmap(base)
#geom_path(data=country_map,aes(x=long,y=lat,group=group),size=0.4)

base
#assign a name to the kml polygon
kml_boundary_rough$Label<-"PSSF"

#plot data
base+
  geom_sf(data = kml_boundary_rough, aes(fill = Label),inherit.aes=FALSE,show.legend = FALSE)+
  geom_sf(data = spdf, aes(fill=id), show.legend = FALSE, size=2,  inherit.aes = FALSE)
  
#select only scouts of interest
df<-df %>% filter(df$Snake_Name %in% names)

#plot data
# base+
#   geom_sf(data = kml_boundary_rough, aes(fill = Label),inherit.aes=FALSE,show.legend = FALSE)+
#   geom_sf(data = df, aes(fill=Snake_Name), show.legend = FALSE, size=2,  inherit.aes = FALSE)

#creating an id-timestamp column to filter data
# df<-df %>% mutate(id.timestamp=paste(id,".",Timestamp,sep=""))
# PSSF_p<-PSSF_p %>% mutate(id.timestamp=paste(id,".",Timestamp,sep=""))
# 
# #filtering telemetry data to only include individuals within the PSSF
# df<-df %>% filter(df$Snake_Name %in% names)

base+
  geom_sf(data = kml_boundary_rough, aes(fill = Label),inherit.aes=FALSE,show.legend = FALSE)+
  geom_point(data=df, aes(x=lon,y=lat))

#removing exact duplicates (coords, time, etc)
#df<-df[-2996,]

#identifying duplicate records
duplicates.df<-df[duplicated(paste(df$id,df$timestamp))|duplicated(paste(df$id,df$timestamp), fromLast=TRUE),] #identify duplicates

#changing time of duplicates with different coords
# df$Time<-ifelse(df$Snake_Name=="Nice" & df$Date == as.Date("2021-01-08") & df$UTM_E == duplicates.df[2,7], 1500,df$Time)
# df$Time<-ifelse(df$Snake_Name=="Steve" & df$Date == as.Date("2021-01-13") & df$UTM_E == duplicates.df[4,7], 1500,df$Time)
# df$Time<-ifelse(df$Snake_Name=="Timmy T" & df$Date == as.Date("2021-01-13") & df$UTM_E == duplicates.df[6,7], 1500,df$Time)
# df$Time<-ifelse(df$Snake_Name=="Mikey" & df$Date == as.Date("2021-01-21") & df$UTM_E == duplicates.df[8,7], 1500,df$Time)
# df$Time<-ifelse(df$Snake_Name=="Ronin" & df$Date == as.Date("2021-03-02") & df$UTM_E == duplicates.df[10,7], 1500,df$Time)
# df$Time<-ifelse(df$Snake_Name=="Estilo" & df$Date == as.Date("2021-03-25") & df$UTM_E == duplicates.df[12,7], 1500,df$Time)
# df$Time<-ifelse(df$Snake_Name=="Ronin" & df$Date == as.Date("2021-11-30") & df$UTM_E == duplicates.df[14,7], 1500,df$Time)
# df$Time<-ifelse(df$Snake_Name=="Nice" & df$Date == as.Date("2022-01-11") & df$UTM_E == duplicates.df[16,7], 1500,df$Time)
# df$Time<-ifelse(df$Snake_Name=="Ronin" & df$Date == as.Date("2023-09-13") & df$UTM_E == duplicates.df[18,7], 1500,df$Time)

#adding leading zeroes to time column
df$Time<-as.integer(df$Time)
df<-df %>% 
  mutate(Time=sprintf("%04d", df$Time))

#recreating the timestamp and id.timestamp columns
df<-df %>% mutate(Timestamp=paste(Date,Time))
df$Timestamp<-as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H%M")
df<-df %>% mutate(id.timestamp=paste(id,".",Timestamp,sep=""))
  
#isolate relevant rows
df<-df[,c(2,3,4,46)]

# assign convenient column names
colnames(df) = c('lon', 'lat','id','Timestamp')

#add date column
df$date<-as.Date(df$Timestamp)

#set relevant columns to factors and time
#df$date<-as.Date(df$date, format = "%Y-%m-%d")
df$id<-as.factor(df$id)
df$Timestamp<-as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H%M")

#visualize filtered data
base+
  geom_sf(data = kml_boundary_rough, aes(fill = Label),inherit.aes=FALSE,show.legend = FALSE)+
  geom_point(data=df, aes(x=lon,y=lat,color=id))+
  scale_fill_viridis_d(option="turbo")+                                #specify viridis turbo palette
  theme(legend.position="none")


#######################################################
library(devtools)
library(pkgbuild)
library(ggspatial)
library(gginnards)
install_url('http://cran.r-project.org/src/contrib/Archive/moveVis/moveVis_0.10.5.tar.gz') #moveVis
# install_url('http://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-8.tar.gz') #maptools
# install_url('http://cran.r-project.org/src/contrib/Archive/ggsn/ggsn_0.5.0.tar.gz') #ggsn
# library(maptools)
# library(ggsn)
library(moveVis)
library(move)




#remove problematic points in animation (Stuart and Rogue 2)
df<-df %>% filter(df$id!="PYBI_81" | df$lon > -81.75723)
df<-df %>% filter(df$id!="PYBI_91" | df$lat < 26.096488)

###################################################################################################
#offsetting all telemetry points to align with new base map
df$lon<-df$lon+.00175
###################################################################################################

#creating the anchor
lon<-runif(n=10,min=-81.753272,max=-81.751295)
lat<-runif(n=10,min=26.088156,max=26.092908)
id<-c("PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007")
Timestamp<-c(as.POSIXct("2024-08-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2024-09-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2024-10-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2024-11-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2024-12-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2025-01-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2025-02-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2025-03-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2025-04-21 12:00:00", format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2025-05-21 12:00:00", format="%Y-%m-%d %H:%M:%S"))
date<-c(as.Date("2024-08-21", format = "%Y-%m-%d"),as.Date("2024-09-21", format = "%Y-%m-%d"),
        as.Date("2024-10-21", format = "%Y-%m-%d"),as.Date("2024-11-21", format = "%Y-%m-%d"),
        as.Date("2024-12-21", format = "%Y-%m-%d"),as.Date("2025-01-21", format = "%Y-%m-%d"),
        as.Date("2025-02-21", format = "%Y-%m-%d"),as.Date("2025-03-21", format = "%Y-%m-%d"),
        as.Date("2025-04-21", format = "%Y-%m-%d"),as.Date("2025-05-21", format = "%Y-%m-%d"))
anchor<-data.frame(lon,lat,id,Timestamp,date)
df<-rbind(df,anchor)

#remove individuals with fewer than 2 points
df<-df %>% filter(id!="PYBI_29")

df.mov<-df2move(df,proj="+proj=longlat +datum=NAD83",x="lon",y="lat",time="Timestamp",track_id="id")

# align df.mov to a uniform time scale
m <- align_move(df.mov, res = 1, unit = "days")

## coding symbols by season

timestamps <- as.data.frame(m$time)
names(timestamps) <- "timestamp"
timestamps$colour <- "orange" #base colour

#setting colours by season
# timestamps[timestamps$timestamp >=as.POSIXct("2018-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2018-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2019-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2019-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2020-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2020-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2021-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2021-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2022-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2022-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2023-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2023-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2024-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# #timestamps[timestamps$timestamp >=as.POSIXct("2024-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding

cols <- timestamps$colour # pull out the colors into a list

m$colour <- cols # now add the "cols" to the "colours" column/feature/not sure what this is of "ani"



#define extent
ext <- extent(-81.7, -81.43, 25.95, 26.16)

# create spatial frames 
frames<- frames_spatial(m,
                         map_service = "mapbox", map_type = "satellite", 
                         map_token = "pk.eyJ1IjoiamFzb25lZGVsa2luZCIsImEiOiJjbTMzOTlzcnAxbWJ4MmpwdWkyYnpmODIwIn0.J242Igdiqc-VQupNItls9w",
                         map_res = 1, ext = ext, equidistant = FALSE) %>% 
  add_labels(x = "Longitude", y = "Latitude")# %>% # add some customizations, such as axis labels
  # moveVis::add_northarrow(position="bottomright") %>% 
  # moveVis::add_scalebar(height=0.02,colour="white") #%>% 
  #add_timestamps(size = 5,type = "label") %>% 
  #add_progress()  

#remove legend and axes elements
frames = add_gg(frames, gg = expr(theme(legend.position = "none"))) # remove legend
frames = add_gg(frames, gg = expr(theme(axis.text.x=element_blank()))) #remove text labels
frames = add_gg(frames, gg = expr(theme(axis.ticks.x=element_blank()))) #remove ticks
frames = add_gg(frames, gg = expr(theme(axis.text.y=element_blank()))) #remove text labels
frames = add_gg(frames, gg = expr(theme(axis.ticks.y=element_blank()))) #remove ticks
frames = add_gg(frames, gg = expr(theme(axis.title.x = element_blank()))) #remove titles
frames = add_gg(frames, gg = expr(theme(axis.title.y = element_blank()))) #remove titles

#add scalebar
frames = add_gg(frames, gg = expr(ggspatial::annotation_scale(location="bl",line_width=.4,text_col="white",style="bar",pad_x=unit(3,"cm"),text_cex=.75,width_hint=.25,height=unit(0.3, "cm"))))

#add north arrow
frames = add_gg(frames, gg = expr(ggspatial::annotation_north_arrow(pad_x=unit(5.22, "cm"),pad_y=unit(0.75, "cm"),location = "bl", which_north = "true", style = north_arrow_nautical(text_size = 15, text_col = "white"))))

#add  title
frames<-add_labels(frames, title = "Picayune Strand State Forest Python Tracking and Removal 2018 to 2025")

#remove old base layer
frames<-lapply(frames,delete_layers, match_type="GeomRaster")

#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create new base layer
base<-get_map(c(left = ext[1]-.08, bottom = ext[3]-.03, right = ext[2]+.08, top = ext[4]+.03),
              source="google",maptype="satellite")
base<-ggmap(base) #convert ggmap to gg object
base

#add new base layer
frames<-lapply(frames,append_layers, object=base$layers[[2]],position="bottom")

###################################################################################################
#read in and plot picayune boundary
kml_boundary <- sf::read_sf(".\\shapefiles\\Parks_Boundary.shp")

#change crs
kml_boundary<-st_transform(kml_boundary, crs = 4269)

#offsetting PSSF boundary to align with new base map
kml_boundary$geometry <-  kml_boundary |> st_geometry() + c(.00185, -.0018)

#redefine crs
st_crs(kml_boundary) = "EPSG:4269"
###################################################################################################

#add PSSF boundary line and set extent
frames = add_gg(frames, gg = expr(geom_sf(data = kml_boundary,color=NA, aes(fill=NA,linewidth=0.25),inherit.aes=FALSE,show.legend = FALSE))) #i have no frickin clue why I need to add an initial boundary before i can edit the thickness of a secondary boundary, but if this jenga tower gets built who am I to criticize the one piece in the middle holding this wobbly mess up?
frames = add_gg(frames, gg = expr(geom_sf(data = kml_boundary,color="springgreen", aes(fill=NA,linewidth=0.13),inherit.aes=FALSE,show.legend = FALSE))) #add PSSF boundary
frames = add_gg(frames, gg = expr(coord_sf(xlim = c(-81.7, -81.43), ylim = c(25.95, 26.16),expand=FALSE)))

#add background rectangle for dates and removal tally
frames = add_gg(frames, gg = expr(geom_rect(aes(xmin = -81.7, xmax =-81.66 , ymin =  25.95, ymax =  25.9625), 
                                 fill = "white"))) # add background rectangle for date
frames = add_gg(frames, gg = expr(geom_rect(aes(xmin = -81.7, xmax =-81.625 , ymin =  25.963, ymax =  25.9755), 
                                                                                      fill = "white"))) # add background rectangle for removal tally
##creating the inset map

#create inset map
inset<-get_map(c(left=-88,bottom=24.5,right=-80,top=31), source="google",maptype="satellite",zoom=6) #option 1 for all of FL
#inset<-get_map(c(left=-84,bottom=24.5,right=-79,top=28.5), source="google",maptype="satellite",zoom=8) #option 2 for zoom in of peninsula
inset<-ggmap(inset)
inset

#create Florida border layer
map <- map_data('state')
florida <- subset(map, region %in% "florida")
florida$lat2<-florida$lat-0.2
florida$long2<-florida$long+0.02

inset_final<-inset+ 
  #geom_sf(data = kml_boundary,fill="red",color="red",linewidth=1,inherit.aes=FALSE,show.legend = FALSE)+
  #geom_point(aes(x=26.080015715876005, y=-81.56204701438494), fill="red",size=3)+
  geom_polygon(data = florida, aes(x=long2, y = lat2, group = group), fill=NA,colour="black", linewidth=4)+ #option 1
  annotate("point", y = 26.080015715876005, x = -81.56204701438494, colour = "red",size=12)+
  #geom_polygon(data = florida, aes(x=long2, y = lat2, group = group), fill=NA,colour="black", linewidth=0.5)+ #option 2
  xlim(-87.75,-80)+  ylim(24,31)+ #option 1
  #xlim(-83,-80)+  ylim(25.2,27.75)+ #option 2
  theme_void()+
  theme(
    axis.line=element_blank(),
    axis.text = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))

inset_final
ggsave(filename="inset.png",width = 180, height = 180, dpi=300, units = "mm",path= "./images")

#create example animations using different fps
#sample<-frames[1000:1100]
#animate_frames(sample, fps = 15, out_file = ".//maps//satellite_map.mov",overwrite=TRUE)

#creating a list of all days in movestack
dates<-data.frame(seq(as.Date(min(timestamps(m))), as.Date(max(timestamps(m))), by="days"))
colnames(dates)[1]<-"Date"

# calculate cumulative removals by date
removal2<-removal_PSSF %>% group_by(Date) %>% summarise(n=n()) #summarizing removals by date
removal2$total.removal<-cumsum(removal2$n) #calculate cumulative removals over time
cum.removal<-merge(dates,removal2,by='Date',all.x=TRUE) #merge date and removal dataframes
library(zoo)
cum.removal<-fill(cum.removal, total.removal, .direction = 'down') #carry cumulative removals down the column rows
cum.removal$total.removal<-ifelse(is.na(cum.removal$total.removal),0,cum.removal$total.removal) #replace NAs with zero
cum.removal$n<-ifelse(is.na(cum.removal$n),0,cum.removal$n) #replace NAs with zero
#creating a list of all days in movestack
dates<-data.frame(seq(as.Date(min(timestamps(m))), as.Date(max(timestamps(m))), by="days"))
colnames(dates)[1]<-"Date"

#reformat date and change to list format
dates$Date<-format(as.Date(dates$Date), "%d-%b-%y")
dates<-as.list(dates)

#add custom timestamps to frames
for(i in 1:length(frames)){
  frames[[i]]<-frames[[i]]+annotate(geom="text", x=-81.6795, y=25.957, label=dates[[1]][i],size=4.5,
                                    color="black")
}



frames[[100]]



#add custom removal tally to frames
frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.6685, y=25.97, label=paste("Python Removals: ",sep=""),size=4.5,
                                           color="black"))) # add background rectangle for date
# for(i in 1:length(frames)){
#   frames[[i]]<-frames[[i]]+annotate(geom="text", x=-81.601, y=26.094355, label=paste("Python Removals: ",sep=""),size=4.5,
#                                     color="black")
# }

# add bolded python  removal numbers to tally
for(i in 1:length(frames)){
  frames[[i]]<-frames[[i]]+annotate(geom="text", x=-81.6335, y=25.97, label=cum.removal$total.removal[i],size=4.5,
                                    color="black",fontface = "bold")
  
}



#convert dates to a data table and create a column to define the season
dates.table<-as.data.frame(dates)
dates.table$Date<-as.Date(dates.table$Date, format = "%d-%b-%y")
dates.table$active.season<-NA
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2018",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2019",format="%m/%d/%Y"),1,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2019",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2020",format="%m/%d/%Y"),2,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2020",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2021",format="%m/%d/%Y"),3,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2021",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2022",format="%m/%d/%Y"),4,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2022",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2023",format="%m/%d/%Y"),5,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2023",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2024",format="%m/%d/%Y"),6,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2024",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2025",format="%m/%d/%Y"),7,dates.table$active.season)

#################################################################################################
#offsetting removal points to align with base map
removal_PSSF$x<-removal_PSSF$x+.00175

#################################################################################################

####################ADDING CHANGING REMOVAL POINTS###############################################################
#run a loop to add removal points to each frame with different shapes and colors to indicate current season removals (Damn it's nice when things work)
# for (i in 1:as.integer(length(frames))) {
# points<-removal %>% filter(removal$Date<=dates.table[[1]][i])   #filter removals to only include records from the targeted date 
# points<-points %>% mutate(active.season=NA) #create a new empty column to identify active season
# points$active.season<-ifelse(points$Date>=as.Date("10/15/2018",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2019",format="%m/%d/%Y"),1,points$active.season) #assign values to indicate the specific active season
# points$active.season<-ifelse(points$Date>=as.Date("10/15/2019",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2020",format="%m/%d/%Y"),2,points$active.season) #assign values to indicate the specific active season
# points$active.season<-ifelse(points$Date>=as.Date("10/15/2020",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2021",format="%m/%d/%Y"),3,points$active.season) #assign values to indicate the specific active season
# points$active.season<-ifelse(points$Date>=as.Date("10/15/2021",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2022",format="%m/%d/%Y"),4,points$active.season) #assign values to indicate the specific active season
# points$active.season<-ifelse(points$Date>=as.Date("10/15/2022",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2023",format="%m/%d/%Y"),5,points$active.season) #assign values to indicate the specific active season
# points$active.season<-ifelse(points$Date>=as.Date("10/15/2023",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2024",format="%m/%d/%Y"),6,points$active.season) #assign values to indicate the specific active season
# points$active.season<-ifelse(points$Date>=as.Date("10/15/2024",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2025",format="%m/%d/%Y"),7,points$active.season) #assign values to indicate the specific active season
# season<-dates.table[[2]][i] #create an object to identify the current numeric season in each frame
# points$shapes<-ifelse(points$active.season==season,"O","X") #create a new column to identiify current season removal points for graphing purposes
# frames[i] = add_gg(frames[i], gg = expr(geom_point(data=points, aes(x=longitude, y=latitude, shape=shapes,color=shapes)))) # add the filtered removal points to the map with point color and shapes organized by the shape column
# frames[i] = add_gg(frames[i], gg = expr(scale_shape_manual(values=c("O"=19,"X"=19)))) # manually change point shapes to desired shape
# frames[i] = add_gg(frames[i], gg = expr(scale_color_manual(values=c("O"="red","X"="red"))))#manually change point colors to the desired color
# }

# #run a loop to add removal points to each frame with different shapes and colors to indicate current season removals (Damn it's nice when things work)
#  for (i in 1:as.integer(length(frames))) {
#  points<-removal %>% filter(removal$Date<=dates.table[[1]][i])   #filter removals to only include records from the targeted date 
#  points<-points %>% mutate(active.season=NA) #create a new empty column to identify active season
#  points$active.season<-ifelse(points$Date>=as.Date("10/15/2018",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2019",format="%m/%d/%Y"),1,points$active.season) #assign values to indicate the specific active season
#  points$active.season<-ifelse(points$Date>=as.Date("10/15/2019",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2020",format="%m/%d/%Y"),2,points$active.season) #assign values to indicate the specific active season
#  points$active.season<-ifelse(points$Date>=as.Date("10/15/2020",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2021",format="%m/%d/%Y"),3,points$active.season) #assign values to indicate the specific active season
#  points$active.season<-ifelse(points$Date>=as.Date("10/15/2021",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2022",format="%m/%d/%Y"),4,points$active.season) #assign values to indicate the specific active season
#  points$active.season<-ifelse(points$Date>=as.Date("10/15/2022",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2023",format="%m/%d/%Y"),5,points$active.season) #assign values to indicate the specific active season
#  points$active.season<-ifelse(points$Date>=as.Date("10/15/2023",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2024",format="%m/%d/%Y"),6,points$active.season) #assign values to indicate the specific active season
#  points$active.season<-ifelse(points$Date>=as.Date("10/15/2024",format="%m/%d/%Y") & points$Date<=as.Date("04/15/2025",format="%m/%d/%Y"),7,points$active.season) #assign values to indicate the specific active season
#  season<-dates.table[[2]][i] #create an object to identify the current numeric season in each frame
#  points$shapes<-ifelse(points$active.season==season,"O","X") #create a new column to identiify current season removal points for graphing purposes
#  frames[i] = add_gg(frames[i], gg = expr(geom_point(data=points, aes(x=longitude, y=latitude, shape=shapes,color=shapes),size=2.5))) # add the filtered removal points to the map with point color and shapes organized by the shape column
#  frames[i] = add_gg(frames[i], gg = expr(scale_shape_manual(values=c("O"=19,"X"=19)))) # manually change point shapes to desired shape
#  frames[i] = add_gg(frames[i], gg = expr(scale_color_manual(values=c("O"="red","X"="salmon"))))#manually change point colors to the desired color
#  }

# #have final image show all removal points as dark red
# points<-removal %>% filter(removal$Date<=dates.table[[1]][2093])   #filter removals to only include records from the targeted date 
# frames[2093] = add_gg(frames[2093], gg = expr(geom_point(data=points, aes(x=longitude, y=latitude),size=2.5,color="red"))) # add the filtered removal points to the map
# 
# #animate and save frames
# animate_frames(frames, fps = 15, out_file = "./animations/removal_added_changing_color_basemap.15fps.mp4",overwrite=TRUE)

###########################ADDING CONSTANT REMOVAL POINTS#####################################################
#run a loop to add removal points to each frame with constant shapes and colors
for (i in 1:as.integer(length(frames))) {
  points<-removal_PSSF %>% filter(removal_PSSF$Date<=dates.table[[1]][i])   #filter removals to only include records from the targeted date 
  frames[i] = add_gg(frames[i], gg = expr(geom_point(data=points, aes(x=x, y=y),size=3,color="red"))) # add the filtered removal points to the map
}
 
##add a final frame to show just removal points
frames<-frames[1:2326]
frames[[2327]]<-frames[[2326]] #create final frame
frames[[2327]]$layers[[2]]<- NULL #remove scout tracklines from final frame
frames[[2327]]

## adding inset to frames
library(png)
library(grid)

# read in inset image
img <- readPNG("./images/inset.png")
legend.img <- readPNG("./images/legend_cropped.png")

g <- rasterGrob(img, interpolate=TRUE)
g2 <- rasterGrob(legend.img, interpolate=TRUE)

#add the inset and legend image to all frames
frames = add_gg(frames, gg = expr(annotation_custom(g, xmin=-81.48, xmax=-81.417, ymin=25.949, ymax=25.984)))
frames = add_gg(frames, gg = expr(annotation_custom(g2, xmin= -81.7, xmax = -81.634375, ymin = 25.9425, ymax = 26.0418125)))

#add the legend components to all frames
# frames = add_gg(frames, gg = expr(geom_rect(aes(xmin = -81.7, xmax =-81.625 , ymin =  25.976, ymax =  26.0135), 
#                                             fill = "white")))#add legend background rectangle
# frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.662, y=26.008, label=paste("Legend",sep=""),size=4.5, color="black")))#add legend title
# frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.677, y=25.998, label=paste("Scout Python",sep=""),size=3.5, color="black")))#add subtitle
# frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.6735, y=25.99084, label=paste("Python Removal",sep=""),size=3.5, color="black"))) #add subtitle
# frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.674, y=25.9835, label=paste("PSSF Boundary",sep=""),size=3.5, color="black"))) #add subtitle
# frames = add_gg(frames, gg = expr(annotate("point", x = -81.635,y = 25.998, colour = "orange",size=4))) #add legend symbols
# frames = add_gg(frames, gg = expr(annotate("point", x = -81.635,y = 25.99084, colour = "red",size=4))) #add legend symbols
# frames = add_gg(frames, gg = expr(annotate("segment", x = -81.6381,xend=-81.6319 ,y = 25.9835, colour = "springgreen",size=1))) #add legend symbols
frames[2326]
#animate and save frames
animate_frames(frames, fps = 15, out_file = "./animations/pssf_final_v2.15fps.mp4", width = 700, height = 700, end_pause = 2, overwrite=TRUE)

##########################################SUMMARIZING REMOVAL DATA########################################################################################



#summarizing removal data
removal.PSSF.summ<-removal_PSSF %>% summarise(n=n(),
                                         n.females= sum(Sex == "Female"),
                                         n.males = sum(Sex == "Male"),
                                    avg.lbs=round(mean(Weight_lbs),digits=2),
                                    avg.lbs.female=round(mean(Weight_lbs[Sex == "Female"]),digits=2),
                                    avg.lbs.male=round(mean(Weight_lbs[Sex == "Male"]),digits=2),
                                    total.lbs=round(sum(Weight_lbs),digits=2),
                                    total.lbs.female=round(sum(Weight_lbs[Sex == "Female"]),digits=2),
                                    total.lbs.male=round(sum(Weight_lbs[Sex == "Male"]),digits=2),
                                    
                                    total.length.cm=round(sum(TotL_cm,na.rm=T),digits=2),
                                    total.length.cm.female=round(sum(TotL_cm[Sex == "Female"],na.rm=T),digits=2),
                                    total.length.cm.male=round(sum(TotL_cm[Sex == "Male"],na.rm=T),digits=2),
                                    avg.length.cm=round(mean(TotL_cm,na.rm=T),digits=2),
                                    avg.length.cm.female=round(mean(TotL_cm[Sex == "Female"],na.rm=T),digits=2),
                                    avg.length.cm.male=round(mean(TotL_cm[Sex == "Male"],na.rm=T),digits=2),
        
                                    total.svl.cm=round(sum(SVL_cm,na.rm=T),digits=2),
                                    total.svl.cm.female=round(sum(SVL_cm[Sex == "Female"],na.rm=T),digits=2),
                                    total.svl.cm.male=round(sum(SVL_cm[Sex == "Male"],na.rm=T),digits=2),
                                    avg.svl.cm=round(mean(SVL_cm,na.rm=T),digits=2),
                                    avg.svl.cm.female=round(mean(SVL_cm[Sex == "Female"],na.rm=T),digits=2),
                                    avg.svl.cm.male=round(mean(SVL_cm[Sex == "Male"],na.rm=T),digits=2)
)



#read in PSRP kml and filter removal data to only include those points
kml_boundary <- sf::read_sf(".\\shapefiles\\PSSRP_Boundary.shp")

#change crs
kml_boundary<-st_transform(kml_boundary, crs = 4269)

#creating a copy of removal to turn into an spdf dataframe
spdf<-removal

#converting point data to spatial points dataframe
coordinates(spdf) <- ~longitude+latitude
proj4string(spdf) <- CRS("+proj=longlat +datum=NAD83")
spdf <- st_as_sf(spdf,coords = 24:25)
spdf <- spdf %>% 
  st_transform(crs = 4269)

#selecting just PSRP removal points
removal_PSRP<-spdf[which(st_intersects(kml_boundary, spdf, sparse = FALSE)), ]

#reconvert to df
removal_PSRP<-as.data.frame(removal_PSRP)

#summarizing PSRP removal data
removal.PSRP.summ<-removal_PSRP %>% summarise(n=n(),
                                              n.females= sum(Sex == "Female"),
                                              n.males = sum(Sex == "Male"),
                                         avg.lbs=round(mean(Weight_lbs),digits=2),
                                         avg.lbs.female=round(mean(Weight_lbs[Sex == "Female"]),digits=2),
                                         avg.lbs.male=round(mean(Weight_lbs[Sex == "Male"]),digits=2),
                                         total.lbs=round(sum(Weight_lbs),digits=2),
                                         total.lbs.female=round(sum(Weight_lbs[Sex == "Female"]),digits=2),
                                         total.lbs.male=round(sum(Weight_lbs[Sex == "Male"]),digits=2),
                                         
                                         total.length.cm=round(sum(TotL_cm,na.rm=T),digits=2),
                                         total.length.cm.female=round(sum(TotL_cm[Sex == "Female"],na.rm=T),digits=2),
                                         total.length.cm.male=round(sum(TotL_cm[Sex == "Male"],na.rm=T),digits=2),
                                         avg.length.cm=round(mean(TotL_cm,na.rm=T),digits=2),
                                         avg.length.cm.female=round(mean(TotL_cm[Sex == "Female"],na.rm=T),digits=2),
                                         avg.length.cm.male=round(mean(TotL_cm[Sex == "Male"],na.rm=T),digits=2),
                                         
                                         total.svl.cm=round(sum(SVL_cm,na.rm=T),digits=2),
                                         total.svl.cm.female=round(sum(SVL_cm[Sex == "Female"],na.rm=T),digits=2),
                                         total.svl.cm.male=round(sum(SVL_cm[Sex == "Male"],na.rm=T),digits=2),
                                         avg.svl.cm=round(mean(SVL_cm,na.rm=T),digits=2),
                                         avg.svl.cm.female=round(mean(SVL_cm[Sex == "Female"],na.rm=T),digits=2),
                                         avg.svl.cm.male=round(mean(SVL_cm[Sex == "Male"],na.rm=T),digits=2)
)


#save new removal df
write.csv(removal_PSSF, file="./data/removal_PSSF.csv")
write.csv(removal_PSRP, file="./data/removal_PSRP.csv")




###########################CODE TESTING AREA######################################################################

#adding a custom legend
# 
# test<-frames[[200]]
# 
# test+
#   annotate(geom="text", x=-81.6685, y=25.97, label=paste("Python Removals: ",sep=""),size=4.5,
#            color="black")+
#   annotate(geom="text", x=-81.6335, y=25.97, label=cum.removal$total.removal[200],size=4.5,
#            color="black",fontface = "bold")
# ggsave(filename="test.png",width = 180, height = 180, dpi=300, units = "mm",path= "./images")
# 
# 
# test+
#   annotation_custom(g2, xmin=-81.7, xmax =(-81.6475+(0.0525*0.15)) , ymin =  25.9485, ymax =  (26.02795+(0.07945*0.15)))
# 
# test+
#   geom_rect(aes(xmin = -81.7, xmax =-81.625 , ymin =  25.976, ymax =  26.0135), 
#             fill = "white")+ #add legend background rectangle
#   annotate(geom="text", x=-81.662, y=26.008, label=paste("Legend",sep=""),size=4.5, color="black")+ #add legend title
#   annotate(geom="text", x=-81.677, y=25.998, label=paste("Scout Python",sep=""),size=3.5, color="black")+ #add  subtitle
#   annotate(geom="text", x=-81.6735, y=25.99084, label=paste("Python Removal",sep=""),size=3.5, color="black")+ #add  subtitle
#   annotate(geom="text", x=-81.674, y=25.9835, label=paste("PSSF Boundary",sep=""),size=3.5, color="black")+ #add  subtitle
#   annotate("point", x = -81.635,y = 25.998, colour = "orange",size=4)+ #add legend symbols
# annotate("point", x = -81.635,y = 25.99084, colour = "red",size=4)+ #add legend symbols
# annotate("segment", x = -81.6381,xend=-81.6319 ,y = 25.9835, colour = "springgreen",size=1) #add legend symbols
# 
# test+
#   annotation_custom(g, xmin=-81.48, xmax=-81.417, ymin=25.949, ymax=25.984)
# 
# test+
#   annotation_custom(g2, xmin= -81.7, xmax = -81.634375, ymin = 25.9425, ymax = 26.0418125)
#                     
# test<-frames[[2093]]
# test  
# ggsave(filename="test.png",width = 180, height = 180, dpi=300, units = "mm",path= "./images")
