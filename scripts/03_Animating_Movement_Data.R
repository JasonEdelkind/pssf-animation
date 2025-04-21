# libraries
library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(raster)
library(ggmap)

options(scipen=999)

# get basemap data
bg = ne_countries(scale = "medium", continent = 'north america', returnclass = "sf")

df = read_csv('http://osmc.noaa.gov/erddap/tabledap/drifter_hourly_qc.csvp?ID%2Clongitude%2Clatitude%2Ctime%2Cve%2Cvn&longitude%3E=-70&longitude%3C=-50&latitude%3E=35&latitude%3C=50&time%3E=2018-01-01&time%3C=2019-01-01')

#convert data to dataframe
df<-as.data.frame(df)

# assign convenient column names
colnames(df) = c('id', 'lon', 'lat', 'time', 've', 'vn')

#add date column
df$date<-as.Date(df$time)

#set relevant columns to factors
df$id<-as.factor(df$id)

# compute daily average positions and speeds
df_ave <- df %>%
  group_by(id,date) %>%
  dplyr::summarise(
    lat = mean(lat), 
    lon = mean(lon),
    spd = mean(sqrt(ve^2+vn^2))
  ) 

# create 'ideal' data with all combinations of data
#create all possible ID-date combinations from the minimum to the maximum date
ideal = expand_grid(
  id = unique(df_ave$id),
  date = seq.Date(from = min(df_ave$date), to = max(df_ave$date), by = 1)
)

# create complete dataset
df_all = left_join(ideal,df_ave)


##plotting static map
p = ggplot()+
  
  # basemap
  geom_sf(data = bg)+
  coord_sf(xlim = range(df_all$lon, na.rm = TRUE), 
           ylim = range(df_all$lat, na.rm = TRUE), 
           expand = FALSE)+
  
  # lines and points
  geom_path(data = df_all, 
            aes(x=lon,y=lat,group=id,color=spd), 
            alpha = 0.3)+
  geom_point(data = df_all, 
             aes(x=lon,y=lat,group=id,fill=spd),
             alpha = 0.7, shape=21, size = 2)+
  
  # formatting
  scale_fill_viridis_c(option = "inferno")+
  scale_color_viridis_c(option = "inferno")+
  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL, 
       fill = 'Speed (m/s)', 
       color = 'Speed (m/s)')+
  theme_dark()+
  theme(panel.grid = element_blank())
p


# animate
anim = p + 
  transition_reveal(along = date)+
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

anim<-animate(anim, nframes = 365, fps = 10)
anim

##Replicating procedure for gila data
df<-read.csv("C:\\Users\\jason\\OneDrive\\Desktop\\ThesisEdelkind\\data\\master.csv")
df<-read.csv("C:\\Users\\jason\\OneDrive\\Desktop\\ThesisEdelkind\\data\\lonlat.csv")


#filter data to only include rows with IDs that start with VF
df<-df %>% filter(str_detect(ID, "^VF"))

#isolate relevant rows
df<-df[,1:4]

# assign convenient column names
colnames(df) = c('lon', 'lat','id','Timestamp')

#set relevant columns to factors and time
df$id<-as.factor(df$id)
df$date<-as.Date(df$Timestamp)
df$Timestamp<-as.POSIXct(df$Timestamp)


# compute daily average positions and speeds
df_ave <- df %>%
  group_by(id,date) %>%
  dplyr::summarise(
    lat = mean(lat), 
    lon = mean(lon))
  

# create 'ideal' data with all combinations of data
#create all possible ID-date combinations from the minimum to the maximum date
ideal = expand_grid(
  id = unique(df_ave$id),
  date = seq.Date(from = min(df_ave$date), to = max(df_ave$date), by = 1)
)

# create complete dataset
df_all = left_join(ideal,df_ave)


##create a base map

#set location for downloaded data
options( geodata_default_path = "C:/Users/jason/OneDrive/Desktop/SDM_Modelling/data")

#download USA spatial vector data
USA <- geodata::gadm(country = "USA", level = 1, version = "latest",path=".//data//SpatVectors")

#plot border spatial vector
plot(USA, xlim = c(min(df$lon),max(df$lon)), ylim = c(min(df$lat),max(df$lat)), col = "lightyellow")

#draw a box around the plot
box()

#set coords for the extent
coords <- list(x=c(min(df$lon),max(df$lon)), y = c(min(df$lat),max(df$lat))) #extent determined by

#create the new extent
New_extent <- extent(coords)

#clip the spatial vector to the new extent
USA <- crop(USA, New_extent)

#plot the clipped spatial vector
plot(USA)

#duplicate the spatial vector
Region <- USA

#turn the spatial vector into a spatial polygons dataframe
Region <- as(Region, "Spatial")

#convert spatial vector to dataframe
Region_map <- fortify(Region)

#Download USA Spatial Vector Data
USA <- geodata::gadm(country = "USA", level = 1, version = "latest",path=".//data//SpatVectors")

#plot border spatial vector
plot(USA, xlim = c(min(df$lon),max(df$lon)), ylim = c(min(df$lat),max(df$lat)), col = "lightyellow")

#draw a box around the plot
box()

#set coords for the extent
coords <- list(x=c(min(df$lon),max(df$lon)), y = c(min(df$lat),max(df$lat))) #extent determined by

#create the new extent
New_extent <- extent(coords)

#clip the spatial vector to the new extent
USA <- crop(USA, New_extent)

#plot the clipped spatial vector
plot(USA)

#duplicate the spatial vector
country <- USA

#turn the spatial vector into a spatial polygons dataframe
country <- as(country, "Spatial")

#convert spatial vector to dataframe
country_map <- fortify(country)


#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create basemap
base<-get_map(c(left=min(df$lon),bottom=min(df$lat),right=max(df$lon),top=max(df$lat)), source="google",maptype="satellite",zoom=13)

#Add the US and Mexico border layers
base<-ggmap(base)+
  geom_path(data=country_map,aes(x=long,y=lat,group=group),size=0.4)

base



##plotting static map
p = base+
  coord_sf(xlim = range(df_all$lon, na.rm = TRUE), 
           ylim = range(df_all$lat, na.rm = TRUE), 
           expand = FALSE)+
  
  # lines and points
  geom_path(data = df_all, 
            aes(x=lon,y=lat,group=id,color=id), 
            alpha = 1,size=1)+
  geom_point(data = df_all, 
             aes(x=lon,y=lat,group=id,fill=id),
             alpha = 0.7, shape=21, size = 4)+
  
  # formatting
 # scale_fill_viridis_c(option = "inferno")+
#  scale_color_viridis_c(option = "inferno")+
#  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL, 
      # fill = 'Speed (m/s)', 
      # color = 'Speed (m/s)'
      )+
  theme_dark()+
  theme(panel.grid = element_blank(),
        legend.position="none")
p

#animate
anim = p + 
  transition_reveal(along = date)+
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

gganimate::animate(anim, nframes = 365, fps = 10)

anim_save(filename="test.gif")

#######################################################
library(devtools)
library(pkgbuild)

install_url('http://cran.r-project.org/src/contrib/Archive/moveVis/moveVis_0.10.5.tar.gz') #moveVis

library(moveVis)
library(move)

data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

# align move_data to a uniform time scale
m <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "streets", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "moveVis.gif")


