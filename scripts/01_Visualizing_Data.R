library(dismo)
library(tidyverse)
library(ggsn)
library(cowplot)
library(maps)
library(mapproj)
library(plyr)
library(raster)
library(rgeos)
library(rgdal)
library(sp)
library(utils)
library(rgbif)
library(sf)
library(ggmap)
library(geodata)
library(dismo)
library(ggplot2)
library(dplyr)

#define species name
myspecies <- c("Python bivittatus")

# download GBIF occurrence data for this species; this takes time if there are many data points!
gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE)

# take a look at the downloaded data:
gbif_data

#select relevant data
myspecies_coords <- gbif_data$data[ , c("decimalLongitude", "decimalLatitude", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references")]

#rename coordinates
colnames(myspecies_coords)[1]<-"x"
colnames(myspecies_coords)[2]<-"y"

#assign coordinates to a new object
coordsH<-myspecies_coords[1:2]

#register API Key
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

base<-get_map("Immokalee, Florida", source="google",maptype="satellite",zoom=8)

#Add the US and Mexico border layers
base<-ggmap(base)

#plotting data
p <- base +
  geom_point(data=coordsH, aes(x=x,y=y),colour="white")+
  labs(title="Burmese Python Occurrence Points", x="Longitude", y="Latitude") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p 



##################################################################################



#set grid cell resolution size
r <- raster(res=0.5)

#cleaning data to one point per grid cell
values(r) <- 1:ncell(r)
coordsH<-as.matrix(coordsH)
s <- gridSample(coordsH, r, n=1)
df<-as.data.frame(s)

#create kde range estimate and 95% contour lines
kd <- ks::kde(df, compute.cont=TRUE)
contour_95 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                    z=estimate, levels=cont["5%"])[[1]])
contour_95 <- data.frame(contour_95)