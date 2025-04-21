library(raster)
library(rgdal)
library(dismo)
library(rJava)
library(plotly)
library(tidyterra)
library(geodata)

#specify file path to the dismo raster layers
path <- file.path(system.file(package="dismo"), 'ex')

##create raster layers 
#create tmin raster layer
#tmin <- worldclim_country("USA", var="tmin", path=tempdir(),res=0.5) 
#define extent
e <- as(extent(-85, -78, 24, 32), 'SpatialPolygons')
#assign crs to extent
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
#crop raster by extent
#tmin <- crop(tmin, e)
#monthly max temp rasters
#tmax <- worldclim_country("USA", var="tmax", path=tempdir(),res=0.5)
#crop raster by extent
#tmax <- crop(tmax, e)
#bio variable rasters
bio <- worldclim_global(var="bio", path=tempdir(),res=0.5) 
#crop raster by extent
bio <- crop(bio, e)

#plot raster layers
#plot(tmin,1,xlim=c(-85,-78), ylim=c(24,32))
#plot(tmax,1,xlim=c(-85,-78), ylim=c(24,32))
plot(bio,1)#,xlim=c(-85,-78), ylim=c(24,32))


#create a raster stack of environmental layers
predictors <- stack(bio)
names(predictors)


library(maptools)


plot(predictors)

#create df of location data
xy <- myspecies_coords[,c(1,2)]
xy<-as.data.frame(xy)

#rename columns
colnames(xy)<-c("Longitude","Latitude")

#filter data to only include FL points
#xy<-xy %>% filter(Longitude<0)

#extract environmental data
presvals <- raster::extract(bio, xy)

#merge location and envronmental data
xy<-cbind(xy,presvals)

#remove points containing NA data
xy<-xy %>% filter(xy$wc2.1_30s_bio_1!="NA")

#create new coords df to make into spatial points object
coords<-xy[,c(1,2)]

#convert df to spatialpoints
coordinates(coords)<-~Longitude+Latitude
class(coords)
projection(coords) <- CRS('+proj=longlat +datum=WGS84')


# first layer of the RasterStack
p<-ggplot() +
  geom_spatraster(data = bio, aes(fill = wc2.1_30s_bio_1)) +
  # You can use coord_sf
  coord_sf(crs = 4326) +
  scale_fill_grass_c(palette = "celsius")+
  geom_point(data=xy, aes(x=Longitude,y=Latitude))
ggplotly(p)



#creating training data
set.seed(0)
group <- kfold(coords, 5)
pres_train <- coords[group != 1, ]
pres_test <- coords[group == 1, ]


#create the maxent model
xm <- maxent(predictors, pres_train)
xm

#plot the maxent model
ped1 <- predict(xm, predictors)  
plot(ped1)




#testing
# background data
bg <- randomPoints(predictors, 1000)

#simplest way to use 'evaluate'
e1 <- evaluate(xm, p=pres_test, a=bg, x=predictors)






########REMOVE MORE variables

##Examining multicolinearity

#removing redundant variables
xy<-xy[-c(6,10)]

pairs(xy[,4:20], lower.panel = NULL)


