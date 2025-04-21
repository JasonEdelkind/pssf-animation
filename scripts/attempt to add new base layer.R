library(gginnards)
library(latticeExtra)
library(rasterVis)
p<-frames[[2076]]
p<-p+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"),
                     axis.ticks=element_blank(), 
                     axis.text.x=element_blank(), 
                     axis.text.y=element_blank(), 
                     axis.title.x=element_blank(), 
                     axis.title.y=element_blank(),
                     plot.margin = unit(c(0, 0, 0, 0), "null"),
                     legend.position = 'none')
p<-delete_layers(p, "GeomRaster")
p

base<-get_map(c(left = ext[1]-.08, bottom = ext[3]-.03, right = ext[2]+.08, top = ext[4]+.03),
              source="google",maptype="satellite")


base<-ggmap(base)


plot<-base+
  theme(    
    axis.ticks=element_blank(), 
    axis.text.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.title.x=element_blank(), 
    axis.title.y=element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    legend.position = 'none'
  ) +
  labs(x=NULL, y=NULL)
plot

#save base map
ggsave(plot=plot,".\\images\\basemap.tiff")

# Create a StackedRaster object from the saved plot
raster <- stack(".\\images\\basemap.tiff") #for colored images

# Get the GeoSpatial Components
lat_long <- ggplot_build(plot)$layout$panel_params[[1]][c("x.range","y.range")] 

# Supply GeoSpatial  data to the StackedRaster 
extent(raster) <- c(lat_long$x.range,lat_long$y.range)
projection(raster) <- CRS("+proj=longlat +datum=WGS84")

gplot(raster) + 
  geom_tile(aes(fill = value))

  
append_layers(p,base$layers[[2]],position="bottom")
p

base
base.adj<-base+
  coord_cartesian(xlim = c(ext[1]+.15, ext[2]+.15), 
                ylim = c(ext[3], ext[4]))
base.adj
append_layers(p,base.adj$layers[[2]],position="bottom")

###################Changing telemetry CRS##########################################

df.mov<-spTransform(df.mov,CRSobj="+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
crs(df.mov)







# Save the base map as a PNG image
ggsave("base_map.png", plot = base, width = 10, height = 10, dpi = 300)

# Step 4: Load the saved image as a raster
base_raster <- raster("base_map.png")

# Step 5: Assign CRS for the base raster (EPSG:3857)
crs(base_raster) <- CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Step 6: Transform the base raster from EPSG:3857 to EPSG:4269 (NAD83)
base_raster_nad83 <- projectRaster(base_raster, crs = CRS("+proj=longlat +datum=NAD83"))

# Step 2: Convert the single grayscale layer to RGB
base_raster_rgb <- stack(base_raster_nad83, base_raster_nad83, base_raster_nad83)
names(base_raster_rgb) <- c("Red", "Green", "Blue")  # Set layer names to RGB

# Step 3: Normalize the raster to the [0, 1] range
base_raster_matrix <- as.matrix(base_raster_rgb)
base_raster_matrix <- base_raster_matrix / max(base_raster_matrix, na.rm = TRUE)  # Normalize

# Shift raster by modifying the matrix
shift_base_raster <- function(raster_matrix, shift_amount) {
  # Shift the matrix (positive values move left, negative move right)
  n_cols <- ncol(raster_matrix)
  shift_matrix <- cbind(matrix(NA, nrow = nrow(raster_matrix), ncol = shift_amount), raster_matrix[, 1:(n_cols - shift_amount)])
  return(shift_matrix)
}

# Shift the raster to the left by 20 columns (pixels)
shifted_raster_matrix <- shift_base_raster(base_raster_matrix, shift_amount = 200)

# Now use the shifted raster matrix in annotation_raster
shifted_plot2 <-p+
  annotation_raster(shifted_raster_matrix, xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4])

append_layers(p,shifted_plot2$layers[[8]],position="bottom")
