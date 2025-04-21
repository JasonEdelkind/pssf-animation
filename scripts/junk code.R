#add date timestamp to animation frames
test<-frames[[2093]]
test$layers[[2]] <- NULL #layer 1 = basemap, layer 2 = scout track lines
test

#create custom legend
test = add_gg(test,gg = expr(annotate(scale_color_manual(name='Legend',
                                                         breaks=c('PSSF Boundary', 'Python Removal', 'Scout Snake'),
                                                         values=c('PSSF Boundary'='lightgreen', 'Python Removal'='red', 'Scout Snake'='orange')))))

#adjust legend size
test = add_gg(test, gg = expr( theme(legend.title=element_text(size=20),
                                     legend.text=element_text(size=14))))



###############ADDING CUSTOM TIMESTAMPS#############################################

library(magick)
anim <- image_read("./moveVis.15fps.gif")
anim <- image_scale(anim, "700")

gc()

#add date to each animation frame (IT REALLT WORKS!!!!!!)
for (i in 1:as.integer(length(anim))) {
  img<-image_draw(anim[i])
  rect(xleft = 309, ybottom = 53, xright = 394, ytop = 81, col = "white", border = "black", lty = "solid", lwd = 1)
  text(351.5, 68, dates[[1]][i], cex = 1)
  dev.off()
  img
  anim[i]<-img
}

gc()
rm(img)

#turn images into animation
animation <- image_animate(anim, fps = 15, optimize = TRUE)

#save animation
image_write(animation, "final15fps700.gif")


################################################################################################################

## adding images to frames
library(png)
library(grid)

# #read in image
img <- readPNG("./images/inset.zoom.png")
g <- rasterGrob(img, interpolate=TRUE)

#add the image to the 100th frame as a test
frame_test <- frames[[100]] 

frame_test+
  annotation_custom(g, xmin=-81.48, xmax=-81.42, ymin=25.949, ymax=25.984)

frame_test

#add the image to all frames
frames = add_gg(frames, gg = expr(annotation_custom(g, xmin=-81.48, xmax=-81.42, ymin=25.949, ymax=25.984)))

# animate frames
animate_frames(frames, fps = 10, out_file = "moveVis.10fpspng.gif",overwrite=TRUE)

## adding images to only select frames

# for (i in 1:as.integer(length(frames)/2)
# ) {  
#   frames[i] = add_gg(frames[i], gg = expr(annotation_custom(g, xmin=-81.53, xmax=-81.43, ymin=26.08, ymax=26.18)))
#   
# }
# frames[[2]]
# 
# # animate frames
# animate_frames(frames, fps = 20, out_file = "moveVis.20fpspartialpng.gif",overwrite=TRUE)

## adding satellite base map

