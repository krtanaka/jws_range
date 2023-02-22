library(ggplot2)
library(sp)
library(raster)
library(colorRamps)
library(patchwork)

# read shapefile
df <- shapefile(paste0("/Users/", Sys.info()[7], "/Desktop/Census_sum_15/Census_sum_15.shp"), verbose = T)
df <- spTransform(df, CRS('+proj=longlat +datum=WGS84'))

plot(df)
degAxis(1)
degAxis(2)

r.raster <- raster()

# Define raster extent
extent(r.raster) <- extent(df)

# list variables
names(df)

# rasterize
ras <- rasterize(x = df, y = r.raster, field = "dens_sm")

otter_df <- as.data.frame(rasterToPoints(ras))

p1 = ggplot() +  
  geom_raster(data = otter_df, aes(x, y, fill = layer), show.legend = T) + 
  coord_equal() + 
  scale_fill_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") +  
  # annotation_map(map = map_data("world")) + 
  ggdark::dark_theme_minimal()

p2 = ggplot(data = otter_df, aes(y, layer, fill = layer, color = layer)) +  
  geom_point(shape = 21, show.legend = T) + 
  geom_smooth() + 
  scale_fill_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") +  
  scale_color_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") +  
  ggdark::dark_theme_minimal()

p1 + p2

save(otter_df, file = paste0("/Users/", Sys.info()[7], "/Desktop/otter_ca.RData"))
