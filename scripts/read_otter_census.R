library(ggplot2)
library(sp)
library(raster)
library(colorRamps)
library(patchwork)
library(maps)
library(dplyr)

rm(list = ls())

# read sea otter census shapefiles
shp_list = list.files(path = "data/otter/", pattern = "\\.shp$", full.names = T); shp_list

for (shp_i in 1:length(shp_list)) {
  
  shp_i = 1
  
  df <- shapefile(shp_list[shp_i], verbose = T)
  df <- spTransform(df, CRS('+proj=longlat +datum=WGS84'))
  
  r.raster <- raster()
  
  # Define raster extent
  extent(r.raster) <- extent(df)
  
  # list variables
  names(df)
  
  # rasterize otter data
  ras <- rasterize(x = df, y = r.raster, field = "dens_sm")
  
  otter_df <- as.data.frame(rasterToPoints(ras))
  
}
  


(p1 = otter_df %>% 
    ggplot(aes(x, y, fill = layer, color = layer)) +  
    geom_raster(show.legend = T) + 
    coord_equal() + 
    scale_fill_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") + 
    annotation_map(map = map_data("usa"), fill = "gray20") +
    labs(x = "lon", y = "lat") + 
    ggdark::dark_theme_minimal())

(p2 = ggplot(data = otter_df, aes(layer, y, fill = layer, color = layer)) +  
    geom_point(shape = 21, show.legend = F) + 
    scale_fill_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") +  
    scale_color_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") + 
    labs(x = "otter_dens_sm", y = "lat") + 
    ggdark::dark_theme_minimal())

p1 + p2

save(otter_df, file = "output/otter_ca.RData")
