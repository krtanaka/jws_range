library(ggplot2)
library(sp)
library(raster)
library(colorRamps)
library(patchwork)
library(maps)
library(dplyr)

rm(list = ls())

# read sea otter census shapefiles, 1985-2019
# https://www.sciencebase.gov/catalog/item/5601b6dae4b03bc34f5445ec
shp_list = list.files(path = "data/otter/", pattern = "\\.shp$", full.names = T); shp_list

otter_df_85_19 = NULL

for (shp_i in 1:length(shp_list)) {
  
  # shp_i = 1
  
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
  
  otter_df_85_19 = rbind(otter_df_85_19, otter_df)
  
}

otter_df = otter_df_85_19 %>% 
  group_by(x, y) %>% 
  summarise(layer = mean(layer))

# plot average otter density along CA coast

p1 = otter_df %>% 
    mutate(x = round(x, 2),
           y = round(y, 2)) %>% 
    group_by(x, y) %>% 
    summarise(layer = mean(layer)) %>% 
    ggplot(aes(x, y, fill = layer)) +  
    geom_raster(show.legend = T) + 
    coord_equal() + 
    scale_fill_gradientn(colours = matlab.like(100),"otter density (m2)", trans = "sqrt") + 
    annotation_map(map = map_data("usa"), fill = "gray20") +
    labs(x = "lon", y = "lat") + 
    ggdark::dark_theme_minimal() +  
    theme(legend.position = c(0.15, 0.2))
  
p2 =otter_df %>% 
    mutate(x = round(x, 2),
           y = round(y, 2)) %>% 
    group_by(y) %>% 
    summarise(layer = mean(layer)) %>% 
    ggplot(aes(layer, y, fill = layer, color = layer)) +  
    geom_point(shape = 21, show.legend = F) + 
    scale_fill_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") +  
    scale_color_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") + 
    labs(x = "otter_dens_sm", y = "lat") + 
    coord_fixed(ratio = 12) + 
    ggdark::dark_theme_minimal()

p1 + p2

save(otter_df, file = "output/otter_ca.RData")
