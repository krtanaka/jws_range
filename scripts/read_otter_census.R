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
  
  print(shp_list[shp_i])
  
}

# summarize otter density at grid level
otter_df = otter_df_85_19 %>% 
  group_by(x, y) %>% 
  summarise(layer = mean(layer))

# plot average otter density along CA coast
p1 = otter_df %>% 
  mutate(x = round(x, 1),
         y = round(y, 1)) %>% 
  group_by(x, y) %>% 
  summarise(layer = mean(layer)) %>% 
  ggplot(aes(x, y)) +  
  annotation_map(map = map_data("usa"), fill = "gray20") +
  geom_point(aes(size = layer, fill = layer), shape = 21, alpha = 0.9) +
  scale_fill_gradientn(colours = matlab.like(100), guide = "legend", trans = "sqrt") +
  coord_equal() + 
  labs(x = "lon", y = "lat") + 
  theme_minimal() +
  guides(fill = guide_legend("otter_dens_sm"),
         size = guide_legend("otter_dens_sm")) + 
  theme(legend.position = c(0.2, 0.2))

p2 = otter_df %>% 
  mutate(x = round(x, 1),
         y = round(y, 1)) %>% 
  group_by(y) %>% 
  summarise(layer = mean(layer)) %>% 
  ggplot(aes(x = y, y = layer, fill = layer)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.8, show.legend = T) + 
  scale_fill_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") +  
  scale_color_gradientn(colours = matlab.like(100),"otter_dens_sm", trans = "sqrt") + 
  coord_flip(expand = F) + 
  labs(y = "otter_dens_sm", x = "lat") + 
  theme_minimal() + 
  theme(legend.position = c(0.8, 0.2))

p1 + p2

# save output
save(otter_df, file = "output/otter_ca.RData")
