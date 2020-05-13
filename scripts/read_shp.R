library(sf)

shp <- list.files('/Users/ktanaka/Dropbox (MBA)/PAPER Climate refugia/CR_R_scripts/CR_shapefiles/s_11au16/', pattern = '\\.shp$') # list all files in the MERRA model folder


s <- st_read('/Users/kisei/Desktop/CR_shapefiles/CR_shapefiles/bathypol.shp')
ggplot() + 
  geom_sf(data = s, color = "black", fill = "cyan1") + 
  coord_sf()

s <- st_read('/Users/Kisei/Desktop/CR_shapefiles/CR_shapefiles/Bathypol1000_10km.shp')
ggplot() + 
  geom_sf(data = s %>% sample_frac(1), fill = "cyan1") + 
  coord_sf()

s

s <- st_read('/Users/Kisei/Desktop/CR_shapefiles/CR_shapefiles/Bathypol_new_poly.shp')
ggplot() + 
  geom_sf(data = s, color = "black", fill = "cyan1") + 
  coord_sf()

