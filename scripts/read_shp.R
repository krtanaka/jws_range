library(sf)

shp <- list.files('/Users/ktanaka/Dropbox (MBA)/PAPER Climate refugia/CR_R_scripts/CR_shapefiles/s_11au16/', pattern = '\\.shp$') # list all files in the MERRA model folder


s <- st_read('/Users/ktanaka/Dropbox (MBA)/PAPER Climate refugia/CR_R_scripts/CR_shapefiles/bathypol.shp')
ggplot() + 
  geom_sf(data = s, color = "black", fill = "cyan1") + 
  coord_sf()

s <- st_read('/Users/ktanaka/Dropbox (MBA)/PAPER Climate refugia/CR_R_scripts/CR_shapefiles/Bathypol100km.shp')
ggplot() + 
  geom_sf(data = s %>% sample_frac(1), fill = "cyan1") + 
  coord_sf()

s <- st_read('/Users/ktanaka/Dropbox (MBA)/PAPER Climate refugia/CR_R_scripts/CR_shapefiles/Shp/Bathypol_new_poly.shp')
ggplot() + 
  geom_sf(data = s, color = "black", fill = "cyan1") + 
  coord_sf()

