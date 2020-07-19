library(sf)

# shp <- list.files('/Users/ktanaka/Dropbox (MBA)/PAPER Climate refugia/CR_R_scripts/CR_shapefiles/s_11au16/',
                  # pattern = '\\.shp$') # list all files in the MERRA model folder

s <- st_read('/Users/kisei/Desktop/CR_shapefiles/CR_shapefiles/bathypol.shp')
s <- st_read('/Users/Kisei/Desktop/CR_shapefiles/CR_shapefiles/Bathypol1000_10km.shp')
s <- st_read('/Users/Kisei/Desktop/CR_shapefiles/CR_shapefiles/Bathypol_new_poly.shp')

ggplot() + 
  geom_sf(data = s %>% sample_frac(1), fill = "cyan1") + 
  coord_sf()

s <- readOGR('/Users/Kisei/Desktop/CR_shapefiles/CR_shapefiles/bathypol.shp')
s <- readOGR('/Users/Kisei/Desktop/CR_shapefiles/CR_shapefiles/Bathypol1000_10km.shp')
s <- readOGR('/Users/Kisei/Desktop/CR_shapefiles/CR_shapefiles/Bathypol_new_poly.shp')

load('/Users/Kisei/jws_range/data/edgar.Rdata')

r = crop(edgar, extent(s))
r = mask(r, s)

plot(log10(r), col = matlab.like(100)); maps::map(add = T, fill = T, col = "gray20")


