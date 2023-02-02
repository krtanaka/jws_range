library(ggplot2)
library(sp)
library(raster)

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

ggplot() +  
  geom_raster(data = otter_df, aes(x, y, fill = layer), show.legend = T) + 
  coord_equal() + 
  scale_fill_viridis_c("otter_dens_sm", trans = "sqrt") + 
  annotation_map(map = map_data("world")) + 
  theme_minimal()

save(otter_df, file = paste0("/Users/", Sys.info()[7], "/Desktop/otter_ca.RData"))
