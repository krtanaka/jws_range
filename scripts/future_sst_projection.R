### California Coast Future SST Projection ###

library(raster)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(colorRamps)
library(maps)
library(reshape2)
library(patchwork)

# download climatology files on your desktop first

# read what's in the netcdf file
nc_open(paste0("/Users/", Sys.info()[7], "/Desktop/nep_8594_t00_10.nc"))

### historical SST climatology (baseline) ###
# Northeast Pacific - Data Access: statistical mean of temperature on 1/10° grid for all decades
# https://www.ncei.noaa.gov/access/ne-pacific-climate/bin/nepregcl.pl
sst_climatology_1 = stack(paste0("/Users/", Sys.info()[7], "/Desktop/nep_8594_t00_10.nc"), varname = "t_an")[[1]] #1985-1994, surface (0m)
sst_climatology_2 = stack(paste0("/Users/", Sys.info()[7], "/Desktop/nep_95A4_t00_10.nc"), varname = "t_an")[[1]] #1995-2004, surface (0m)
sst_climatology_3 = stack(paste0("/Users/", Sys.info()[7], "/Desktop/nep_A5B2_t00_10.nc"), varname = "t_an")[[1]] #2005-2012, surface (0m)
sst_climatology = mean(sst_climatology_1, sst_climatology_2, sst_climatology_3) #1985-2012 average

par(mfrow = c(2,2))
plot(sst_climatology_1, col = matlab.like(10), zlim = c(8, 28)); map(add = T, fill = T, col = "gray20")
plot(sst_climatology_2, col = matlab.like(10), zlim = c(8, 28)); map(add = T, fill = T, col = "gray20")
plot(sst_climatology_3, col = matlab.like(10), zlim = c(8, 28)); map(add = T, fill = T, col = "gray20")
plot(sst_climatology, col = matlab.like(10), zlim = c(8, 28)); map(add = T, fill = T, col = "gray20")

### future SST ensemble projection ###
# "CMIP6 ENSMN ssp585 anomaly (2020-2049)-(1985-2014)"
# https://psl.noaa.gov/ipcc/cmip6/
future_sst = raster('data/myplot.2430241.1669930440.21609.nc', varname = "anomaly")

### apply delta future projection ###
### see doi.org/10.1111/ddi.13069 ###
future_sst = raster::rotate(future_sst)
future_sst = resample(future_sst, sst_climatology, method = "bilinear")
future_sst = sst_climatology + future_sst

# add bathymetry data
bathymetry = raster("data/etopo180_9352_4538_72ee.nc")
bathymetry = resample(bathymetry, future_sst, method = "bilinear")
future_sst = stack(future_sst, sst_climatology, bathymetry)
names(future_sst) = c("Future_2020-2049", "Baseline_1985-2014", "Bathymetry")

future_sst = rasterToPoints(future_sst) %>% as.data.frame() %>% na.omit()
future_sst$Difference = future_sst$Future - future_sst$Baseline

p1 = future_sst %>% 
  melt(id = 1:2, measure = 3:4) %>% 
  ggplot(aes(x, y, fill = value)) + 
  geom_raster() + 
  facet_wrap(~variable) + 
  scale_fill_gradientn("SST", colours = matlab.like(100)) + 
  annotation_map(map = map_data("world")) + 
  labs(x = "Lon", y = "Lat") + 
  coord_fixed() + 
  theme_minimal()

p2 = future_sst %>% 
  melt(id = 1:2, measure = 6) %>% 
  ggplot(aes(x, y, fill = value)) + 
  geom_raster() + 
  facet_wrap(~variable) + 
  scale_fill_gradientn("deg C", colours = matlab.like(100)) +
  annotation_map(map = map_data("world")) + 
  labs(x = "Lon", y = "Lat") + 
  coord_fixed() + 
  theme_minimal()

hist(future_sst$Difference)

png(paste0("/Users/", Sys.info()[7], "/Desktop/future_CC_SST_projection.png"), height = 8, width = 19, units = "in", res = 300)
p1 + p2
dev.off()


save(future_sst, file = "output/future_sst.Rdata")
