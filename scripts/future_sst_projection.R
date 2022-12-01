### California Coast Future SST Projection ###

library(raster)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(colorRamps)
library(maps)

setwd(paste0("/Users/", Sys.info()[7], "/Desktop"))

# read what's in the netcdf file
nc_open('nep_8594_t00_10.nc')

### historical SST climatology (baseline) ###
# Northeast Pacific - Data Access: statistical mean of temperature on 1/10° grid for all decades
# https://www.ncei.noaa.gov/access/ne-pacific-climate/bin/nepregcl.pl
sst_climatology_1 = stack('nep_8594_t00_10.nc', varname = "t_an")[[1]] #1985-1994, surface (0m)
sst_climatology_2 = stack('nep_95A4_t00_10.nc', varname = "t_an")[[1]] #1995-2004, surface (0m)
sst_climatology_3 = stack('nep_A5B2_t00_10.nc', varname = "t_an")[[1]] #2005-2012, surface (0m)
sst_climatology = mean(sst_climatology_1, sst_climatology_2, sst_climatology_3) #1985-2012 average

plot(sst_climatology_1, col = matlab.like(100), zlim = c(8, 28))
plot(sst_climatology_2, col = matlab.like(100), zlim = c(8, 28))
plot(sst_climatology_3, col = matlab.like(100), zlim = c(8, 28))
plot(sst_climatology, col = matlab.like(100), zlim = c(8, 28))

### future SST ensemble projection ###
# "CMIP6 ENSMN ssp585 anomaly (2020-2049)-(1985-2014)"
# https://psl.noaa.gov/ipcc/cmip6/
future_sst = raster('myplot.2430241.1669930440.21609.nc', varname = "anomaly")

### apply delta future projection ###
### see doi.org/10.1111/ddi.13069 ###
future_sst = raster::rotate(future_sst)
future_sst = resample(future_sst, sst_climatology, method = "bilinear")
future_sst = sst_climatology + future_sst

plot(future_sst, col = matlab.like(100), zlim = c(8, 28)); map(add = T, fill = T)
