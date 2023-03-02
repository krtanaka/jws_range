rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
library(sp)
library(maptools)
library(rgdal)
library(patchwork)

# gridded bathymetry data
load('data/depth_0.25.Rdata')
depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)
depth$x = round(depth$x, 2)
depth$y = round(depth$y, 2)

# juvnile white shark thermal affinity model
load('data/occupancy.RData')
occup = subset(occup, Depth_Range == "0-20m")
occup = subset(occup, Bin_width == "0.5 deg C")
s = occup[,c("Temperature", "count")]
plot(s, bty = "l")
colnames(s) = c("z", "p")
s$p = (s$p-min(s$p))/(max(s$p) - min(s$p))

# add Large Marine Ecosystem GIS shapefile (#66 = California Current)
lme <- readOGR("data/LME66/LMEs66.shp")
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(lme) <- CRS.new

# future SST data from "future_sst_projection.R"
load("output/future_sst.Rdata")

colnames(future_sst) = c("x", "y", "z")
future_sst$z = plyr::round_any(future_sst$z, 0.5, floor)
future_sst = merge(future_sst, s)
future_sst = left_join(future_sst, depth)

future_sst %>% 
  ggplot(aes(x, y, fill = p)) + 
  # geom_point(shape = 21) + 
  geom_raster() + 
  coord_fixed() + 
  scale_fill_viridis_c()
