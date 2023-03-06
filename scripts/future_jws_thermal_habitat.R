# project future juvenile white shark thermal habitat distribution 
# analyze spatial overlap between jws thermal habitat and sea otter density

rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
library(sp)
library(maptools)
library(rgdal)
library(patchwork)
library(colorRamps)

# load the juvenile white shark thermal affinity model
load('data/occupancy.RData')
thermal_occupancy = occup %>% 
  subset(Depth_Range == "0-20m") %>% 
  subset(Bin_width == "0.5 deg C") %>% 
  dplyr::select(Temperature, count)

colnames(thermal_occupancy) = c("temp", "prop")
thermal_occupancy$prop = (thermal_occupancy$prop-min(thermal_occupancy$prop))/(max(thermal_occupancy$prop) - min(thermal_occupancy$prop))
plot(thermal_occupancy, bty = "n")

# load Large Marine Ecosystem (LME) GIS shapefile (#66 = California Current)
lme <- readOGR("data/LME66/LMEs66.shp")
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(lme) <- CRS.new

# load future SST data from "future_sst_projection.R"
load("output/future_sst.Rdata")

baseline_jws_habitat = future_sst[,c(1:2, 4:5)]

colnames(baseline_jws_habitat) = c("x", "y", "temp", "depth")
baseline_jws_habitat$temp = plyr::round_any(baseline_jws_habitat$temp, 0.5, floor)
baseline_jws_habitat = merge(baseline_jws_habitat, thermal_occupancy)

# add LME layer, subset by California Current LME
latlon = baseline_jws_habitat[,c("x", "y")]
coordinates(latlon) = ~x+y
proj4string(latlon) <- CRS.new
area <- over(latlon, lme)
colnames(area)[1] = "lme"
baseline_jws_habitat = cbind(baseline_jws_habitat, area[2])
baseline_jws_habitat = baseline_jws_habitat %>% subset(LME_NUMBER == "3")

# plot JWS thermal habitat distribution 1984-2014 (baseline)
(p1 = baseline_jws_habitat %>% 
    subset(depth > -500) %>% 
    ggplot(aes(x, y, fill = prop)) + 
    geom_point(shape = 21) +
    geom_raster() +
    coord_fixed() + 
    theme_minimal() + 
    xlab("") + ylab("") + 
    annotation_map(map = map_data("world")) + 
    theme(legend.position = c(0.1, 0.2)) + 
    ggtitle("Modeled distribution of JWS \nthermal habitat suitability, 1985-2014") + 
    scale_fill_gradientn("", colours = matlab.like(100)))

future_jws_habitat = future_sst[,c(1:3, 5)]

colnames(future_jws_habitat) = c("x", "y", "temp", "depth")
future_jws_habitat$temp = plyr::round_any(future_jws_habitat$temp, 0.5, floor)
future_jws_habitat = merge(future_jws_habitat, thermal_occupancy)

# add LME layer, subset by California Current LME
latlon = future_jws_habitat[,c("x", "y")]
coordinates(latlon) = ~x+y
proj4string(latlon) <- CRS.new
area <- over(latlon, lme)
colnames(area)[1] = "lme"
future_jws_habitat = cbind(future_jws_habitat, area[2])
future_jws_habitat = future_jws_habitat %>% subset(LME_NUMBER == "3")

# plot JWS thermal habitat distribution 2020-2049 (future)
(p2 = future_jws_habitat %>% 
    subset(depth > -500) %>% 
    ggplot(aes(x, y, fill = prop)) + 
    geom_point(shape = 21, alpha = 0.5) +
    geom_raster() +
    coord_fixed() + 
    theme_minimal() + 
    xlab("") + ylab("") + 
    annotation_map(map = map_data("world")) + 
    theme(legend.position = c(0.1, 0.2)) + 
    ggtitle("Projected distribution of JWS \nthermal habitat suitability, 2020-2049") + 
    scale_fill_gradientn("", colours = matlab.like(100)))

future_jws_habitat$period = "2020-2049"
baseline_jws_habitat$period = "1984-2014"

rbind(future_jws_habitat, baseline_jws_habitat) %>% 
  subset(depth > -500) %>% 
  ggplot(aes(prop, period, color = period)) + 
  geom_joy()

library(patchwork)
p1 + p2
