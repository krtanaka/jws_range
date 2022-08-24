rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
library(sp)
library(maptools)
library(rgdal)

cores = detectCores()/2
registerDoParallel(cores = cores)

dir = Sys.info()[7]

setwd(paste0('/Users/', dir, '/jws_range/data/'))

load('data/depth_0.25.Rdata')
depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)

load('data/occupancy.RData')
occup = subset(occup, Depth_Range == "0-20m")
occup = subset(occup, Bin_width == "0.5 deg C")
s = occup[,c("Temperature", "count")]
colnames(s) = c("z", "p")
# s$p = s$p/sum(s$p)
s$p = (s$p-min(s$p))/(max(s$p) - min(s$p))

plot(s, bty = "l")

#add lme
lme <- readOGR("data/LME66/LMEs66.shp")
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(lme) <- CRS.new

r = foreach(year = 1981:2020, .combine = rbind, .packages = c('dplyr', 'raster')) %dopar% {
  
  # year = 2019
  
  load(paste0("data/sst.day.mean.", year , ".RData"))
  
  year_sum = NULL
  
  for (day in 1:dim(df)[3]) {
    
    # day = 1
    
    d = df[[day]]
    time = as.character(names(d))
    time = gsub("X", "", time)
    time = gsub("\\.", "-", time)
    d = rasterToPoints(d)
    d = as.data.frame(d)
    colnames(d) = c("x", "y", "z")
    # d$z = round(d$z, 1)
    d$z = plyr::round_any(d$z, 0.5, floor)
    d = merge(d, s)
    d = merge(d, depth)
    d$time = time
    
    #add lme layer
    latlon = d[,c("x", "y")]
    coordinates(latlon) = ~x+y
    proj4string(latlon) <- CRS.new
    area <- over(latlon, lme)
    colnames(area)[1] = "lme"
    d = cbind(d, area[2])
    d = d %>% subset(LME_NUMBER == "3")
    
    # d %>% ggplot(aes(x, y, fill = p)) +
    #   geom_tile() +
    #   scale_fill_viridis_c("")

    year_sum = rbind(year_sum, d)
    
  }
  
  # year_y = aggregate(.~x+y, year_sum, mean)
  year_y = year_sum
  year_y$year = year
  year_y
  
  # qplot(year_y$x, year_y$y, color = year_y$z)
  
  # cold = rbind(cold, year_y)
  
  ### print out itiration id so you know where you are###
  # print(year)
  
}

df = as.data.frame(r)
save(df, file = "/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/t_probablistic_lme.Rdata")

