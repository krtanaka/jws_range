library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)

rm(list = ls())

for (y in 1981:2020) {
  
  # y = 1986
  
  df = stack(paste0("/Users/Kisei/Dropbox/oisst/sst.day.mean.", y, ".nc"), varname = "sst")
  e = extent(220, 260, 22.50, 47.50) #California Current LME lat-lon range
  df = crop(df, e); rm(e)
  df = raster::rotate(df) #rotate to -180:180
  assign("df", df, .GlobalEnv)
  df = readAll(df)
  save(df, file = paste0("/Users/ktanaka/Dropbox (MBA)/Data/oisst/sst.day.mean.", y, ".RData"))
  
  print(y)
  
}


d1 = stack("/Users/ktanaka/Desktop/oisst/crm_vol7.nc")
d2 = stack("/Users/ktanaka/Desktop/oisst/crm_vol6.nc")

d = rasterToPoints(d1)
d = as.data.frame(d)
d = subset(d, layer < 0)
d = subset(d, layer > -1000)
d$x = round(d$x, 1)
d$y = round(d$y, 1)
d1 = aggregate(.~x+y, d, mean)

d = rasterToPoints(d2)
d = as.data.frame(d)
d = subset(d, layer < 0)
d = subset(d, layer > -1000)
d$x = round(d$x, 1)
d$y = round(d$y, 1)
d2 = aggregate(.~x+y, d, mean)

d = rbind(d1, d2)
colnames(d) = c("x", "y", "depth")

d = rasterFromXYZ(d)


o = df[[1]]
o = raster::rotate(o)

d = resample(d, o, method = "bilinear") 
