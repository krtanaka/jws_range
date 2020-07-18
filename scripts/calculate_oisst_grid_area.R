rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/")

load("t_coldtail.Rdata")

df = df %>%
  group_by(x, y) %>% 
  summarise(p = 1)

df %>%
  group_by(x, y) %>% 
  summarise(p = 1)%>%
  rasterFromXYZ() %>% 
  area()

lat_list = names(table(df$y))

lat_area = NULL

for (i in 1:length(lat_list)) {
  
  # i = 1
  lat = as.numeric(lat_list[[i]])
  lat_2 = lat + 0.25
  
  if (i == 100) {
    
    r = data.frame(y = 47.375, 
                   area = 521.9257)
    
    r = as.data.frame(r)
    
  } else {
    
    r = df %>%
      # subset(y = 22.625) %>% 
      # subset(y <= 22.875 & y >= 22.625) %>%
      # subset(y <= 47.375 & y >= 47.125) %>%
      subset(y <= lat_2 & y >= lat) %>%
      rasterFromXYZ() %>% 
      area()
    
    r = unique(r@data@values)
    
    r1 = cbind(lat, r[2])
    r2 = cbind(lat_2, r[2])
    
    r = r1
    
    colnames(r) = c("y", "area")
    
    
  }
  

  lat_area = rbind(lat_area, r) 
  
  print(r)
  
}

lat = df %>% 
  group_by(y) %>% 
  summarise(p = 1)

save(lat_area, file = "/Users/Kisei/jws_range/data/lat_area.RData")
