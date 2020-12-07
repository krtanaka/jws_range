library(sp)
library(dplyr)
library(raster)
library(rgdal)
library(ggplot2)
library(metR)

rm(list = ls())

r = raster::stack() 

for (y in 1982:1983) {
  
  # y = 1986
  
  load(paste0("/Users/ktanaka/jws_range/data/sst.day.mean.", y, ".RData"))
  
  df = mean(df)
  
  r = stack(r, df)
  
  print(y)
  
}

r = mean(r)
r = rasterToPoints(r)
r = as.data.frame(r)

#add lme
lme <- readOGR(paste0("/Users/", Sys.info()[7] , "/jws_range/data/LME66/LMEs66.shp"))
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(lme) <- CRS.new

#add lme layer
latlon = r[,c("x", "y")]
coordinates(latlon) = ~x+y
proj4string(latlon) <- CRS.new
area <- over(latlon, lme)
colnames(area)[1] = "lme"
r = cbind(r, area[2])
r = r %>% subset(LME_NUMBER == "3")

pdf("~/Desktop/sst_climatology.pdf", width = 5, height = 5)

r %>% 
  ggplot(aes(x, y, fill = "California Current Large Marine Ecosystem")) + 
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  geom_tile(aes(height = 0.3, width = 0.3)) +
  annotate(geom = "text", x = -120.5, y = 37.8, label = "San Francisco", fontface = "italic", color = "white", size = 6) +
  annotate(geom = "text", x = -120.5, y = 34.44, label = "Point Conception", fontface = "italic", color = "white", size = 6) +
  coord_quickmap(xlim = c(-131.9, -109.9), ylim = c(22, 48)) +
  scale_x_longitude() +
  scale_y_latitude(breaks = c(47.4, 37.8, 34.4, 22.9)) +
  scale_fill_discrete("") + 
  theme_minimal() +
  theme(legend.position = "top", 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())

dev.off()