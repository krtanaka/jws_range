library(ggpubr)
library(dplyr)
library(raster)
library(metR)

rm(list = ls())

#1982-2019
r = raster::stack() 

for (y in 1982:2019) {
  
  # y = 1986
  
  load(paste0("~/jws_range/data/sst.day.mean.", y, ".RData"))
  
  df = mean(df)
  
  r = stack(r, df)
  
  print(y)
  
}

r = mean(r)
r = rasterToPoints(r)
r1 = as.data.frame(r)


#2014-2019
r = raster::stack() 

for (y in 2014:2019) {
  
  # y = 1986
  
  load(paste0("~/jws_range/data/sst.day.mean.", y, ".RData"))
  
  df = mean(df)
  
  r = stack(r, df)
  
  print(y)
  
}

r = mean(r)
r = rasterToPoints(r)
r2 = as.data.frame(r)

#2014-2015
r = raster::stack() 

for (y in 2014:2015) {
  
  # y = 1986
  
  load(paste0("~/jws_range/data/sst.day.mean.", y, ".RData"))
  
  df = mean(df)
  
  r = stack(r, df)
  
  print(y)
  
}

r = mean(r)
r = rasterToPoints(r)
r3 = as.data.frame(r)

#Sep 2015
r = raster::stack() 

for (y in 2015) {
  
  y = 2015
  
  load(paste0("~/jws_range/data/sst.day.mean.", y, ".RData"))
  
  df = mean(df[[244:273]])
  
  r = stack(r, df)
  
  print(y)
  
}

r = mean(r)
r = rasterToPoints(r)
r4 = as.data.frame(r)

r1$year = "1982-2019"
r2$year = "2014-2019"
r3$year = "2014-2015"
r4$year = "Sep 2015"

r1$c =  ifelse(r1$layer <= 15.2 & r1$layer >= 15, 1, 0)
r1$h =  ifelse(r1$layer <= 22 & r1$layer >= 21.8, 1, 0)

r1$cc =  ifelse(r2$layer <= 15.2 & r2$layer >= 15, 1, 0)
r1$hh =  ifelse(r2$layer <= 22 & r2$layer >= 21.8, 1, 0)

r1$ccc =  ifelse(r3$layer <= 15.2 & r3$layer >= 15, 1, 0)
r1$hhh =  ifelse(r3$layer <= 22 & r3$layer >= 21.8, 1, 0)

r1$cccc =  ifelse(r4$layer <= 15.2 & r4$layer >= 15, 1, 0)
r1$hhhc =  ifelse(r4$layer <= 22 & r4$layer >= 21.8, 1, 0)

r3 = data.frame(x = r1$x, y = r2$y, layer = r3$layer - r1$layer, year = "2014-2019 anomalies")
r4 = data.frame(x = r1$x, y = r2$y, layer = r4$layer - r1$layer, year = "Sep 2015 anomalies")

r = rbind(r3, r4)

basemap(limits = c(-126, -110, 22.9, 47.4)) + geom_spatial_point(data = r1, aes(x, y, color = layer)) + scale_color_viridis_c()

pdf("~/Desktop/sst_climatology.pdf", width = 15, height = 10)

p1 = ggplot() + 
  xlab("") +
  ylab("") +
  geom_raster(data = r1, aes(x, y, fill = layer)) + 
  geom_point(data = r1, aes(x, y, color = layer), alpha = 0.9, size = 3) +
  geom_point(data = r1, aes(x, y), color = "red3", alpha = r1$h) +
  geom_point(data = r1, aes(x, y), color = "blue3", alpha = r1$c) +
  geom_point(data = r1, aes(x, y), color = "red2", alpha = r1$hh) +
  geom_point(data = r1, aes(x, y), color = "blue2", alpha = r1$cc) +
  geom_point(data = r1, aes(x, y), color = "red1", alpha = r1$hhh) +
  geom_point(data = r1, aes(x, y), color = "blue1", alpha = r1$ccc) +
  scale_fill_viridis_c("°C", breaks = c(round(min(r1$layer), 1), 
                                          round(mean(r1$layer), 1), 
                                          round(max(r1$layer), 1))) +
  scale_color_viridis_c("°C", breaks = c(round(min(r1$layer), 1), 
                                           round(mean(r1$layer), 1), 
                                           round(max(r1$layer), 1))) + 
  scale_x_longitude(limits = c(-126, -110)) +
  scale_y_latitude(limits = c(22.9, 47.4)) +
  annotation_map(map_data("world")) +
  annotate(geom = "text", x = -114.5, y = 28, label = "Vizcaíno Bay", fontface = "italic", color = "white", size = 6) + 
  annotate(geom = "text", x = -120.5, y = 34.4486, label = "Point Conception", fontface = "italic", color = "white", size = 6) +   
  annotate(geom = "text", x = -121.94, y = 36.8, label = "Monterey Bay", fontface = "italic", color = "white", size = 6) +
  theme_minimal() +
  coord_fixed() + 
  facet_wrap(.~year) + 
  theme(legend.position = "right")

dev.off()

pdf("~/Desktop/s5a.pdf", width = 4, height = 4)

p1 = r1 %>% 
  ggplot(aes(x, y, fill = round(layer, 0))) + 
  geom_raster() +
  scale_fill_viridis_c("°C", breaks = c(round(min(r1$layer), 1), 
                                        round(mean(r1$layer), 1), 
                                        round(max(r1$layer), 1))) +
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = c(-126, -110), ylim = c(22.9, 47.4)) +
  scale_x_longitude() +
  scale_y_latitude() +
  theme_minimal() +
  # coord_fixed() + 
  facet_wrap(.~year) + 
  theme(legend.position = c(0.8,0.8),
        legend.title = element_text(color = "white", size = 12),
        legend.text = element_text(color = "white", size = 12))
p1

dev.off()

pdf("~/Desktop/s5b.pdf", width = 4, height = 4)

p2 = r3 %>% 
  ggplot(aes(x, y, fill = round(layer, 1))) + 
  geom_raster() +
  scale_fill_viridis_c("°C", breaks = c(round(min(r3$layer), 1), 
                                          round(mean(r3$layer), 1), 
                                          round(max(r3$layer), 1))) +
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = c(-126, -110), ylim = c(22.9, 47.4), ) +
  scale_x_longitude() +
  scale_y_latitude() +
  theme_minimal() +
  # coord_fixed() + 
  facet_wrap(.~year) + 
  theme(legend.position = c(0.8,0.8),
        legend.title = element_text(color = "white", size = 12),
        legend.text = element_text(color = "white", size = 12))

p2

dev.off()

pdf("~/Desktop/s5.pdf", width = 6, height = 6)

library(patchwork)
p1 + p2

dev.off()

