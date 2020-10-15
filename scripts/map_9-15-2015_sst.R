library(ggpubr)
library(dplyr)
library(raster)
library(ggOceanMaps)
library(metR)
library(PlotSvalbard)
library(ggspatial)

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

#11-15 September 2015 
r = raster::stack() 

for (y in 2015) {
  
  y = 2015
  
  load(paste0("~/jws_range/data/sst.day.mean.", y, ".RData"))
  
  df = mean(df[[254:258]])
  
  r = stack(r, df)
  
  print(y)
  
}

r = mean(r)
r = rasterToPoints(r)
r2 = as.data.frame(r)

r1$year = "1982-2019"
r2$year = "Sep 15 2015"

r = rbind(r1, r2)

pdf("~/Desktop/Sep_15_2015.pdf", width = 4, height = 4)

r %>% 
  ggplot(aes(x, y, fill = round(layer, 0))) + 
  geom_tile(aes(height = 0.3, width = 0.3)) +
  scale_fill_viridis_c("°C") +
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = c(-126, -110), ylim = c(22.9, 47.4)) +
  annotate(geom = "text", x = -111, y = 47, label = "2015-09-15", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14))

dev.off()