library(ggpubr)
library(dplyr)

rm(list = ls())

scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
  return(scale_x_continuous("", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
} 

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

r1$year = "1982-2019"
r2$year = "2014-2019"

r3 = data.frame(x = r1$x, y = r2$y, layer = r2$layer - r1$layer, year = "2014-2019 anomalies")

r = rbind(r1, r3)

pdf("~/Desktop/sst_climatology.pdf", width = 10, height = 10)

p1 = r1 %>% 
  ggplot(aes(x, y, color = layer, fill = layer))  + 
  xlab("") +
  ylab("") +
  geom_point(alpha = 0.9, size = 3) +
  geom_raster() +
  annotation_map(map_data("world")) +
  scale_color_viridis_c("degC", breaks = c(round(min(r1$layer), 1), 
                                          round(mean(r1$layer), 1), 
                                          round(max(r1$layer), 1))) +
  scale_fill_viridis_c("degC", breaks = c(round(min(r1$layer), 1), 
                                          round(mean(r1$layer), 1), 
                                          round(max(r1$layer), 1))) +
  scale_x_longitude(xmin=-180, xmax=180, step=10, limits = c(-126, -110)) +
  scale_y_latitude(ymin=-180, ymax=180, step=10, limits = c(22.9, 47.4)) +
  annotate(geom = "text", x = -114.5, y = 28, label = "Vizcaíno Bay", fontface = "italic", color = "white", size = 6) + 
  annotate(geom = "text", x = -120.5, y = 34.4486, label = "Point Conception", fontface = "italic", color = "white", size = 6) +   
  annotate(geom = "text", x = -121.94, y = 36.8, label = "Monterey Bay", fontface = "italic", color = "white", size = 6) +
  theme_minimal() +
  coord_fixed() + 
  facet_wrap(.~year) + 
  theme(legend.position = "right")

p2 = r3 %>% 
  ggplot(aes(x, y, color = layer, fill = layer))  + 
  xlab("") +
  ylab("") +
  geom_point(alpha = 0.9, size = 3) +
  geom_raster() +
  annotation_map(map_data("world")) +
  scale_color_viridis_c("degC", breaks = c(round(min(r3$layer), 1), 
                                           round(mean(r3$layer), 1), 
                                           round(max(r3$layer), 1))) +
  scale_fill_viridis_c("degC", breaks = c(round(min(r3$layer), 1), 
                                          round(mean(r3$layer), 1), 
                                          round(max(r3$layer), 1))) +
  scale_x_longitude(xmin=-180, xmax=180, step=10, limits = c(-126, -110)) +
  scale_y_latitude(ymin=-180, ymax=180, step=10, limits = c(22.9, 47.4)) +
  theme_minimal() +
  coord_fixed() + 
  facet_wrap(.~year) + 
  theme(legend.position = "right")

cowplot::plot_grid(p1, p2)

dev.off()
