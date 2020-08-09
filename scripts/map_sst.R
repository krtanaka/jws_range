rm(list = ls())

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
r = as.data.frame(r)

pdf("~/Desktop/sst_climatology_1982_2019.pdf", width = 5, height = 5)

r %>% ggplot(aes(x, y, color = layer))  + 
  xlab("") +
  ylab("") +
  geom_point(alpha = 0.9, size = 2) + 
  annotation_map(map_data("world")) +
  scale_color_viridis_c("SST") + 
  scale_x_longitude(xmin=-180, xmax=180, step=10, limits = c(-126, -110)) +
  scale_y_latitude(ymin=-180, ymax=180, step=10, limits = c(22.9, 47.4)) +
  annotate(geom = "text", x = -114.5, y = 28, label = "Vizcaíno Bay", 
           fontface = "italic", color = "white", size = 6) + 
  annotate(geom = "text", x = -120.5, y = 34.4486, label = "Point Conception", 
           fontface = "italic", color = "white", size = 6) +   
  annotate(geom = "text", x = -121.94, y = 36.8, label = "Monterey Bay", 
           fontface = "italic", color = "white", size = 6) +
  ggpubr::theme_pubr() +
  coord_fixed() + 
  theme(legend.position = "right")

dev.off()
