library(ggOceanMaps)
library(metR)

places = data.frame(x = c(-114.5, -120.5, -121.94), 
                    y = c(28, 34.5, 36.8),
                    label = c("Vizcaíno Bay", "Point Conception", "Monterey Bay"))

pdf("~/Desktop/S1.pdf", height = 8, width = 8)

basemap(limits = c(-126, -110, 22.9, 47.4), 
        land.col = "gray20", 
        land.border.col = NA, 
        bathymetry = TRUE, bathy.style = "poly_greys") + 
  geom_spatial_point(data = places, aes(x, y, color = label), size = 5) +
  scale_color_discrete("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  labs(x = "", y = "") + 
  theme_minimal(I(20)) + 
  theme(legend.position = c(0.75,0.72),
        legend.title = element_text(color = "white", size = 12),
        legend.text = element_text(color = "white", size = 12)) + 
  guides(color=guide_legend(override.aes=list(fill=NA)))

dev.off()
  
  