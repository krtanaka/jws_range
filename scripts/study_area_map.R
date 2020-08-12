library(ggOceanMaps)

places = data.frame(x = c(-114.5, -120.5, -121.94), 
                    y = c(28, 34.5, 36.8),
                    label = c("Vizcaíno Bay", "Point Conception", "Monterey Bay"))

pdf("~/Desktop/S1.pdf", height = 10, width = 10)

basemap(limits = c(-126, -110, 22.9, 47.4), 
        land.col = "gray20", 
        land.border.col = NA, 
        bathymetry = TRUE, bathy.style = "poly_greys") + 
  geom_spatial_point(data = places, aes(x, y, color = label), size = 5) +
  scale_color_discrete("") + 
  labs(x = "", y = "") + 
  theme_minimal(I(20))

dev.off()
  
  