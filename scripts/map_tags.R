rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")

JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)

d = JWS_Corrected
d$month = substr(as.character(d$Time_s), 6, 7)
plot(table(d$month))

d$count = 1
d = d %>% group_by(lat_pop, lon_pop, id, sex) %>% summarise(count = sum(count))
d$id = as.factor(d$id)

setwd('/Users/Kisei/Desktop/')
pdf("tag_locations.pdf", height = 8, width = 8)

# p1 = 
  d %>%
  ggplot(aes(lon_pop, lat_pop,
             color = id,
             label = id)) +
  # borders(xlim = range(d$lon_pop),
  #         ylim = range(d$lat_pop),
  #         fill = "gray10") +
  # coord_map(xlim = range(pretty(d$lon_pop)),
  #           ylim = range(pretty(d$lat_pop))) +
  ylim(c(22.9, 47.4)) + 
  xlim(c(-128, -110)) + 
  geom_point() +
  annotation_map(map_data("world")) +
  ggrepel::geom_text_repel(aes(color = id), box.padding = 5) +
  xlab("Longitude") + ylab("Latitude") +
  theme_pubr() +
  coord_fixed() + 
  theme(legend.position = "none")

# p2 = d %>%
#   ggplot(aes(lon_pop, lat_pop,
#              color = log10(count))) +
#   # borders(xlim = range(d$lon_pop),
#   #         ylim = range(d$lat_pop),
#   #         fill = "gray10") +
#   # coord_map(xlim = range(pretty(d$lon_pop)),
#   #           ylim = range(pretty(d$lat_pop))) +
#   ylim(c(22.9, 47.4)) + 
#   xlim(c(-128, -110)) + 
#   geom_point(size = 5, alpha = 0.8) +
#   xlab("Longitude") + ylab("Latitude") +
#   annotation_map(map_data("world")) +
#   theme_pubr() +
#   coord_fixed() + 
#   scale_color_viridis_c("log10(n)") +
#   theme(legend.position = c(0.05, 0.15))

# cowplot::plot_grid(p1, p2)

  
dev.off()
