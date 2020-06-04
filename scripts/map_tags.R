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

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/supplement/')
pdf("tag_locations.pdf", height = 7, width = 10)
p1 = d %>%
  ggplot(aes(lon_pop, lat_pop,
             color = id,
             label = id)) +
  borders(xlim = range(d$lon_pop),
          ylim = range(d$lat_pop),
          fill = "gray10") +
  coord_map(xlim = range(pretty(d$lon_pop)),
            ylim = range(pretty(d$lat_pop))) +
  geom_point() +
  ggrepel::geom_text_repel(aes(color = id), box.padding = 3) +
  xlab("Longitude") + ylab("Latitude") +
  # theme_classic2() +
  theme(legend.position = "none")

p2 = d %>%
  ggplot(aes(lon_pop, lat_pop,
             color = log10(count))) +
  borders(xlim = range(d$lon_pop),
          ylim = range(d$lat_pop),
          fill = "gray10") +
  coord_map(xlim = range(pretty(d$lon_pop)),
            ylim = range(pretty(d$lat_pop))) +
  geom_point(size = 5, alpha = 0.8) +
  xlab("Longitude") + ylab("Latitude") +
  # theme_classic2() +
  scale_color_viridis_c("log10(n)") +
  theme(legend.position = c(0.05, 0.15))

cowplot::plot_grid(p1, p2)
dev.off()