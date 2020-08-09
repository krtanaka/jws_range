rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")

JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)
JWS_Corrected$month = substr(as.character(JWS_Corrected$Time_s), 6, 7)
plot(table(JWS_Corrected$month))

JWS_Corrected$count = 1
JWS_Corrected$year = substr(as.character(JWS_Corrected$Time_s), 1, 4)
plot(table(JWS_Corrected$year))

d = JWS_Corrected %>% group_by(lat_pop, lon_pop, id, sex) %>% summarise(count = sum(count))
d$id = as.factor(d$id)

p1 =  d %>%
  ggplot(aes(lon_pop, lat_pop,
             color = id,
             label = id)) +
  ylim(c(22.9, 47.4)) + 
  xlim(c(-126, -110)) + 
  annotation_map(map_data("world")) +
  geom_point(size = 5, alpha = 0.8) +
  ggrepel::geom_text_repel(aes(color = id), box.padding = 1, point.padding = 3) +
  xlab("Longitude (dec deg)") + ylab("Latitude (dec deg)") +
  theme_pubr() +
  coord_fixed() + 
  theme(legend.position = "none")

p2 = d %>%
  ggplot(aes(lon_pop, lat_pop,
             color = log10(count))) +
  ylim(c(22.9, 47.4)) +
  xlim(c(-126, -110)) +
  xlab("Longitude (dec deg)") + ylab("Latitude (dec deg)") +
  annotation_map(map_data("world")) +
  geom_point(size = 5, alpha = 0.8) +
  theme_pubr() +
  coord_fixed() +
  scale_color_viridis_c("log10(n)") +
  theme(legend.position = c(0.15, 0.2))

df1 = JWS_Corrected %>% 
  group_by(id) %>% 
  summarise(lat = mean(lat_rel),
            lon = mean(lon_rel), 
            count = sum(count)) %>% 
  mutate(pop_rel = "Begin")

df2 = JWS_Corrected %>% 
  group_by(id) %>% 
  summarise(lat = mean(lat_pop),
            lon = mean(lon_pop), 
            count = sum(count)) %>% 
  mutate(pop_rel = "End")

df = rbind(df1, df2)

p3 = ggplot(df, aes(lon, lat, group = id)) +   
  annotation_map(map_data("world")) +
  geom_point(aes(shape = pop_rel, color = log10(count)), size = 5, alpha = 0.9) +
  scale_shape_manual(values = c(16, 1), "") +
  # geom_line() +
  scale_color_viridis_c("log10(n)") +
  facet_wrap(.~id, scales = "fixed", ncol = 4) +
  # ylim(range(pretty(df$lat))) +
  # xlim(range(pretty(df$lon))) +
  xlab("Longitude (dec deg)") + ylab("Latitude (dec deg)") +
  coord_fixed() +
  scale_x_continuous(breaks = round(seq(min(df$lon), max(df$lon), by = 10), 0)) + 
  cowplot::theme_cowplot()
  
setwd('~/Desktop/')
pdf("s1a.pdf", height = 6, width = 6)
p1
dev.off()

pdf("s1b.pdf", height = 7, width = 7)
p3
dev.off()



