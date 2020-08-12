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
JWS_Corrected$year = as.numeric(JWS_Corrected$year)
JWS_Corrected %>% group_by(id) %>% summarise(y = mean(year))
plot(table(JWS_Corrected$year))

d = JWS_Corrected %>% group_by(lat_pop, lon_pop, id, sex) %>% summarise(count = sum(count))
d$id = as.factor(d$id)

scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(abs(x),"^o", "*W")), ifelse(x > 0, parse(text=paste0(abs(x),"^o", "*E")),x))))
  return(scale_x_continuous("", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
} 

pdf("~/Desktop/s2a.pdf", width = 6, height = 6)

d %>%
  ggplot(aes(lon_pop, lat_pop,
             color = id,
             label = id)) +
  scale_x_longitude(xmin=-180, xmax=180, step=5, limits = c(-126, -110)) +
  scale_y_latitude(ymin=-180, ymax=180, step=5, limits = c(22.9, 47.4)) +
  annotation_map(map_data("world")) +
  geom_point(size = 5, alpha = 0.8) +
  ggrepel::geom_text_repel(aes(color = id), box.padding = 1, point.padding = 3) +
  # xlab("Longitude (dec deg)") + ylab("Latitude (dec deg)") +
  theme_minimal() +
  coord_fixed() + 
  theme(legend.position = "none")

dev.off()

d %>%
  ggplot(aes(lon_pop, lat_pop,
             color = log10(count))) +
  scale_x_longitude(xmin=-180, xmax=180, step=5, limits = c(-126, -110)) +
  scale_y_latitude(ymin=-180, ymax=180, step=5, limits = c(22.9, 47.4)) +
  xlab("Longitude (dec deg)") + ylab("Latitude (dec deg)") +
  annotation_map(map_data("world")) +
  geom_point(size = 5, alpha = 0.8) +
  scale_color_viridis_c("log10(n)") +
  theme_minimal() +
  coord_fixed() + 
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

pdf("s1b.pdf", height = 10, width = 10)

p3 = ggplot(df, aes(lon, lat, group = id)) +   
  annotation_map(map_data("world")) +
  geom_point(aes(shape = pop_rel, color = log10(count)), size = 5, alpha = 0.8) +
  scale_shape_manual(values = c(16, 17), "") +
  scale_color_viridis_c("log10(n)") +
  facet_wrap(.~id, scales = "fixed", ncol = 4) +
  xlab("Longitude (dec deg)") + ylab("Latitude (dec deg)") +
  coord_fixed() +
  scale_x_longitude(xmin=-180, xmax=180, step=4) +
  scale_y_latitude(ymin=-180, ymax=180, step=2) +
  theme_minimal_grid()
  
p3

dev.off()



