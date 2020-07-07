rm(list = ls())

library(dplyr)
library(ggpubr)
library(ggthemes)
library(raster)
library(scales)
library(zoo)

load("/Users/Kisei/jws_range/data/lat_area.RData")
load("/Users/ktanaka/jws_range/data/lat_area.RData")

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")

df = merge(df, lat_area)
df = df %>% subset(depth > -1000)
df$month = substr(df$time, 6, 7)

#################################
### probablistic habitat maps ###
#################################

setwd("/Users/ktanaka//Desktop")

d1 = df %>%
  subset(time %in% c("2015-09-15", "2005-03-16")) %>%
  group_by(x, y, time) %>%
  summarise(p = mean(p, na.rm = T)) 

d2 = df %>%
  subset(year %in% c(1982:2019)) %>%
  group_by(x, y) %>%
  summarise(p = mean(p, na.rm = T)) %>% 
  mutate(time = "1982-2019")

d3 = rbind(d1, d2)

pdf(paste0("Probabilistic_maps_", Sys.Date(), ".pdf"), height = 10, width = 10)

d3 %>%
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +  
  annotation_map(map_data("world")) +
  coord_fixed() + 
  xlab("Longitude (dec deg)") + ylab("Latitude (dec deg)") +
  cowplot::theme_cowplot() +
  facet_wrap(.~time, ncol = 3) +
  scale_x_continuous(breaks = round(seq(min(df$x), max(df$x), by = 10), 0)) + 
  theme(legend.position = "right")

dev.off()

rm(d1, d2, d3)

#############################
### calculate area extent ###
#############################

area = df %>% 
  subset(year %in% c(1982:2019)) %>%
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area))

summary(area$area) #daily average

area$year = substr(area$time, 1, 4)

area_year = area %>% 
  group_by(year) %>% 
  summarise(area = mean(area)) %>% 
  mutate(year = as.numeric(year))

summary(area_year$area) #annual average

#daily trend 1982-2019
area$t_step = seq(1, 13879, by = 1)
summary(lm(area ~ t_step, data = area))

#annual trend 1982-2019
summary(lm(area ~ year, data = area_year))

par(mfrow = c(1,2))
#annual trend between 1982-2019
plot(area_year, tyle = "b")
summary(lm(area ~ year, data = area_year))
abline(lm(area ~ year, data = area_year))

#annual trend between 2014-2019
plot(area_year %>% subset(year %in% c(2014:2019)), tyle = "b")
summary(lm(area ~ year, data = area_year %>% subset(year %in% c(2014:2019))))
abline(summary(lm(area ~ year, data = area_year %>% subset(year %in% c(2014:2019)))))
