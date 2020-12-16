rm(list = ls())

library(dplyr)
library(ggpubr)
library(ggthemes)
library(raster)
library(scales)
library(zoo)

load("/Users/Kisei/jws_range/data/lat_area.RData")
load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")

df = merge(df, lat_area)
df = df %>% subset(depth > -1000)
df$month = substr(df$time, 6, 7)

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

pdf(paste0("~/Desktop/s6_Probabilistic_maps_", Sys.Date(), ".pdf"), height = 5, width = 10)

d3 %>%
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +  
  annotation_map(map_data("world"), fill = "gray40") +
  coord_fixed() + 
  metR::scale_x_longitude() +
  metR::scale_y_latitude() +
  # theme_minimal(I(15)) +
  theme_pubr() + 
  facet_wrap(.~time, ncol = 3) +
  theme(legend.position = "right")

dev.off()

rm(d1, d2, d3)

#############################
### calculate area extent ###
#############################

top = df %>% 
  subset(year %in% c(1982:2019)) %>%
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area),
            sst = mean(z)) %>% 
  top_n(20, area)


bottom = df %>% 
  subset(year %in% c(1982:2019)) %>%
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area),
            sst = mean(z), 
            year = unique(year)) %>% 
  top_n(-20, area)

readr::write_csv(bottom, "~/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/figures/supplement/TableS2_habitat_area_time.csv")

area = df %>% 
  subset(year %in% c(1982:2019)) %>%
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area),
            sst = mean(z)) %>% 
  top_n(-20, area)



  

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
