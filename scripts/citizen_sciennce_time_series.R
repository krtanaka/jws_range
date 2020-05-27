rm(list = ls())

library(ggpubr)
library(readr)
library(plyr)
library(mapdata)
library(maptools)
library(dplyr) 

d1 <- read_csv("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/citizen science/Eric/EricMailander_Observations.csv")

d1$trips = 1

d1 = d1 %>% group_by(year) %>% 
  summarise(trips = sum(trips, na.rm = T),
            sharks = sum(n_sharks, na.rm = T)) %>% 
  mutate(sharks_per_trip = sharks/trips, 
         cpue = scales::rescale(sharks_per_trip, to = c(0,1)),
         category = "boat & drone") %>% 
  select(year, cpue, category)


d2 <- read_csv("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/citizen science/iNat/iNat_observs-90879_21May2020.csv")

d2$year = substr(as.character(d2$observed_on), 1, 4)
d2$sharks = 1

d2 = d2 %>% group_by(year) %>% 
  summarise(sharks = sum(sharks, na.rm = T)) %>% 
  mutate(cpue = scales::rescale(sharks, to = c(0,1)),
         category = "iNat") %>% 
  select(year, cpue, category)


setwd("~/Desktop")
png(paste0("Fig.1_", Sys.Date(), ".png"), res = 300, height = 3, width = 3, units = "in")
rbind(d1, d2) %>% 
  ggplot(aes(year, cpue, color = category, group = category)) +
  geom_point(size = 3, alpha = 0.8) + 
  geom_line(size = 2, alpha = 0.8) +
  scale_color_viridis_d("") + 
  theme_pubr() + 
  ylab("Normalized Frequency") + 
  theme(legend.position = c(0.15, 0.95))
dev.off()
