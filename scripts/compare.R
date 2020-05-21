rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

load("/Users/Kisei/jws_range/data/lat_area.RData")

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/t_IQR.Rdata")

df = merge(df, lat_area)

df %>%
  # subset(month %in% c("06", "07", "08", "09", "10")) %>% 
  # subset(year %in% c(1982:2019)) %>% 
  subset(y <= 40 & y >= 30) %>% 
  subset(x >= -125 & x <= -116) %>% 
  group_by(x, y) %>% 
  summarise(p = mean(z, na.rm = T)) %>% 
  mutate(type = "mean") %>% 
  ggplot(aes(x, y, fill = p)) +
  geom_raster() +
  scale_fill_viridis_c("Mean") +  
  borders(fill = "gray10") +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  ylab("") + xlab("") + 
  theme_classic() + 
  theme(legend.position = c(0.2, 0.2))

t1 = df %>% 
  group_by(time) %>% 
  # subset(y <= 40 & y >= 30) %>% 
  # subset(x >= -125 & x <= -116) %>% 
  mutate(area = case_when(z > 0 ~ area,
                          z < 1  ~ 0)) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time))

colnames(t1) = c("time", "area")
t1$type = "binary"

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/t_probablistic.Rdata")

df = merge(df, lat_area)

t2 = df %>% 
  group_by(time) %>% 
  # subset(y <= 40 & y >= 30) %>% 
  # subset(x >= -125 & x <= -116) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time))

colnames(t2) = c("time", "area")
t2$type = "probablistic"

t = rbind(t1, t2)

library(zoo)
library(ggpubr)
library(gridExtra)

ggplot(t, aes(x = time, y = area, color = area, fill = type)) +
  # geom_line(aes(y = rollmean(area, 10, na.pad = TRUE))) +
  scale_color_viridis_c("km^2") + 
  stat_smooth(method = "loess", span = 0.1) +
  ylab("Total Habitat Area (km^2)") + 
  ggtitle("10-day running mean") + 
  theme_classic2() 

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/t_probablistic.Rdata")

df = merge(df, lat_area)
df$month = substr(as.character(df$time), 6, 7)

t3 = df %>% 
  group_by(time) %>% 
  subset(y <= 40 & y >= 30) %>%
  subset(x >= -125 & x <= -116) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "30-40° N")

t4 = df %>% 
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         month = substr(as.character(time), 6, 7),
         type = "June-October")

t4 = t4 %>% 
  mutate(month = substr(as.character(time), 6, 7))

t4$area = ifelse(t4$month %in% c("06", "07", "08", "09", "10"), t4$area, NA)

t4 = t4[,c("time", "area", "type")]

t5 = df %>% 
  group_by(time) %>% 
  subset(y <= 40 & y >= 30) %>%
  subset(x >= -125 & x <= -116) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "30-40° N & June-October")

t5 = t5 %>% 
  mutate(month = substr(as.character(time), 6, 7))

t5$area = ifelse(t5$month %in% c("06", "07", "08", "09", "10"), t5$area, NA)

t5 = t5[,c("time", "area", "type")]

t6 = df %>% 
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "Untrimmed")

t = rbind(t3, t4, t5, t6)

ggplot(t, aes(x = time, y = area, color = type, fill = type)) +
  # geom_line(aes(y = rollmean(area, 10, na.pad = TRUE))) +
  # scale_color_viridis_c("km^2") +
  # scale_fill_viridis_c() + 
  stat_smooth(method = "loess", span = 0.1, aes(color = type), show.legend = F) +
  ylab("Total Habitat Area (km^2)") + 
  ggtitle("10-day running mean. Based on probablistic model. Loess fit with span = 0.1") + 
  facet_wrap(.~type, scales = "free_y")
  theme_classic2() 




