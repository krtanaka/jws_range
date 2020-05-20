rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

# grid_cell_size = 615.5493

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
  subset(y <= 40 & y >= 30) %>% 
  subset(x >= -125 & x <= -116) %>% 
  mutate(area = case_when(z > 0 ~ area,
                          z < 1  ~ 0)) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time))

# t1 = df %>%
#   mutate(month = substr(as.character(time), 6, 7),
#          year = substr(as.character(time), 1, 4)) %>% 
#   # subset(month %in% c("06", "07", "08", "09", "10")) %>% 
#   # subset(year %in% c(1982:2019)) %>% 
#   # subset(y <= 36.88 & y >= 30.54) %>% 
#   # subset(x >= -122 & x <= -116) %>% 
#   group_by(time) %>%
#   summarise(
#     total = n(),
#     good = sum(z>0, na.rm = T),
#     area = sum(area)
#     # ,prop = good/total
#     ) %>% 
#   mutate(total = total*grid_cell_size,
#          good = good*grid_cell_size,
#          time = as.Date(time))

# t1 = t1[,c("time", "good")]

colnames(t1) = c("time", "area")
t1$type = "binary"

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/t_probablistic.Rdata")

df = merge(df, lat_area)

t2 = df %>% 
  group_by(time) %>% 
  subset(y <= 40 & y >= 30) %>% 
  subset(x >= -125 & x <= -116) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time))

# t2 = df %>%
#   mutate(month = substr(as.character(time), 6, 7),
#          year = substr(as.character(time), 1, 4)) %>% 
#   # subset(month %in% c("06", "07", "08", "09", "10")) %>% 
#   subset(year %in% c(1982:2019)) %>% 
#   subset(y <= 36.88 & y >= 30.54) %>% 
#   subset(x >= -122 & x <= -116) %>% 
#   mutate(p = p*grid_cell_size) %>% 
#   group_by(time) %>% 
#   summarise(p = sum(p)) %>% 
#   mutate(time = as.Date(time))
# 
# t2 = t2[,c("time", "p")]
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

t1 = t; t1$calender = "Jan-Dec"
t2 = t; t2$calender = "Jun-Oct"

t$area = ifelse(t$month %in% c("06", "07", "08", "09", "10"), t$area, NA)



