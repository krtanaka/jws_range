rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

load("C:/Users/Kisei/jws_range/results/thermal_occupancy.Rdata")
 
df$month = substr(as.character(df$time), 6, 7)
df = subset(df, month %in% c("06", "07", "08", "09", "10"))
table(df$month)

df$period = ""

df$period = ifelse(df$year %in% c(1982:1986), "1982-1986", df$period)
df$period = ifelse(df$year %in% c(1987:1991), "1987-1991", df$period)
df$period = ifelse(df$year %in% c(1992:1996), "1992-1996", df$period)
df$period = ifelse(df$year %in% c(1997:2001), "1997-2001", df$period)
df$period = ifelse(df$year %in% c(2002:2006), "2002-2006", df$period)
df$period = ifelse(df$year %in% c(2007:2011), "2007-2011", df$period)
df$period = ifelse(df$year %in% c(2012:2016), "2012-2016", df$period)
df$period = ifelse(df$year %in% c(2017:2019), "2017-2019", df$period)

map = df %>% 
  group_by(x, y, period, month) %>% 
  summarise(p = mean(p, na.rm = T)) 

df %>%
  group_by(x, y) %>% 
  summarise(p = mean(z)) %>% 
  rasterFromXYZ() %>% 
  area()

grid_cell_size = (521.9257+709.1729)/2

m = map %>%
  subset(period != "") %>%
  ggplot(aes(x, y, fill = p)) +
  geom_raster() +
  scale_fill_viridis_c("") +
  # borders(fill = "gray10") +
  # coord_quickmap(xlim = range(map$x),
  #                ylim = range(map$y)) +
  facet_grid(month~ period)

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/figure 4 total habitat area/')
pdf('map_probabilistic.pdf', height = 8, width = 10)
print(m)
dev.off()


load("C:/Users/Kisei/jws_range/results/thermal_occupancy.Rdata")

t = df %>%
  group_by(time) %>%
  summarise(lat_m = sum(p*y)/sum(p)) %>% 
  mutate(time = as.Date(time))

t$month = substr(as.character(t$time), 6, 7)
t$year = substr(as.character(t$time), 1, 4)

t1 = t; t1$calender = "Jan-Dec"
t2 = t; t2$calender = "Jun-Oct"

t2$lat_m = ifelse(t2$month %in% c("06", "07", "08", "09", "10"), t2$lat_m, NA)

t = rbind(t1, t2)

library(zoo)
library(ggpubr)

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/figure 4 total habitat area/')
pdf('habitat_probablistic_a.pdf', height = 4, width = 8)
ggplot(t, aes(x = time, y = lat_m, color = lat_m)) +
  geom_line(aes(y = rollmean(lat_m, 10, na.pad = TRUE))) +
  scale_color_viridis_c("Lat (deg)") + 
  geom_smooth(method = "loess", span = 0.1) +
  ylab("Latitudinal mean of JWS thermal occupancy (deg)") + 
  ggtitle("10-day running mean") + 
  theme_classic2() + 
  facet_wrap(.~calender, ncol = 2, scales = "free_y")
dev.off()

pdf('habitat_probablistic_b.pdf', height = 4, width = 8)
ggplot(t, aes(x = time, y = lat_m, color = lat_m)) +
  # geom_line(aes(y = rollmean(lat_m, 10, na.pad = TRUE))) +
  scale_color_viridis_c("Lat (deg)") + 
  geom_smooth(method = "loess", span = 0.1) +
  ylab("Latitudinal mean of JWS thermal occupancy (deg)") + 
  ggtitle("10-day running mean") + 
  theme_classic2() + 
  facet_wrap(.~calender, ncol = 2, scales = "free_y")
dev.off()

