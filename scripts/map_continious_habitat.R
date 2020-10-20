rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(marmap)
library(metR)

# get bathymetry data
b = getNOAA.bathy(lon1 = -126, lon2 = -109, lat1 = 22.9, lat2 = 47.4, resolution = 4)
b = fortify.bathy(b)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")

df$month = substr(as.character(df$time), 6, 7)
df$year = substr(as.character(df$time), 1, 4)
df$time_step = substr(as.character(df$time), 1, 7)
df$time_step = df$year

p0 = df %>%
  subset(year %in% c(1982:2019)) %>% 
  group_by(x, y) %>% 
  summarise(p = mean(p, na.rm = T)) 

p0 = ggplot() + 
  geom_contour(data = b, 
               aes(x = x, y = y, z = z),
               breaks = c(-1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000),
               size = c(0.3),
               colour = "grey") +
  geom_tile(data = p0, aes(x, y, fill = p, height = 0.3, width = 0.3)) + 
  scale_fill_viridis_c("", limits = c(0, 1), breaks = c(0, 1)) +  
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  annotate(geom = "text", x = -111, y = 47, label = "1982-2019 mean", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  ylab("") + xlab("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14))

map = df %>%
  subset(year %in% c(1982:2019)) %>% 
  group_by(x, y, time_step) %>% #monthly time step
  summarise(p = mean(p, na.rm = T)) 
  # summarise(p = sum(p, na.rm = T))

map = as.data.frame(map)

time_list = unique(map$time_step)

mm = NULL

for (t in 1:length(time_list)) {
  
  # t = 1
  
  m = subset(map, time_step == time_list[[t]])
  m = m[,c("p")]
  mm = cbind(mm, m)
  
}

mm = as.data.frame(mm)
colnames(mm) = unique(map$time_step)

xy = subset(map, time_step == time_list[[1]])
xy = xy[,1:2]

change = cbind(xy, mm)

betaf = function(vec){
  
  n = length(vec)
  
  beta = lm(vec ~ seq(1:n))$coef[2] #this is for 1982-2011
  
  # p = summary(lm(vec ~ seq(1:36)))$ coefficients [2,4]
  return(beta) # beta gives you a slope, if you want p-value, change it to p
  #   return(p) # beta gives you a slope, if you want p-value, change it to p
  
}

res = as.data.frame(apply(change[, 3:length(names(change))], 1, betaf)) 
change = cbind(change[,1:2], res)
colnames(change)[3] = "p"

change$p = change$p*38

p1 = ggplot() + 
  geom_contour(data = b, 
               aes(x = x, y = y, z = z),
               breaks = c(-1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000),
               size = c(0.3),
               colour = "grey") +
  geom_tile(data = change, aes(x, y, fill = p, height = 0.3, width = 0.3)) + 
  scale_fill_viridis_c("") +  
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  annotate(geom = "text", x = -111, y = 47, label = "1982-2019 change", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  ylab("") + xlab("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14))

p2 = df %>%
  subset(time %in% c("2005-03-16")) %>%
  group_by(x, y, time) %>%
  summarise(p = mean(p, na.rm = T)) 

p2 = ggplot() + 
  geom_contour(data = b, 
               aes(x = x, y = y, z = z),
               breaks = c(-1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000),
               size = c(0.3),
               colour = "grey") +
  geom_tile(data = p2, aes(x, y, fill = p, height = 0.3, width = 0.3)) + 
  scale_fill_viridis_c("", limits = c(0, 1), breaks = c(0, 1)) +  
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  annotate(geom = "text", x = -111, y = 47, label = "2005-03-16", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  annotate(geom = "text", x = -111, y = 45.8, label = "~271000 sq.km", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  ylab("") + xlab("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14))

p3 = df %>%
  subset(time %in% c("2015-09-15")) %>%
  group_by(x, y, time) %>%
  summarise(p = mean(p, na.rm = T))


p3 = ggplot() + 
  geom_contour(data = b, 
               aes(x = x, y = y, z = z),
               breaks = c(-1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000),
               size = c(0.3),
               colour = "grey") +
  geom_tile(data = p3, aes(x, y, fill = p, height = 0.3, width = 0.3)) + 
  scale_fill_viridis_c("", limits = c(0, 1), breaks = c(0, 1)) +  
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  annotate(geom = "text", x = -111, y = 47, label = "2015-09-15", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  annotate(geom = "text", x = -111, y = 45.8, label = "~59100 sq.km", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  ylab("") + xlab("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14))


pdf('~/Desktop/s6.pdf', height = 5, width = 10)
gridExtra::grid.arrange(p2, p3, ncol = 2)
dev.off()

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
  subset(month %in% c("06", "07", "08", "09", "10")) %>% 
  ggplot(aes(x, y, fill = p)) +
  geom_raster() +
  scale_fill_viridis_c("") +
  # borders(fill = "gray10") +
  # coord_quickmap(xlim = range(map$x),
  #                ylim = range(map$y)) +
  facet_grid(month~ period)

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/figure 4 total habitat area/')
pdf('map_probabilistic_b.pdf', height = 8, width = 10)
print(m)
dev.off()


load("/Users/Kisei/jws_range/results/thermal_occupancy.Rdata")
load("/Users/ktanaka/jws_range/results/thermal_occupancy.Rdata")

t = df %>%
  group_by(time) %>%
  summarise(p = sum(p*y)/sum(p)) %>% 
  mutate(time = as.Date(time))

t = df %>%
  group_by(time) %>%
  summarise(p = sum(p)) %>% 
  mutate(time = as.Date(time))

t = df %>%
  mutate(p = p*grid_cell_size) %>% 
  group_by(time) %>% 
  summarise(p = sum(p)) %>% 
  mutate(time = as.Date(time))

t$month = substr(as.character(t$time), 6, 7)
t$year = substr(as.character(t$time), 1, 4)

t1 = t; t1$calender = "Jan-Dec"
t2 = t; t2$calender = "Jun-Oct"

t2$p = ifelse(t2$month %in% c("06", "07", "08", "09", "10"), t2$p, NA)

t = rbind(t1, t2)

library(zoo)
library(ggpubr)

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/figure 4 total habitat area/')
pdf('habitat_probablistic_a.pdf', height = 4, width = 8)
t1 = ggplot(t, aes(x = time, y = p, color = p)) +
  geom_line(aes(y = rollmean(p, 10, na.pad = TRUE))) +
  scale_color_viridis_c("") + 
  geom_smooth(method = "loess", span = 0.1) +
  # ylab("Latitudinal mean of JWS thermal occupancy (deg)") + 
  ylab("Total Habitat Area (km^2)") +
  # ylab("summed probability") +
  ggtitle("10-day running mean") + 
  theme_classic2() + 
  facet_wrap(.~calender, ncol = 2, scales = "free_y") + 
  theme(legend.position = "none",
        legend.justification = c(1,1))
dev.off()

pdf('habitat_probablistic_b.pdf', height = 4, width = 8)
t2 = ggplot(t, aes(x = time, y = p, color = p)) +
  # geom_line(aes(y = rollmean(p, 10, na.pad = TRUE))) +
  scale_color_viridis_c("Lat (deg)") + 
  geom_smooth(method = "loess", span = 0.1) +
  # ylab("Latitudinal mean of JWS thermal occupancy (deg)") + 
  ylab("Total Habitat Area (km^2)") + 
  ggtitle("10-day running mean") + 
  theme_classic2() + 
  facet_wrap(.~calender, ncol = 2, scales = "free_y")
dev.off()

png('/Users/ktanaka/Desktop/fig.4_probablistic_draft.png', height = 5, width = 7, res = 100, units = "in")
grid.arrange(t1, t2, ncol = 1)
dev.off()
