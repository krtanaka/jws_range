rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

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

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")

df$month = substr(as.character(df$time), 6, 7)
df$year = substr(as.character(df$time), 1, 4)
df$time_step = substr(as.character(df$time), 1, 7)
df$time_step = df$year

p1 = df %>%
  subset(year %in% c(1982:2019)) %>% 
  group_by(x, y) %>% 
  summarise(p = mean(p, na.rm = T)) %>% 
  mutate(type = "mean") %>% 
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("", limits = c(0,1), breaks = c(0,0.5, 1)) +  
  borders(fill = "gray10") +
  coord_quickmap(xlim = range(df$x), ylim = range(df$y)) +
  scale_x_longitude(xmin=-180, xmax=180, step=5) +
  scale_y_latitude(ymin=-180, ymax=180, step=5) +
  theme_minimal() + 
  theme(legend.position = c(0.15, 0.2)) + 
  ggtitle("Mean 1982-2019")

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

# change$p = change$p*38

p2 = change %>% 
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("",
                       limit = c(max(change$p)*-1, max(change$p))) +  
  borders(fill = "gray10") +
  coord_quickmap(xlim = range(df$x), ylim = range(df$y)) +
  scale_x_longitude(xmin=-180, xmax=180, step=5) +
  scale_y_latitude(ymin=-180, ymax=180, step=5) +
  theme_minimal() + 
  theme(legend.position = c(0.15, 0.2)) + 
  ggtitle("Linear Trend 1982-2019")

p3 = df %>%
  subset(time %in% c("2005-03-16")) %>%
  group_by(x, y, time) %>%
  summarise(p = mean(p, na.rm = T)) %>% 
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +  
  borders(fill = "gray10") +
  coord_quickmap(xlim = range(df$x), ylim = range(df$y)) +
  scale_x_longitude(xmin=-180, xmax=180, step=5) +
  scale_y_latitude(ymin=-180, ymax=180, step=5) +
  theme_minimal() + 
  theme(legend.position = c(0.15, 0.2))+ 
  ggtitle("2005-03-16")

p4 = df %>%
  subset(time %in% c("2015-09-15")) %>%
  group_by(x, y, time) %>%
  summarise(p = mean(p, na.rm = T)) %>% 
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +  
  borders(fill = "gray10") +
  coord_quickmap(xlim = range(df$x), ylim = range(df$y)) +
  scale_x_longitude(xmin=-180, xmax=180, step=5) +
  scale_y_latitude(ymin=-180, ymax=180, step=5) +
  theme_minimal() + 
  theme(legend.position = c(0.15, 0.2))+ 
  ggtitle("2015-09-15")

pdf('~/Desktop/s6.pdf', height = 10, width = 15)
# gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
cowplot::plot_grid(p1, p2, p3, p4, ncol = 4)
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
