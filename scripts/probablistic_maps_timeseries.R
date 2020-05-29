rm(list = ls())

library(dplyr)
library(ggpubr)
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

setwd("/Users/Kisei/Desktop")

png(paste0("Fig.4_probabilistic_maps_", Sys.Date(), ".png"), res = 500, height = 10, width = 10, units = "in")

df %>%
  subset(month %in% c("06", "07", "08")) %>%
  subset(year %in% c(1984, 2019)) %>%
  group_by(x, y, year, month) %>% 
  summarise(p = mean(p, na.rm = T)) %>% 
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +  
  borders(fill = "gray10") +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  ylab("") + xlab("") + 
  theme_minimal(I(20)) +
  facet_grid(month~year) + 
  scale_x_continuous(breaks = round(seq(min(df$x), max(df$x), by = 10),0)) + 
  theme(legend.position = "right")

dev.off()

####################################
### lat-bin specific time series ###
####################################

t0 = df %>% 
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "22.9° N - 47.4° N (Cali CC LME)")

t1 = df %>% 
  group_by(time) %>% 
  subset(y >= 22.9 & y <= 33.4) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "22.9° N - 33.4° N (S.boundary Cali CC LME - Point Conception)")

t2 = df %>% 
  group_by(time) %>% 
  subset(y >= 33.4 & y <= 37.4) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "33.4° N - 37.4° N (Point Conception - San Francisco)")

t3 = df %>% 
  group_by(time) %>% 
  subset(y >= 37.4) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "37.4° N - 47.4° N (San Francisco - N.boundary Cali CC LME)")

full_time_series = t0$time

t3 = merge(t0$time, t3, all = T)


t = rbind(
  # t0,
  t1, t2, t3)

t$type <- factor(t$type, levels = c(
  # "22.9° N - 47.4° N (S.boundary Cali CC LME - N.boundary Cali CC LME)",
  "37.4° N - 47.4° N (San Francisco - N.boundary Cali CC LME)",
  "33.4° N - 37.4° N (Point Conception - San Francisco)",
  "22.9° N - 33.4° N (S.boundary Cali CC LME - Point Conception)"))

# p1 = ggplot(t, aes(x = time, y = area, color = area)) +
#   geom_line(aes(y = rollmean(area, 10, na.pad = TRUE)), alpha = 0.5) +
#   stat_smooth(method = "loess", span = 0.1) +
#   scale_colour_viridis_c("km^2") + 
#   ylab("JWS Thermal Habitat (sq.km)") + 
#   ggtitle("10-day running mean. Based on probablistic model") + 
#   facet_wrap(.~type, scales = "free_y", nrow = 1) + 
#   scale_y_continuous(labels = scientific) + 
#   theme_pubr(I(20)) + 
#   theme(legend.position = "none")
   
p1 = ggplot(t0, aes(x = time, y = area, color = "type", fill = "type")) +
  stat_smooth(method = "loess", span = 0.1, aes(color = "type"), show.legend = T) +
  ylab("JWS Thermal Habitat (sq.km)") + 
  scale_y_continuous(labels = scientific) + 
  facet_wrap(.~type, scales = "free_y", ncol = 1) +
  theme_pubr(I(10)) + 
  theme(legend.position = "none")

p2 = ggplot(t, aes(x = time, y = area, color = type, fill = type)) +
  scale_fill_viridis_d("") +
  scale_colour_viridis_d("") +
  stat_smooth(method = "loess", span = 0.1, aes(color = type), show.legend = T) +
  ylab("") + 
  scale_y_continuous(labels = scientific) + 
  facet_wrap(.~type, scales = "free_y", ncol = 1) + 
  theme_pubr(I(10)) + 
  theme(legend.position = "none")

png(paste0("Fig.4_binned_lat_timeseries_", Sys.Date(), ".png"), height = 7, width = 10, res = 300, units = 'in')
cowplot::plot_grid(p1, p2, ncol = 2)
dev.off()


#########################
### stacked area plot ###
#########################

t = rbind(t0, t1, t2, t3)

time = subset(t, type == "22.9° N - 47.4° N (Cali CC LME)")

d1 = subset(t, type == "22.9° N - 33.4° N (S.boundary Cali CC LME - Point Conception)")
d1$step = seq(1, dim(d1)[1], by = 1)
d1 = predict(loess(area~step, d1, span = 0.1), d1$step)
d1 = as.data.frame(d1)
d1$type = "22.9° N - 33.4° N (S.boundary Cali CC LME - Point Conception)"
d1 = cbind(time$time, d1)
colnames(d1) = c("time", "area", "type")

d2 = subset(t, type == "33.4° N - 37.4° N (Point Conception - San Francisco)")
d2$step = seq(1, dim(d2)[1], by = 1)
d2 = predict(loess(area~step, d2, span = 0.1), d2$step)
d2 = as.data.frame(d2)
d2$type = "33.4° N - 37.4° N (Point Conception - San Francisco)"
d2 = cbind(time$time, d2)
colnames(d2) = c("time", "area", "type")

d3 = subset(t, type == "37.4° N - 47.4° N (San Francisco - N.boundary Cali CC LME)")
d3$step = seq(1, dim(d3)[1], by = 1)
d3 = predict(loess(area~step, d3, span = 0.1), d3$step)
d3 = as.data.frame(d3)
d3$type = "37.4° N - 47.4° N (San Francisco - N.boundary Cali CC LME)"
d3 = cbind(time$time, d3)
colnames(d3) = c("time", "area", "type")

# d2$area = d2$area - d1$area

d = rbind(d1, d2)

d$type <- factor(d$type, levels = c("22.9° N - 47.4° N (California CC LME)", "22.9° N - 42° N (No WA)"))

p3 = d %>% ggplot(aes(x =time, y=area, fill=rev(type))) + 
  geom_area(position = "identity", alpha = 0.8) +
  ylab("JWS Thermal Habitat (sq.km)") + 
  scale_fill_viridis_d("") + 
  theme_pubr(I(20)) + 
  scale_y_continuous(labels = scientific) + 
  theme(legend.position = c(0.25, 0.95))

setwd("/Users/Kisei/Desktop")

png(paste0("Fig.4_1_", Sys.Date(), ".png"), height = 7, width = 12, res = 100, units = 'in')
p1
dev.off()

png(paste0("Fig.4_2_", Sys.Date(), ".png"), height = 10, width = 10, res = 300, units = 'in')
p2
dev.off()

png(paste0("Fig.4_3_", Sys.Date(), ".png"), height = 7, width = 10, res = 100, units = 'in')
# grid.arrange(p1, p2, ncol = 1)
p3
dev.off()

df %>%
  subset(year %in% c(1982:2019)) %>%
  subset(y <= 42) %>%
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



