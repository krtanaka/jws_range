rm(list = ls())

library(dplyr)
library(ggpubr)
library(raster)
library(scales)
library(zoo)

load("/Users/Kisei/jws_range/data/lat_area.RData")
load("/Users/ktanaka/jws_range/data/lat_area.RData")

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/t_IQR.Rdata")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/t_IQR.Rdata")

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

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")

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

ggplot(t, aes(x = time, y = area, color = area)) +
  geom_line(aes(y = rollmean(area, 10, na.pad = TRUE))) +
  scale_color_viridis_c("km^2") +
  # scale_fill_viridis_d("km^2") + 
  stat_smooth(method = "loess", span = 0.1) +
  ylab("Total Habitat Area (km^2)") + 
  ggtitle("10-day running mean") + 
  facet_wrap(.~ type, ncol = 1)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")

df = merge(df, lat_area)
df = df %>% subset(depth > -1000)

t3 = df %>% 
  group_by(time) %>% 
  subset(y <= 42) %>%
  # subset(x >= -125 & x <= -116) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "22.9° N - 42° N (No WA)")

t4 = df %>% 
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "22.9° N - 47.4° N (California CC LME)")

t = rbind(t3, t4)
d$type <- factor(d$type, levels = c("22.9° N - 47.4° N (California CC LME)", "22.9° N - 42° N (No WA)"))

p1 = ggplot(t, aes(x = time, y = area, color = area)) +
  geom_line(aes(y = rollmean(area, 10, na.pad = TRUE)), alpha = 0.5) +
  stat_smooth(method = "loess", span = 0.1) +
  scale_colour_viridis_c("km^2") + 
  ylab("JWS Thermal Habitat (sq.km)") + 
  ggtitle("10-day running mean. Based on probablistic model") + 
  facet_wrap(.~type, scales = "free_y", nrow = 1) + 
  scale_y_continuous(labels = scientific) + 
  theme_pubr(I(20)) + 
  theme(legend.position = "none")

p2 = ggplot(t, aes(x = time, y = area, color = type, fill = type)) +
  scale_fill_viridis_d("") + 
  scale_colour_viridis_d("") + 
  stat_smooth(method = "loess", span = 0.1, aes(color = type), show.legend = T) +
  ylab("JWS Thermal Habitat (sq.km)") + 
  scale_y_continuous(labels = scientific) + 
  # ggtitle("Loess fit with span = 0.1. Based on probablistic model.") + 
  # facet_wrap(.~type, scales = "free_y", nrow = 1)
  theme_pubr(I(20)) + 
  theme(legend.position = c(0.3, 0.9))

time = subset(t, type == "22.9° N - 42° N (No WA)")

d1 = subset(t, type == "22.9° N - 42° N (No WA)")
d1$step = seq(1, dim(d1)[1], by = 1)
d1 = predict(loess(area~step, d1, span = 0.1), d1$step)
d1 = as.data.frame(d1)
d1$type = "22.9° N - 42° N (No WA)"
d1$type = "22.9° N - 47.4° N (California CC LME)"
d1 = cbind(time$time, d1)
colnames(d1) = c("time", "area", "type")

d2 = subset(t, type == "22.9° N - 47.4° N (California CC LME)")
d2$step = seq(1, dim(d2)[1], by = 1)
d2 = predict(loess(area~step, d2, span = 0.1), d2$step)
d2 = as.data.frame(d2)
d2$type = "22.9° N - 47.4° N (California CC LME)"
d2$type = "22.9° N - 42° N (No WA)"
d2 = cbind(time$time, d2)
colnames(d2) = c("time", "area", "type")

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

setwd("/Users/ktanaka/Desktop")

png(paste0("Fig.4_1_", Sys.Date(), ".png"), height = 7, width = 12, res = 100, units = 'in')
p1
dev.off()

png(paste0("Fig.4_2_", Sys.Date(), ".png"), height = 7, width = 10, res = 100, units = 'in')
# grid.arrange(p1, p2, ncol = 1)
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



