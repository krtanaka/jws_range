rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

dir = Sys.info()[7] 

load(paste0('/Users/', dir, '/Dropbox/PAPER Kisei Bia JWS range shift/data/cold_shoulder_', Sys.Date(), '.Rdata'))

df$month = substr(as.character(df$time), 6, 7)
df = subset(df, month %in% c("06", "07", "08", "09", "10"))
table(df$month)

map = df %>% 
  # group_by(x, y) %>%
  subset(year %in% c(1982:2019)) %>% 
  mutate(period = ifelse(year %in% c(1981:2010), "1982-2010", "2010-2019")) %>% 
  group_by(x, y, period) %>%
  summarise(z = mean(z))

m = map %>% 
  ggplot(aes(x, y, color = period, group = factor(period))) + 
  geom_smooth(
    data = subset(map, z > 0),
    method = "gam",
    aes(color = period), 
    se = F) + 
  scale_color_viridis_d("") +
  ggtitle("11.55 deg C thermocline") + 
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) + 
  # facet_wrap(.~ period) +
  xlab("Longitude") + ylab("Latitude") +
  theme_classic() + 
  theme(legend.position = c(0.2, 0.1))

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/')
pdf('cold_shoulder_1.pdf', height = 6, width = 4)
print(m)
dev.off()

map = df %>% 
  subset(year %in% c(1982:2019)) %>% 
  group_by(x, y, year) %>%
  summarise(z = mean(z))

m = map %>% 
  subset(year %in% c(1982:2019)) %>%
  ggplot(aes(x, y, group = year)) +  
  geom_smooth(
    data = subset(map, z > 0),
    method = "lm",
    aes(color = year), 
    se = F, 
    alpha = 0.5) + 
  scale_color_viridis_c("") +
  ggtitle("11.55 deg C thermocline") + 
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_classic() + 
  theme(legend.position = c(0.15, 0.18))

pdf('cold_shoulder_2.pdf', height = 6, width = 4)
print(m)
dev.off()

lat = df %>% 
  group_by(year) %>% 
  subset(z > 0 & depth > -1000) %>%
  # summarise(z = mean(z)) %>%
  summarise(y_mean = sum(z*y)/sum(z))

m = map %>% 
  subset(z > 0) %>%
  subset(year %in% c(1982:2019)) %>%
  ggplot(aes(x, y, color = year)) +  
  geom_hline(data = subset(lat, year %in% c(1982:2019)), 
             aes(yintercept = y_mean, color = year), size = 2, alpha = 0.8) + 
  scale_color_viridis_c("") +
  ggtitle("11.55 deg C thermocline latitudinal mean") + 
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_classic() + 
  theme(legend.position = c(0.15, 0.18))

pdf('cold_shoulder_3.pdf', height = 6, width = 4)
print(m)
dev.off()

#time series
load(paste0('/Users/', dir, '/Dropbox/PAPER Kisei Bia JWS range shift/data/cold_shoulder_', Sys.Date(), '.Rdata'))
df = df %>% subset(z > 0 & depth > -1000)

df$year = substr(as.character(df$time), 1, 4)
df$month = substr(as.character(df$time), 6, 7)

t1 = df %>% 
  group_by(time) %>% 
  mutate(latm = sum(z*y)/sum(z)) %>% 
  summarise(coldline = mean(latm)) %>% 
  mutate(time = as.Date(time))

t2 = t1 %>% 
  mutate(year = substr(as.character(time), 1, 4)) %>% 
  group_by(year) %>% 
  summarise(coldline = mean(coldline))


lat$time = as.Date(lat$time)

year = lat %>% group_by(year) %>% summarise(coldline = mean(coldline))

lat$coldline = ifelse(lat$month %in% c("06", "07", "08", "09", "10"), lat$coldline, NA)

library(zoo)

pdf('cold_shoulder_time_series_1.pdf', height = 6, width = 10)
pdf('cold_shoulder_time_series_2.pdf', height = 5, width = 10)

ggplot(t1, aes(x = time, y = coldline, color = coldline)) +
  geom_line(aes(y = rollmean(coldline, 10, na.pad = TRUE))) +
  scale_color_viridis_c("") + 
  # stat_smooth(method = "loess",
  #             color = "gray60", 
  #             fill = "gray60") +
  ylab("11.55 deg C thermocline latitudinal mean") + 
  xlab("") + 
  # ggtitle("10-day running mean") + 
  # ggtitle("10-day running mean, June-October") + 
  # theme_minimal(I(20)) +
  theme(legend.position = "none")

ggplot(t2, aes(x = year, y = coldline, color = coldline)) +
  geom_point() +
  scale_color_viridis_c("") + 
  stat_smooth(method = "loess",
              color = "gray60",
              fill = "gray60") +
  ylab("11.55 deg C thermocline latitudinal mean") + 
  xlab("") + 
  # theme_minimal(I(20)) +
  theme(legend.position = "none")

dev.off()

