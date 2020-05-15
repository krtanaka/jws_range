rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/")

load(paste0('cold_shoulder_', Sys.Date(), '.Rdata'))

# d = df %>% sample_frac(0.01)
d = df; rm(df)

d$month = substr(as.character(d$time), 6, 7)
d = subset(d, month %in% c("06", "07", "08", "09", "10"
                             # , "11", "12", "01", "02", "03"
                             ))
table(d$month)

map = d %>% 
  # group_by(x, y) %>%
  subset(year %in% c(1982:2019)) %>% 
  
  mutate(period = case_when(year %in% c(1982:2013) ~ "1982-2013",
                            year %in% c(2014:2015) ~ "2014-2015",
                            year %in% c(2016:2019) ~ "2016-2019")) %>% 
  # mutate(period = ifelse(year %in% c(1982:2013), "1982-2013", "2014-2019")) %>% 
  group_by(x, y, period) %>%
  summarise(z = mean(z))

m = map %>% 
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill = z), alpha = 0.9) + 
  geom_smooth(
    data = subset(map, z > 0.01), aes(color = period, group = period),
    method = "gam",
    # method = "loess",
    # method = "auto",
    span = 0.1,
    se = T) + 
  scale_color_viridis_d("") +
  scale_fill_viridis_c("") + 
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) + 
  # facet_wrap(.~ period) +
  xlab("Longitude") + ylab("Latitude")

m

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/figures/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/")

pdf('cold_shoulder_1.pdf', height = 6, width = 6)
print(m)
dev.off()

map = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  group_by(x, y, year) %>%
  summarise(z = mean(z))

m = map %>% 
  subset(year %in% c(1982:2019)) %>%
  ggplot(aes(x, y)) +  
  # geom_tile(aes(fill = z), alpha = 0.9) +
  geom_smooth(
    data = subset(map, z > 0.01),
    # method = "gam",
    # method = "auto",
    method = "loess",
    span = 0.1,
    aes(color = year, group = year), 
    se = F) + 
  scale_color_viridis_c("") +
  scale_fill_viridis_c("") + 
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) + 
  xlab("Longitude") + ylab("Latitude")

m

pdf('cold_shoulder_2.pdf', height = 6, width = 6)
print(m)
dev.off()

lat = d %>% 
  group_by(year) %>% 
  subset(z > 0 & depth > -1000) %>%
  # subset(z > 0) %>%
  # summarise(z = mean(z)) %>%
  summarise(y_mean = sum(z*y)/sum(z))

m = map %>% 
  subset(z > 0) %>%
  subset(year %in% c(1982:2019)) %>%
  ggplot(aes(x, y)) +  
  geom_hline(data = subset(lat, year %in% c(1982:2019)), 
             aes(yintercept = y_mean, color = year), size = 2, alpha = 0.8) + 
  scale_color_viridis_c("") +
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_classic()

m

pdf('cold_shoulder_3.pdf', height = 6, width = 6)
print(m)
dev.off()

#time series
setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/")

load(paste0('cold_shoulder_', Sys.Date(), '.Rdata'))

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
  geom_line(aes(y = rollmean(coldline, 1, na.pad = TRUE))) +
  scale_color_viridis_c("") + 
  # stat_smooth(method = "loess",
  #             color = "gray60", 
  #             fill = "gray60") +
  ylab("JWS coldshoulder latitudinal mean") + 
  xlab("") + 
  ggtitle("10-day running mean") +
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

