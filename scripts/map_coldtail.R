rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/")

load("t_coldtail.Rdata")

d = df %>% sample_frac(0.5); rm(df)
# d = df; rm(df)

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/figures/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/supplement/")

map = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  mutate(month = substr(as.character(time), 6, 7)) %>% 
  # subset(month %in% c("06", "07", "08", "09", "10")) %>% 
  mutate(period = case_when(year %in% c(1982:2013) ~ "1982-2013",
                            year %in% c(2014:2015) ~ "2014-2015",
                            year %in% c(2016:2019) ~ "2016-2019")) %>%
  group_by(x, y, period) %>%
  summarise(z = mean(z),
            d = mean(depth, na.rm = T))


c1 = map %>% subset(period == "1982-2013") %>% group_by(x) %>% summarise(max_z = max(z, na.rm = T))
c2 = map %>% subset(period == "2014-2015") %>% group_by(x) %>% summarise(max_z = max(z, na.rm = T))
c3 = map %>% subset(period == "2016-2019") %>% group_by(x) %>% summarise(max_z = max(z, na.rm = T))

c1 = merge(subset(map, period == "1982-2013") , c1)
c2 = merge(subset(map, period == "2014-2015") , c2)
c3 = merge(subset(map, period == "2016-2019") , c3)

c1$zz = ifelse(c1$z < c1$max_z, 0, 1) 
c2$zz = ifelse(c2$z < c2$max_z, 0, 1)
c3$zz = ifelse(c3$z < c3$max_z, 0, 1) 

c = rbind(c1, c2, c3)

lm = map %>% 
  # subset(d > -1000) %>%
  group_by(period) %>% 
  summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T))

c %>% 
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = z)) + 
  geom_hline(data = lm, aes(yintercept = y, group = period, color = "red"), show.legend = F) + 
  # geom_smooth(data = subset(c, zz > 0 & x < -121 & y > 35 & y < 42), 
  #             aes(color = "red", group = period), 
  #             method = "auto", span = 0.1, se = T, size = 3) + 
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) +
  facet_wrap(.~ period, scales = "fixed") +
  scale_color_discrete("") +
  scale_fill_viridis_c("") + 
  theme_classic() + 
  xlab("Longitude") + ylab("Latitude")

# m = map %>% 
#   ggplot(aes(x, y)) + 
#   geom_tile(aes(fill = z), alpha = 1) + 
#   # geom_smooth(
#   #   data = subset(map, z > 0.01), aes(color = period, group = period),
#   #   method = "gam",
#   #   span = 0.1,
#   #   se = T) +
#   geom_smooth(
#     data = subset(map, z > 0), aes(color = period, group = period),
#     method = "gam",
#     span = 0.1,
#     se = F) +
#   # geom_smooth(
#   #   data = subset(map, z > 0.01), aes(color = period, group = period),
#   #   method = "auto",
#   #   span = 0.1,
#   #   se = T) + 
#   scale_color_discrete("") +
#   scale_fill_viridis_c("") + 
#   borders(fill = "gray20") +
#   coord_quickmap(xlim = c(-127, -115),
#                  ylim = c(30, 45)) +
#   # coord_quickmap(xlim = range(map$x),
#   #                ylim = range(map$y)) +
#   # facet_wrap(.~ period, scales = "fixed") +
#   xlab("Longitude") + ylab("Latitude")

# m

pdf('/Users/Kisei/Desktop/cold_tail.pdf', height = 6, width = 10)
print(m)
dev.off()

map = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  group_by(x, y, year) %>%
  summarise(z = mean(z))

m = map %>% 
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = z), alpha = 1) +
  # geom_smooth(
  #   data = subset(map, z > 0.01),
  #   method = "gam",
  #   # method = "loess",
  #   # method = "auto",
  #   span = 0.1,
  #   se = T) +
  scale_color_viridis_d("") +
  scale_fill_viridis_c("") + 
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) +
  # coord_quickmap(xlim = range(map$x),
  #                ylim = range(map$y)) + 
  facet_wrap(.~ year, ncol = 10) +
  xlab("Longitude") + ylab("Latitude")

m

pdf('cold_shoulder_2.pdf', height = 6, width = 18)
print(m)
dev.off()

lat = d %>% 
  subset(z > 0 & depth > -1000) %>%
  group_by(year) %>% 
  summarise(y_mean = sum(z*y)/sum(z))

map %>% 
  ggplot(aes(x, y)) +  
  geom_hline(data = subset(lat, year %in% c(1982:2019)), 
             aes(yintercept = y_mean, color = year), size = 2, alpha = 0.8) + 
  scale_color_viridis_c("") +
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_classic()


#time series
setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/")

load('t_coldtail.Rdata')

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

t3 = t1 %>% 
  mutate(month = substr(as.character(time), 6, 7)) %>% 
  mutate(coldline = ifelse(month %in% c("06", "07", "08", "09", "10"), coldline, NA))

library(zoo)
pdf('cold_shoulder_time_series_1.pdf', height = 6, width = 8)

ggplot(t3, aes(x = time, y = coldline, color = coldline)) +
  geom_line(aes(y = rollmean(coldline, 10, na.pad = TRUE))) +
  scale_color_viridis_c("") + 
  stat_smooth(method = "loess",
              span = 0.6,
              color = "gray60",
              fill = "gray60") +
  ylab("JWS coldshoulder latitudinal mean") + 
  xlab("") + 
  # ggtitle("10-day running mean") +
  ggtitle("10-day running mean, June-October") +
  theme_classic() +
  theme(legend.position = "none")

dev.off()

