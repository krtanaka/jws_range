rm(list = ls())

library(dplyr)
library(ggpubr)
library(raster)
library(gridExtra)

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/")

load("t_coldtail.Rdata")

# reduce file size
d = df %>% sample_frac(0.01); rm(df)
# d = df; rm(df)

### time series ###
# t1 = d %>% 
#   mutate(month = substr(as.character(time), 6, 7)) %>% 
#   subset(year %in% c(1982:2019)) %>% 
#   group_by(year) %>% 
#   summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T))%>% 
#   mutate(group = "Jan-Dec")
# 
# t2 = d %>% 
#   mutate(month = substr(as.character(time), 6, 7)) %>% 
#   subset(year %in% c(1982:2019)) %>% 
#   subset(depth > -1000) %>%
#   group_by(year) %>% 
#   summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T))%>% 
#   mutate(group = "Jan-Dec, Depth < 1000 m")
# 
# t3 = d %>% 
#   mutate(month = substr(as.character(time), 6, 7)) %>% 
#   subset(year %in% c(1982:2019)) %>% 
#   subset(month %in% c("06", "07", "08", "09", "10")) %>%
#   group_by(year) %>% 
#   summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T)) %>% 
#   mutate(group = "June-Oct")
# 
# t4 = d %>% 
#   mutate(month = substr(as.character(time), 6, 7)) %>% 
#   subset(year %in% c(1982:2019)) %>% 
#   subset(depth > -1000) %>%
#   subset(month %in% c("06", "07", "08", "09", "10")) %>%
#   group_by(year) %>% 
#   summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T)) %>% 
#   mutate(group = "June-Oct, Depth < 1000 m")

t1 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(depth > -1000) %>%
  group_by(year) %>% 
  summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T)) %>% 
  mutate(group = "22.9° N - 47.4° N")

t2 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(depth > -1000) %>%
  subset(y <= 42) %>%
  group_by(year) %>% 
  summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T)) %>% 
  mutate(group = "22.9° N - 42° N")

rbind(t1, t2) %>% 
  ggplot(aes(year, y, color = group)) + 
  geom_point() + geom_line() + 
  facet_wrap(.~ group, scales = "fixed") +
  ylab("JWS coldtail latitudinal center of gravity (dec deg)") + 
  theme_pubr() +
  theme(legend.position = "none")

t1 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(depth > -1000) %>%
  group_by(year) %>% 
  summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T))%>% 
  mutate(period = case_when(year %in% c(1982:2014) ~ "1982-2014",
                            year %in% c(2014:2019) ~ "2014-2019"),
         group = "22.9° N - 47.4° N")

t2 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(depth > -1000) %>%
  subset(y <= 42) %>%
  group_by(year) %>% 
  summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T))%>% 
  mutate(period = case_when(year %in% c(1982:2014) ~ "1982-2014",
                            year %in% c(2014:2019) ~ "2014-2019"),
         group = "22.9° N - 42° N")

l1 = t1 %>% group_by(period) %>% summarise(z = mean(y)); l1
l2 = t2 %>% group_by(period) %>% summarise(z = mean(y)); l2

forecast_1 = data.frame(year = 2020, 
                      y = (37.02439 + 37.03631)/2,
                      period = "forecast")

forecast_2 = data.frame(year = 2020, 
                      y = (33.61341 + 33.81032)/2,
                      period = "forecast")

t = rbind(t, forecast)

p1 = t1 %>%  
  ggplot(aes(year, y)) + 
  geom_point() + 
  geom_point(data = forecast_1, aes(year, y, color = period), size = 5) +
  geom_line() +
  ylab("JWS coldtail latitudinal center of gravity (dec deg)") + xlab("") + 
  geom_segment(aes(x = 1982, xend = 2014, y = l1$z[1], yend = l1$z[1], color = "1982-2014"), show.legend = T, size = 2) + 
  geom_segment(aes(x = 2014, xend = 2019, y = l1$z[2], yend = l1$z[2], color = "2014-2019"), show.legend = F, size = 2) + 
  theme_pubr(I(15)) + 
  ggtitle(unique(t1$group)) + 
  theme(legend.position = c(0.2, 0.9), 
        legend.title = element_blank())

p2 = t2 %>%  
  ggplot(aes(year, y)) + 
  geom_point() + 
  geom_point(data = forecast_2, aes(year, y, color = period), size = 5) +
  geom_line() +
  ylab("JWS coldtail latitudinal center of gravity (dec deg)") + xlab("") + 
  geom_segment(aes(x = 1982, xend = 2014, y = l2$z[1], yend = l2$z[1], color = "1982-2014"), show.legend = T, size = 2) + 
  geom_segment(aes(x = 2014, xend = 2019, y = l2$z[2], yend = l2$z[2], color = "2014-2019"), show.legend = F, size = 2) + 
  theme_pubr(I(15)) + 
  ggtitle(unique(t2$group)) + 
  theme(legend.position = c(0.2, 0.9), 
        legend.title = element_blank())

cowplot::plot_grid(p1, p2)

t1 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(depth > -1000) %>%
  group_by(year) %>% 
  summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T))%>% 
  mutate(period = case_when(year %in% c(1982:2014) ~ "1982-2014",
                            year %in% c(2014:2019) ~ "2014-2020"))

forecast_1 = data.frame(year = 2020, 
                        y = (37.02439 + 37.03631)/2,
                        period = "2014-2020")

t = rbind(t1, forecast_1)


l = t %>% group_by(period) %>% summarise(z = mean(y)); l

l$year = c(1998, 2017)

t %>%  
  ggplot(aes(year, y)) + 
  geom_point() + 
  geom_point(data = forecast_1, aes(year, y), size = 10, shape = 1) +
  geom_line() +
  ylab("JWS coldtail latitudinal center of gravity (dec deg)") + xlab("") + 
  geom_segment(aes(x = 1982, xend = 2014, y = l$z[1], yend = l$z[1]), color = "blue",show.legend = T, size = 2) + 
  geom_segment(aes(x = 2014, xend = 2020, y = l$z[2], yend = l$z[2]), color = "red", show.legend = F, size = 2) + 
  geom_label(data=l, 
             aes(y = z, x = year, 
                 label = period),
             nudge_x = c(0, 0),
             nudge_y = c(0, 0)) +
  theme_pubr(I(15)) +
  theme(legend.position = c(0.2, 0.9),
        legend.title = element_blank())

### map ###
p2 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(y <= 46.16) %>% #no WA
  mutate(period = case_when(year %in% c(1982:2014) ~ "1982-2014",
                            year %in% c(2014:2019) ~ "2014-2019")) %>%
  group_by(x, y, period) %>%
  summarise(z = mean(z),
            d = mean(depth, na.rm = T))%>% 
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill = z)) + 
  # geom_hline(data = lat, aes(yintercept = z, group = period, color = "red"),
  #            show.legend = F, size = 2) +
  borders(fill = "gray20") +
  ylab("") + xlab("") + 
  coord_quickmap(xlim = c(-130, -115),
                 ylim = c(28, 46.16)) +
  # coord_quickmap(xlim = range(d$x),
  #                ylim = range(d$y)) +
  facet_wrap(.~ period, scales = "fixed") +
  scale_color_discrete("") +
  scale_fill_viridis_c("") + 
  scale_x_continuous(breaks = round(seq(min(c$x), max(c$x), by = 10),0)) + 
  theme_pubr(I(15)) + 
  theme(legend.position = "right", 
        legend.justification = c(1,1))

setwd("/Users/ktanaka/Desktop/")
setwd("/Users/Kisei/Desktop/")

png("Fig.3a.png", width = 5, height = 6, units = "in", res = 100)
p1
dev.off()

png("Fig.3b.png", width = 7, height = 5, units = "in", res = 100)
p2
dev.off()































map = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(y < 42) %>% 
  mutate(month = substr(as.character(time), 6, 7)) %>% 
  # subset(month %in% c("06", "07", "08", "09", "10")) %>%
  mutate(period = case_when(year %in% c(1982:2013) ~ "1982-2013",
                            # year %in% c(2014:2015) ~ "2014-2015",
                            year %in% c(2014:2019) ~ "2016-2019")) %>%
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

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/figures/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/")

png("Fig.4.png", width = 6, height = 3, units = "in", res = 100)

c %>% 
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill = z)) + 
  geom_hline(data = lm, aes(yintercept = y, group = period, color = "red"), show.legend = F, size = 2) + 
  # geom_smooth(data = subset(c, zz > 0 
  #                           # & x < -112
  #                           # & y > 35 
  #                           # & y < 42
  #                           ),
  #             aes(color = "red", group = period),
  #             method = "loess", span = 0.1) +
  borders(fill = "gray20") +
  coord_quickmap(xlim = c(-127, -115),
                 ylim = c(30, 45)) +
  # coord_quickmap(xlim = range(c$x),
  #                ylim = range(c$y)) +
  facet_wrap(.~ period, scales = "fixed") +
  scale_color_discrete("") +
  scale_fill_viridis_c("") + 
  theme_classic() + 
  scale_x_continuous(breaks = round(seq(min(c$x), max(c$x), by = 5),0)) + 
  # ggtitle("June-October") +
  ggtitle("January-December, depth unrestricted") +
  xlab("") + ylab("")

dev.off()

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

