rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

load("C:/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/t_breadth_2020-05-16.Rdata")

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
  group_by(x, y, period) %>% 
  summarise(p = mean(z)) 
# mutate(y_mean = sum(z*y)/sum(z))

df %>%
  group_by(x, y) %>% 
  summarise(p = mean(z)) %>% 
  rasterFromXYZ() %>% 
  area()

grid_cell_size = (521.9257+709.1729)/2

xlims = range(map$x); ylims = range(map$y)

m = map %>% 
  subset(period != "") %>%
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +
  borders(fill = "gray10") +
  coord_quickmap(xlim = xlims,
                 ylim = ylims) +
  facet_wrap(.~ period, nrow = 2) + 
  theme_void()

m

load("C:/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/t_breadth_2020-05-16.Rdata")

t = df %>%
  group_by(time) %>%
  summarise(
    total = n(),
    good = sum(z>0, na.rm = T),
    prop = good/total) %>% 
  mutate(total = total*grid_cell_size,
         good = good*grid_cell_size,
         time = as.Date(time))

t$month = substr(as.character(t$time), 6, 7)
t$year = substr(as.character(t$time), 1, 4)

t1 = t; t1$calender = "Jan-Dec"
t2 = t; t2$calender = "Jun-Oct"

t2$good = ifelse(t2$month %in% c("06", "07", "08", "09", "10"), t2$good, NA)

t = rbind(t1, t2)
# t$good = log10(t$good)

library(zoo)
library(ggpubr)

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/figure 4 total habitat area/')
pdf('total_habitat_area_binary.pdf', height = 4, width = 8)
ggplot(t, aes(x = time, y = good, color = good)) +
  # geom_line(aes(y = rollmean(good, 10, na.pad = TRUE))) +
  scale_color_viridis_c("km^2") + 
  stat_smooth(method = "loess", span = 0.1) +
  ylab("Total Habitat Area (km^2)") + 
  ggtitle("10-day running mean") + 
  theme_classic2() + 
  facet_wrap(.~calender, ncol = 2, scales = "free_y")
dev.off()

