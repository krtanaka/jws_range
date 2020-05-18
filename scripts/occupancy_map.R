rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

dir = Sys.info()[7]
load("C:/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/t_breadth_2020-05-16.Rdata")
 
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

xlims = range(map$x); ylims = range(map$y)
m = map %>% 
  subset(period != "") %>%
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +
  borders(xlim = xlims,
          ylim = ylims,
          fill = "gray10") +
  coord_quickmap(xlim = xlims,
                 ylim = ylims) +
  facet_wrap(.~ period, nrow = 2) + 
  theme_void() + 
  theme(legend.position = "bottom", 
        legend.justification = c(1, 0.1))

m

setwd(paste0('/Users/', dir, '/Dropbox/PAPER Kisei Bia JWS range shift/figures/'))
pdf('thermal_occupancy_map.pdf', height = 5, width = 5)
print(m)
dev.off()


load("C:/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/t_breadth_2020-05-16.Rdata")

df$month = substr(as.character(df$time), 6, 7)

t = df %>%
  group_by(time) %>%
  summarise(
    total = n(),
    good = sum(z>0, na.rm = T),
    prop = good/total) %>% 
  mutate(time = as.Date(time),
         month = substr(as.character(time), 6, 7)) %>% 
  mutate(prop = ifelse(month %in% c("06", "07", "08", "09", "10"), prop, NA))

# lat = df %>% group_by(time) %>% 
#   # summarise(total_habitat = mean(z, na.rm = T))
#   mutate(latm = sum(p*y)/sum(p)) %>% 
#   summarise(thermal_occupancy = mean(latm))

library(zoo)

ggplot(t, aes(x = time, y = prop, color = prop)) +
  geom_line(aes(y=rollmean(prop, 10, na.pad = TRUE))) +
  scale_color_viridis_c("") + 
  stat_smooth(method = "loess", span = 0.1) +
  theme_minimal() + 
  theme(legend.position = "none")

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/')
pdf('thermal_occupancy_lat_mean.pdf', height = 4, width = 10)
print(t)
dev.off()

