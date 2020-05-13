rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

dir = Sys.info()[7]
setwd(paste0('/Users/', dir, '/jws_range/results/'))
load(paste0('thermal_occupancy_', Sys.Date(), '.Rdata'))

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
  summarise(p = mean(p)) 
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

load('/Users/Kisei/jws_range/results/thermal_occupancy_2020-05-11.Rdata')

# df$month = substr(as.character(df$time), 6, 7)
# df = subset(df, month %in% c("06", "07", "08", "09", "10"))
# table(df$month)

lat = df %>% group_by(time) %>% 
  mutate(latm = sum(p*y)/sum(p)) %>% 
  summarise(thermal_occupancy = mean(latm))

lat$time = as.Date(lat$time)

lat$month = substr(as.character(lat$time), 6, 7)
lat$year = substr(as.character(lat$time), 1, 4)

# ggplot(lat, aes(x = as.numeric(month), y = thermal_occupancy, fill = year)) +
#   geom_line(alpha = 0.5) + 
#   scale_color_viridis_c("Lat")

library(zoo)

t = ggplot(lat, aes(x = time, y = thermal_occupancy, color = thermal_occupancy)) +
  geom_line(aes(y=rollmean(thermal_occupancy, 30, na.pad = TRUE))) +
  scale_color_viridis_c("Lat") + 
  # stat_smooth(method = "loess", formula = y ~ x, size = 7) + 
  ylab("Mean_Latitude") + 
  ggtitle("Temporal changes in latitudinal mean of JWS thermal ocupancy, 30-day running mean") + 
  theme_minimal() + 
  theme(legend.position = "none")

# lat$time2 = substr(as.character(lat$time), 1, 4)
# lat = lat %>% group_by(time2) %>% mutate(med_lat = median(thermal_occupancy))
# 
# t = ggplot(lat, aes(x = time2, y = thermal_occupancy)) +
#   geom_boxplot(aes(fill = med_lat), outlier.shape = NA) + 
#   scale_fill_viridis_c() + 
#   stat_smooth(method = "loess", formula = y ~ x, size = 7) +
#   ylab("Mean_Latitude") + 
#   ggtitle("Temporal changes in latitudinal mean of shark thermal ocupancy, 30-day running mean") + 
#   theme_classic() +
#   theme(legend.position = "bottom")
# 
# t
setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/')
pdf('thermal_occupancy_lat_mean.pdf', height = 4, width = 10)
print(t)
dev.off()

