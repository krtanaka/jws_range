rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
cores = detectCores()/2
registerDoParallel(cores = cores)

load('/Users/Kisei/jws_range/data/dummy.Rdata')

d$temp = round(d$temp, 1)
d$temp = d$temp + 5
d = aggregate(count ~ temp, data = d, FUN = "sum")

library(mgcv)
g = gam(count~s(temp, k = 3), data = d, family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", gamma = 1.4)
d$pred_count = predict(g, d, se.fit = F, type = "response")
d$pred_count = ifelse(d$pred_count < 0, 0, d$pred_count)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
d$pred_count = range01(d$pred_count)

s = d[,c("temp", "pred_count")]
colnames(s) = c("z", "p")
s$p = s$p/sum(s$p)

load("/Users/Kisei/jws_range/data/depth_0.25.Rdata")

depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)

r = foreach(year = 1981:2020, .combine = rbind) %dopar% {
  
  # year = 2019
  
  load(paste0("/Users/Kisei/jws_range/data/sst.day.mean.", year , ".RData"))
  # load(paste0("/Users/Kisei/Dropbox/oisst/sst.day.mean.", year , ".RData"))
  
  year_sum = NULL
  
  for (day in 1:dim(df)[3]) {
    
    # day = 36
    
    d = df[[day]]
    time = as.character(names(d))
    time = gsub("X", "", time)
    time = gsub("\\.", "-", time)
    d = rasterToPoints(d)
    d = as.data.frame(d)
    colnames(d) = c("x", "y", "z")
    d$z = round(d$z, 1)
    d = merge(d, s)
    d = merge(d, depth)
    d$time = time
    
    xlims = range(d$x); ylims = range(d$y)
    d %>% ggplot(aes(x, y, fill = p)) +
      geom_tile() +
      scale_fill_viridis_c("") +
      borders(xlim = xlims,
              ylim = ylims,
              fill = "gray20") +
      coord_quickmap(xlim = xlims,
                     ylim = ylims) +
      theme_minimal()
    
    year_sum = rbind(year_sum, d)
    
  }
  
  # year_y = aggregate(.~x+y, year_sum, mean)
  year_y = year_sum
  year_y$year = year
  year_y
  
  # qplot(year_y$x, year_y$y, color = year_y$z)
  
  # cold = rbind(cold, year_y)
  
  # print(year)
  
}

df = as.data.frame(r)
save(df, file = paste0("/Users/ktanaka/Dropbox (MBA)/Data/oisst/shark_habitat_", Sys.Date(), ".Rdata"))

load('/Users/ktanaka/Dropbox (MBA)/Data/oisst/shark_habitat_2020-05-07.Rdata')

xlims = range(df$x); ylims = range(df$y)

map = df %>% 
  group_by(x, y, year) %>% 
  summarise(p = mean(p)) %>% 
  mutate(time = ifelse(year %in% c(1981:2000), "1982-2000", "2001-2019"))
  # mutate(y_mean = sum(z*y)/sum(z))

xlims = range(map$x); ylims = range(map$y)
m = map %>% 
  subset(year %in% c(1982:2019)) %>%
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +
  borders(xlim = xlims,
          ylim = ylims,
          fill = "gray20") +
  coord_quickmap(xlim = xlims,
                 ylim = ylims) +
  facet_wrap(.~time) + 
  theme_minimal()

pdf('/Users/ktanaka/Dropbox (MBA)/Data/oisst/map.pdf', height = 4, width = 6)
print(m)
dev.off()

lat = df %>% group_by(time) %>% 
  mutate(latm = sum(p*y)/sum(p)) %>% 
  summarise(thermal_occupancy = mean(latm))

lat$time = as.Date(lat$time)

library(zoo)

t = ggplot(lat, aes(x = time, y = thermal_occupancy, color = thermal_occupancy)) +
  geom_line(aes(y=rollmean(thermal_occupancy, 30, na.pad = TRUE))) +
  scale_color_viridis_c("Lat") + 
  # stat_smooth(method = "loess", formula = y ~ x, size = 7) + 
  ylab("Mean_Latitude") + 
  ggtitle("Temporal changes in latitudinal mean of shark thermal ocupancy, 30-day running mean") + 
  theme_minimal() + 
  theme(legend.position = "none")

pdf('/Users/ktanaka/Dropbox (MBA)/Data/oisst/time.pdf', height = 4, width = 8)
print(t)
dev.off()

