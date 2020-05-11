rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
# cores = detectCores()/2
# registerDoParallel(cores = cores)

load("/Users/ktanaka/jws_range/data/depth_0.25.Rdata")
load("/Users/Kisei/jws_range/data/depth_0.25.Rdata")

depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)

r = foreach(year = 1981:2020, .combine = rbind) %dopar% {
  
  # year = 2019
  
  load(paste0("/Users/ktanaka/jws_range/data/sst.day.mean.", year , ".RData"))
  
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
    d = merge(d, depth, all = T)
    d$z = ifelse(d$z < 18.1 & d$z > 17.9, 1, 0)
    d$time = time
    
    # d %>%
    #   ggplot(aes(x, y, color = z)) +
    #   geom_point() +
    #   scale_color_viridis_c() +
    #   geom_smooth(data = subset(d, z > 0.5))

    year_sum = rbind(year_sum, d)
    
  }
  
  year_y = year_sum
  year_y$year = year
  year_y
  
}

df = as.data.frame(r)
save(df, file = paste0("/Users/ktanaka/jws_range/results/cold_shoulder_", Sys.Date(), ".Rdata"))

load('/Users/Kisei/jws_range/results/cold_shoulder_2020-05-08.Rdata')

df$month = substr(as.character(df$time), 6, 7)
df = subset(df, month %in% c("06", "07", "08", "09", "10"))
table(df$month)

map = df %>% 
  # group_by(x, y) %>% 
  group_by(x, y, year) %>%
  summarise(z = mean(z))

map %>% 
  ggplot(aes(x, y, fill = z)) + 
  geom_tile() +
  # geom_point() + 
  scale_fill_viridis_c() + 
  facet_wrap(.~ year) +
  geom_smooth(data = subset(map, z > 0), se = F)

map = subset(df, z > 0)

xlims = range(map$x); ylims = range(map$y); ylims = c(35, 47); xlims = c(-126, -120)

lat = df %>% 
  group_by(year) %>% 
  # summarise(z = mean(z)) %>%
  summarise(y_mean = sum(z*y)/sum(z))

map = df %>% 
  group_by(x, y, year) %>% 
  summarise(z = mean(z))

map %>% 
  subset(z > 0) %>%
  subset(year %in% c(1982:2019)) %>%
  ggplot(aes(x, y, fill = z)) +  
  geom_tile() + 
  geom_hline(data = subset(lat, year %in% c(1982, 2019)), 
             aes(yintercept = y_mean, color = factor(year)), lwd = 2) +
  scale_fill_viridis_c("") +
  scale_color_discrete("") + 
  borders(xlim = xlims,
          ylim = ylims, 
          fill = "gray20") +
  coord_quickmap(xlim = xlims,
                 ylim = ylims) + 
  theme_minimal()
  
map %>%
  subset(z > 0) %>%
  # subset(x < -115) %>%
  # subset(x > -130) %>%
  subset(year %in% c(1982, 2014, 2019)) %>%
  # sample_frac(0.01) %>% 
  ggplot(aes(x, y, color = factor(year))) + 
  geom_point() +
  # geom_smooth(se = F, method = "loess") +
  # geom_smooth(se = F) +
  scale_color_viridis_d("") +
  borders(xlim = xlims,
          ylim = ylims, 
          fill = "gray20") +
  coord_quickmap(xlim = xlims,
                 ylim = ylims) + 
  theme_minimal()

lat = df %>% group_by(time) %>% 
  mutate(latm = sum(z*y)/sum(z)) %>% 
  summarise(coldline = mean(latm))

lat$time = as.Date(lat$time)

library(zoo)

ggplot(lat, aes(x = time, y = coldline, color = coldline)) +
  geom_line(aes(y=rollmean(coldline, 10, na.pad = TRUE))) +
  scale_color_viridis_c("Lat") + 
  # stat_smooth(method = "loess", formula = y ~ x, size = 7) + 
  ylab("Mean_Latitude") + 
  ggtitle("10-day running mean") + 
  theme_minimal() + 
  theme(legend.position = "bottom")

