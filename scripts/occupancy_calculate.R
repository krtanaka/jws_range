rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
cores = detectCores()/2
registerDoParallel(cores = cores)

dir = Sys.info()[7]
setwd(paste0('/Users/', dir, '/jws_range/data/'))

load('dummy.Rdata')

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

load('depth_0.25.Rdata')

depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)

load('occupancy.RData')
occup = subset(occup, Depth_Range == "0-20m")
occup = subset(occup, Bin_width == "0.5 deg C")
s = occup[,c("Temperature", "count")]
colnames(s) = c("z", "p")
plot(s)

r = foreach(year = 1981:2020, .combine = rbind) %dopar% {
  
  # year = 2019
  
  load(paste0("/Users/ktanaka/jws_range/data/sst.day.mean.", year , ".RData"))

  year_sum = NULL
  
  for (day in 1:dim(df)[3]) {
    
    # day = 180
    
    d = df[[day]]
    time = as.character(names(d))
    time = gsub("X", "", time)
    time = gsub("\\.", "-", time)
    d = rasterToPoints(d)
    d = as.data.frame(d)
    colnames(d) = c("x", "y", "z")
    # d$z = round(d$z, 1)
    d$z = plyr::round_any(d$z, 0.5, floor)
    d = merge(d, s)
    d = merge(d, depth)
    d$time = time
    
    # xlims = range(d$x); ylims = range(d$y)
    # d %>% ggplot(aes(x, y, fill = p)) +
    #   geom_tile() +
    #   scale_fill_viridis_c("") +
    #   borders(xlim = xlims,
    #           ylim = ylims,
    #           fill = "gray20") +
    #   coord_quickmap(xlim = xlims,
    #                  ylim = ylims) +
    #   theme_minimal()
    
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
save(df, file = paste0("/Users/ktanaka/jws_range/results/thermal_occupancy_", Sys.Date(), ".Rdata"))

