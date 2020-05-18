rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
cores = detectCores()/2
registerDoParallel(cores = cores)

dir = Sys.info()[7] 

load(paste0("/Users/", dir, "/jws_range/data/depth_0.25.Rdata"))

depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)

r = foreach(year = 1981:2020, .combine = rbind) %dopar% {
  
  # year = 2019
  
  load(paste0("/Users/", dir, "/jws_range/data/sst.day.mean.", year , ".RData"))
  
  year_sum = NULL
  
  for (day in 1:dim(df)[3]) {
    
    # day = 153
    # day = 180
    
    d = df[[day]]
    time = as.character(names(d))
    time = gsub("X", "", time)
    time = gsub("\\.", "-", time)
    d = rasterToPoints(d)
    d = as.data.frame(d)
    colnames(d) = c("x", "y", "z")
    d = merge(d, depth)
    d$z = ifelse(d$z <= 21.4 & d$z >= 15.3, 1, 0)
    d$time = time
    
    # d %>%
    #   ggplot(aes(x, y, fill = z)) +
    #   geom_tile() +
    #   # geom_smooth(data = subset(d, z > 0)
    #   #             , method = "auto",
    #   #             span = 0.1
    #   #             ) +
    #   borders(xlim = range(d$x),
    #           ylim = range(d$y),
    #           fill = "gray20") +
    #   coord_quickmap(xlim = range(d$x),
    #                  ylim = range(d$y))
    
    year_sum = rbind(year_sum, d)
    
  }
  
  year_y = year_sum
  year_y$year = year
  year_y
  
}

df = as.data.frame(r)
save(df, 
     file = paste0("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/t_breadth_", 
                   Sys.Date(), 
                   ".Rdata"))

