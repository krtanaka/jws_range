rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
library(sp)
library(maptools)
library(rgdal)

cores = detectCores()/2
registerDoParallel(cores = cores)

## Kisei uses as a general directory syntax to avoid personal names and corrections
## this avoids having to replace ktanaka' with 'kvanhoutan' for example
dir = Sys.info()[7] 

load(paste0("/Users/", dir, "/jws_range/data/depth_0.25.Rdata"))

depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)

mode = c("coldtail", "IQR")[1]

#add lme
lme <- readOGR(paste0("/Users/", Sys.info()[7] , "/jws_range/data/LME66/LMEs66.shp"))
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(lme) <- CRS.new

r = foreach(year = 1981:2020, .combine = rbind, .packages = c('dplyr', 'raster')) %dopar% {
  
  # year = 2020
  
  load(paste0("/Users/", dir, "/jws_range/data/sst.day.mean.", year , ".RData"))
  
  year_sum = NULL
  
  for (day in 1:dim(df)[3]) {
    
    # day = 150

    d = df[[day]]
    time = as.character(names(d))
    time = gsub("X", "", time)
    time = gsub("\\.", "-", time)
    d = rasterToPoints(d)
    d = as.data.frame(d)
    colnames(d) = c("x", "y", "z")
    
    #add lme layer
    latlon = d[,c("x", "y")]
    coordinates(latlon) = ~x+y
    proj4string(latlon) <- CRS.new
    area <- over(latlon, lme)
    colnames(area)[1] = "lme"
    d = cbind(d, area[2])
    # d = d %>% subset(LME_NUMBER == "3")
    
    if (mode == "coldtail") {
      
      d = merge(d, depth, all = T)
      d$z = ifelse(d$z <= 15.6 & d$z >= 14.7, 1, 0) # cold edge = 15.1 +/- 0.4 (centered around 15.1)
      
      
    } else {
      
      d = merge(d, depth)
      d$z = ifelse(d$z <= 21.9 & d$z >= 15.1, 1, 0) # 95 IQR 0.5 temp bin
      
    }
    
    d$time = time
    
    # d %>%
    #   ggplot(aes(x, y, fill = z)) +
    #   geom_tile() +
    #   stat_smooth(data = subset(d, z > 0), method = "auto") +
    #   borders(fill = "gray20") +
    #   coord_quickmap(xlim = range(d$x), ylim = range(d$y)) +
    #   scale_fill_viridis_c() +
    #   theme_void() +
    #   theme(legend.position = "none")

    year_sum = rbind(year_sum, d)
    
  }
  
  year_y = year_sum
  year_y$year = year
  year_y
  
}

df = as.data.frame(r)
save(df, file = paste0("/Users/", dir, "/Desktop/t_", mode,".Rdata"))
# save(df, file = paste0("/Users/", dir, "/Desktop/t_", mode,"_lme.Rdata"))

