rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
library(sp)
library(maptools)
library(rgdal)
library(patchwork)

cores = detectCores()/2
registerDoParallel(cores = cores)

# gridded bathymetry data
load('data/depth_0.25.Rdata')
depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)

# juvnile white shark thermal affinity model
load('data/occupancy.RData')
occup = subset(occup, Depth_Range == "0-20m")
occup = subset(occup, Bin_width == "0.5 deg C")
s = occup[,c("Temperature", "count")]
plot(s, bty = "l")
colnames(s) = c("z", "p")
s$p = (s$p-min(s$p))/(max(s$p) - min(s$p))

# add Large Marine Ecosystem GIS shapefile (#66 = California Current)
lme <- readOGR("data/LME66/LMEs66.shp")
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(lme) <- CRS.new

r = foreach(year = 1981:2020, .combine = rbind, .packages = c('dplyr', 'raster')) %dopar% {
  
  # year = 2019
  
  load(paste0("data/sst.day.mean.", year , ".RData"))
  
  year_sum = NULL
  
  for (day in 1:dim(df)[3]) {
    
    # day = 1
    
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
    
    #add lme layer
    latlon = d[,c("x", "y")]
    coordinates(latlon) = ~x+y
    proj4string(latlon) <- CRS.new
    area <- over(latlon, lme)
    colnames(area)[1] = "lme"
    d = cbind(d, area[2])
    d = d %>% subset(LME_NUMBER == "3")
    
    year_sum = rbind(year_sum, d)
    
  }
  
  # year_y = aggregate(.~x+y, year_sum, mean)
  year_y = year_sum
  year_y$year = year
  year_y
  
  ### print out itiration id so you know where you are###
  # print(year)
  
}

df = as.data.frame(r)
save(df, file = "output/jws_thermal_habitats.Rdata")
load("output/jws_thermal_habitats.Rdata")

library(marmap)
library(metR)
library(colorRamps)

# import NOAA Bathymetry data
b = getNOAA.bathy(lon1 = -126, lon2 = -109, lat1 = 22.9, lat2 = 47.4, resolution = 4)
b = fortify.bathy(b)

df$month = substr(as.character(df$time), 6, 7)
df$year = substr(as.character(df$time), 1, 4)
df$time_step = substr(as.character(df$time), 1, 7)
df$time_step = df$year

p0 = df %>%
  subset(year %in% c(1982:2019)) %>% 
  group_by(x, y) %>% 
  summarise(p = mean(p, na.rm = T)) 

# 1982-2019 average map
(p0 = ggplot() + 
  geom_contour(data = b, 
               aes(x = x, y = y, z = z),
               breaks = c(-1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000),
               size = c(0.3),
               colour = "grey") +
  geom_tile(data = p0, aes(x, y, fill = p, height = 0.3, width = 0.3)) + 
  scale_fill_viridis_c("", limits = c(0, 1), breaks = c(0, 1)) +  
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  annotate(geom = "text", x = -111, y = 47, label = "1982-2019 mean", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  ylab("") + xlab("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14)))

map = df %>%
  subset(year %in% c(1982:2019)) %>% 
  group_by(x, y, time_step) %>% #monthly time step
  summarise(p = mean(p, na.rm = T)) 

map = as.data.frame(map)

time_list = unique(map$time_step)

mm = NULL

for (t in 1:length(time_list)) {
  
  # t = 1
  
  m = subset(map, time_step == time_list[[t]])
  m = m[,c("p")]
  mm = cbind(mm, m)
  
}

mm = as.data.frame(mm)
colnames(mm) = unique(map$time_step)

xy = subset(map, time_step == time_list[[1]])
xy = xy[,1:2]

change = cbind(xy, mm)

betaf = function(vec){
  
  n = length(vec)
  
  beta = lm(vec ~ seq(1:n))$coef[2] 
  
  # p = summary(lm(vec ~ seq(1:36)))$ coefficients [2,4]
  return(beta) # beta gives you a slope, if you want p-value, change it to p
  #   return(p) # beta gives you a slope, if you want p-value, change it to p
  
}

res = as.data.frame(apply(change[, 3:length(names(change))], 1, betaf)) 
change = cbind(change[,1:2], res)
colnames(change)[3] = "p"

# slope coefficient for 1982-2019
change$p = change$p*38

(p1 = ggplot() + 
  geom_contour(data = b, 
               aes(x = x, y = y, z = z),
               breaks = c(-1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000),
               size = c(0.3),
               colour = "grey") +
  geom_tile(data = change, aes(x, y, fill = p, height = 0.3, width = 0.3)) + 
  scale_fill_viridis_c("") +  
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  annotate(geom = "text", x = -111, y = 47, label = "1982-2019 change", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  ylab("") + xlab("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14)))

p2 = df %>%
  subset(time %in% c("2005-03-16")) %>%
  group_by(x, y, time) %>%
  summarise(p = mean(p, na.rm = T)) 

(p2 = ggplot() + 
  geom_contour(data = b, 
               aes(x = x, y = y, z = z),
               breaks = c(-1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000),
               size = c(0.3),
               colour = "grey") +
  geom_tile(data = p2, aes(x, y, fill = p, height = 0.3, width = 0.3)) + 
  scale_fill_viridis_c("", limits = c(0, 1), breaks = c(0, 1)) +  
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  annotate(geom = "text", x = -111, y = 47, label = "2005-03-16", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  annotate(geom = "text", x = -111, y = 45.8, label = "~271000 sq.km", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  ylab("") + xlab("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14)))

p3 = df %>%
  subset(time %in% c("2015-09-15")) %>%
  group_by(x, y, time) %>%
  summarise(p = mean(p, na.rm = T))


(p3 = ggplot() + 
  geom_contour(data = b, 
               aes(x = x, y = y, z = z),
               breaks = c(-1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000),
               size = c(0.3),
               colour = "grey") +
  geom_tile(data = p3, aes(x, y, fill = p, height = 0.3, width = 0.3)) + 
  scale_fill_viridis_c("", limits = c(0, 1), breaks = c(0, 1)) +  
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  coord_quickmap(xlim = range(df$x),
                 ylim = range(df$y)) + 
  annotate(geom = "text", x = -111, y = 47, label = "2015-09-15", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  annotate(geom = "text", x = -111, y = 45.8, label = "~59100 sq.km", 
           hjust = 1, vjust = 1, color = "white", size = 5) + 
  ylab("") + xlab("") + 
  scale_x_longitude() +
  scale_y_latitude() +
  theme_void() + 
  theme(legend.position = c(0.8,0.75),
        axis.text = element_blank(),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14)))

p2 + p3
