rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(doParallel)
# cores = detectCores()/2
# registerDoParallel(cores = cores)

dir = Sys.info()[7] 

load(paste0("/Users/", dir, "/jws_range/data/depth_0.25.Rdata"))

depth = d
depth = rasterToPoints(depth)
depth = as.data.frame(depth)

load(paste0("/Users/", dir, "/jws_range/data/percentiles.RData"))
load(paste0("/Users/", dir, "/jws_range/data/occupancy.RData"))

occup %>% subset(Bin_width == "0.5 deg C" & Depth_Range == "0-20m") %>% 
  ggplot(aes(Temperature, count)) + geom_point()

d = occup %>% subset(Bin_width == "0.5 deg C" & Depth_Range == "0-20m")

library(eHOF)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

x = d$Temperature
y = d$count
y = ifelse(d$count > summary(d$count)[2], 1, 0)

#Build eHOF model
m <- HOF(y, x, bootstrap=NULL) 
m

#restrict model selection between 1-5
models = c(1:5)

#choose best model between 1-5
model_type = pick.model(m, model = eHOF.modelnames[models])

#take out best model data
best_model = m$models[[model_type]]

#Central Borders parameters from best model
param = Para(m, pick.model(m, model = eHOF.modelnames[models]))$centralBorder

#create df for plotting
df = data.frame(m$x, best_model$fitted)

# rounding and aggregating to speedup plotting
df$m.x = round(df$m.x, 1) 
df = aggregate(best_model.fitted ~ m.x, data = df, FUN = "mean")

plot(m,
     para = T,
     onlybest = T,
     boxp = F,
     marginal = c('n'),
     # gam.se = T,
     # color = 5,
     model = pick.model(m, model = eHOF.modelnames[models]),
     # yl = c(0,0.5),
     lwd = 2)

Para(m)

# l <- list(m, Para(m)$centralBorder)
l <- list("model_type" = model_type,
          "df" = df, 
          "Low" = Para(m, pick.model(m, model = eHOF.modelnames[models]))$centralBorder[1], 
          "High" = Para(m, pick.model(m, model = eHOF.modelnames[models]))$centralBorder[2], 
          "T_opt" = Para(m, pick.model(m, model = eHOF.modelnames[models]))$opt[1])
l


r = foreach(year = 1981:2020, .combine = rbind) %dopar% {
  
  # year = 2019
  
  load(paste0("/Users/", dir, "/jws_range/data/sst.day.mean.", year , ".RData"))
  
  year_sum = NULL
  
  for (day in 1:dim(df)[3]) {
    
    # day = 153
    # day = 305
    
    d = df[[day]]
    time = as.character(names(d))
    time = gsub("X", "", time)
    time = gsub("\\.", "-", time)
    d = rasterToPoints(d)
    d = as.data.frame(d)
    colnames(d) = c("x", "y", "z")
    d = merge(d, depth, all = T)
    d$z = ifelse(d$z < 11.6 & d$z > 11.5, 1, 0)
    d$time = time
    
    # d %>%
    #   ggplot(aes(x, y)) +
    #   geom_smooth(data = subset(d, z > 0), method = "gam") + 
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
save(df, file = paste0("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/cold_shoulder_", Sys.Date(), ".Rdata"))

