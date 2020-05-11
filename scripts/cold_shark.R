cold = NULL

for (year in 1982:2019) {
  
  # year = 2019
  
  load(paste0("C:/Users/Kisei/Dropbox/oisst/sst.day.mean.", year , ".RData"))
  
  year_sum = NULL
  
  for (day in 1:dim(df)[3]) {
    
    # day = 1
    
    d = df[[day]]
    d = rasterToPoints(d)
    d = as.data.frame(d)
    colnames(d) = c("x", "y", "z")
    d$z = ifelse(d$z < 12 & d$z > 10, 1, 0)
    
    year_sum = rbind(year_sum, d)
    # qplot(d$x, d$y, color = d$z)
    
  }
  
  year_y = aggregate(.~x+y, year_sum, mean)
  year_y$year = year
  
  # qplot(year_y$x, year_y$y, color = year_y$z)
  
  cold = rbind(cold, year_y)
  
  print(year)
  
}

cold %>% ggplot(aes(x, y, fill = z)) + 
  geom_raster() + 
  scale_fill_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
  scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
  facet_wrap(~year) + 
  theme_void() 

d = subset(cold, z > 0)

ggplot(d, aes(x=factor(year), y=z)) + 
  geom_boxplot()
