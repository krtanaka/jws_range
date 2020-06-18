rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/")

load("t_coldtail.Rdata")

# reduce file size
d = df %>% 
  sample_frac(1)%>% 
  # subset(y <= 42) %>%
  subset(depth > -1000) %>%
  mutate(year = substr(as.character(time), 1, 4),
         month = substr(as.character(time), 6, 7),
         day = substr(as.character(time), 9, 10))

d1 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  group_by(year) %>% 
  summarise(y_all = sum(z*y, na.rm = T)/sum(z, na.rm = T)) 

d2 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(month %in% c('01','02','03','04')) %>% 
  group_by(year) %>% 
  summarise(y_jfma = sum(z*y, na.rm = T)/sum(z, na.rm = T)) 

d = merge(d1, d2, all = T)

#2020 Jan-Apr
d_2020 = df %>% 
  sample_frac(1)%>% 
  # subset(y <= 42) %>%
  subset(depth > -1000) %>%
  mutate(year = substr(as.character(time), 1, 4),
         month = substr(as.character(time), 6, 7),
         day = substr(as.character(time), 9, 10)) %>% 
  subset(year %in% c(2020)) %>% 
  subset(month %in% c('01','02','03','04')) %>% 
  summarise(y_2020 = sum(z*y, na.rm = T)/sum(z, na.rm = T)) 

d_2020 = as.numeric(d_2020)

#method 1
lm = lm(y_all ~ y_jfma, data = d)
b = lm$coefficients[2]
a = lm$coefficients[1]
r = summary(lm); r = r$r.squared
plot(d$y_jfma, d$y_all, pch = 20, axes = F, xlab = "Jan-Apr", ylab = "Jan-Dec")
# box(bty = "l")
abline(lm)
axis(1); axis(2, las = 1)
legend("topleft", legend = c(paste0("slope=", round(b, 2)),
                             paste0("adj.R2=", round(r, 2))), bty = "n")

lm$coefficients[2]*d_2020 + lm$coefficients[1]

scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
  return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}
scale_x_longitude <- function(xmin=-90, xmax=90, step=0.5, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_x_continuous("January-April", breaks = xbreaks, labels = xlabels, ...))
}    
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("January-December", breaks = ybreaks, labels = ylabels, ...))
}    

d %>% 
  ggplot(aes(y_jfma, y_all, 
             color = as.numeric(year),
             )) +
  geom_point(size = 5, shape = 1, stroke = 2) +
  geom_smooth(method = "lm", se = F, color = "gray40") + 
  scale_color_viridis_c("") + 
  scale_x_longitude(xmin=-30, xmax=40, step=1) +
  scale_y_latitude(ymin=-30, ymax=40, step=1) +
  theme_few() + 
  coord_fixed() + 
  # theme(legend.position = c(0.1, 0.85)) + 
  # annotate("text", 
  #          x = Inf, y = -Inf, 
  #          vjust = -1, hjust = 1,
  #          label = paste0("slope = ", round(b, 2), "\n  adj.R2 = ", round(r, 2))) + 
  annotate("text", 
           x = Inf, y = -Inf, 
           vjust = -1, hjust = 1,
           label = paste0("R2 = ", round(r, 2))) 
  

  

# method 2
d$ratio = d$y_jfma/d$y_all
avg_ratio = mean(d$ratio)
d_2020/avg_ratio

