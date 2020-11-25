rm(list = ls())

library(dplyr)
library(ggpubr)
library(raster)
library(gridExtra)

##############################################
#### run "calculate_binom_habitat.R first ####
#### #########################################

# daily latitdinal positons of juvenile white shark cold edge
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/")
load("t_coldtail.Rdata")

# reduced file size
d = df %>% sample_frac(0.01); rm(df)

# original file size
d = df; rm(df)


t1 = d %>% 
  subset(year %in% c(1982:2019)) %>% 
  subset(depth > -1000) %>%
  group_by(year) %>% 
  summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T))%>% 
  mutate(period = case_when(year %in% c(1982:2013) ~ "1982-2013",
                            year %in% c(2014:2019) ~ "2014-2020"))

forecast_1 = data.frame(year = 2020, 
                        # y = (37.02439 + 37.03631)/2,
                        y = 35.45347,
                        period = "2014-2020")

t = rbind(t1, forecast_1)

t_2020 = d %>% 
  subset(year %in% c(2020)) %>% 
  subset(depth > -1000) %>%
  group_by(year) %>% 
  summarise(y = sum(z*y, na.rm = T)/sum(z, na.rm = T)) %>% 
  mutate(period = "2020")


l = t %>% group_by(period) %>% summarise(z = mean(y)); l

l$year = c(1998, 2017)

scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("Latitude (dec deg)", breaks = ybreaks, labels = ylabels, ...))
}    

t %>%  
  ggplot(aes(year, y)) + 
  geom_point() + 
  geom_point(data = forecast_1, aes(year, y), size = 10, shape = 1) +
  geom_point(data = t_2020, aes(year, y), size = 10, shape = 2) +
  
  geom_line() +
  ylab("Latitude (dec deg)") + xlab("") + 
  scale_y_latitude(ymin=-30, ymax=40, step=1) +
  geom_segment(aes(x = 1982, xend = 2013, y = l$z[1], yend = l$z[1]), color = "blue",show.legend = T, size = 2) + 
  geom_segment(aes(x = 2014, xend = 2020, y = l$z[2], yend = l$z[2]), color = "red", show.legend = F, size = 2) + 
  geom_label(data=l, 
             aes(y = z, x = year, 
                 label = period),
             nudge_x = c(0, 0),
             nudge_y = c(0, 0)) +
  theme_few(I(15)) +
  theme(legend.position = c(0.2, 0.9),
        legend.title = element_blank())

