rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)

setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/")
setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/")

load("t_coldtail.Rdata")

# reduce file size
d = df %>% sample_frac(0.001); rm(df)
# d = df; rm(df)

d = d %>% 
  mutate(year = substr(as.character(time), 1, 4),
         month = substr(as.character(time), 6, 7),
         day = substr(as.character(time), 9, 10)) %>% 
  group_by(x, y, year, month, day) %>% 
  summarise(z = mean(z, na.rm = T))

d %>% filter_all(any_vars(!is.na(.)))

library(mgcv)
g = gam(z ~ month + year + day + s(x, y), data = d, 
        binomial("logit"), gamma = 1.4)


sm <- ma(ts, order=12) # 12 month moving average
lines(sm, col="red") # plot
