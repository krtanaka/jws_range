## load .csv file containing Monterey Bay JWS observations
## build one plot that ensembles a single JWS time series
## from each of the available data streams: iNat entries, Eric's obs, Chris Lowe acoustic immigrants
## run a loess though rescaled data

library(data.table)
library(viridis)
library(ggthemes)

mbay_jws <- read.csv('~/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/community science/JWS_obs_Mbay_2010_2019.csv', header = T)

ggplot(data = mbay_jws, aes(x=year, y=rescale)) +
  theme_few()+
  geom_point(aes(color = source)) +
  geom_smooth(method = "loess", formula = y ~ x, span = 0.7, se = TRUE) +
  ## geom_smooth(aes(color = source, fill = source), method = "loess", formula = y ~ x, span = 0.7, se = TRUE) +
  scale_x_continuous(limits = c(2010,2019),
                     breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1,1.2)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE)
