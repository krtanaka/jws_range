## load .csv file containing Monterey Bay JWS observations
## build one plot that ensembles a single JWS time series
## from each of the available data streams: iNat entries, Eric's obs, Chris Lowe acoustic immigrants
## run a loess though rescaled data

library(data.table)
library(viridis)
library(ggthemes)
library(ggplot2)
library(ggthemes)

themeKV <- theme_few() +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", size=.5),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))

## load data from within repo
mbay_jws <- read.csv('./data/JWS_obs_Mbay_2010_2019.csv', header = T)

colnames(mbay_jws)[1] = "year"

ggplot(data = mbay_jws, aes(x=year, y=rescale)) +
  themeKV+
  geom_point(aes(color = source)) +
  geom_smooth(method = "loess", formula = y ~ x, span = 0.7, se = TRUE) +
  ## geom_smooth(aes(color = source, fill = source), method = "loess", formula = y ~ x, span = 0.7, se = TRUE) +
  scale_x_continuous(limits = c(2010,2019),
                     breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1,1.2)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE)
