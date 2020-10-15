## Let's analyze what depths constitute "the surface" for JWS
## Oceanography in the California Current, and Monterey, says the mixed layer is 0-50m
## however, we can also derive some alignment between indiv from the tags
## Let's use tag observations to derive and plot cumulative ave, sd, and CV
## of time at depth 0<=n, where n gets increasingly larger from 0... 287m (max depth)
## use hollow circles, 'shape=1'

JWS_depth_params <- read.csv('~/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/JWS_tag_params.csv', header=T)

library(ggthemes)
library(viridis)

ggplot(JWS_depth_params, aes(x = DEPTH, y = VALUE)) +
  theme_few()+
  geom_point(aes(color = PARAM), alpha = 0.6, shape = 1, size = 2) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  geom_vline(aes(xintercept = 20), linetype = "dashed")+
  facet_wrap(~PARAM, scales = "free_y", ncol = 1)
  