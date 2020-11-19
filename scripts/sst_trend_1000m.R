rm(list = ls())

load("/Users/Kisei/jws_range/data/temp_depth_1000m.RData")
load("/Users/ktanaka/jws_range/data/temp_depth_1000m.RData")

library(ggpubr)
library(dplyr)

temp = df

#####################
### temp analysis ###
#####################

temp %>%
  subset(year %in% c(1982:2019)) %>%
  subset(y < 35) %>% 
  # subset(time %in% c("2015-09-15", "2005-03-16")) %>%
  # group_by(x, y, year, time) %>%
  group_by(x, y, year) %>%
  summarise(p = mean(z, na.rm = T)) %>% 
  ggplot(aes(x, y, fill = p)) +
  geom_tile() +
  scale_fill_viridis_c("") +  
  annotation_map(map_data("world")) +
  coord_fixed() + 
  xlab("Longitude (dec deg)") + ylab("Latitude (dec deg)") +
  # facet_wrap(.~time) +
  scale_x_continuous(breaks = round(seq(min(df$x), max(df$x), by = 10),0)) + 
  theme(legend.position = "right")

sst_ts = temp %>% 
  subset(year %in% c(1982:2019)) %>%
  subset(y < 35) %>% 
  group_by(year) %>% 
  summarise(sst = mean(z)) %>% 
  mutate(year = as.numeric(year),
         sst = sst-mean(sst))

summary(lm(sst ~ year, data = sst_ts))

sst = sst_ts %>% 
  ggplot(aes(x = year, y = sst)) +
  geom_point(size = 3, alpha = 0.8) + 
  geom_line() +
  # scale_fill_viridis_c("") +  
  # scale_color_viridis_c("") +  
  theme_pubr(I(15)) + 
  scale_x_continuous(breaks = seq(1982, 2019, 5)) + 
  xlab("") + ylab("SST (°C)") +
  stat_smooth(method = "lm", linetype = "dashed", color = "gray20", se = F) 

sst

setwd("/Users/Kisei/Desktop")
setwd("/Users/ktanaka/Desktop")

pdf(paste0("S7_", Sys.Date(), ".pdf"), height = 5, width = 7)
sst
dev.off()

