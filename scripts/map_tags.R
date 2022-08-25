rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)
library(metR)

# download "JWS_Corrected.RData" from https://osf.io/vcwjp/
load("/Users/kisei.tanaka/Desktop/JWS_Corrected.RData")

JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)
JWS_Corrected$month = substr(as.character(JWS_Corrected$Time_s), 6, 7)
plot(table(JWS_Corrected$month))

JWS_Corrected$count = 1
JWS_Corrected$year = substr(as.character(JWS_Corrected$Time_s), 1, 4)
JWS_Corrected$year = as.numeric(JWS_Corrected$year)
JWS_Corrected %>% group_by(id) %>% summarise(y = mean(year))
plot(table(JWS_Corrected$year))

d = JWS_Corrected %>% group_by(lat_pop, lon_pop, id, sex) %>% summarise(count = sum(count))
d$id = as.factor(d$id)

d %>%
  ggplot(aes(lon_pop, lat_pop,
             color = id,
             label = id)) +
  coord_quickmap(xlim = c(-126, -110), ylim = c(22.9, 47.4)) +
  borders(fill = "gray10", colour = "gray10", size = 0.5) +
  geom_point(size = 5, alpha = 0.9) +
  scale_color_discrete("") + 
  theme_minimal() +
  theme(legend.position = c(0.62, 0.86), 
        legend.text = element_text(color = "white"), 
        panel.background = element_rect(fill = alpha('gray80', 0.4), size = 0.01)) +
  guides(color = guide_legend(ncol = 2))

df1 = JWS_Corrected %>% 
  group_by(id) %>% 
  summarise(lat = mean(lat_rel),
            lon = mean(lon_rel), 
            count = sum(count)) %>% 
  mutate(pop_rel = "Begin")

df2 = JWS_Corrected %>% 
  group_by(id) %>% 
  summarise(lat = mean(lat_pop),
            lon = mean(lon_pop), 
            count = sum(count)) %>% 
  mutate(pop_rel = "End")

df = rbind(df1, df2)

(p3 = ggplot(df, aes(lon, lat, group = id)) +  
  annotation_map(map_data("world"))+ #Add the map as a base layer before the points
  geom_point(aes(shape = pop_rel, color = log10(count)), alpha = 0.9) +
  scale_shape_manual(values = c(16, 17), "") +
  scale_color_viridis_c("log10(n)") +
  facet_wrap(.~id, scales = "fixed", ncol = 4))



