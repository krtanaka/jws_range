rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")

JWS_Corrected = JWS_Corrected %>% as.data.frame %>% sample_frac(0.001)

# JWS_Corrected$Temperature = round(JWS_Corrected$Temperature, 1)
JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)

d1 = subset(JWS_Corrected, Depth <= 20); d1 = d1[,c("Temperature", "Depth", "id")]; d1$Depth_Range = "0-20m"; d1$count = 1
d2 = subset(JWS_Corrected, Depth <= 2); d2 = d2[,c("Temperature", "Depth", "id")]; d2$Depth_Range = "0-2m"; d2$count = 1

pdf("thermal_profile_tag_raw.pdf", height = 10, width = 10)

rbind(d1, d2) %>% 
  sample_frac(1) %>%
  ggplot(aes(x=Temperature, fill = Depth_Range, color = Depth_Range)) +
  geom_density(alpha = 0.8) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d() + 
  facet_wrap(.~id, scale = "free_y")

dev.off()

d1 = d1 %>%
  group_by(id, Temperature) %>%
  summarize(count = n()) %>%
  mutate(count = range01(count),
         Depth_Range = "0-20m")

d2 = d2 %>%
  group_by(id, Temperature) %>%
  summarize(count = n()) %>%
  mutate(count = range01(count),
         Depth_Range = "0-2m")

d = rbind(d1, d2) 

pdf("thermal_profile_tag_normalized.pdf", height = 10, width = 10)
p = d %>% ggplot(aes(Temperature, count, color = Depth_Range, fill = Depth_Range)) + 
  # geom_bar(stat="identity", position = position_dodge(width = 0.5)) +
  geom_density(stat = "identity", alpha = 0.8) +
  facet_wrap(.~id) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  # theme_minimal() + 
  ylab("Freq") + 
  theme(legend.position = "right")
p
dev.off()
