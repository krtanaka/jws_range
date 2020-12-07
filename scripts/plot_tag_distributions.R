rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")

JWS_Corrected = JWS_Corrected %>% as.data.frame %>% sample_frac(1)

JWS_Corrected$Temperature = round(JWS_Corrected$Temperature, 1)
JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)

d1 = subset(JWS_Corrected, Depth <= 20); d1 = d1[,c("Temperature", "Depth", "id")]; d1$Depth_Range = "0-20m"; d1$count = 1
d2 = subset(JWS_Corrected, Depth <= 2); d2 = d2[,c("Temperature", "Depth", "id")]; d2$Depth_Range = "0-2m"; d2$count = 1

pdf("~/Desktop/s3_thermal_profile_tag_raw.pdf", height = 6, width = 8)

# rbind(d1, d2) %>% 
#   sample_frac(1) %>%
#   ggplot(aes(x=Temperature, fill = Depth_Range, color = Depth_Range)) +
#   geom_density(alpha = 0.8) +
#   scale_fill_viridis_d() + 
#   scale_color_viridis_d() + 
#   facet_wrap(.~id, scale = "free_y")

n <- ddply(.data = d1, .(id), summarize, n = paste("n =", formatC(length(count),format="e", digits = 1)))

d1 %>% 
  group_by(id) %>% 
  summarise(median = wtd.quantile(Temperature, q = 0.025, weight = count, na.rm = T),
            max = max(Temperature),
            min = min(Temperature))

d1 %>% 
  sample_frac(0.01) %>%
  ggplot(aes(x = Temperature, fill = id, color = id)) +
  geom_density(alpha = 1) +
  scale_fill_viridis_d("") + 
  scale_color_viridis_d("") + 
  ggpubr::theme_pubr() + 
  xlab("Temperature (°C)") + ylab("Proportion of time spent") + 
  theme(legend.position = "none") + 
  facet_wrap(.~id, scale = "free_y") +
  geom_text(data = n, aes(x = Inf, y = Inf, label = n), colour="black", hjust = 1, vjust = 1)

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

pdf("thermal_profile_tag_normalized.pdf", height = 4, width = 6)

# p = d %>% ggplot(aes(Temperature, count, color = Depth_Range, fill = Depth_Range)) + 
#   # geom_bar(stat="identity", position = position_dodge(width = 0.5)) +
#   geom_density(stat = "identity", alpha = 0.8) +
#   facet_wrap(.~id) +
#   scale_color_viridis_d() +
#   scale_fill_viridis_d() +
#   # theme_minimal() + 
#   ylab("Freq") + 
#   theme(legend.position = "right")

d1 %>% ggplot(aes(Temperature, count, color = id, fill = id)) + 
  # geom_bar(stat="identity", position = position_dodge(width = 0.5)) +
  geom_density(stat = "identity", alpha = 0.5) +
  # facet_wrap(.~id) +
  scale_fill_viridis_d("") + 
  scale_color_viridis_d("") + 
  theme_pubr() + 
  xlab("Temperature (deg C)") + 
  ylab("Freq") + 
  theme(legend.position = "right")

# p

dev.off()
