rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# download "JWS_Corrected.RData" from https://osf.io/vcwjp/
load("/Users/kisei.tanaka/Desktop/JWS_Corrected.RData")

JWS_Corrected = JWS_Corrected %>% as.data.frame %>% sample_frac(1)

JWS_Corrected$Temperature = round(JWS_Corrected$Temperature, 1)
JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)

d1 = subset(JWS_Corrected, Depth <= 20); d1 = d1[,c("Temperature", "Depth", "id")]; d1$Depth_Range = "0-20m"; d1$count = 1
d2 = subset(JWS_Corrected, Depth <= 2); d2 = d2[,c("Temperature", "Depth", "id")]; d2$Depth_Range = "0-2m"; d2$count = 1

library(plyr)
n <- ddply(.data = d1, .(id), summarize, n = paste("n =", formatC(length(count),format="e", digits = 1)))
detach("package:plyr", unload = TRUE)

d1 %>% 
  group_by(id) %>% 
  summarise(median = wtd.quantile(Temperature, q = 0.025, weight = count, na.rm = T),
            max = max(Temperature),
            min = min(Temperature))

d1 %>% 
  sample_frac(0.01) %>%
  ggplot(aes(x = Temperature, fill = id, color = id)) +
  geom_density(alpha = 0.8) +
  ggpubr::theme_pubr() + 
  xlab("Temperature (°C)") + ylab("Proportion of time spent") + 
  theme(legend.position = "none") + 
  facet_wrap(.~id, scale = "free_y") +
  geom_text(data = n, aes(x = Inf, y = Inf, label = n), colour="black", hjust = 1, vjust = 1)

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

(d1 %>% ggplot(aes(Temperature, count, color = id, fill = id)) + 
  # geom_bar(stat="identity", position = position_dodge(width = 0.5)) +
  geom_density(stat = "identity", alpha = 0.8) +
  # facet_wrap(.~id) +
  scale_fill_viridis_d("") + 
  scale_color_viridis_d("") + 
  theme_pubr() + 
  xlab("Temperature (deg C)") + 
  ylab("Freq") + 
  theme(legend.position = "right"))

