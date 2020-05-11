rm(list = ls())

library(ggpubr)
library(dplyr)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
probs <- c(0, 0.025, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95, 0.975, 1)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/JWS_Corrected.RData")

JWS_Corrected$Temperature = round(JWS_Corrected$Temperature, 1)

d1 = subset(JWS_Corrected, Depth <= 20); d1 = d1[,c("Temperature", "Depth", "id")]
d2 = subset(JWS_Corrected, Depth <= 2); d2 = d2[,c("Temperature", "Depth", "id")]

d1$count = 1
d2$count = 1

d1 = d1 %>% 
  group_by(id, Temperature) %>% 
  summarize(count = n()) %>% 
  mutate(count = range01(count))

d2 = d2 %>% 
  group_by(id, Temperature) %>% 
  summarize(count = n()) %>% 
  mutate(count = range01(count))

d1$Depth_Range = "0-20m"
d2$Depth_Range = "0-2m"

q_01 = aggregate(count ~ Temperature, data = d1, FUN = "sum")
q1 = quantile(q_01$Temperature, probs = probs); order =  names(q1)
q1 = as.data.frame(q1)
q1$Depth = "0-20m"
colnames(q1) = c("temp", "p")
q1 = data.table::setDT(q1, keep.rownames = TRUE)[]

q_02 = aggregate(count ~ Temperature, data = d2, FUN = "sum")
q2 = quantile(q_02$Temperature, probs = probs)
q2 = as.data.frame(q2)
q2$Depth = "0-2m"
colnames(q2) = c("temp", "p")
q2 = data.table::setDT(q2, keep.rownames = TRUE)[]

p = rbind(q1, q2)

save(p, file = '/Users/Kisei/jws_range/data/percentiles.RData')

q$rn = factor(q$rn, levels = order)

q %>% ggplot(aes(rn, temp, color = p)) + geom_point() + geom_text(label = q$temp)

d = rbind(d1, d2)

pdf("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/depth_thermal_density_tag.pdf", height = 5, width = 10)

p = d %>% ggplot(aes(Temperature, count, color = Depth_Range, fill = Depth_Range)) + 
  geom_bar(stat="identity", position = position_dodge(width = 0.5), width = 0.5) +
  facet_wrap(.~id) +
  # scale_color_viridis_d() + 
  # scale_fill_viridis_d() + 
  theme_minimal() + 
  ylab("Freq") + 
  theme(legend.position = "right")

p

dev.off()

t = d

t$Temperature = round(t$Temperature, 1)

t = t %>% group_by(Temperature, Depth_Range) %>% 
  summarise(count = sum(count))

pdf("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/depth_thermal_density_combined.pdf", height = 4, width = 5)

p = t %>% 
  ggplot(aes(x = Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.5)) +  
  # scale_color_viridis_d() + 
  # scale_fill_viridis_d() + 
  ylab("Freq") +  
  theme_classic2() 
  
p

dev.off()

occup = t %>% 
  group_by(Depth_Range) %>% 
  mutate(sum = colSums(.[3]),
         count = count/sum)

pdf("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/shark_thermal_occupancy_1.pdf", height = 4, width = 5)

occup %>% 
  ggplot(aes(x = Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.5)) +  
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  ylab("Thermal_Occupancy") +  
  theme_classic2() 

dev.off()

occup = as.data.frame(occup)

save(occup, file = "/Users/Kisei/jws_range/data/occupancy.RData")



