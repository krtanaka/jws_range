rm(list = ls())

library(ggpubr)
library(dplyr)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
probs <- c(0, 0.025, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95, 0.975, 1)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/JWS_Corrected.RData")

# JWS_Corrected$Temperature = round(JWS_Corrected$Temperature, 1)

d = JWS_Corrected
d$count = 1
d = d %>% group_by(lat_pop, lon_pop, id, sex) %>% summarise(count = sum(count))
d$id = as.factor(d$id)

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/')

pdf("tag_locations.pdf", height = 10, width = 7.5)
d %>% 
  ggplot(aes(lon_pop, lat_pop, 
             color = id,
             label = id)) + 
  borders(xlim = range(d$lon_pop),
          ylim = range(d$lat_pop),
          fill = "gray10") +
  coord_map(xlim = range(pretty(d$lon_pop)),
            ylim = range(pretty(d$lat_pop))) +
  geom_point() +
  ggrepel::geom_text_repel(aes(color = id), box.padding = 2) + 
  theme_classic2() + 
  theme(legend.position = "none")
dev.off()

pdf("tag_counts.pdf", height = 10, width = 7.5)
d %>% 
  ggplot(aes(lon_pop, lat_pop, 
             color = log10(count))) +
  borders(xlim = range(d$lon_pop),
          ylim = range(d$lat_pop),
          fill = "gray10") +
  coord_map(xlim = range(pretty(d$lon_pop)),
            ylim = range(pretty(d$lat_pop))) +
  geom_point(size = 5, alpha = 0.8) +
  theme_classic2() + 
  scale_color_viridis_c("log10(n)") + 
  theme(legend.position = c(0.1, 0.1))
dev.off()

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
p

save(p, file = '/Users/Kisei/jws_range/data/percentiles.RData')

# q$rn = factor(q$rn, levels = order)
# q %>% ggplot(aes(rn, temp, color = p)) + geom_point() + geom_text(label = q$temp)

d = rbind(d1, d2)

pdf("thermal_profile_tag.pdf", height = 5, width = 10)

p = d %>% ggplot(aes(Temperature, count, color = Depth_Range, fill = Depth_Range)) + 
  geom_bar(stat="identity", position = position_dodge(width = 0.5)) +
  facet_wrap(.~id) +
  # scale_color_viridis_d() + 
  # scale_fill_viridis_d() + 
  theme_minimal() + 
  ylab("Freq") + 
  theme(legend.position = "right")

p

dev.off()

t = d

t1 = t
t2 = t
t3 = t

t1$Temperature = round(t1$Temperature, 0)
t2$Temperature = round(t2$Temperature, 1)
t3$Temperature = plyr::round_any(t3$Temperature, 0.5, floor)

t1 = t1 %>% group_by(Temperature, Depth_Range) %>% summarise(count = sum(count))
t2 = t2 %>% group_by(Temperature, Depth_Range) %>% summarise(count = sum(count))
t3 = t3 %>% group_by(Temperature, Depth_Range) %>% summarise(count = sum(count))

t1$Bin_width = "1 deg C"
t2$Bin_width = "0.1 deg C"
t3$Bin_width = "0.5 deg C"

t = rbind(t1, t2, t3)

pdf("thermal_profile.pdf", height = 3, width = 10)

p = t %>% 
  ggplot(aes(x = Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.5)) +  
  # scale_color_viridis_d() +
  # scale_fill_viridis_d() +
  ylab("Freq") +  
  facet_wrap(.~ Bin_width, scales = "free", ncol = 3) + 
  theme_minimal() 
  
p

dev.off()

occup = t %>% 
  group_by(Depth_Range, Bin_width) %>% 
  mutate(sum = colSums(.[3]),
         count = count/sum)

pdf("thermal_occupancy.pdf", height = 3, width = 10)

occup %>% 
  ggplot(aes(x = Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.5)) +  
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  ylab("Thermal_Occupancy")  +  
  facet_wrap(.~ Bin_width, scales = "free", ncol = 3) + 
  theme_minimal() 

dev.off()

occup = as.data.frame(occup)

save(occup, file = "/Users/Kisei/jws_range/data/occupancy.RData")



