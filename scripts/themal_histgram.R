rm(list = ls())

library(ggpubr)
library(dplyr)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
probs <- c(0, 0.025, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95, 0.975, 1)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/JWS_Corrected.RData")

# JWS_Corrected$Temperature = round(JWS_Corrected$Temperature, 1)

# JWS_Corrected = subset(JWS_Corrected, id != c("JWS_14_07_12P0072_12398")) #remove Monterey Bay release

d = JWS_Corrected
d$month = substr(as.character(d$Time_s), 6, 7)
plot(table(d$month))

d$count = 1
d = d %>% group_by(lat_pop, lon_pop, id, sex) %>% summarise(count = sum(count))
d$id = as.factor(d$id)

setwd('/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/')

pdf("tag_locations.pdf", height = 10, width = 8)
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
  ggrepel::geom_text_repel(aes(color = id), box.padding = 3) +
  xlab("Longitude") + ylab("Latitude") +
  # theme_classic2() + 
  theme(legend.position = "none")
dev.off()

pdf("tag_counts.pdf", height = 5, width = 6)
d %>% 
  ggplot(aes(lon_pop, lat_pop, 
             color = log10(count))) +
  borders(xlim = range(d$lon_pop),
          ylim = range(d$lat_pop),
          fill = "gray10") +
  coord_map(xlim = range(pretty(d$lon_pop)),
            ylim = range(pretty(d$lat_pop))) +
  geom_point(size = 5, alpha = 0.8) +
  xlab("Longitude") + ylab("Latitude") +
  # theme_classic2() + 
  scale_color_viridis_c("log10(n)") + 
  theme(legend.position = c(0.15, 0.2))
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
  # theme_minimal() + 
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
  facet_wrap(.~ Bin_width, scales = "free", ncol = 3)+ 
  theme_cowplot() 
  
p

dev.off()

occup = t %>% 
  group_by(Depth_Range, Bin_width) %>% 
  mutate(sum = colSums(.[3]),
         count = count/sum)

library(mgcv)

d = subset(occup, Bin_width == "0.5 deg C")

g = gam(count~s(Temperature, k = 5), 
        data = d, 
        family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", gamma = 1.4)

d$pred_count = predict(g, d, se.fit = F, type = "response")

d$weight = (d$count-min(d$count))/(max(d$count) - min(d$count))
d$weight = (d$pred_count-min(d$pred_count))/(max(d$pred_count) - min(d$pred_count))

q = 90

hq = 1-(1-q/100)/2
lq = (1-q/100)/2

d = as.data.frame(d)

low = wtd.quantile(d$Temperature, q = lq, weight = d$weight, na.rm = T)
high = wtd.quantile(d$Temperature, q = hq, weight = d$weight, na.rm = T)

T_opt=sum(d$weight * d$Temperature/sum(d$weight)) #just weighted mean

pdf('thermal_percentiles.pdf', height = 5, width = 7)

plot(d[,c(1, 3)],  xlab = "deg C", ylab = "JWS Thermal Occupancy", 
     axes = F, type = "b", pch =20)
abline(v = low, lty = 2); abline(v = high, lty = 2)
abline(v = T_opt, lty = 2, lwd = 2)
points(d$Temperature, d$pred_count, col = 4, type = "b", pch = 20)
legend("topleft", c(paste0("0.05p = ", round(low,1)),
                     paste0("0.95p = ", round(high,1)),
                     paste0("T_opt = ", round(T_opt,1))), 
       lty = 2, lwd = c(1,2), bty = "n")
axis(1)
axis(2, las = 2)
box(bty = "l")

dev.off()

pdf("thermal_occupancy_0.1_0.5_1.pdf", height = 3, width = 10)

occup %>% 
  # subset(Bin_width == "0.5 deg C") %>%
  ggplot(aes(x = Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.3)) +  
  scale_color_viridis_d("Depth range") +
  scale_fill_viridis_d("Depth range") +
  ylab("Thermal_Occupancy")  +  
  facet_wrap(.~ Bin_width, scales = "free", ncol = 3) + 
  theme_cowplot() 

dev.off()

pdf("thermal_occupancy_0.5.pdf", height = 5, width = 5)

occup %>% 
  subset(Bin_width == "0.5 deg C") %>%
  ggplot(aes(x = Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.3)) +  
  scale_color_viridis_d("Depth range") +
  scale_fill_viridis_d("Depth range") +
  ylab("Thermal_Occupancy")  +  
  facet_wrap(.~ Bin_width, scales = "free", ncol = 3) + 
  theme_cowplot() 

dev.off()

pdf("thermal_occupancy_1.pdf", height = 5, width = 5)

occup %>% 
  subset(Bin_width == "1 deg C") %>%
  ggplot(aes(x = Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.3)) +  
  scale_color_viridis_d("Depth range") +
  scale_fill_viridis_d("Depth range") +
  ylab("Thermal_Occupancy")  +  
  facet_wrap(.~ Bin_width, scales = "free", ncol = 3) + 
  theme_cowplot() 

dev.off()

occup = as.data.frame(occup)

save(occup, file = "/Users/Kisei/jws_range/data/occupancy.RData")



