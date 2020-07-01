rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
probs <- c(0, 0.025, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95, 0.975, 1)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")

JWS_Corrected = JWS_Corrected %>% as.data.frame %>% sample_frac(0.001)

# JWS_Corrected$Temperature = round(JWS_Corrected$Temperature, 1)
JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)

d1 = subset(JWS_Corrected, Depth <= 20); d1 = d1[,c("Temperature", "Depth", "id")]; d1$Depth_Range = "0-20m"; d1$count = 1
d2 = subset(JWS_Corrected, Depth <= 2); d2 = d2[,c("Temperature", "Depth", "id")]; d2$Depth_Range = "0-2m"; d2$count = 1

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

# pdf("thermal_profile.pdf", height = 3, width = 10)
# 
t %>%
  ggplot(aes(x = Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  # geom_bar(stat="identity", position = position_dodge(width = 0.5)) +
  geom_density(stat = "identity", alpha = 0.5) +
  # scale_color_viridis_d() +
  # scale_fill_viridis_d() +
  ylab("Freq") +
  facet_wrap(.~ Bin_width, scales = "free", ncol = 3)+
  theme_cowplot()
#   
# dev.off()

occup = t %>% 
  group_by(Depth_Range, Bin_width) %>% 
  mutate(sum = colSums(.[3]),
         count = count/sum)

d = subset(occup, Bin_width == "0.5 deg C" & Depth_Range == "0-20m")

library(mgcv)
g1 = gam(count ~ s(Temperature), data = d, family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", gamma = 1.4)
g2 = gam(count ~ s(Temperature, k = 5), data = d, family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", gamma = 1.4)
g3 = gam(count ~ s(Temperature, k = 3), data = d, family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", gamma = 1.4)
d$pred_count_1 = predict(g1, d, se.fit = F, type = "response")
d$pred_count_2 = predict(g2, d, se.fit = F, type = "response")
d$pred_count_3 = predict(g3, d, se.fit = F, type = "response")
d$weight_obs = (d$count-min(d$count))/(max(d$count) - min(d$count))
d$weight_m1 = (d$pred_count_1 - min(d$pred_count_1))/(max(d$pred_count_1) - min(d$pred_count_1))
d$weight_m2 = (d$pred_count_2 - min(d$pred_count_2))/(max(d$pred_count_2) - min(d$pred_count_2))
d$weight_m3 = (d$pred_count_3 - min(d$pred_count_3))/(max(d$pred_count_3) - min(d$pred_count_3))

q = 95

hq = 1-(1-q/100)/2
lq = (1-q/100)/2

d = as.data.frame(d)

l_o = wtd.quantile(d$Temperature, q = lq, weight = d$weight_obs, na.rm = T)
l_1 = wtd.quantile(d$Temperature, q = lq, weight = d$weight_m1, na.rm = T)
l_2 = wtd.quantile(d$Temperature, q = lq, weight = d$weight_m2, na.rm = T)
l_3 = wtd.quantile(d$Temperature, q = lq, weight = d$weight_m3, na.rm = T)
t_l = data.frame(t = rbind(l_o, l_1, l_2, l_3), g = c("Observation", "Model_A", "Model_B", "Model_C"))
colnames(t_l) = c("t", "g")
h_o = wtd.quantile(d$Temperature, q = hq, weight = d$weight_obs, na.rm = T)
h_1 = wtd.quantile(d$Temperature, q = hq, weight = d$weight_m1, na.rm = T)
h_2 = wtd.quantile(d$Temperature, q = hq, weight = d$weight_m2, na.rm = T)
h_3 = wtd.quantile(d$Temperature, q = hq, weight = d$weight_m3, na.rm = T)
t_h = data.frame(t = rbind(h_o, h_1, h_2, h_3), g = c("Observation", "Model_A", "Model_B", "Model_C"))
colnames(t_h) = c("t", "g")
T_opt_o = sum(d$weight_obs * d$Temperature/sum(d$weight_obs)) #just weighted mean
T_opt_1 = sum(d$weight_m1 * d$Temperature/sum(d$weight_m1)) #just weighted mean
T_opt_2 = sum(d$weight_m2 * d$Temperature/sum(d$weight_m2)) #just weighted mean
T_opt_3 = sum(d$weight_m3 * d$Temperature/sum(d$weight_m3)) #just weighted mean
t_opt = data.frame(t = rbind(T_opt_o, T_opt_1, T_opt_2, T_opt_3), g = c("Observation", "Model_A", "Model_B", "Model_C"))
colnames(t_opt) = c("t", "g")

d1 = d[,c("Temperature", "count", "weight_obs")]; d1$g = "Observation"; colnames(d1) = c("t", "n", "w", "g")
d2 = d[,c("Temperature", "pred_count_1", "weight_m1")]; d2$g = "Model_A"; colnames(d2) = c("t", "n", "w", "g")
d3 = d[,c("Temperature", "pred_count_2", "weight_m2")]; d3$g = "Model_B"; colnames(d3) = c("t", "n", "w", "g")
d4 = d[,c("Temperature", "pred_count_3", "weight_m3")]; d4$g = "Model_C"; colnames(d4) = c("t", "n", "w", "g")

d = rbind(d1, d2, d3, d4); rm(d1, d2, d3, d4)

setwd("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/figure 2 thermal niche profiling")
pdf(paste0('Fig.2a_', Sys.Date(), '.pdf'), height = 8, width = 6)
# png(paste0('Fig.2a_', Sys.Date(), '.png'), height = 5, width = 4, units = "in", res = 100)

# d$g <- factor(d$g, levels = c("Observation", "Model_A", "Model_B", "Model_C"))
# d %>%
#   ggplot(aes(t, n, color = g)) +
#   geom_point(size = 2, alpha = 0.8) +
#   geom_vline(data = t_opt, mapping = aes(xintercept = t, color = g), show.legend = FALSE) +
#   geom_vline(data = t_h, mapping = aes(xintercept = t, color = g), show.legend = FALSE) +
#   geom_vline(data = t_l, mapping = aes(xintercept = t, color = g), show.legend = FALSE) +
#   xlim(9, 27) +
#   geom_text(data = t_opt, aes(x = 9, y = c(0.004, 0.0038, 0.0036, 0.0034),
#                               label = paste0("t_opt=", round(t, 1))), hjust = 0, show.legend = FALSE) +
#   geom_text(data = t_l, aes(x = 9, y = c(0.003, 0.0028, 0.0026, 0.0024),
#                               label = paste0("0.05p=", round(t, 1))), hjust = 0, show.legend = FALSE) +
#   geom_text(data = t_h, aes(x = 9, y = c(0.002, 0.0018, 0.0016, 0.0014),
#                               label = paste0("0.95p=", round(t, 1))), hjust = 0, show.legend = FALSE) +
#   scale_color_viridis_d("") +
#   ylab("JWS Thermal Occupancy") + xlab("Temperature (deg C)") +
#   theme(legend.position = c(0.75, 0.9))

library(Hmisc)
library(plyr)

box = d %>% subset(g == "Observation")

df.wm = ddply(box, .(g), summarize, wmean = round(wtd.mean(t, w, na.rm = T), 2))

d1 = d %>% 
  subset(g == "Observation") %>% 
  ggplot(aes(x = g, y = t, weight = w)) + 
  geom_boxplot(width = 0.1, fatten = NULL, color = "#352A87", fill = "#352A87", alpha = 0.8) + 
  # geom_point(data = df.wm, aes(x = g, y = wmean), size = 3, inherit.aes = FALSE) + 
  coord_flip() + 
  theme_pubr() + 
  ylab("Temperature (deg C)") + xlab("") + 
  theme(legend.position = "none",
        axis.line = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = unit(c(0.5, 0.4, 0.5, 2),"cm")) #top, right, bottom, left

d2 = d %>% 
  subset(g == "Observation") %>% 
  ggplot(aes(t,  w,  color = g)) + 
  # geom_point(size = 2, color = "#352A87", alpha = 0.8) +
  geom_bar(color = "#352A87", fill = "#352A87", stat = "identity", position = "identity", width = 0.5, alpha = 0.8) +
  geom_segment(aes(x = 10, xend = 15.13, y = 0, yend = 0), size = 2, color = "#33B7A0", alpha = 0.8) + 
  geom_segment(aes(x = 15.13, xend = 15.13, y = 0, yend = 1), size = 2, color = "#33B7A0", alpha = 0.8) +  
  geom_segment(aes(x = 15.13, xend = 21.9, y = 1, yend = 1), size = 2, color = "#33B7A0", alpha = 0.8) +  
  geom_segment(aes(x = 21.9, xend = 21.9, y = 1, yend = 0), size = 2, color = "#33B7A0", alpha = 0.8) + 
  geom_segment(aes(x = 21.9, xend = 25, y = 0, yend = 0), size = 2, color = "#33B7A0", alpha = 0.8) + 
  annotate("text", x = 10, y = 1, label = "Binary", color = "#33B7A0", hjust = 0, size = 4) + 
  annotate("text", x = 10, y = 0.9, label = "Probablistic", color = "#352A87", hjust = 0, size = 4) + 
  ylab("JWS Thermal Occupancy") + xlab("Temperature (deg C)")

cowplot::plot_grid(d1, d2, cols = 1)

dev.off()

occup = as.data.frame(occup)

# save(occup, file = "/Users/Kisei/jws_range/data/occupancy.RData")

d = subset(occup, Bin_width == "0.5 deg C")

q = 95

hq = 1-(1-q/100)/2
lq = (1-q/100)/2

d = d %>% group_by(Depth_Range) %>% 
  mutate(count = (count-min(count))/(max(count) - min(count)),
         l = wtd.quantile(Temperature, q = lq, weight = count, na.rm = T),
         h = wtd.quantile(Temperature, q = hq, weight = count, na.rm = T),
         o = sum(count * Temperature/sum(count)))

d$Depth_Range <- factor(d$Depth_Range, levels = c("0-20m", "0-2m"))

setwd("C:/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/figures/figure 2 thermal niche profiling/")

pdf(paste0("Fig.1_", Sys.Date(), ".pdf"), height = 4, width = 6)
# png(paste0("Fig.1_", Sys.Date(), ".png"), height = 4, width = 6, res = 500, units = "in")

d1 = d %>% 
  # subset(Depth_Range == "0-20m") %>% 
  ggplot(aes(Temperature, count)) + 
  # geom_boxplot(aes(fill = Depth_Range))
  geom_bar(aes(fill = Depth_Range), stat = "identity", position = "identity", width = 0.5, alpha = 0.8) +
  # geom_density(aes(fill = Depth_Range, color = Depth_Range), stat = "identity", position = "identity", alpha = 0.1) +
  geom_segment(data = subset(d, Depth_Range == "0-2m"), aes(x = l, xend = h, y = 1.03, yend = 1.03, color = Depth_Range), show.legend = F) + 
  geom_segment(data = subset(d, Depth_Range == "0-2m"), aes(x = l, xend = l, y = 1.00, yend = 1.03, color = Depth_Range), show.legend = F) + 
  geom_segment(data = subset(d, Depth_Range == "0-2m"), aes(x = h, xend = h, y = 1.00, yend = 1.03, color = Depth_Range), show.legend = F) + 
 
  geom_segment(data = subset(d, Depth_Range == "0-20m"), aes(x = l, xend = h, y = 1.05, yend = 1.05, color = Depth_Range), show.legend = F) + 
  geom_segment(data = subset(d, Depth_Range == "0-20m"), aes(x = l, xend = l, y = 1.01, yend = 1.05, color = Depth_Range), show.legend = F) + 
  geom_segment(data = subset(d, Depth_Range == "0-20m"), aes(x = h, xend = h, y = 1.01, yend = 1.05, color = Depth_Range), show.legend = F) + 
  
  scale_color_viridis_d("Depth") + 
  scale_fill_viridis_d("Depth") + 
  theme_pubr() + 
  annotate("text", label = "Core Thermal Habitat (95 IQR)", x = 17.96, y = 1.09) + 
  ylab("Normalized Thermal Occupancy") + xlab("Temperature (deg C)") + 
  theme(legend.position = c(0.1, 0.9))


d2 = d %>% 
  ggplot()  + 
  geom_boxplot(aes(count, Temperature, fill = Depth_Range, color = Depth_Range), position=position_dodge(0.5), alpha = 0.8) + 
  scale_color_viridis_d("") + 
  scale_fill_viridis_d("") + 
  coord_flip() + 
  theme_pubr() + 
  theme(legend.position = "none")

library(Hmisc)
library(plyr)
df.wm = ddply(d, .(Depth_Range), summarize, wmean = round(wtd.mean(Temperature, count, na.rm = T), 2))

d2 = d %>% 
  ggplot(aes(x = Depth_Range, y = Temperature, weight = count, fill = Depth_Range, color = Depth_Range,)) + 
  geom_boxplot(width = 0.1, fatten = NULL) + 
  geom_point(data = df.wm, aes(x = Depth_Range, y = wmean), size = 3, inherit.aes = FALSE) + 
  scale_fill_viridis_d("") + 
  scale_color_viridis_d("") + 
  coord_flip() + 
  theme_pubr() + 
  theme(legend.position = "none")

gridExtra::grid.arrange(d1,d2,nrow=2)


dev.off()


  


