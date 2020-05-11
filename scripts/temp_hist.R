library(ggplot2)
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
quantile(q_01$Temperature, probs = probs)

q_02 = aggregate(count ~ Temperature, data = d2, FUN = "sum")
quantile(q_02$Temperature, probs = probs)

d = rbind(d1, d2)

d %>% ggplot(aes(Temperature, count, color = Depth_Range, fill = Depth_Range)) + 
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  facet_wrap(.~id, scales = "free_x") +
  theme(legend.position = "right")

d$Temperature = round(d$Temperature, 1)

d = d %>% group_by(Temperature, Depth_Range) %>% 
  summarise(count = sum(count))

d %>% 
  ggplot(aes(x=Temperature, y = count, color = Depth_Range, fill = Depth_Range)) +
  geom_bar(stat="identity", position = position_dodge(width = 1), alpha = 0.2) 
  

