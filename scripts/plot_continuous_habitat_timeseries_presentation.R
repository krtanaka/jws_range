rm(list = ls())

library(dplyr)
library(ggpubr)
library(ggthemes)
library(raster)
library(scales)
library(zoo)

load("/Users/Kisei/jws_range/data/lat_area.RData")
load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/t_probablistic.Rdata")

df = merge(df, lat_area)
df = df %>% subset(depth > -1000)
df$month = substr(df$time, 6, 7)

########################################################
### lat-bin specific time series (daily or annually) ###
########################################################

df = df %>% subset(year %in% c(1982:2019))

t0 = df %>% 
  group_by(time) %>% 
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "Cali CC LME")

t0_year = t0 %>% 
  mutate(year = substr(time, 1, 4)) %>% 
  group_by(year) %>% 
  summarise(area = mean(area)) %>% 
  mutate(year = as.numeric(year),
         type = "Cali CC LME")

t1 = df %>% 
  group_by(time) %>% 
  subset(y >= 22.9 & y <= 34.4) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "S.boundary - Point Conception")

t1_year = t1 %>% 
  mutate(year = substr(time, 1, 4)) %>% 
  group_by(year) %>% 
  summarise(area = mean(area)) %>% 
  mutate(year = as.numeric(year),
         type = "S.boundary - Point Conception")

t2 = df %>% 
  group_by(time) %>% 
  subset(y >= 34.4 & y <= 37.8) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "Point Conception - San Francisco")

t2_year = t2 %>% 
  mutate(year = substr(time, 1, 4)) %>% 
  group_by(year) %>% 
  summarise(area = mean(area)) %>% 
  mutate(year = as.numeric(year),
         type = "Point Conception - San Francisco")

t3 = df %>% 
  group_by(time) %>% 
  subset(y >= 37.8) %>%
  mutate(area = area * p) %>% 
  summarise(area = sum(area)) %>% 
  mutate(time = as.Date(time),
         type = "San Francisco - N.boundary")

t3_year = t3 %>% 
  mutate(year = substr(time, 1, 4)) %>% 
  group_by(year) %>% 
  summarise(area = mean(area)) %>% 
  mutate(year = as.numeric(year),
         type = "San Francisco - N.boundary")

t3_missing_days = data.frame(time = setdiff(as.character(t0$time), as.character(t3$time)),
                             area = NA, 
                             type = "San Francisco - N.boundary")

t3_missing_days$time = as.Date(t3_missing_days$time)
t3 = rbind(t3, t3_missing_days)
t3 <- t3[order(t3$time),]

t = rbind(t1, t2, t3)
t = rbind(t0, t1, t2, t3)


t$type <- factor(t$type, levels = c(
  "22.9° N - 47.4° N (Cali CC LME)",
  "37.8° N - 47.4° N (San Francisco - N.boundary Cali CC LME)",
  "34.4° N - 37.8° N (Point Conception - San Francisco)",
  "22.9° N - 34.4° N (S.boundary Cali CC LME - Point Conception)"))

t_year = rbind(t0_year, t1_year, t2_year, t3_year)

t_year$type <- factor(t_year$type, levels = c(
  "22.9° N - 47.4° N (Cali CC LME)",
  "37.8° N - 47.4° N (San Francisco - N.boundary Cali CC LME)",
  "34.4° N - 37.8° N (Point Conception - San Francisco)",
  "22.9° N - 34.4° N (S.boundary Cali CC LME - Point Conception)"))

pdf(paste0("Fig.4_", Sys.Date(), ".pdf"), height = 7, width = 7)
ggplot(t_year, aes(x = year, y = area/10000, color = type, fill = type)) +
  # geom_point(, alpha = 0.8) +
  stat_smooth(method = "loess", span = 0.2, aes(color = type)) +
  stat_smooth(method = "lm", linetype = "dashed", se = F) +
  ylab(bquote('Available Habitat ('*10^4~km^2*')')) +  xlab("") +
  facet_wrap(.~type, ncol = 2, scales = "free_y") + 
  # ggdark::dark_theme_classic(I(12)) +
  # theme_few(I(12)) +
  theme(legend.position = "none")
dev.off()

