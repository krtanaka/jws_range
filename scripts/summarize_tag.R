rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)

# download "JWS_Corrected.RData" from https://osf.io/vcwjp/
load("/Users/kisei.tanaka/Desktop/JWS_Corrected.RData")

JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)
JWS_Corrected$month = substr(as.character(JWS_Corrected$Time_s), 6, 7)
JWS_Corrected$year = substr(as.character(JWS_Corrected$Time_s), 1, 4)

table = JWS_Corrected %>% 
  group_by(id) %>% 
  summarise(n = n(), 
            Begin_date = min(Date),
            End_date = max(Date),
            Sex = unique(sex),
            Release_location = unique(lon_pop),
            Release_locatio = unique(lat_pop),
            Pop_up_location = unique(lon_rel),
            Pop_up_locatio = unique(lat_rel),
            Total_length = median(length),
            Median_temp = median(Temperature), 
            Max_temp = max(Temperature),
            Min_temp = min(Temperature),
            Median_depth = median(Depth),
            max_depth = max(Depth))

table