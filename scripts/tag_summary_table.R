rm(list = ls())

library(ggpubr)
library(dplyr)
library(cowplot)
library(reldist)

load("/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")
load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/tags/JWS_Corrected.RData")

JWS_Corrected$id = substr(as.character(JWS_Corrected$id), 1, 9)
JWS_Corrected$month = substr(as.character(JWS_Corrected$Time_s), 6, 7)
JWS_Corrected$year = substr(as.character(JWS_Corrected$Time_s), 1, 4)

table = JWS_Corrected %>% 
  group_by(id) %>% 
  summarise(n = n(), 
            begin_date = min(Date),
            end_date = max(Date),
            sex = unique(sex),
            total_length = median(length),
            median_temp = median(Temperature), 
            max_temp = max(Temperature),
            min_temp = min(Temperature),
            median_depth = median(Depth),
            max_depth = max(Depth),
            min_depth = min(Depth))

table

readr::write_csv(table, "~/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/figures/supplement/TableS1_jws_tag_summary.csv")
