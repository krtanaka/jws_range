library(bbmle)
library(stats4)
library(ggplot2)
library(tidyverse)
library(FSA)
library(tidyr)
library(scales)
library(sf)
library(readr)
library(dplyr)
library(lwgeom)
library(ggjoy)
library(ggridges)
library(RColorBrewer)
library(readr)
library(data.table)
library(viridis)
library(ggthemes)


## load randomized output of shark lengths from Eric's observations
## this the quasi bootstrapping approach from Eric's log book records
## convert to data frame and collapse all 1000 columns into 1
## this makes a tall or long df from a wide one
GWSlength_wide <- read.csv('lengths_random.csv')
## GWSlength_wide <- read.csv('lengths_random_saleric.csv')
kyle_vector_list = vector("list")
for (i in 4:dim(GWSlength_wide)[2]) {
  # i = 4
  df_i = GWSlength_wide[,c(1:3, i)]
  colnames(df_i) = c("year", "unique_ID", "date", "length_m")
  kyle_vector_list[[i]] = df_i
  print(i)
}
GWSlength_long = rbindlist(kyle_vector_list)
write.csv(GWSlength_long, 'GWSlength_long.csv')

## buld a histogram from these length observation data
## use ggplot to make a density plot
## helps are here https://rpkgs.datanovia.com/ggpubr/

## old version of the plot
## ggplot(GWSlength_long, aes(x=length_m)) +
## theme_few()+
## geom_density(color="#E7B800", fill="#E7B800", alpha=0.4) +
## geom_vline(aes(xintercept=mean(length_m)), color="#E7B800", linetype="dashed")
## + facet_wrap(~year, scales = "free_y", ncol=2)
##
## load Sal's observations from Eric's boat, from 5 trips in 2018-19
## use ggplot to make a density plot
## Sal_length <- read.csv('Sal_GWS_lengths.csv', header=T)
## ggplot(Sal_length, aes(x=length_m, fill=year)) +
## theme_few()+
## geom_density(color="#E7B800", fill="#E7B800", alpha=0.4) +
## scale_x_continuous(limits=c(0.5,5.3),breaks=c(1,2,3,4,5)) +
## geom_vline(aes(xintercept=mean(length_m)), color="#E7B800", linetype="dashed")


## this csv adds Sal's obs without randomizations at endof the 'long' csv we made above
## I could but yet have not added some random jitter/wiggle to Sal's obs
## this below treats Sal's davlidated observations as literal fact which is okay IMO
GWSlength_long_saleric <- read.csv('./data/GWSlength_long_saleric.csv', header = T)
ggplot(GWSlength_long_saleric, aes(x = length_m, group = source, color = source)) +
  ggthemes::theme_few()+
  geom_density(aes(color = source, fill = source), alpha=0.4) +
  scale_color_manual(values = c("#56B4E9", "#E7B800")) +
  scale_fill_manual(values = c("#56B4E9", "#E7B800")) +
  geom_vline(aes(xintercept = mean(length_m)), color = "#56B4E9", linetype="dashed")

d1 = subset(GWSlength_long_saleric, source == "sal")
d2 = subset(GWSlength_long_saleric, source == "eric")

## Kisei had 1.75-3, I am revising to 1-2.5
## based on Sal's feedback 
d1$juv = ifelse(d1$length_m >= 1 & d1$length_m <= 2.5, 1, 0)
d2$juv = ifelse(d2$length_m >= 1 & d2$length_m <= 2.5, 1, 0)

sum(d1$juv)/dim(d1)[1]
sum(d2$juv)/dim(d2)[1]

