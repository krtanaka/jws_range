library(dplyr)
library(ggplot2)
library(readr)

setwd("/Users/kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/citizen science/iNat/")

i <- read_csv("iNat_observs-90879_21May2020.csv")
# i <- read_csv("observations-79116.csv")
i$year = substr(as.character(i$observed_on), 1, 4)

i = i %>% 
  subset(latitude < 37.2) %>% 
  mutate(year = substr(as.character(observed_on), 1, 4))

i_dummy = i[1,]
i_dummy$year = 2013
i_dummy$latitude = 37.99
i_dummy$longitude = -121.8
  
ii = rbind(i, i_dummy)


i.n <- plyr::ddply(.data=i, 
                 .(year), 
                 summarize, 
                 n=paste0("n=", length(user_id)))
i.2013 = data.frame(year = 2013, n = "n=0")
i.n = rbind(i.n, i.2013)

png('/Users/Kisei/Desktop/Fig.1.draft.png', height = 5, width = 9, units = 'in', res = 500)

ii %>% ggplot(aes(longitude, latitude, color = species_guess)) + 
  scale_color_viridis_d("") + 
  annotation_map(map_data("usa")) + #Add the map as a base layer before the points
  # borders(fill = "gray50") +
  coord_quickmap(xlim = range(pretty(i$longitude)),
                 ylim = range(pretty(i$latitude))) +
  geom_point(size = 5, alpha = 0.8) + 
  scale_x_continuous(breaks = round(seq(min(i$longitude), max(i$latitude), by = 0.5),0)) + 
  facet_wrap(.~ year, scales = "fixed", ncol = 8) +
  geom_text(data = i.n, aes(x = -Inf, y = -Inf, label = n, vjust = -1, hjust = -0.5), 
            colour = "black", inherit.aes = FALSE, parse = FALSE) + 
  xlab("") + ylab("") + 
  theme_pubr() +
  theme(legend.position = "bottom", 
        legend.justification = c(1,0))

dev.off()
