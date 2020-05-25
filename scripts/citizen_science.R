setwd("C:/Users/Kisei/Dropbox/PAPER Kisei Bia JWS range shift/data/citizen science")

i <- read_csv("iNat_observs-90879_21May2020.csv")
# i <- read_csv("observations-79116.csv")
i$year = substr(as.character(i$observed_on), 1, 4)

i = i %>% 
  subset(latitude < 37.2) %>% 
  mutate(year = substr(as.character(observed_on), 1, 4))

library(dplyr)
library(ggplot2)

i.n <- plyr::ddply(.data=i, 
                 .(year), 
                 summarize, 
                 n=paste("n =", length(user_id)))

png('/Users/Kisei/Desktop/Fig.1.draft.png', height = 5, width = 9, units = 'in', res = 500)

i %>% ggplot(aes(longitude, latitude, color = species_guess)) + 
  scale_color_viridis_d("") + 
  borders(fill = "gray50") +
  coord_quickmap(xlim = range(pretty(i$longitude)),
                 ylim = range(pretty(i$latitude))) +
  geom_point(size = 5, alpha = 0.5) + 
  scale_x_continuous(breaks = round(seq(min(i$longitude), max(i$latitude), by = 0.5),0)) + 
  facet_wrap(.~ year, scales = "fixed", ncol = 7) +
  geom_text(data = i.n, aes(x = -Inf, y = -Inf, label = n, vjust = -1, hjust = -0.5), 
            colour = "black", inherit.aes = FALSE, parse = FALSE) + 
  xlab("") + ylab("") + 
  theme_classic() + 
  theme(legend.position = "bottom", 
        legend.justification = c(1,0))

dev.off()
