
grid_cell_size = 615.5493

load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/t_IQR.Rdata")

t1 = df %>%
  group_by(time) %>%
  summarise(
    total = n(),
    good = sum(z>0, na.rm = T)
    # ,prop = good/total
    ) %>% 
  mutate(total = total*grid_cell_size,
         good = good*grid_cell_size,
         time = as.Date(time))

t1 = t1[,c("time", "good")]
colnames(t1) = c("time", "area")
t1$type = "binary"

load("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei Bia JWS range shift/data/t_probablistic.Rdata")
t2 = df %>%
  mutate(p = p*grid_cell_size) %>% 
  group_by(time) %>% 
  summarise(p = sum(p)) %>% 
  mutate(time = as.Date(time))

t2 = t2[,c("time", "p")]
colnames(t2) = c("time", "area")
t2$type = "probablistic"

t = rbind(t1, t2)

t$month = substr(as.character(t$time), 6, 7)
t$year = substr(as.character(t$time), 1, 4)

library(zoo)
library(ggpubr)
library(gridExtra)

ggplot(t, aes(x = time, y = area, color = area, fill = type)) +
  geom_line(aes(y = rollmean(area, 10, na.pad = TRUE))) +
  scale_color_viridis_c("km^2") + 
  stat_smooth(method = "loess", span = 0.1) +
  ylab("Total Habitat Area (km^2)") + 
  ggtitle("10-day running mean") + 
  theme_classic2() 

t1 = t; t1$calender = "Jan-Dec"
t2 = t; t2$calender = "Jun-Oct"

t$area = ifelse(t$month %in% c("06", "07", "08", "09", "10"), t$area, NA)



