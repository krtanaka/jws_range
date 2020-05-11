library(raster)

# d = stack("/Users/ktanaka/Dropbox (MBA)/Data/oisst/gebco_2020_n50.0_s20.0_w-130.0_e-110.0.nc")
load("/Users/Kisei/jws_range/data/gebco_2020_n50.0_s20.0_w-130.0_e-110.0.RData") 
e = extent(-140, -100, 22.50, 47.50)
d = crop(d, e); rm(e)

load("/Users/Kisei/jws_range/data/sst.day.mean.1984.RData")

d = rasterToPoints(d)
d = as.data.frame(d)
d = subset(d, layer < 0)
d = subset(d, layer > -1000)
d$x = round(d$x, 1)
d$y = round(d$y, 1)
d = aggregate(.~x+y, d, mean)
colnames(d) = c("x", "y", "depth")
d = rasterFromXYZ(d)

o = df[[1]]
d = resample(d, o, method = "bilinear") 
plot(d)

setwd('/Users/Kisei/jws_range/data/')
save(d, file = "depth_0.25.Rdata")