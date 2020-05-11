library(raster)

d = stack("/Users/ktanaka/Dropbox (MBA)/Data/oisst/gebco_2020_n50.0_s20.0_w-130.0_e-110.0.nc")
e = extent(-140, -100, 22.50, 47.50) #California Current LME lat-lon range, just to reduce Hadley_SST data size
d = crop(d, e); rm(e)

load("/Users/ktanaka/Dropbox (MBA)/Data/oisst/sst.day.mean.1984.RData")

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

save(d, file = "/Users/Kisei/Dropbox/oisst/depth_0.25.Rdata")

load('/Users/Kisei/Dropbox/oisst/depth_0.25.Rdata')
