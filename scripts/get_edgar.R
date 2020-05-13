library(raster)
library(rasterVis)

rm(list = ls())
e = extent(220, 260, 22.50, 47.50) #California Current LME lat-lon range

###########
### CH4 ###
###########

GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/edgar_shipping/v50_CH4_2015_monthly_TNR_Ship_nc/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/edgar_shipping/v50_CH4_2015_monthly_TNR_Ship_nc/", GHG_ts) # add parent directories
emissions <- "emi_ch4"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  year_month_read = crop(year_month_read, e)
  year_month_read = raster::rotate(year_month_read) #rotate to -180:180
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

ch4 = mean(ghg)

################
### CO2 excl ###
################
GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/edgar_shipping/v50_CO2_excl_short-cycle_org_C_2015_monthly_TNR_Ship_nc/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/edgar_shipping/v50_CO2_excl_short-cycle_org_C_2015_monthly_TNR_Ship_nc/", GHG_ts) # add parent directories
emissions <- "emi_co2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  year_month_read = crop(year_month_read, e)
  year_month_read = raster::rotate(year_month_read) #rotate to -180:180
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

co2_excl = mean(ghg)

###############
### CO2 org ###
###############
GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/edgar_shipping/v50_CO2_org_short-cycle_C_2015_monthly_TNR_Ship_nc/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/edgar_shipping/v50_CO2_org_short-cycle_C_2015_monthly_TNR_Ship_nc/", GHG_ts) # add parent directories
emissions <- "emi_co2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  year_month_read = crop(year_month_read, e)
  year_month_read = raster::rotate(year_month_read) #rotate to -180:180
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

co2_org = mean(ghg)

###########
### N2O ###
###########
GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/edgar_shipping/v50_N2O_2015_monthly_TNR_Ship_nc/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/edgar_shipping/v50_N2O_2015_monthly_TNR_Ship_nc/", GHG_ts) # add parent directories
emissions <- "emi_n2o"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  year_month_read = crop(year_month_read, e)
  year_month_read = raster::rotate(year_month_read) #rotate to -180:180
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

n2o = mean(ghg)

df = sum(ch4, co2_excl, co2_org, n2o)

plot(log(df), col = matlab.like(100) ); maps::map(add = T, fill = T, resolution = 0)

load('/Users/ktanaka/jws_range/data/depth_0.25.Rdata')

df = resample(df, d, method = "bilinear") 
plot(df)

