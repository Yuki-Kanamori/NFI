require(tidyverse)
require(MODISTools)

dir = "/Users/Yuki/Dropbox/NFI"
setwd(dir)

site_lonlat = read.csv("site_lonlat.csv", fileEncoding = "CP932")

modis = NULL
for(i in 1:nrow(site_lonlat)){
  lon = site_lonlat[i, "lon"]
  lat = site_lonlat[i, "lat"]
  tag = site_lonlat[i, "tag"]
  
  subset <- mt_subset(product = "MOD11A2",
                      lat = lat,
                      lon = lon,
                      band = "LST_Day_1km",
                      start = "2000-02-18",
                      end = "2004-03-31",
                      km_lr = 0,
                      km_ab = 0,
                      site_name = tag,
                      internal = TRUE)
  
  subset$time = as.Date(subset$calendar_date)
  temperature = subset$value * as.double(subset$scale) 
  temperature[temperature == 0] <- NA
  subset$lst = temperature - 273.15
  
  subset2 = subset %>% select(time, site, longitude, latitude, lst) %>% rename(tag = site) %>% mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)))
  
  modis = rbind(modis, subset2)
}
modis_t1 = modis
setwd(dir)
save(modis_t1, "modis_t1.Rdata")