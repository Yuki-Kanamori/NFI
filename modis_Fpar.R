require(tidyverse)
require(MODISTools)

dir = "/Users/Yuki/Dropbox/NFI"
setwd(dir)

site_lonlat = read.csv("site_lonlat.csv", fileEncoding = "CP932")
site_lonlat2 = site_lonlat2 %>% rename(site_name = tag)

modis_Fpar_t1 = mt_batch_subset(df = site_lonlat2,
                           product = "MCD15A2H",
                           band = "Fpar_500m",
                           km_lr = 0,
                           km_ab = 0,
                           start = "1999-04-01",
                           end = "2004-03-31",
                           internal = TRUE)
setwd(dir)
save(modis_Fpar_t1, "modis_Fpar_t1.Rdata")

modis_Fpar_t2 = mt_batch_subset(df = site_lonlat2,
                                product = "MCD15A2H",
                                band = "Fpar_500m",
                                km_lr = 0,
                                km_ab = 0,
                                start = "2004-04-01",
                                end = "2009-03-31",
                                internal = TRUE)
setwd(dir)
save(modis_Fpar_t2, "modis_Fpar_t2.Rdata")

modis_Fpar_t3 = mt_batch_subset(df = site_lonlat2,
                                product = "MCD15A2H",
                                band = "Fpar_500m",
                                km_lr = 0,
                                km_ab = 0,
                                start = "2009-04-01",
                                end = "2014-03-31",
                                internal = TRUE)
setwd(dir)
save(modis_Fpar_t3, "modis_Fpar_t3.Rdata")

modis_Fpar_t4 = mt_batch_subset(df = site_lonlat2,
                                product = "MCD15A2H",
                                band = "Fpar_500m",
                                km_lr = 0,
                                km_ab = 0,
                                start = "2014-04-01",
                                end = "2019-03-31",
                                internal = TRUE)
setwd(dir)
save(modis_Fpar_t4, "modis_Fpar_t4.Rdata")