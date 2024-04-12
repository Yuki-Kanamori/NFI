# require(httr)
require(ncdf4)
require(tidyverse)

# data = c("AMD_Area1_TMP_mea.nc")
# url = paste("https://www.data.jma.go.jp/gmd/goos/data/pub/JMA-product/mgd_sst_glb_D/", substr(time2[i], 1, 4), "/re_mgd_sst_glb_D", time2[i], ".txt.gz", sep = "")
# file = paste("/Users/Yuki/Dropbox/SST/all/mgd_sst_glb_D", time2[i], ".txt.gz", sep = "")
# download.file(url, file)
# 
# url = paste0("https://amd.rd.naro.go.jp/opendap/AMD/Area1/", i, "/", data, ".ascii?linear_scale([0:1:364][0:1:799][0:1:559])")
# url = "https://amd.rd.naro.go.jp:443/opendap/AMD/1999/eTMP_max/AMDy1999p3623eTMP_max.nc?TMP_max[0:1:364][0:1:79][0:1:79],lat[0:1:79],lon[0:1:79],time[0:1:364]"
# file = paste0("/Users/Yuki/Dropbox/LST/mean/AMDy1999p3623eTMP_max.nc.nc4")
# GET(url, authenticate("kanayuki", "CU8D0qye2024"), write_disk(file))

loc_lst = read.csv("/Users/Yuki/Dropbox/LST/mean/loc_sp.csv", fileEncoding = "CP932") 
since = ymd("1900-01-01")

a1 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/LST/mean/")
  year = i
  
  nc <- nc_open(paste0("area1/", year, "AMD_Area1_TMP_mea.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "TMP_mea")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = lst, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(lst))
  
  a1 = rbind(a1, year_all3)
}



a1 = a1 %>% mutate(tag = paste(lon, lat, sep = "_"))
tag1 = a1 %>% select(lon, lat, tag) %>% distinct(tag, .keep_all = TRUE)
a1_2 = a1 %>% group_by(nendo, tag) %>% summarize(mean = mean(mean))

