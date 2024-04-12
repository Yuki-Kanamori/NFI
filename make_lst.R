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

# area1
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

a1_2 = a1 %>% group_by(nendo, no) %>% summarize(mean = mean(mean))
lst_a1 = a1_2
summary(lst_a1)
save(lst_a1, file = "lst_a1.Rdata")


# area2
a2 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/LST/mean/")
  year = i
  
  nc <- nc_open(paste0("area2/", year, "AMD_Area2_TMP_mea.nc.nc4"))
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
  
  a2 = rbind(a2, year_all3)
}

a2_2 = a2 %>% group_by(nendo, no) %>% summarize(mean = mean(mean))
lst_a2 = a2_2
save(lst_a2, file = "lst_a2.Rdata")


# area3
a3 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/LST/mean/")
  year = i
  
  nc <- nc_open(paste0("area3/", year, "AMD_Area3_TMP_mea.nc.nc4"))
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
  
  a3 = rbind(a3, year_all3)
}

a3_2 = a3 %>% group_by(nendo, no) %>% summarize(mean = mean(mean))
lst_a3 = a3_2
save(lst_a3, file = "lst_a3.Rdata")


# area4
a4 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/LST/mean/")
  year = i
  
  nc <- nc_open(paste0("area4/", year, "AMD_Area4_TMP_mea.nc.nc4"))
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
  
  a4 = rbind(a4, year_all3)
}

a4_2 = a4 %>% group_by(nendo, no) %>% summarize(mean = mean(mean))
lst_a4 = a4_2
save(lst_a4, file = "lst_a4.Rdata")



# area5
a5 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/LST/mean/")
  year = i
  
  nc <- nc_open(paste0("area5/", year, "AMD_Area5_TMP_mea.nc.nc4"))
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
  
  a5 = rbind(a3, year_all3)
}

a5_2 = a5 %>% group_by(nendo, no) %>% summarize(mean = mean(mean))
lst_a5 = a5_2
save(lst_a5, file = "lst_a5.Rdata")


# area6
a6 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/LST/mean/")
  year = i
  
  nc <- nc_open(paste0("area6/", year, "AMD_Area6_TMP_mea.nc.nc4"))
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
  
  a6 = rbind(a6, year_all3)
}

a6_2 = a6 %>% group_by(nendo, no) %>% summarize(mean = mean(mean))
lst_a6 = a6_2
save(lst_a6, file = "lst_a6.Rdata")
