# require(httr)
require(ncdf4)
require(tidyverse)

loc_lst = read.csv("/Users/Yuki/Dropbox/LST/mean/loc_sp.csv", fileEncoding = "CP932") 
head(loc_lst, 3)
since = ymd("1900-01-01")

# area1
a1 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/SSD/")
  year = i
  
  nc <- nc_open(paste0("area1/", year, "AMD_Area1_SSD.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "SSD")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = ssd, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(ssd))
  
  a1 = rbind(a1, year_all3)
}

a1_2 = a1 %>% group_by(nendo, no) %>% summarize(mean_ssd = mean(mean))
ssd_a1 = a1_2
summary(ssd_a1)
save(ssd_a1, file = "ssd_a1.Rdata")


# area2
a2 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/SSD/")
  year = i
  
  nc <- nc_open(paste0("area2/", year, "AMD_Area2_SSD.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "SSD")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = ssd, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(ssd))
  
  a2 = rbind(a2, year_all3)
}

a2_2 = a2 %>% group_by(nendo, no) %>% summarize(mean_ssd = mean(mean))
ssd_a2 = a2_2
save(ssd_a2, file = "ssd_a2.Rdata")


# area3
a3 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/SSD/")
  year = i
  
  nc <- nc_open(paste0("area3/", year, "AMD_Area3_SSD.nc.nc4"))
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
    
    temp2 = temp %>% gather(key = lat, value = ssd, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(ssd))
  
  a3 = rbind(a3, year_all3)
}

a3_2 = a3 %>% group_by(nendo, no) %>% summarize(mean_ssd = mean(mean))
ssd_a3 = a3_2
save(ssd_a3, file = "ssd_a3.Rdata")


# area4
a4 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/SSD/")
  year = i
  
  nc <- nc_open(paste0("area4/", year, "AMD_Area4_SSD.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "SSD")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = ssd, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(ssd))
  
  a4 = rbind(a4, year_all3)
}

a4_2 = a4 %>% group_by(nendo, no) %>% summarize(mean_ssd = mean(mean))
ssd_a4 = a4_2
save(ssd_a4, file = "ssd_a4.Rdata")



# area5
a5 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/SSD/")
  year = i
  
  nc <- nc_open(paste0("area5/", year, "AMD_Area5_SSD.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "SSD")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = ssd, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(ssd))
  
  a5 = rbind(a3, year_all3)
}

a5_2 = a5 %>% group_by(nendo, no) %>% summarize(mean_ssd = mean(mean))
ssd_a5 = a5_2
save(ssd_a5, file = "ssd_a5.Rdata")


# area6
a6 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/SSD/")
  year = i
  
  nc <- nc_open(paste0("area6/", year, "AMD_Area6_SSD.nc.nc4"))
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
    
    temp2 = temp %>% gather(key = lat, value = ssd, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(ssd))
  
  a6 = rbind(a6, year_all3)
}

a6_2 = a6 %>% group_by(nendo, no) %>% summarize(mean_ssd = mean(mean))
ssd_a6 = a6_2
save(ssd_a6, file = "ssd_a6.Rdata")


ssd_all = rbind(a1_2, a2_2, a3_2, a4_2, a5_2, a6_2) %>% group_by(nendo, no) %>% summarize(mean = mean(mean_ssd))
loc_sp = read.csv("/Users/Yuki/Dropbox/NFI/loc_sp.csv")
head(loc_sp)
ssd_all = left_join(ssd_all, loc_sp, by = "no")
head(ssd_all, 3)
save(ssd_all, file = "ssd_all.Rdata")
summary(ssd_all)
