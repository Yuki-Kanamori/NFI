require(ncdf4)
require(tidyverse)

loc_lst = read.csv("/Users/Yuki/Dropbox/LST/mean/loc_sp.csv", fileEncoding = "CP932") 
head(loc_lst, 3)
length(unique(loc_lst$no))
since = ymd("1900-01-01")

# area1
a1 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/GSR/")
  year = i
  
  nc <- nc_open(paste0("area1/", year, "AMD_Area1_GSR.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "GSR")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = gsr, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(gsr))
  
  a1 = rbind(a1, year_all3)
}

a1_2 = a1 %>% group_by(nendo, no) %>% summarize(mean_gsr = mean(mean))
gsr_a1 = a1_2
summary(gsr_a1)
setwd("/Users/Yuki/Dropbox/GSR/")
save(gsr_a1, file = "gsr_a1.Rdata")


# area2
a2 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/GSR/")
  year = i
  
  nc <- nc_open(paste0("area2/", year, "AMD_Area2_GSR.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "GSR")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = gsr, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(gsr))
  
  a2 = rbind(a2, year_all3)
}

a2_2 = a2 %>% group_by(nendo, no) %>% summarize(mean_gsr = mean(mean))
gsr_a2 = a2_2
setwd("/Users/Yuki/Dropbox/GSR/")
save(gsr_a2, file = "gsr_a2.Rdata")


# area3
a3 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/GSR/")
  year = i
  
  nc <- nc_open(paste0("area3/", year, "AMD_Area3_GSR.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "GSR")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = gsr, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(gsr))
  
  a3 = rbind(a3, year_all3)
}

a3_2 = a3 %>% group_by(nendo, no) %>% summarize(mean_gsr = mean(mean))
gsr_a3 = a3_2
setwd("/Users/Yuki/Dropbox/GSR/")
save(gsr_a3, file = "gsr_a3.Rdata")


# area4
a4 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/GSR/")
  year = i
  
  nc <- nc_open(paste0("area4/", year, "AMD_Area4_GSR.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "GSR")
  # dim(T) #[1]  640 480 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = gsr, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(gsr))
  
  a4 = rbind(a4, year_all3)
}

a4_2 = a4 %>% group_by(nendo, no) %>% summarize(mean_gsr = mean(mean))
gsr_a4 = a4_2
setwd("/Users/Yuki/Dropbox/GSR/")
save(gsr_a4, file = "gsr_a4.Rdata")



# area5
a5 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/GSR/")
  year = i
  
  nc <- nc_open(paste0("area5/", year, "AMD_Area5_GSR.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "GSR")
  # dim(T) #[1] 400 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = gsr, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    head(temp2, 3)
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(gsr))
  
  a5 = rbind(a5, year_all3)
}

# check Kyushu
head(a5, 3)
a5 = left_join(a5, loc_lst, by = "no")
pcod_s <- st_as_sf(a5, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = mean), pch=15,cex=0.5) +
  facet_wrap(~ nendo) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))


a5_2 = a5 %>% group_by(nendo, no) %>% summarize(mean_gsr = mean(mean))
gsr_a5 = a5_2
setwd("/Users/Yuki/Dropbox/GSR/")
save(gsr_a5, file = "gsr_a5.Rdata")


# area6
a6 = NULL
for(i in 1999:2024){
  setwd("/Users/Yuki/Dropbox/GSR/")
  year = i
  
  nc <- nc_open(paste0("area6/", year, "AMD_Area6_GSR.nc.nc4"))
  print(nc)
  T <- ncvar_get(nc, "GSR")
  # dim(T) #[1] 560 800 365 lon, lat, time
  
  time = data.frame(number = nc[["dim"]][["time"]][["vals"]]) %>% mutate(time = since + days(number))
  
  temp = NULL
  year_all = NULL
  for(j in 1:nrow(time)){
    temp = data.frame(T[, , j])
    colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
    temp$lon = nc[["dim"]][["lon"]][["vals"]]
    
    temp2 = temp %>% gather(key = lat, value = gsr, -lon) %>% mutate(time = paste(time[j, "time"])) %>% na.omit() %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_"))
    
    loc = loc_lst
    temp3 = left_join(loc, temp2, by = "tag2")
    
    year_all = rbind(year_all, temp3)
  }
  
  year_all2 = year_all %>% mutate(nendo = ifelse(month < 4, (as.numeric(i)-1), i))
  year_all3 = year_all2 %>% group_by(no, nendo) %>% summarize(mean = mean(gsr))
  
  a6 = rbind(a6, year_all3)
}

# # check Kyushu
# head(a6, 3)
# a6 = left_join(a6, loc_lst, by = "no")
# pcod_s <- st_as_sf(a6, coords=c("lon", "lat"))
# ggplot(pcod_s) + 
#   geom_sf(aes(color = mean), pch=15,cex=0.5) +
#   # facet_wrap(~ year) + 
#   theme_minimal() +
#   scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))


a6_2 = a6 %>% group_by(nendo, no) %>% summarize(mean_gsr = mean(mean))
gsr_a6 = a6_2
setwd("/Users/Yuki/Dropbox/GSR/")
save(gsr_a6, file = "gsr_a6.Rdata")



# 結合 ----------------------------------------------------------------------
setwd("/Users/Yuki/Dropbox/GSR/")
load("gsr_a1.Rdata"); load("gsr_a2.Rdata"); load("gsr_a3.Rdata")
load("gsr_a4.Rdata"); load("gsr_a5.Rdata"); load("gsr_a6.Rdata")

gsr_all = rbind(gsr_a1, gsr_a2, gsr_a3, gsr_a4, gsr_a5, gsr_a6) %>% group_by(nendo, no) %>% summarize(mean = mean(mean_gsr))
loc_sp = read.csv("/Users/Yuki/Dropbox/NFI/loc_sp.csv")
head(loc_sp)
gsr_all = left_join(gsr_all, loc_sp, by = "no")
head(gsr_all, 3)
save(gsr_all, file = "gsr_all.Rdata")
summary(gsr_all)
