
# packages ----------------------------------------------------------------
require(tidyverse)
require(ncdf4)
require(fuzzyjoin)

# data (species) ---------------------------------------------------------------
dirname = "/Users/Yuki/Dropbox/NFI"
setwd(dir = dirname)

load("akamatsu_n_0.Rdata") # 変なデータを抜く前のデータ．こっちの方が地点の情報が多いから
df = df_n_0_akamatsu
head(df, 3)
unique(df$type) # 変なデータを抜く前．データ全部と考えて良い．
loc_sp = df %>% mutate(tag = paste(lon, lat, sep ="_")) %>% select(lon, lat, tag) %>% distinct(tag, .keep_all = TRUE)
# write.csv(loc_sp, "loc_sp.csv", fileEncoding = "CP932", row.names = F)
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = n), pch=15,cex=0.5) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))



loc_sp2 = read.csv("/Users/Yuki/Dropbox/LST/mean/loc_sp.csv") # akamatsu_n_0.Rdataに含まれている位置情報と同じ数だけデータがある
summary(loc_sp2)


# data (lst) --------------------------------------------------------------
setwd("/Users/Yuki/Dropbox/LST/mean/")

# a1
nc <- nc_open(paste0("area1/1999AMD_Area1_TMP_mea.nc.nc4"))
print(nc)
T <- ncvar_get(nc, "TMP_mea")

temp = data.frame(T[, , 1])
colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
temp$lon = nc[["dim"]][["lon"]][["vals"]]
temp1 = temp %>% gather(key = lat, value = lst, -lon) %>% mutate(time = paste("1999-01-01")) %>% na.omit() %>%
  mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_")) %>% select(lon, lat, tag2)

# a2
nc <- nc_open(paste0("area2/1999AMD_Area2_TMP_mea.nc.nc4"))
print(nc)
T <- ncvar_get(nc, "TMP_mea")

temp = data.frame(T[, , 1])
colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
temp$lon = nc[["dim"]][["lon"]][["vals"]]
temp2 = temp %>% gather(key = lat, value = lst, -lon) %>% mutate(time = paste("1999-01-01")) %>% na.omit() %>%
  mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_")) %>% select(lon, lat, tag2)

# a3
nc <- nc_open(paste0("area3/1999AMD_Area3_TMP_mea.nc.nc4"))
print(nc)
T <- ncvar_get(nc, "TMP_mea")

temp = data.frame(T[, , 1])
colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
temp$lon = nc[["dim"]][["lon"]][["vals"]]
temp3 = temp %>% gather(key = lat, value = lst, -lon) %>% mutate(time = paste("1999-01-01")) %>% na.omit() %>%
  mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_")) %>% select(lon, lat, tag2)

# a4
nc <- nc_open(paste0("area4/1999AMD_Area4_TMP_mea.nc.nc4"))
print(nc)
T <- ncvar_get(nc, "TMP_mea")

temp = data.frame(T[, , 1])
colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
temp$lon = nc[["dim"]][["lon"]][["vals"]]
temp4 = temp %>% gather(key = lat, value = lst, -lon) %>% mutate(time = paste("1999-01-01")) %>% na.omit() %>%
  mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_")) %>% select(lon, lat, tag2)

# a5
nc <- nc_open(paste0("area5/1999AMD_Area5_TMP_mea.nc.nc4"))
print(nc)
T <- ncvar_get(nc, "TMP_mea")

temp = data.frame(T[, , 1])
colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
temp$lon = nc[["dim"]][["lon"]][["vals"]]
temp5 = temp %>% gather(key = lat, value = lst, -lon) %>% mutate(time = paste("1999-01-01")) %>% na.omit() %>%
  mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_")) %>% select(lon, lat, tag2)

# a6
nc <- nc_open(paste0("area6/1999AMD_Area6_TMP_mea.nc.nc4"))
print(nc)
T <- ncvar_get(nc, "TMP_mea")

temp = data.frame(T[, , 1])
colnames(temp) = nc[["dim"]][["lat"]][["vals"]]
temp$lon = nc[["dim"]][["lon"]][["vals"]]
temp6 = temp %>% gather(key = lat, value = lst, -lon) %>% mutate(time = paste("1999-01-01")) %>% na.omit() %>%
  mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10)), tag2 = paste(lon, lat, sep = "_")) %>% select(lon, lat, tag2)

loc_lst = rbind(temp1, temp2, temp3, temp4, temp5, temp6) %>% select(lon, lat, tag2) %>% distinct(tag2, .keep_all = TRUE) %>% mutate(lat = as.numeric(lat))



# fuzzyjoin ---------------------------------------------------------------
loc_sp$no = rep(1:nrow(loc_sp))

all_pairs = NULL
for(i in 9440:nrow(loc_sp)){
  sp = loc_sp[i, ]
  lon1 = sp$lon-2
  lon2 = sp$lon+2
  lat1 = sp$lat-2
  lat2 = sp$lat+2
  
  lst = loc_lst %>% filter(between(lon, lon1, lon2), between(lat, lat1, lat2))
  
  if(nrow(lst) == 0){
    sp = loc_sp[i, ]
    lon1 = sp$lon-8
    lon2 = sp$lon+8
    lat1 = sp$lat-8
    lat2 = sp$lat+8
    
    lst = loc_lst %>% filter(between(lon, lon1, lon2), between(lat, lat1, lat2))
  }
  
  pairs = sp %>%
    geo_left_join(lst, max_dist = 10, unit = "km", distance_col = "distance") %>% filter(distance == min(distance))
  
  all_pairs = rbind(all_pairs, pairs)
}

loc_sp2 = left_join(loc_sp, all_pairs %>% select(no, lon.y, lat.y, tag2), by = "no")
write.csv(loc_sp2, "loc_sp.csv", fileEncoding = "CP932", row.names = F)
