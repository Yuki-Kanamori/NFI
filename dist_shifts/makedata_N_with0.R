require(tidyverse)


# time1 -------------------------------------------------------------------
dir_data1 = "/Users/Yuki/Dropbox/NFI/NFI_1_CSV/第1期"
setwd(dir_data1)

site1 = read.csv("01_プロット.csv", fileEncoding = "CP932")
head(site1)
site1 = site1 %>% select("格子点ID", "世界北緯１", "世界東経１")
colnames(site1) = c("site_id", "lat", "lon")

type1 = read.csv("03_様式２−２.csv", fileEncoding = "CP932")
type1 = type1 %>% select("格子点ID", "ＩＤ番号", "林種")
colnames(type1) = c("site_id", "no_id", "type")

ele1 = read.csv("02_様式２−１.csv", fileEncoding = "CP932")
ele1 = ele1 %>% select("格子点ID", "標高", "傾斜")
colnames(ele1) = c("site_id", "elevation", "slope")
site1 = left_join(site1, ele1, by = "site_id")


# 小円
s1 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s1 = s1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(s1) = c("site_id", "no_id","species", "DBH")
s1 = left_join(site1, s1, by = "site_id") 
s1 = left_join(s1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type") 

df_s1 = s1 %>% filter(DBH > 1) %>% filter(DBH <= 5) %>% mutate(cate = "small")
nrow(df_s1) #460244
summary(df_s1) 

df_s12 = df_s1 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "small")


# 中円
m1 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m1 = m1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(m1) = c("site_id", "no_id","species", "DBH")
m1 = left_join(site1, m1, by = "site_id")
m1 = left_join(m1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(m1)

df_m1 = rbind(s1, m1) %>% filter(DBH > 5) %>% filter(DBH <= 18) %>% mutate(cate = "middle")
nrow(df_m1) #689832
summary(df_m1)

df_m12 = df_m1 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "middle")

# 大円
l1 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l1 = l1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(l1) = c("site_id", "no_id","species", "DBH")
l1 = left_join(site1, l1, by = "site_id") %>% na.omit()
l1 = left_join(l1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope","species", "DBH", "type")
summary(l1)

df_l1 = rbind(s1, m1, l1) %>% filter(DBH > 18) %>% mutate(cate = "large")
nrow(df_l1) #487075
summary(df_l1)

df_l12 = df_l1 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "large")

# d1 = rbind(s1, m1, l1) %>% mutate(area = (DBH/2)^2*3.14)
# d1_area = d1 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))

time1 = rbind(df_s12, df_m12, df_l12) %>% mutate(tag = paste(lon, lat, sep = "_"))


# time2 -------------------------------------------------------------------
dir_data2 = "/Users/Yuki/Dropbox/NFI/NFI_2_CSV/第2期"
setwd(dir_data2)

site2 = read.csv("01_プロット.csv", fileEncoding = "CP932")
site2 = site2 %>% select("格子点ID", "世界北緯１", "世界東経１")
colnames(site2) = c("site_id", "lat", "lon")

type2 = read.csv("03_様式２−２.csv", fileEncoding = "CP932")
type2 = type2 %>% select("格子点ＩＤ", "ＩＤ番号", "林種")
colnames(type2) = c("site_id", "no_id", "type")

ele2 = read.csv("02_様式２−１.csv", fileEncoding = "CP932")
ele2 = ele2 %>% select("格子点ＩＤ", "標高", "傾斜")
colnames(ele2) = c("site_id", "elevation", "slope")
site2 = left_join(site2, ele2, by = "site_id")


# 小円
s2 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s2 = s2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(s2) = c("site_id", "no_id","species", "DBH")
s2 = left_join(site2, s2, by = "site_id")
s2 = left_join(s2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type") 

df_s2 = s2 %>% filter(DBH > 1) %>% filter(DBH <= 5) %>% mutate(cate = "small")
summary(df_s2)

df_s22 = df_s2 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "small")


# 中円
m2 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m2 = m2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(m2) = c("site_id", "no_id","species", "DBH")
m2 = left_join(site2, m2, by = "site_id")
m2 = left_join(m2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

df_m2 = rbind(s2, m2) %>% filter(DBH > 5) %>% filter(DBH <= 18) %>% mutate(cate = "middle")
summary(df_m2)

df_m22 = df_m2 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "middle")



# 大円
l2 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l2 = l2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(l2) = c("site_id", "no_id","species", "DBH")
l2 = left_join(site2, l2, by = "site_id")
l2 = left_join(l2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(l2)

df_l2 = rbind(s2, m2, l2) %>% filter(DBH > 18) %>% mutate(cate = "large")
summary(df_l2)

df_l22 = df_l2 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "large")

time2 = rbind(df_s22, df_m22, df_l22) %>% mutate(tag = paste(lon, lat, sep = "_"))



# time3 -------------------------------------------------------------------
dir_data3 = "/Users/Yuki/Dropbox/NFI/NFI_3_CSV/第3期"
setwd(dir_data3)

site3 = read.csv("01_プロット.csv", fileEncoding = "CP932")
site3 = site3 %>% select("格子点ID", "世界北緯1", "世界東経1")
colnames(site3) = c("site_id", "lat", "lon")

type3 = read.csv("19_様式７.csv", fileEncoding = "CP932")
type3 = type3 %>% select("格子点ＩＤ", "標高", "林種")
colnames(type3) = c("site_id", "elevation", "type")
site3 = left_join(site3, type3, by = "site_id")

slope3 = read.csv("03_様式２.csv", fileEncoding = "CP932")
slope3 = slope3 %>% select("格子点ＩＤ", "斜面傾斜")
colnames(slope3) = c("site_id", "slope")
site3 = left_join(site3, slope3, by = "site_id")


# 小円
s3 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s3 = s3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(s3) = c("site_id", "species", "DBH")
s3 = left_join(site3, s3, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(s3)

df_s3 = s3 %>% filter(DBH > 1) %>% filter(DBH <= 5) %>% mutate(cate = "small")
summary(df_s3)

df_s32 = df_s3 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "small")


# 中円
m3 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m3 = m3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(m3) = c("site_id", "species", "DBH")
m3 = left_join(site3, m3, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(m3)

df_m3 = rbind(s3, m3) %>% filter(DBH > 5) %>% filter(DBH <= 18) %>% mutate(cate = "middle")
summary(df_m3)

df_m32 = df_m3 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "middle")

# 大円
l3 = read.csv("06_様式３ー大円.csv", fileEncoding = "CP932")
l3 = l3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(l3) = c("site_id", "species", "DBH")
l3 = left_join(site3, l3, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(l3)

df_l3 = rbind(s3, m3, l3) %>% filter(DBH > 18) %>% mutate(cate = "large")
summary(df_l3)

df_l32 = df_l3 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "large")

time3 = rbind(df_s32, df_m32, df_l32) %>% mutate(tag = paste(lon, lat, sep = "_"))
head(time3, 3)



# time4 -------------------------------------------------------------------
dir_data4 = "/Users/Yuki/Dropbox/NFI/NFI_4_CSV/第4期"
setwd(dir_data4)

site4 = read.csv("01_プロット.csv", fileEncoding = "CP932")
site4 = site4 %>% select("格子点ID", "世界北緯1", "世界東経1")
colnames(site4) = c("site_id", "lat", "lon")

type4 = read.csv("21_様式７.csv", fileEncoding = "CP932")
type4 = type4 %>% select("格子点ＩＤ", "標高", "林種")
colnames(type4) = c("site_id", "elevation", "type")
site4 = left_join(site4, type4, by = "site_id")

slope4 = read.csv("03_様式２.csv", fileEncoding = "CP932")
slope4 = slope4 %>% select("格子点ＩＤ", "斜面傾斜")
colnames(slope4) = c("site_id", "slope")
site4 = left_join(site4, slope4, by = "site_id")

# 小円
s4 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s4 = s4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(s4) = c("site_id", "species", "DBH")
s4 = left_join(site4, s4, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(s4)

df_s4 = s4 %>% filter(DBH > 1) %>% filter(DBH <= 5) %>% mutate(cate = "small")
summary(df_s4)

df_s42 = df_s4 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "small")

# 中円
m4 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m4 = m4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(m4) = c("site_id", "species", "DBH")
m4 = left_join(site4, m4, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(m4)

df_m4 = rbind(s4, m4) %>% filter(DBH > 5) %>% filter(DBH <= 18) %>% mutate(cate = "middle")
summary(df_m4)

df_m42 = df_m4 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "middle")

# 大円
l4 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l4 = l4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(l4) = c("site_id", "species", "DBH")
l4 = left_join(site4, l4, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(l4)

df_l4 = rbind(s4, m4, l4) %>% filter(DBH > 18) %>% mutate(cate = "large")
summary(df_l4)

df_l42 = df_l4 %>% group_by(lon, lat, type, site_id, species) %>% count() %>% mutate(cate = "large")

time4 = rbind(df_s42, df_m42, df_l42) %>% mutate(tag = paste(lon, lat, sep = "_"))



# データの統合 ------------------------------------------------------------------
head(time1, 3); head(time2, 3); head(time3, 3); head(time4, 3); 
all = rbind(time1 %>% mutate(time = 1), time2 %>% mutate(time = 2), time3 %>% mutate(time = 3), time4 %>% mutate(time = 4))

# setwd("/Users/Yuki/Dropbox/NFI")
# write.csv(all, "all.csv", fileEncoding = "CP932")

# read.csv("all.csv", fileEncoding = "CP932")


# 個体数データに0データを入れる -------------------------------------------------------------














# time1
time1_n = time1 %>% group_by(type, site_id, species, cate) %>% count()
time1_n = left_join(time1_n, site1, by = "site_id") %>% mutate(year = 1, tag = paste(lon, lat, sep = "_"))

lonlat_t1 = time1 %>% select(lon, lat, type, cate, elevation, slope) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% distinct(tag, .keep_all = T) #13865
c_small = lonlat_t1 %>% filter(cate == "small") #11673
c_mid = lonlat_t1 %>% filter(cate == "middle") #1987
c_large = lonlat_t1 %>% filter(cate == "large") #205

splist_t1 = time1 %>% select(species) %>% distinct()

time1_2 = NULL
for(i in 1:nrow(splist_t1)){
  sp = splist_t1[i, 1]
  t1 = time1_n %>% filter(species == sp) 
  t1 = t1[, c("tag", "type", "species", "n", "cate")]
  
  t2 = left_join(lonlat_t1, t1, by = c("tag", "type", "cate")) %>% mutate(year = 1)
  
}





time1_2 = NULL
for(i in 1:nrow(splist_t1)){
  sp = splist_t1[i, 1]
  t1 = time1_n %>% filter(species == sp) 
  t1 = t1[, c("tag", "type", "species", "n", "cate")]
  
  t2 = left_join(lonlat_t1, t1, by = c("tag", "type")) %>% mutate(year = 1)
  t2$species = paste(splist_t1[i, 1])
  t2 = t2 %>% replace_na(list(n = 0))
  time1_2 = rbind(time1_2, t2)
}
df_time1_0 = time1_2
save(df_time1_0, file = "df_time1_0.Rdata")


# time2
time2_n = time2 %>% group_by(type, site_id, species) %>% count()
time2_n = left_join(time2_n, site2, by = "site_id") %>% mutate(year = 2, tag = paste(lon, lat, sep = "_"))

lonlat_t2 = time2 %>% select(lon, lat, type, elevation, slope) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% distinct(tag, .keep_all = T) #13940

splist_t2 = time2 %>% select(species) %>% distinct() %>% filter(species == "アカマツ")

time2_2 = NULL
for(i in 1:nrow(splist_t2)){
  sp = splist_t2[i, 1]
  t1 = time2_n %>% filter(species == sp) 
  t1 = t1[, c("tag", "type", "species", "n")]
  
  t2 = left_join(lonlat_t2, t1, by = c("tag", "type")) %>% mutate(year = 2)
  t2$species = paste(splist_t2[i, 1])
  t2 = t2 %>% replace_na(list(n = 0))
  time2_2 = rbind(time2_2, t2)
}
df_time2_0 = time2_2
save(df_time2_0, file = "df_time2_0.Rdata")


# time3
time3_n = time3 %>% group_by(type, site_id, species) %>% count()
time3_n = left_join(time3_n, site3 %>% select(-type), by = "site_id") %>% mutate(year = 3, tag = paste(lon, lat, sep = "_"))
head(time3_n)

lonlat_t3 = time3 %>% select(lon, lat, type, elevation, slope) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% distinct(tag, .keep_all = T) #13286

splist_t3 = time3 %>% select(species) %>% distinct() %>% filter(species == "アカマツ")

time3_2 = NULL
for(i in 1:nrow(splist_t3)){
  sp = splist_t3[i, 1]
  t1 = time3_n %>% filter(species == sp) 
  t1 = t1[, c("tag", "type", "species", "n")]
  
  t2 = left_join(lonlat_t1, t1, by = c("tag", "type")) %>% mutate(year = 3)
  t2$species = paste(splist_t3[i, 1])
  t2 = t2 %>% replace_na(list(n = 0))
  time3_2 = rbind(time3_2, t2)
}
df_time3_0 = time3_2
save(df_time3_0, file = "df_time3_0.Rdata")


# time4
time4_n = time4 %>% group_by(type, site_id, species) %>% count()
time4_n = left_join(time4_n, site4 %>% select(-type), by = "site_id") %>% mutate(year = 3, tag = paste(lon, lat, sep = "_"))

lonlat_t4 = time4 %>% select(lon, lat, type, elevation, slope) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% distinct(tag, .keep_all = T) #12648

splist_t4 = time1 %>% select(species) %>% distinct() %>% filter(species == "アカマツ")

time4_2 = NULL
for(i in 1:nrow(splist_t4)){
  sp = splist_t4[i, 1]
  t1 = time4_n %>% filter(species == sp) 
  t1 = t1[, c("tag", "type", "species", "n")]
  
  t2 = left_join(lonlat_t4, t1, by = c("tag", "type")) %>% mutate(year = 4)
  t2$species = paste(splist_t4[i, 1])
  t2 = t2 %>% replace_na(list(n = 0))
  time4_2 = rbind(time4_2, t2)
}
unique(time4_2$species)
df_time4_0 = time4_2
save(df_time4_0, file = "df_time4_0.Rdata")


df_n_0_akamatsu = rbind(time1_2, time2_2, time3_2, time4_2) %>% mutate(effort = 0.1)
site_lonlat = rbind(lonlat_t1, lonlat_t2, lonlat_t3, lonlat_t4) %>% distinct(tag, .keep_all = T)

setwd("/Users/Yuki/Dropbox/NFI")
save(df_n_0_akamatsu, file = "akamatsu_n_0.Rdata")

sugi_n_0 = df_n_0
unique(sugi_n_0$species)
save(sugi_n_0, file = "sugi_n_0.Rdata")
