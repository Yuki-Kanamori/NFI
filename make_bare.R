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

df_s1 = s1 %>% filter(DBH > 1) %>% filter(DBH <= 5)
summary(df_s1)


# 中円
m1 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m1 = m1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(m1) = c("site_id", "no_id","species", "DBH")
m1 = left_join(site1, m1, by = "site_id")
m1 = left_join(m1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(m1)

df_m1 = rbind(s1, m1) %>% filter(DBH > 5) %>% filter(DBH <= 18)
summary(df_m1)


# 大円
l1 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l1 = l1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(l1) = c("site_id", "no_id","species", "DBH")
l1 = left_join(site1, l1, by = "site_id") %>% na.omit()
l1 = left_join(l1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope","species", "DBH", "type")
summary(l1)

df_l1 = rbind(s1, m1, l1) %>% filter(DBH > 18)
summary(df_l1)

# d1 = rbind(s1, m1, l1) %>% mutate(area = (DBH/2)^2*3.14)
# d1_area = d1 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))

time1 = rbind(df_s1, df_m1, df_l1) %>% mutate(tag = paste(lon, lat, sep = "_"))


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

df_s2 = s2 %>% filter(DBH > 1) %>% filter(DBH <= 5)
summary(df_s2)


# 中円
m2 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m2 = m2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(m2) = c("site_id", "no_id","species", "DBH")
m2 = left_join(site2, m2, by = "site_id")
m2 = left_join(m2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

df_m2 = rbind(s2, m2) %>% filter(DBH > 5) %>% filter(DBH <= 18)
summary(df_m2)


# 大円
l2 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l2 = l2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(l2) = c("site_id", "no_id","species", "DBH")
l2 = left_join(site2, l2, by = "site_id")
l2 = left_join(l2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(l2)

df_l2 = rbind(s2, m2, l2) %>% filter(DBH > 18)
summary(df_l2)

# d2 = rbind(s2, m2, l2) %>% mutate(area = (DBH/2)^2*3.14)
# d2_area = d2 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))

time2 = rbind(df_s2, df_m2, df_l2) %>% mutate(tag = paste(lon, lat, sep = "_"))



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

df_s3 = s3 %>% filter(DBH > 1) %>% filter(DBH <= 5)
summary(df_s3)


# 中円
m3 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m3 = m3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(m3) = c("site_id", "species", "DBH")
m3 = left_join(site3, m3, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(m3)

df_m3 = rbind(s3, m3) %>% filter(DBH > 5) %>% filter(DBH <= 18)
summary(df_m3)


# 大円
l3 = read.csv("06_様式３ー大円.csv", fileEncoding = "CP932")
l3 = l3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(l3) = c("site_id", "species", "DBH")
l3 = left_join(site3, l3, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(l3)

df_l3 = rbind(s3, m3, l3) %>% filter(DBH > 18)
summary(df_l3)

# d3 = rbind(s3, m3, l3) %>% mutate(area = (DBH/2)^2*3.14)
# d3_area = d3 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))

time3 = rbind(df_s3, df_m3, df_l3) %>% mutate(tag = paste(lon, lat, sep = "_"))
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

df_s4 = s4 %>% filter(DBH > 1) %>% filter(DBH <= 5)
summary(df_s4)

# 中円
m4 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m4 = m4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(m4) = c("site_id", "species", "DBH")
m4 = left_join(site4, m4, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(m4)

df_m4 = rbind(s4, m4) %>% filter(DBH > 5) %>% filter(DBH <= 18)
summary(df_m4)

# 大円
l4 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l4 = l4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(l4) = c("site_id", "species", "DBH")
l4 = left_join(site4, l4, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(l4)

df_l4 = rbind(s4, m4, l4) %>% filter(DBH > 18)
summary(df_l4)

# d4 = rbind(s4, m4, l4) %>% mutate(area = (DBH/2)^2*3.14)
# d4_area = d4 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))

time4 = rbind(df_s4, df_m4, df_l4) %>% mutate(tag = paste(lon, lat, sep = "_"))



# データの統合 ------------------------------------------------------------------
head(time1, 3); head(time2, 3); head(time3, 3); head(time4, 3); 
all = rbind(time1 %>% select(-no_id) %>% mutate(time = 1), time2 %>% select(-no_id) %>% mutate(time = 2), time3 %>% mutate(time = 3), time4 %>% mutate(time = 4))

# setwd("/Users/Yuki/Dropbox/NFI")
# write.csv(all, "all.csv", fileEncoding = "CP932")

# read.csv("all.csv", fileEncoding = "CP932")


# 胸高断面積データに0データを入れる -------------------------------------------------------------
# time1
head(time1, 3)
time1 = time1 %>% mutate(area = (DBH/2/100)^2*3.14) #半径×半径×3.14．cmなのでmに直す
time1_a = time1 %>% group_by(type, site_id, species) %>% summarize(sum_area = sum(area))
time1_a = left_join(time1_a, site1, by = "site_id") %>% mutate(year = 1, tag = paste(lon, lat, sep = "_"))
summary(time1_a)

sum_a1 = time1_a %>% group_by(tag, type) %>% summarize(total_area = sum(sum_area)) %>% mutate(bare = 1000-total_area, year = 1)



# time2
time2 = time2 %>% mutate(area = (DBH/2/100)^2*3.14)
time2_a = time2 %>% group_by(type, site_id, species) %>% summarize(sum_area = sum(area))
time2_a = left_join(time2_a, site2, by = "site_id") %>% mutate(year = 2, tag = paste(lon, lat, sep = "_"))

sum_a2 = time2_a %>% group_by(tag, type) %>% summarize(total_area = sum(sum_area)) %>% mutate(bare = 1000-total_area, year = 2)



# time3
time3 = time3 %>% mutate(area = (DBH/2/100)^2*3.14)
time3_a = time3 %>% group_by(type, site_id, species) %>% summarize(sum_area = sum(area))
time3_a = left_join(time3_a, site3 %>% select(-type), by = "site_id") %>% mutate(year = 3, tag = paste(lon, lat, sep = "_"))
head(time3_a)

sum_a3 = time3_a %>% group_by(tag, type) %>% summarize(total_area = sum(sum_area)) %>% mutate(bare = 1000-total_area, year = 3)



# time4
time4 = time4 %>% mutate(area = (DBH/2/100)^2*3.14)
time4_a = time4 %>% group_by(type, site_id, species) %>% summarize(sum_area = sum(area))
time4_a = left_join(time4_a, site4 %>% select(-type), by = "site_id") %>% mutate(year = 3, tag = paste(lon, lat, sep = "_"))

sum_a4 = time4_a %>% group_by(tag, type) %>% summarize(total_area = sum(sum_area)) %>% mutate(bare = 1000-total_area, year = 4)


bare = rbind(sum_a1, sum_a2, sum_a3, sum_a4)
summary(bare)
setwd("/Users/Yuki/Dropbox/NFI")
save(bare, file = "bare.Rdata")
