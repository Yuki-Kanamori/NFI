require(tidyverse)


# time1 -------------------------------------------------------------------
dir_data1 = "/Users/Yuki/Dropbox/NFI/NFI_1_CSV/第1期"
setwd(dir_data1)

site1 = read.csv("01_プロット.csv", fileEncoding = "CP932")
site1 = site1 %>% select("格子点ID", "世界北緯１", "世界東経１")
colnames(site1) = c("site_id", "lat", "lon")

type1 = read.csv("03_様式２−２.csv", fileEncoding = "CP932")
type1 = type1 %>% select("格子点ID", "ＩＤ番号", "林種")
colnames(type1) = c("site_id", "no_id", "type")

ele1 = read.csv("02_様式２−１.csv", fileEncoding = "CP932")
ele1 = ele1 %>% select("格子点ID", "標高")
colnames(ele1) = c("site_id", "elevation")
site1 = left_join(site1, ele1, by = "site_id")

s1 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s1 = s1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(s1) = c("site_id", "no_id","species", "DBH")
s1 = left_join(site1, s1, by = "site_id") 
s1 = left_join(s1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "species", "DBH", "type")

m1 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m1 = m1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(m1) = c("site_id", "no_id","species", "DBH")
m1 = left_join(site1, m1, by = "site_id")
m1 = left_join(m1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "species", "DBH", "type")

l1 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l1 = l1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(l1) = c("site_id", "no_id","species", "DBH")
l1 = left_join(site1, l1, by = "site_id") %>% na.omit()
l1 = left_join(l1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "species", "DBH", "type")

d1 = rbind(s1, m1, l1)



# time2 -------------------------------------------------------------------
dir_data2 = "/Users/Yuki/Dropbox/NFI/NFI_2_CSV/第2期"
setwd(dir_data2)

site2 = read.csv("01_プロット.csv", fileEncoding = "CP932")
site2 = site2 %>% select("格子点ID", "世界北緯１", "世界東経１")
colnames(site2) = c("site_id", "lat", "lon")

type2 = read.csv("03_様式２−２.csv", fileEncoding = "CP932")
type2 = type2 %>% select("格子点ＩＤ", "ＩＤ番号", "林種")
colnames(type2) = c("site_id", "no_id", "type")

ele2 = read.csv("02_様式２−１.csv", fileEncoding = "CP932")
ele2 = ele2 %>% select("格子点ＩＤ", "標高")
colnames(ele2) = c("site_id", "elevation")
site2 = left_join(site2, ele2, by = "site_id")

s2 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s2 = s2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(s2) = c("site_id", "no_id","species", "DBH")
s2 = left_join(site2, s2, by = "site_id")
s2 = left_join(s2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "species", "DBH", "type")

m2 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m2 = m2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(m2) = c("site_id", "no_id","species", "DBH")
m2 = left_join(site2, m2, by = "site_id")
m2 = left_join(m2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "species", "DBH", "type")

l2 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l2 = l2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(l2) = c("site_id", "no_id","species", "DBH")
l2 = left_join(site2, l2, by = "site_id")
l2 = left_join(l2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "species", "DBH", "type")

d2 = rbind(s2, m2, l2)


# time3 -------------------------------------------------------------------
dir_data3 = "/Users/Yuki/Dropbox/NFI/NFI_3_CSV/第3期"
setwd(dir_data3)

site3 = read.csv("01_プロット.csv", fileEncoding = "CP932")
site3 = site3 %>% select("格子点ID", "世界北緯1", "世界東経1")
colnames(site3) = c("site_id", "lat", "lon")

type3 = read.csv("19_様式７.csv", fileEncoding = "CP932")
type3 = type3 %>% select("格子点ＩＤ", "標高", "林種")
colnames(type3) = c("site_id", "elevation", "type")
site3 = left_join(site3, type3, by = "site_id")

s3 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s3 = s3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(s3) = c("site_id", "species", "DBH")
s3 = left_join(site3, s3, by = "site_id") %>% select("site_id", "lat", "lon", "species", "DBH", "type")

m3 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m3 = m3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(m3) = c("site_id", "species", "DBH")
m3 = left_join(site3, m3, by = "site_id") %>% select("site_id", "lat", "lon", "species", "DBH", "type")

l3 = read.csv("06_様式３ー大円.csv", fileEncoding = "CP932")
l3 = l3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(l3) = c("site_id", "species", "DBH")
l3 = left_join(site3, l3, by = "site_id") %>% select("site_id", "lat", "lon", "species", "DBH", "type")

d3 = rbind(s3, m3, l3)



# time4 -------------------------------------------------------------------
dir_data4 = "/Users/Yuki/Dropbox/NFI/NFI_4_CSV/第4期"
setwd(dir_data4)

site4 = read.csv("01_プロット.csv", fileEncoding = "CP932")
site4 = site4 %>% select("格子点ID", "世界北緯1", "世界東経1")
colnames(site4) = c("site_id", "lat", "lon")

type4 = read.csv("21_様式７.csv", fileEncoding = "CP932")
type4 = type4 %>% select("格子点ＩＤ", "標高", "林種")
colnames(type4) = c("site_id", "elevation", "type")
site4 = left_join(site4, type4, by = "site_id")

s4 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s4 = s4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(s4) = c("site_id", "species", "DBH")
s4 = left_join(site4, s4, by = "site_id") %>% select("site_id", "lat", "lon", "species", "DBH", "type")

m4 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m4 = m4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(m4) = c("site_id", "species", "DBH")
m4 = left_join(site4, m4, by = "site_id") %>% select("site_id", "lat", "lon", "species", "DBH", "type")

l4 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l4 = l4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(l4) = c("site_id", "species", "DBH")
l4 = left_join(site4, l4, by = "site_id") %>% select("site_id", "lat", "lon", "species", "DBH", "type")

d4 = rbind(s4, m4, l4)


