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
ele1 = ele1 %>% select("格子点ID", "標高", "傾斜")
colnames(ele1) = c("site_id", "elevation", "slope")
site1 = left_join(site1, ele1, by = "site_id")

s1 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s1 = s1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(s1) = c("site_id", "no_id","species", "DBH")
s1 = left_join(site1, s1, by = "site_id") 
s1 = left_join(s1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

m1 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m1 = m1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(m1) = c("site_id", "no_id","species", "DBH")
m1 = left_join(site1, m1, by = "site_id")
m1 = left_join(m1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")
summary(m1)

l1 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l1 = l1 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(l1) = c("site_id", "no_id","species", "DBH")
l1 = left_join(site1, l1, by = "site_id") %>% na.omit()
l1 = left_join(l1, type1, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope","species", "DBH", "type")
summary(l1)

d1 = rbind(s1, m1, l1) %>% mutate(area = (DBH/2)^2*3.14)
d1_area = d1 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))


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
ele2 = ele2 %>% select("格子点ＩＤ", "標高", "傾斜")
colnames(ele2) = c("site_id", "elevation", "slope")
site2 = left_join(site2, ele2, by = "site_id")

s2 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s2 = s2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(s2) = c("site_id", "no_id","species", "DBH")
s2 = left_join(site2, s2, by = "site_id")
s2 = left_join(s2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

m2 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m2 = m2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(m2) = c("site_id", "no_id","species", "DBH")
m2 = left_join(site2, m2, by = "site_id")
m2 = left_join(m2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

l2 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l2 = l2 %>% select("格子点ＩＤ", "ＩＤ番号", "樹種", "胸高直径")
colnames(l2) = c("site_id", "no_id","species", "DBH")
l2 = left_join(site2, l2, by = "site_id")
l2 = left_join(l2, type2, by = c("site_id", "no_id")) %>% select("site_id", "no_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

d2 = rbind(s2, m2, l2) %>% mutate(area = (DBH/2)^2*3.14)
d2_area = d2 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))


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

slope3 = read.csv("03_様式２.csv", fileEncoding = "CP932")
slope3 = slope3 %>% select("格子点ＩＤ", "斜面傾斜")
colnames(slope3) = c("site_id", "slope")
site3 = left_join(site3, slope3, by = "site_id")

s3 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s3 = s3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(s3) = c("site_id", "species", "DBH")
s3 = left_join(site3, s3, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

m3 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m3 = m3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(m3) = c("site_id", "species", "DBH")
m3 = left_join(site3, m3, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

l3 = read.csv("06_様式３ー大円.csv", fileEncoding = "CP932")
l3 = l3 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(l3) = c("site_id", "species", "DBH")
l3 = left_join(site3, l3, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

d3 = rbind(s3, m3, l3) %>% mutate(area = (DBH/2)^2*3.14)
d3_area = d3 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))


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

slope4 = read.csv("03_様式２.csv", fileEncoding = "CP932")
slope4 = slope4 %>% select("格子点ＩＤ", "斜面傾斜")
colnames(slope4) = c("site_id", "slope")
site4 = left_join(site4, slope4, by = "site_id")

s4 = read.csv("04_様式３−小円.csv", fileEncoding = "CP932")
s4 = s4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(s4) = c("site_id", "species", "DBH")
s4 = left_join(site4, s4, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

m4 = read.csv("05_様式３−中円.csv", fileEncoding = "CP932")
m4 = m4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(m4) = c("site_id", "species", "DBH")
m4 = left_join(site4, m4, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

l4 = read.csv("06_様式３−大円.csv", fileEncoding = "CP932")
l4 = l4 %>% select("格子点ＩＤ", "樹種", "胸高直径")
colnames(l4) = c("site_id", "species", "DBH")
l4 = left_join(site4, l4, by = "site_id") %>% select("site_id", "lat", "lon", "elevation", "slope", "species", "DBH", "type")

d4 = rbind(s4, m4, l4) %>% mutate(area = (DBH/2)^2*3.14)
d4_area = d4 %>% group_by(site_id, species, type) %>% summarize(sum_area = sum(area))


# 種ごとのデータ数 ----------------------------------------------------------------
species_n = rbind(d1 %>% select(species, DBH, type),
                  d2 %>% select(species, DBH, type),
                  d3 %>% select(species, DBH, type),
                  d4 %>% select(species, DBH, type)) %>% count(species, type) %>% arrange(-n, species)
species_n2 = rbind(d1 %>% select(species, DBH, type),
                  d2 %>% select(species, DBH, type),
                  d3 %>% select(species, DBH, type),
                  d4 %>% select(species, DBH, type)) %>% count(species) %>% arrange(-n)
sugi = species_n %>% filter(species == "スギ")
hinoki = species_n %>% filter(species == "ヒノキ")
konara = species_n %>% filter(species == "コナラ")
hisakaki = species_n %>% filter(species == "ヒサカキ")
akamatsu = species_n %>% filter(species == "アカマツ")



# 外れ値のチェック ----------------------------------------------------------------
d1_a_sugi = d1_area %>% filter(species == "スギ")
d2_a_sugi = d2_area %>% filter(species == "スギ")
d3_a_sugi = d3_area %>% filter(species == "スギ")
d4_a_sugi = d4_area %>% filter(species == "スギ")

a_sugi = left_join(d1_a_sugi, d2_a_sugi, by = c("site_id", "species", "type")) %>% mutate(ratio1 = sum_area.y/sum_area.x)
a_sugi = left_join(a_sugi, d3_a_sugi, by = c("site_id", "species", "type")) %>% mutate(ratio2 = sum_area/sum_area.y)
a_sugi = left_join(a_sugi, d4_a_sugi, by = c("site_id", "species", "type")) %>% mutate(ratio3 = sum_area.y.y/sum_area.x.x)

out1 = a_sugi %>% filter(ratio1 > 3)
out2 = a_sugi %>% filter(ratio2 > 3)
out3 = a_sugi %>% filter(ratio3 > 3)



# スギの個体数データ（天然林） ---------------------------------------------------------------
# 小円
sugi_s1_n_n = s1 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_s2_n_n = s2 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_s3_n_n = s3 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_s4_n_n = s4 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_s_n_n = left_join(sugi_s1_n_n %>% rename(n1 = n), sugi_s2_n_n %>% rename(n2 = n), by = c("site_id", "type"))
sugi_s_n_n = left_join(sugi_s_n_n, sugi_s3_n_n %>% rename(n3 = n), by = c("site_id", "type"))
sugi_s_n_n = left_join(sugi_s_n_n, sugi_s4_n_n %>% rename(n4 = n), by = c("site_id", "type"))

# 中円
sugi_m1_n_n = m1 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_m2_n_n = m2 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_m3_n_n = m3 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_m4_n_n = m4 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_m_n_n = left_join(sugi_m1_n_n %>% rename(n1 = n), sugi_m2_n_n %>% rename(n2 = n), by = c("site_id", "type"))
sugi_m_n_n = left_join(sugi_m_n_n, sugi_m3_n_n %>% rename(n3 = n), by = c("site_id", "type"))
sugi_m_n_n = left_join(sugi_m_n_n, sugi_m4_n_n %>% rename(n4 = n), by = c("site_id", "type"))

# 大円
sugi_l1_n_n = l1 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_l2_n_n = l2 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_l3_n_n = l3 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_l4_n_n = l4 %>% filter(species == "スギ", type == "天然林") %>% group_by(type, site_id) %>% count()
sugi_l_n_n = left_join(sugi_l1_n_n %>% rename(n1 = n), sugi_m2_n_n %>% rename(n2 = n), by = c("site_id", "type"))
sugi_l_n_n = left_join(sugi_l_n_n, sugi_l3_n_n %>% rename(n3 = n), by = c("site_id", "type"))
sugi_l_n_n = left_join(sugi_l_n_n, sugi_l4_n_n %>% rename(n4 = n), by = c("site_id", "type"))


# スギの個体数データ（人工林） ---------------------------------------------------------------
# 小円
sugi_s1_n_a = s1 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_s2_n_a = s2 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_s3_n_a = s3 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_s4_n_a = s4 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_s_n_a = left_join(sugi_s1_n_a %>% rename(n1 = n), sugi_s2_n_a %>% rename(n2 = n), by = c("site_id", "type"))
sugi_s_n_a = left_join(sugi_s_n_a, sugi_s3_n_a %>% rename(n3 = n), by = c("site_id", "type"))
sugi_s_n_a = left_join(sugi_s_n_a, sugi_s4_n_a %>% rename(n4 = n), by = c("site_id", "type"))
summary(sugi_s_n_a)

# 中円
sugi_m1_n_a = m1 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_m2_n_a = m2 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_m3_n_a = m3 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_m4_n_a = m4 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_m_n_a = left_join(sugi_m1_n_a %>% rename(n1 = n), sugi_m2_n_a %>% rename(n2 = n), by = c("site_id", "type"))
sugi_m_n_a = left_join(sugi_m_n_a, sugi_m3_n_a %>% rename(n3 = n), by = c("site_id", "type"))
sugi_m_n_a = left_join(sugi_m_n_a, sugi_m4_n_a %>% rename(n4 = n), by = c("site_id", "type"))
summary(sugi_m_n_a)

# 大円
sugi_l1_n_a = l1 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_l2_n_a = l2 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_l3_n_a = l3 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_l4_n_a = l4 %>% filter(species == "スギ", type == "人工林") %>% group_by(type, site_id) %>% count()
sugi_l_n_a = left_join(sugi_l1_n_a %>% rename(n1 = n), sugi_m2_n_a %>% rename(n2 = n), by = c("site_id", "type"))
sugi_l_n_a = left_join(sugi_l_n_a, sugi_l3_n_a %>% rename(n3 = n), by = c("site_id", "type"))
sugi_l_n_a = left_join(sugi_l_n_a, sugi_l4_n_a %>% rename(n4 = n), by = c("site_id", "type"))
summary(sugi_l_n_a)


# スギのDBHデータ（天然林） ---------------------------------------------------------------
# 小円(> 1cm)
sugi_s1_d_n = s1 %>% filter(species == "スギ", type == "天然林")
summary(sugi_s1_d_n)
hist(sugi_s1_d_n$DBH)

sugi_s2_d_n = s2 %>% filter(species == "スギ", type == "天然林")
summary(sugi_s2_d_n)
hist(sugi_s2_d_n$DBH)

sugi_s3_d_n = s3 %>% filter(species == "スギ", type == "天然林")
summary(sugi_s3_d_n)
hist(sugi_s3_d_n$DBH)

sugi_s4_d_n = s4 %>% filter(species == "スギ", type == "天然林")
summary(sugi_s4_d_n)
hist(sugi_s4_d_n$DBH)

# 中円(> 5cm)
sugi_m1_d_n = m1 %>% filter(species == "スギ", type == "天然林")
summary(sugi_m1_d_n) # min = 1.2
hist(sugi_m1_d_n$DBH)

sugi_m2_d_n = m2 %>% filter(species == "スギ", type == "天然林")
summary(sugi_m2_d_n) # min = -987がある
hist(sugi_m2_d_n$DBH)

sugi_m3_d_n = m3 %>% filter(species == "スギ", type == "天然林")
summary(sugi_m3_d_n) # min = 4.7
hist(sugi_m3_d_n$DBH)

sugi_m4_d_n = m4 %>% filter(species == "スギ", type == "天然林")
summary(sugi_m4_d_n) # min = 5
hist(sugi_m4_d_n$DBH)

# 大円（> 18cm)
sugi_l1_d_n = l1 %>% filter(species == "スギ", type == "天然林")
summary(sugi_l1_d_n) # min = 1
hist(sugi_l1_d_n$DBH)

sugi_l2_d_n = l2 %>% filter(species == "スギ", type == "天然林")
summary(sugi_l2_d_n) # min = 1.2
hist(sugi_l2_d_n$DBH)

sugi_l3_d_n = l3 %>% filter(species == "スギ", type == "天然林")
summary(sugi_l3_d_n) # min = 9.7
hist(sugi_l3_d_n$DBH)

sugi_l4_d_n = l4 %>% filter(species == "スギ", type == "天然林")
summary(sugi_l4_d_n)
hist(sugi_l4_d_n$DBH)
