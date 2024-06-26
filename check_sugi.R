
# package -----------------------------------------------------------------
require(tidyverse)


# directory ---------------------------------------------------------------
dirname = "/Users/Yuki/Dropbox/NFI"
setwd(dir = dirname)


# load data ---------------------------------------------------------------
load("sugi_a_0.Rdata")
summary(df)
unique(df$type)
df = df %>% 
  filter(type == "天然林") %>%
  mutate(cpue = sum_area/effort, tag = paste(lon, lat, sep = "_")) %>% 
  select(year, lat, lon, sum_area, cpue, species, slope, elevation) %>% 
  rename(spp = species)
summary(df)
df_a = df
summary(df_a)

load("sugi_n_0.Rdata")
df = sugi_n_0
summary(df)
unique(df$type)
df = df %>% 
  filter(type == "天然林") %>%
  mutate(cpue = n/effort, tag = paste(lon, lat, sep = "_")) %>% 
  select(year, lat, lon, n, cpue, species, slope, elevation) %>% 
  rename(spp = species)
summary(df)
df_n = df
summary(df_n)


# データのチェック --------------------------------------------------------------
# area
time1 = df_a %>% filter(year == 1) %>% select(sum_area, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time1 = sum_area) %>% select(tag, time1)

time2 = df_a %>% filter(year == 2) %>% select(sum_area, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time2 = sum_area) %>% select(tag, time2)

time3 = df_a %>% filter(year == 3) %>% select(sum_area, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time3 = sum_area) %>% select(tag, time3)

time4 = df_a %>% filter(year == 4) %>% select(sum_area, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time4 = sum_area) %>% select(tag, time4)

tag = df_a %>% select(lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% select(tag) %>% distinct()

tag2_a = left_join(tag, time1, by = "tag")
tag2_a = left_join(tag2_a, time2, by = "tag")
summary(tag2_a)
tag2_a = left_join(tag2_a, time3, by = "tag")
tag2_a = left_join(tag2_a, time4, by = "tag")
summary(tag2_a)

# tag2_a$time1 = tag2$time1+0.001
# tag2_a$time2 = tag2$time2+0.001
# tag2_a$time3 = tag2$time3+0.001
# tag2_a$time4 = tag2$time4+0.001

tag3_a = tag2_a %>% mutate(check = rowSums(tag2_a[, 2:5], na.rm = T)) %>% filter(check != 0)

plot(x = tag3_a$time1, y = tag3_a$time2)
plot(x = tag3_a$time2, y = tag3_a$time3)
plot(x = tag3_a$time3, y = tag3_a$time4)

tag3_a$time1 = ifelse(tag3_a$time1 == 0, tag3_a$time1+0.1, tag3_a$time1)
tag3_a$time2 = ifelse(tag3_a$time2 == 0, tag3_a$time2+0.1, tag3_a$time2)
tag3_a$time3 = ifelse(tag3_a$time3 == 0, tag3_a$time3+0.1, tag3_a$time3)
tag3_a$time4 = ifelse(tag3_a$time4 == 0, tag3_a$time4+0.1, tag3_a$time4)

rate_a = data.frame(tag = tag3_a$tag, time1to2_a = tag3_a$time2/tag3_a$time1, time2to3_a = tag3_a$time3/tag3_a$time2, time3to4_a = tag3_a$time4/tag3_a$time3)
summary(rate_a)


# n
time1 = df_n %>% filter(year == 1) %>% select(n, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time1 = n) %>% select(tag, time1)

time2 = df_n %>% filter(year == 2) %>% select(n, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time2 = n) %>% select(tag, time2)

time3 = df_n %>% filter(year == 3) %>% select(n, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time3 = n) %>% select(tag, time3)

time4 = df_n %>% filter(year == 4) %>% select(n, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time4 = n) %>% select(tag, time4)

tag = df_n %>% select(lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% select(tag) %>% distinct()

tag2_n = left_join(tag, time1, by = "tag")
tag2_n = left_join(tag2_n, time2, by = "tag")
tag2_n = left_join(tag2_n, time3, by = "tag")
tag2_n = left_join(tag2_n, time4, by = "tag")

tag3_n = tag2_n %>% mutate(check = rowSums(tag2_n[, 2:5], na.rm = T)) %>% filter(check != 0)

plot(x = tag3_n$time1, y = tag3_n$time2)
plot(x = tag3_n$time2, y = tag3_n$time3)
plot(x = tag3_n$time3, y = tag3_n$time4)

tag3_n$time1 = ifelse(tag3_n$time1 == 0, tag3_n$time1+0.1, tag3_n$time1)
tag3_n$time2 = ifelse(tag3_n$time2 == 0, tag3_n$time2+0.1, tag3_n$time2)
tag3_n$time3 = ifelse(tag3_n$time3 == 0, tag3_n$time3+0.1, tag3_n$time3)
tag3_n$time4 = ifelse(tag3_n$time4 == 0, tag3_n$time4+0.1, tag3_n$time4)

rate_n = data.frame(tag = tag3_n$tag, time1to2_n = tag3_n$time2/tag3_n$time1, time2to3_n = tag3_n$time3/tag3_n$time2, time3to4_n = tag3_n$time4/tag3_n$time3)
summary(rate_n)


options(scipen=100)
rate = left_join(rate_a, rate_n, by = "tag")
rate[is.na(rate)] = -50
rate = rate %>% mutate(check = rowSums(rate[, 2:7]))
# options(digits=2)
rate2 = rate %>% filter(check != -300) %>% mutate(time1to2_a = if_else(time1to2_a == -50, NA_real_, time1to2_a), time2to3_a = if_else(time2to3_a == -50, NA_real_, time2to3_a), time3to4_a = if_else(time3to4_a == -50, NA_real_, time3to4_a), time1to2_n = if_else(time1to2_n == -50, NA_real_, time1to2_n), time2to3_n = if_else(time2to3_n == -50, NA_real_, time2to3_n), time3to4_n = if_else(time3to4_n == -50, NA_real_, time3to4_n)) %>% select(-check)

setwd(dir)
write.csv(rate2, "rate_sugi_nat.csv", fileEncoding = "CP932", row.names = F)


# 木の数が減ってるのに面積が増えているもの
check1 = rate2 %>% filter(time1to2_n < 1) %>% filter(time1to2_a >= 2.5)
check2 = rate2 %>% filter(time2to3_n < 1) %>% filter(time2to3_a >= 2.5)
check3 = rate2 %>% filter(time3to4_n < 1) %>% filter(time3to4_a >= 2.5)

setwd(dir)
write.csv(check1, "Nneg_time1to2.csv", fileEncoding = "CP932", row.names = F)
write.csv(check2, "Nneg_time2to3.csv", fileEncoding = "CP932", row.names = F)
write.csv(check3, "Nneg_time3to4.csv", fileEncoding = "CP932", row.names = F)

plot(x = rate2$time1to2_n, rate2$time1to2_a)
plot(x = rate2$time2to3_n, rate2$time2to3_a)
plot(x = rate2$time3to4_n, rate2$time3to4_a)


# 木の数が増えてるのに面積が減っているもの
check1 = rate2 %>% filter(time1to2_n >= 1) %>% filter(time1to2_a < 0.8)
check2 = rate2 %>% filter(time2to3_n >= 1) %>% filter(time2to3_a < 0.8)
check3 = rate2 %>% filter(time3to4_n >= 1) %>% filter(time3to4_a < 0.8)

setwd(dir)
write.csv(check1, "Npos_time1to2.csv", fileEncoding = "CP932", row.names = F)
write.csv(check2, "Npos_time2to3.csv", fileEncoding = "CP932", row.names = F)
write.csv(check3, "Npos_time3to4.csv", fileEncoding = "CP932", row.names = F)



# データの除去 ------------------------------------------------------------------
setwd(dir)

# load data ---------------------------------------------------------------
load("sugi_a_0.Rdata")
df = df_a_0_sugi
summary(df)
unique(df$type)
df = df %>% 
  filter(type == "天然林") %>%
  mutate(cpue = sum_area/effort, tag = paste(lon, lat, sep = "_")) %>% 
  select(year, lat, lon, sum_area, cpue, species, slope, elevation) %>% 
  rename(spp = species)
summary(df)
df_a = df
summary(df_a)


load("sugi_n_0.Rdata")
df = sugi_n_0
summary(df)
unique(df$type)
df = df %>% 
  filter(type == "天然林") %>%
  mutate(cpue = n/effort, tag = paste(lon, lat, sep = "_")) %>% 
  select(year, lat, lon, n, cpue, species, slope, elevation) %>% 
  rename(spp = species)
summary(df)
df_n = df
summary(df_n)


head(df_a, 3)
head(df_n, 3)

df = rbind(df_a %>% rename(obs = sum_area) %>% mutate(area_n = "area"),
           df_n %>% rename(obs = n) %>% mutate(area_n = "n")) %>% mutate(tag = paste(lon, lat, sep = "_"))

df_t1 = df %>% filter(year == 1)
df_t2 = df %>% filter(year == 2)
df_t3 = df %>% filter(year == 3)
df_t4 = df %>% filter(year == 4)

# 該当データを抜く（木の数が減ってるのに面積が増えているもの）
check1 = read.csv("Nneg_time1to2.csv", fileEncoding = "CP932")
check2 = read.csv("Nneg_time2to3.csv", fileEncoding = "CP932")
check3 = read.csv("Nneg_time3to4.csv", fileEncoding = "CP932")

# t = 1
tag_t1 = df_t1 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c1 = check1 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag1 = left_join(tag_t1, tag_c1, by = "tag") 
check_tag1[is.na(check_tag1)] = 0

df_t1 = left_join(df_t1, check_tag1, by = "tag")
df_t1 = df_t1 %>% filter(data2 != -1)


# t = 2
# データ2からcheck1を除去
tag_t2 = df_t2 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c1 = check1 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag2 = left_join(tag_t2, tag_c1, by = "tag") 
check_tag2[is.na(check_tag2)] = 0

df_t2 = left_join(df_t2, check_tag2, by = "tag")
df_t2 = df_t2 %>% filter(data2 != -1)

# データ2からcheck2を除去
tag_t2 = df_t2 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c2 = check2 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag2 = left_join(tag_t2, tag_c2, by = "tag") 
check_tag2[is.na(check_tag2)] = 0

df_t2 = left_join(df_t2, check_tag2, by = "tag")
df_t2 = df_t2 %>% filter(data2.y != -1)


# t = 3
# データ3からチェック2を除去
tag_t3 = df_t3 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c2 = check2 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag3 = left_join(tag_t3, tag_c2, by = "tag") 
check_tag3[is.na(check_tag3)] = 0

df_t3 = left_join(df_t3, check_tag3, by = "tag")
df_t3 = df_t3 %>% filter(data2 != -1)


# データ3からチェック3を除去
tag_t3 = df_t3 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c3 = check3 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag3 = left_join(tag_t3, tag_c3, by = "tag") 
check_tag3[is.na(check_tag3)] = 0

df_t3 = left_join(df_t3, check_tag3, by = "tag")
df_t3 = df_t3 %>% filter(data2.y != -1)


# t = 4
# データ4からチェック3を除去
tag_t4 = df_t4 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c3 = check3 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag4 = left_join(tag_t4, tag_c3, by = "tag") 
check_tag4[is.na(check_tag4)] = 0

df_t4 = left_join(df_t4, check_tag4, by = "tag")
df_t4 = df_t4 %>% filter(data2 != -1)



# 該当データを抜く（木の数が減ってるのに面積が増えているもの）
check1 = read.csv("Npos_time1to2.csv", fileEncoding = "CP932")
check2 = read.csv("Npos_time2to3.csv", fileEncoding = "CP932")
check3 = read.csv("Npos_time3to4.csv", fileEncoding = "CP932")

# t = 1
tag_t1 = df_t1 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c1 = check1 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag1 = left_join(tag_t1, tag_c1, by = "tag") 
check_tag1[is.na(check_tag1)] = 0

df_t1 = left_join(df_t1, check_tag1, by = "tag")
df_t1 = df_t1 %>% filter(data2.y != -1)


# t = 2
# データ2からcheck1を除去
tag_t2 = df_t2 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c1 = check1 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag2 = left_join(tag_t2, tag_c1, by = "tag") 
check_tag2[is.na(check_tag2)] = 0

df_t2 = left_join(df_t2, check_tag2, by = "tag")
df_t2 = df_t2 %>% filter(data2.y != -1)

# データ2からcheck2を除去
tag_t2 = df_t2 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c2 = check2 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag2 = left_join(tag_t2, tag_c2, by = "tag") 
check_tag2[is.na(check_tag2)] = 0

df_t2 = left_join(df_t2, check_tag2, by = "tag")
df_t2 = df_t2 %>% filter(data2.y.y != -1)


# t = 3
# データ3からチェック2を除去
tag_t3 = df_t3 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c2 = check2 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag3 = left_join(tag_t3, tag_c2, by = "tag") 
check_tag3[is.na(check_tag3)] = 0

df_t3 = left_join(df_t3, check_tag3, by = "tag")
df_t3 = df_t3 %>% filter(data2.y != -1)


# データ3からチェック3を除去
tag_t3 = df_t3 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c3 = check3 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag3 = left_join(tag_t3, tag_c3, by = "tag") 
check_tag3[is.na(check_tag3)] = 0

df_t3 = left_join(df_t3, check_tag3, by = "tag")
df_t3 = df_t3 %>% filter(data2.y.y != -1)


# t = 4
# データ4からチェック3を除去
tag_t4 = df_t4 %>% select(tag) %>% distinct(tag) %>% mutate(data = 1)
tag_c3 = check3 %>% select(tag) %>% distinct(tag) %>% mutate(data2 = -1)
check_tag4 = left_join(tag_t4, tag_c3, by = "tag") 
check_tag4[is.na(check_tag4)] = 0

df_t4 = left_join(df_t4, check_tag4, by = "tag")
df_t4 = df_t4 %>% filter(data2.y != -1)


df_sugi = rbind(df_t1 %>% select(tag, lon, lat, year, obs, cpue, spp, slope, elevation, area_n),
                df_t2 %>% select(tag, lon, lat, year, obs, cpue, spp, slope, elevation, area_n),
                df_t3 %>% select(tag, lon, lat, year, obs, cpue, spp, slope, elevation, area_n),
                df_t4 %>% select(tag, lon, lat, year, obs, cpue, spp, slope, elevation, area_n))
save(df_sugi, file = "df_sugi_natural.Rdata")

