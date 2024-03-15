
# package -----------------------------------------------------------------
require(didyverse)


# directory ---------------------------------------------------------------
dirname = "/Users/Yuki/Dropbox/NFI"
setwd(dir = dirname)


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

check1 = rate2 %>% filter(time1to2_n < 1) %>% filter(time1to2_a >= 2.5)
check2 = rate2 %>% filter(time2to3_n < 1) %>% filter(time2to3_a >= 2.5)
check3 = rate2 %>% filter(time3to4_n < 1) %>% filter(time3to4_a >= 2.5)
