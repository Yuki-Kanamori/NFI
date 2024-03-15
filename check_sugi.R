
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
  mutate(cpue = sum_area/effort) %>% 
  select(year, lat, lon, sum_area, cpue, species, slope, elevation) %>% 
  rename(spp = species)
summary(df)
df_a = df


load("sugi_n_0.Rdata")
df = sugi_n_0
summary(df)
unique(df$type)
df = df %>% 
  filter(type == "天然林") %>%
  mutate(cpue = n/effort) %>% 
  select(year, lat, lon, n, cpue, species, slope, elevation) %>% 
  rename(spp = species)
summary(df)
df_n = df

# データのチェック --------------------------------------------------------------
# area
time1 = df_a %>% filter(year == 1) %>% select(sum_area, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time1 = sum_area) %>% select(tag, time1)

time2 = df_a %>% filter(year == 2) %>% select(sum_area, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time2 = sum_area) %>% select(tag, time2)

time3 = df_a %>% filter(year == 3) %>% select(sum_area, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time3 = sum_area) %>% select(tag, time3)

time4 = df_a %>% filter(year == 4) %>% select(sum_area, lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_"), time4 = sum_area) %>% select(tag, time4)

tag = df_a %>% select(lon, lat) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% select(tag) %>% distinct()

tag2_a = left_join(tag, time1, by = "tag")
tag2_a = left_join(tag2_a, time2, by = "tag")
tag2_a = left_join(tag2_a, time3, by = "tag")
tag2_a = left_join(tag2_a, time4, by = "tag")

rate_a = data.frame(tag = tag2_a$tag, time1to2_a = ((tag2_a$time2)+0.001)/((tag2_a$time1)+0.001), time2to3_a = ((tag2_a$time3)+0.001)/((tag2_a$time2)+0.001), time3to4_a = ((tag2_a$time4)+0.01)/((tag2_a$time3)+0.01))
summary(rate)


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

rate_n = data.frame(tag = tag2$tag, time1to2_n = ((tag2_n$time2)+0.001)/((tag2_n$time1)+0.001), time2to3_n = ((tag2_n$time3)+0.001)/((tag2_n$time2)+0.001), time3to4_n = ((tag2_n$time4)+0.01)/((tag2_n$time3)+0.01))

rate = left_join(rate_a, rate_n, by = "tag")
rate[is.na(rate)] = -50
rate = rate %>% mutate(check = rowSums(rate[, 2:7]))

rate2 = rate %>% filter(check != 6) %>% filter(check != -300) %>% mutate(time1to2_a = if_else(time1to2_a == -50, NA_real_, time1to2_a), time2to3_a = if_else(time2to3_a == -50, NA_real_, time2to3_a), time3to4_a = if_else(time3to4_a == -50, NA_real_, time3to4_a), time1to2_n = if_else(time1to2_n == -50, NA_real_, time1to2_n), time2to3_n = if_else(time2to3_n == -50, NA_real_, time2to3_n), time3to4_n = if_else(time3to4_n == -50, NA_real_, time3to4_n))

setwd(dir)
write.csv(rate2 %>% select(-check), "rate_sugi_nat.csv", fileEncoding = "CP932", row.names = F)
