require(tidyverse)
require(ggplot2)
require(maps)
require(mapdata)
require(mapproj)


# directory ---------------------------------------------------------------
dir = "/Users/Yuki/Dropbox/NFI"


# data --------------------------------------------------------------------
setwd(dir = dir)
load("df_allN.Rdata") # df_all


# 都道府県情報 ------------------------------------------------------------------
# 1期
dir_data1 = "/Users/Yuki/Dropbox/NFI/NFI_1_CSV/第1期"
setwd(dir_data1)

pref1 = read.csv("01_プロット.csv", fileEncoding = "CP932") %>% select("格子点ID", "都道府県", "土地利用")
colnames(pref1) = c("site_id", "pref", "usage")

elev1 = read.csv("02_様式２−１.csv", fileEncoding = "CP932") %>% select("格子点ID","標高") %>% rename(site_id = 格子点ID, elevation = 標高)

pref1 = left_join(pref1, elev1, by = "site_id")
pref1$time = 1


# 2期
dir_data2 = "/Users/Yuki/Dropbox/NFI/NFI_2_CSV/第2期"
setwd(dir_data2)

pref2 = read.csv("01_プロット.csv", fileEncoding = "CP932") %>% select("格子点ID", "都道府県", "土地利用")
colnames(pref2) = c("site_id", "pref", "usage")

elev2 = read.csv("02_様式２−１.csv", fileEncoding = "CP932") %>% select("格子点ＩＤ","標高") %>% rename(site_id = 格子点ＩＤ, elevation = 標高)

pref2 = left_join(pref2, elev2, by = "site_id")
pref2$time = 2


# 3期
dir_data3 = "/Users/Yuki/Dropbox/NFI/NFI_3_CSV/第3期"
setwd(dir_data3)

pref3 = read.csv("01_プロット.csv", fileEncoding = "CP932") %>% select("格子点ID", "都道府県", "土地利用")
colnames(pref3) = c("site_id", "pref", "usage")

elev3 = read.csv("19_様式７.csv", fileEncoding = "CP932") %>% select("格子点ＩＤ","標高") %>% rename(site_id = 格子点ＩＤ, elevation = 標高)

pref3 = left_join(pref3, elev3, by = "site_id")
pref3$time = 3


# 4期
dir_data4 = "/Users/Yuki/Dropbox/NFI/NFI_4_CSV/第4期"
setwd(dir_data4)

pref4 = read.csv("01_プロット.csv", fileEncoding = "CP932") %>% select("格子点ID", "都道府県", "土地利用")
colnames(pref4) = c("site_id", "pref", "usage")

elev4 = read.csv("21_様式７.csv", fileEncoding = "CP932") %>% select("格子点ＩＤ","標高") %>% rename(site_id = 格子点ＩＤ, elevation = 標高)

pref4 = left_join(pref4, elev4, by = "site_id")
pref4$time = 4


df_pref = rbind(pref1, pref2, pref3, pref4) %>% distinct(site_id, time, .keep_all = TRUE)
nrow(pref1)+nrow(pref2)+nrow(pref3)+nrow(pref4)



# 樹木データに都道府県と土地利用情報を付与する --------------------------------------------------
df_all2 = left_join(df_all, df_pref, by = c("site_id", "time")) %>% filter(usage == "森林")
unique(df_all2$pref)



# 東北のデータを抽出 ---------------------------------------------------------------
tohoku = c("青森", "岩手", "秋田", "宮城", "福島", "山形")
tohoku = c("長崎県", "鹿児島県", "佐賀県", "熊本県", "宮崎県", "大分県", "福岡県")

df_tohoku = df_all2 %>% filter(pref %in% tohoku)
unique(df_tohoku$pref)

pre_cog = df_tohoku %>% group_by(time, site_id, lon, lat, species) %>% summarize(count = sum(n)) %>% mutate(effort_ha = 0.1) %>% mutate(dens = count/effort_ha)



# 各種の出現パターンから優占種などを確認する -------------------------------------------------------------------
# 調査期間全体で密度が多い種
options(scipen=10)
dominant = pre_cog %>% group_by(species) %>% summarize(mean_dens = mean(dens)) %>% filter(mean_dens > 0) %>% arrange(-mean_dens)
head(dominant$species)

# 各調査期間での密度
dominant2 = pre_cog %>% group_by(species, time) %>% summarize(mean_dens = mean(dens)) %>% filter(mean_dens > 0) %>% arrange(-mean_dens)

# 各調査期間での空間的出現頻度
pre_cog2 = pre_cog %>% mutate(pa = ifelse(dens > 0, 1, 0)) %>% group_by(species, time) %>% summarize(sum = sum(pa))
n_survey = pre_cog %>% distinct(site_id, time) %>% group_by(time) %>% count()
pre_cog2 = left_join(pre_cog2, n_survey, by = "time") %>% mutate(s_freq = sum/n) %>% filter(s_freq > 0) %>% arrange(-s_freq)

# 調査期間全体での空間的出現頻度
pre_cog2 = pre_cog %>% mutate(pa = ifelse(dens > 0, 1, 0)) %>% group_by(species) %>% summarize(sum = sum(pa))
n_survey = pre_cog %>% distinct(site_id)
pre_cog2 = pre_cog2 %>% mutate(n = nrow(n_survey)) %>% mutate(s_freq = sum/n) %>% filter(s_freq > 0) %>% arrange(-s_freq)



# 重心の変化 -------------------------------------------------------
# 緯度方向
haha_lat = pre_cog %>% group_by(time, species) %>% summarize(haha = sum(dens))
ko_lat = pre_cog %>% mutate(ko = dens*lat) %>% group_by(time, species) %>% summarize(ko = sum(ko))
cog_lat = left_join(ko_lat, haha_lat, by = c("time", "species")) %>% mutate(cog = ko/haha)

length(unique(ko_lat$species))

sp = unique(ko_lat$species)
df_lm = NULL
for(i in 1:1313){
  df = cog_lat %>% ungroup() %>% filter(species == sp[[i]])
  df2 = df %>% na.omit()
  
  if(nrow(df2) <= 1) next
  lm = summary(lm(cog ~ time, data = df2))
  lm2 = lm[["coefficients"]] %>% data.frame() %>% mutate(species = sp[[i]], param = c("intercept", "time"))
  
  df_lm = rbind(df_lm, lm2)
}

inc_lat = df_lm %>% filter(param == "time", Pr...t.. <= 0.05, Estimate > 0)


# 高度方向
haha_ele = pre_cog %>% group_by(time, species) %>% summarize(haha = sum(dens))
ele = df_pref %>% filter(usage == "森林") %>% group_by(site_id) %>% 
  select(site_id, elevation) %>% na.omit() %>% summarize(mean_ele = mean(elevation))
ko_ele = left_join(pre_cog, ele, by = c("site_id"))  %>% mutate(ko = dens*mean_ele) %>% group_by(time, species) %>% summarize(ko = sum(ko))
cog_ele = left_join(ko_ele, haha_ele, by = c("time", "species")) %>% mutate(cog = ko/haha)

sp2 = unique(ko_ele$species)

df_lm2 = NULL
for(i in 1:1313){
  df = cog_ele %>% ungroup() %>% filter(species == sp2[[i]])
  df2 = df %>% na.omit()
  
  if(nrow(df2) <= 1) next
  lm = summary(lm(cog ~ time, data = df2))
  lm2 = lm[["coefficients"]] %>% data.frame() %>% mutate(species = sp2[[i]], param = c("intercept", "time"))
  
  df_lm2 = rbind(df_lm2, lm2)
}

inc_ele = df_lm2 %>% filter(param == "time", Pr...t.. <= 0.05, Estimate > 0)
nrow(inc_ele)/length(unique(df_lm2$species))*100

check_ele = df_lm2 %>% filter(param == "time", Pr...t.. <= 0.05)



# 調査地点（天然林，森林） --------------------------------------------------------------------
tone = pre_cog %>% filter(species == "オオイタヤメイゲツ")
summary(tone$dens)
head(tone)

# 日本地図の設定（変更しない）
japan = map_data("japan") %>% mutate(long = long - 0.01, lat = lat - 0.01)

g = ggplot() + 
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + 
  coord_map(xlim = c(128, 132), ylim = c(27, 35))

p = geom_point(data = tone, aes(x = lon, y = lat, color = dens), shape = 15, size = 1)
c = scale_color_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
# c = scale_fill_gradientn(colours = c("white", "blue", "red"))
f = facet_wrap(~ time, ncol = 4)
labs = labs(title = "", x = "", y = "", color = "Density")

g+p+c+f+labs+theme_bw()
