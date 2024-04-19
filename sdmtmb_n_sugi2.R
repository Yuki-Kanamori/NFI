# 0. Prepare the data -------------------------------------------
dirname = "/Users/Yuki/Dropbox/NFI"
setwd(dir = dirname)

# Packages
require(tidyverse)
require(sdmTMB)
library(sf)
library(sp)

# read the data
# df = read.csv("sugi_n.csv", fileEncoding = "CP932")
# load("sugi_n_0.Rdata")
load("df_sugi_natural.Rdata")
df = df_sugi
summary(df)

df = df %>% 
  # mutate(cpue = n/effort) %>% 
  # select(year, lat, lon, n, cpue, species, slope, elevation) %>% 
  # rename(spp = species)
  filter(area_n == "n")
summary(df)
summary(df$obs)

# df = df %>% na.omit()


# 気温データ
load("/Users/Yuki/Dropbox/LST/mean/lst_all.Rdata")
head(df, 3)
head(lst_all, 3)
lst_all = lst_all %>% select(nendo, tag, mean)
lst_all1 = lst_all %>% filter(between(nendo, 1999, 2003)) %>% mutate(year = 1) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
lst_all2 = lst_all %>% filter(between(nendo, 2004, 2008)) %>% mutate(year = 2) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
lst_all3 = lst_all %>% filter(between(nendo, 2009, 2013)) %>% mutate(year = 3) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
lst_all4 = lst_all %>% filter(between(nendo, 2014, 2018)) %>% mutate(year = 4) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
lst_all2 = rbind(lst_all1, lst_all2, lst_all3, lst_all4) %>% rename(lst = mean)
df = left_join(df, lst_all2, by = c("tag", "year"))
summary(df) 
# df = df %>% na.omit()
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = obs), pch=15,cex=0.5) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))


# 日射量
load("/Users/Yuki/Dropbox/GSR/gsr_all.Rdata")
head(df, 3)
head(gsr_all, 3)
gsr_all = gsr_all %>% select(nendo, tag, mean)
gsr_all1 = gsr_all %>% filter(between(nendo, 1999, 2003)) %>% mutate(year = 1) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
gsr_all2 = gsr_all %>% filter(between(nendo, 2004, 2008)) %>% mutate(year = 2) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
gsr_all3 = gsr_all %>% filter(between(nendo, 2009, 2013)) %>% mutate(year = 3) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
gsr_all4 = gsr_all %>% filter(between(nendo, 2014, 2018)) %>% mutate(year = 4) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
gsr_all2 = rbind(gsr_all1, gsr_all2, gsr_all3, gsr_all4) %>% rename(gsr = mean)
df = left_join(df, gsr_all2, by = c("tag", "year"))
summary(df) 
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = obs), pch=15,cex=0.5) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))

# 降水量
load("/Users/Yuki/Dropbox/APCP/apcp_all.Rdata")
head(df, 3)
head(apcp_all, 3)
apcp_all = apcp_all %>% select(nendo, tag, mean)
apcp_all1 = apcp_all %>% filter(between(nendo, 1999, 2003)) %>% mutate(year = 1) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
apcp_all2 = apcp_all %>% filter(between(nendo, 2004, 2008)) %>% mutate(year = 2) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
apcp_all3 = apcp_all %>% filter(between(nendo, 2009, 2013)) %>% mutate(year = 3) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
apcp_all4 = apcp_all %>% filter(between(nendo, 2014, 2018)) %>% mutate(year = 4) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
apcp_all2 = rbind(apcp_all1, apcp_all2, apcp_all3, apcp_all4) %>% rename(apcp = mean)
df = left_join(df, apcp_all2, by = c("tag", "year"))
summary(df) 
# df = df %>% na.omit()
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = obs), pch=15,cex=0.5) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))




# 裸地データ
load("/Users/Yuki/Dropbox/NFI/bare.Rdata")
head(df, 3); head(bare, 3)
bare_nat = bare %>% filter(type == "天然林") %>% select(tag, bare, year)
df = left_join(df, bare_nat, by = c("tag", "year"))
summary(df)
df = df %>% mutate(fyear = as.factor(year))
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = obs), pch=15,cex=0.5) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))

df = df %>% na.omit() %>% filter(elevation > 0) %>% mutate(pa = ifelse(cpue > 0, 1, 0))
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = obs), pch=15,cex=0.5) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))



plot(x = df$elevation, y = df$cpue)
plot(x = df$lst, y = df$cpue)
plot(x = df$gsr, y = df$cpue)
plot(x = df$bare, y = df$cpue)

cor = df %>% select(elevation, lst, gsr, apcp, bare)
require(psych)
pairs.panels(cor)


# データの地図 ------------------------------------------------------------------
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s%>% filter(cpue>0)) + 
  geom_sf(aes(color = cpue), pch=15,cex=1) +
  facet_wrap(~ year, ncol = 4) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
# scale_color_gradient(low = "lightgrey", high = "red")
summary(pcod_s %>% filter(obs > 0))


# 空間メッシュの作成 ---------------------------------------------------------------
mesh <- make_mesh(df, xy_cols = c("lat", "lon"), type = "kmeans", seed = 0, cutoff = 0.05, n_knots = 500)
plot(mesh, pch=1)


# フィッティング -----------------------------------------------------------------
fit2<- sdmTMB(
  # formula = list(pa ~ s(elevation) + as.factor(year), cpue ~ s(bare) + s(gsr) + as.factor(year)),
  cpue ~ s(elevation) + s(bare) + s(gsr)  + as.factor(year),
  data = df,
  mesh = mesh,
  family = delta_lognormal(),
  time = "year",
  spatial = "on",
  spatiotemporal = "RW",
  anisotropy = TRUE,
  reml=FALSE)
  # control = sdmTMBcontrol(map = TRUE))
sanity(fit2)

fit3<- sdmTMB(
  # formula = list(pa ~ s(elevation) + as.factor(year), cpue ~ s(bare) + s(gsr) + as.factor(year)),
  cpue ~ s(elevation) + s(bare) + as.factor(year),
  data = df,
  mesh = mesh,
  family = delta_lognormal(),
  time = "year",
  spatial = "on",
  spatiotemporal = "iid",
  anisotropy = TRUE,
  reml=FALSE)
# control = sdmTMBcontrol(map = TRUE))
sanity(fit3)

fit4<- sdmTMB(
  # formula = list(pa ~ s(elevation) + as.factor(year), cpue ~ s(bare) + s(gsr) + as.factor(year)),
  cpue ~ s(elevation) + s(bare) + as.factor(year),
  data = df,
  mesh = mesh,
  family = delta_lognormal(),
  time = "year",
  spatial = "on",
  spatiotemporal = "AR1",
  anisotropy = TRUE,
  reml=FALSE)
# control = sdmTMBcontrol(map = TRUE))
sanity(fit4)

AIC(fit2);AIC(fit3);AIC(fit4)



p = predict(fit3, type = "response", return_tmb_object = TRUE)
p_map = p$data

p_map[1:3, ]
p_s = st_as_sf(p_map, coords=c("lon", "lat"))
summary(p_s)
ggplot(p_s %>% filter(est1 > 0.1)) + 
  geom_sf(aes(color = est), pch=15,cex=1) +
  facet_wrap(~year, ncol = 4) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
# scale_color_gradient(low = "lightgrey", high = "red")
summary(p_s$est)
summary(df$cpue)


# 説明変数の効果 -----------------------------------------------------------------
visreg_delta(fit2, xvar = "elevation", model = 1, gg = TRUE)
visreg_delta(fit2, xvar = "gsr", model = 1, gg = TRUE)
visreg_delta(fit2, xvar = "bare", model = 1, gg = TRUE)

visreg_delta(fit2, xvar = "elevation", model = 2, gg = TRUE)
visreg_delta(fit2, xvar = "gsr", model = 2, gg = TRUE)
visreg_delta(fit2, xvar = "bare", model = 2, gg = TRUE)



# 残差 ----------------------------------------------------------------------
p_map$resids <- residuals(fit3)
hist(p_map$resids)

qqnorm(p_map$resids);abline(a = 0, b = 1)
