# 0. Prepare the data -------------------------------------------
dirname = "/Users/Yuki/Dropbox/NFI"
setwd(dir = dirname)

# Packages
require(tidyverse)
require(sdmTMB)
library(sf)
library(sp)
require(DHARMa)
require(visreg)

# read the data
# df = read.csv("sugi_n.csv", fileEncoding = "CP932")
# load("sugi_n_0.Rdata")
load("df_hinoki_natural.Rdata")
df = df_hinoki
summary(df)
unique(df$type)

df = df %>% 
  # mutate(cpue = obs/effort) %>% 
  # select(year, lat, lon, n, cpue, species, slope, elevation) %>% 
  # rename(spp = species)
  filter(area_n == "n")
summary(df)
summary(df$obs)

df = df %>% na.omit() %>% filter(elevation > 0) %>% mutate(cpue = obs/100)
hist(df$obs, breaks=seq(0, 370, 5))

# t1 = df %>% filter(year == 1)
# t2 = df %>% filter(year == 2)
# t3 = df %>% filter(year == 3)
# t4 = df %>% filter(year == 4)

plot(x = df$elevation, y = df$cpue)
df2 = df %>% mutate(pa = ifelse(cpue > 0, 1, 0))
plot(x = df2$elevation, y = df2$pa)


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


# 日射量
load("/Users/Yuki/Dropbox/GSR/gsr_all.Rdata")
head(df, 3)
head(gsr_all, 3)
gsr_all = gsr_all %>% select(nendo, tag, mean)
gsr_all1 = gsr_all %>% filter(between(nendo, 1999, 2003)) %>% mutate(year = 1) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
gsr_all2 = lst_all %>% filter(between(nendo, 2004, 2008)) %>% mutate(year = 2) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
gsr_all3 = lst_all %>% filter(between(nendo, 2009, 2013)) %>% mutate(year = 3) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
gsr_all4 = lst_all %>% filter(between(nendo, 2014, 2018)) %>% mutate(year = 4) %>% group_by(tag, year) %>% summarize(mean = mean(mean))
gsr_all2 = rbind(gsr_all1, gsr_all2, gsr_all3, gsr_all4) %>% rename(gsr = mean)
df = left_join(df, gsr_all2, by = c("tag", "year"))
summary(df) 
df = df %>% na.omit()

# 裸地データ
load("/Users/Yuki/Dropbox/NFI/bare.Rdata")
head(df, 3); head(bare, 3)
bare_nat = bare %>% filter(type == "天然林") %>% select(tag, bare, year)
df = left_join(df, bare_nat, by = c("tag", "year"))
summary(df)
df = df %>% na.omit() %>% mutate(fyear = as.factor(year))


plot(x = df$elevation, y = df$cpue)
plot(x = df$lst, y = df$cpue)
plot(x = df$gsr, y = df$cpue)
plot(x = df$bare, y = df$cpue)


# データの地図 ------------------------------------------------------------------
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = cpue), pch=15,cex=0.5) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
# scale_color_gradient(low = "lightgrey", high = "red")
summary(pcod_s %>% filter(obs > 0))


# 空間メッシュの作成 ---------------------------------------------------------------
mesh <- make_mesh(df, xy_cols = c("lat", "lon"), type = "kmeans", seed = 0, cutoff = 0.05, n_knots = 500)
plot(mesh, pch=1)



# フィッティング -----------------------------------------------------------------
fit1<- sdmTMB(
  cpue ~ s(elevation) + s(bare) + s(gsr)  + fyear,
  data = df,
  mesh = mesh,
  family = delta_lognormal(),
  spatial = "on",
  anisotropy = TRUE,
  reml=FALSE)

fit1
tidy(fit1, conf.int = TRUE)
tidy(fit1, effects = "ran_pars", conf.int = TRUE)
sanity(fit1)

# ggeffects::ggpredict(fit1, "elevation[0:1500, by = 100]") |> plot()


fit2<- sdmTMB(
  cpue ~ s(elevation) + s(bare) + s(gsr)  + fyear,
  data = df,
  mesh = mesh,
  family = delta_lognormal(),
  spatial = "on",
  time = "year",
  spatiotemporal = "iid",
  anisotropy = TRUE,
  reml=FALSE)

fit2
tidy(fit2, conf.int = TRUE)
tidy(fit2, effects = "ran_pars", conf.int = TRUE)
sanity(fit2)


fit3<- sdmTMB(
  obs ~ s(elevation) + s(slope) + as.factor(year),
  data = df,
  mesh = mesh,
  family = poisson(),
  spatial = "on",
  time = "year",
  spatiotemporal = "AR1",
  anisotropy = TRUE,
  reml=FALSE)


fit4<- sdmTMB(
  obs ~ s(elevation) + s(slope) + as.factor(year),
  data = df,
  mesh = mesh,
  family = poisson(),
  spatial = "on",
  time = "year",
  spatiotemporal = "RW",
  anisotropy = TRUE,
  reml=FALSE)


AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)

# 予測 ----------------------------------------------------------------------
df2 = df %>% mutate(lonlat = paste(lon, lat, sep = "_"))
grid = df2 %>% select(lonlat, lon, lat, elevation, slope, lst, gsr, bare) 
grid = grid %>% distinct(lonlat, .keep_all = TRUE)

grid2 = NULL
for(year in unique(df$fyear)){
  grid_sub = data.frame(fyear = year, grid[, c("lon", "lat", "elevation", "slope", "lst", "gsr", "bare")])
  grid2 = rbind(grid2, grid_sub)
}

grid2[1:2,]
p = predict(fit1, newdata = grid2, type = "response", return_tmb_object = TRUE)
p_map = predict(fit1, newdata = grid2, type = "response")
p = predict(fit2, newdata = grid2, type = "response")
p = predict(fit3, newdata = grid2, type = "response")
p = predict(fit4, newdata = grid2, type = "response")
p_map[1:3, ]
p_s = st_as_sf(p_map, coords=c("lon", "lat"))
summary(p_s)
ggplot(p_s %>% filter(est2 > 0)) + 
  geom_sf(aes(color = est), pch=15,cex=0.5) +
  facet_wrap(~fyear) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
# scale_color_gradient(low = "lightgrey", high = "red")
summary(p_s$est)
summary(df$cpue)

p_s %>% p_sp_s %>% group_by(year) %>% summarize(mean = mean(est))

hist(p_s$est, breaks = seq(0, 580, 5))



# 説明変数の効果 -----------------------------------------------------------------
ind_dg <- get_index(p, bias_correct = TRUE)
visreg_delta(fit1, xvar = "elevation", model = 1, gg = TRUE, by = "fyear")
visreg_delta(fit1, xvar = "bare", model = 2, gg = TRUE, by = "fyear")

visreg_delta(fit1, xvar = "elevation", model = 1, gg = TRUE)
visreg_delta(fit1, xvar = "bare", model = 1, gg = TRUE)
visreg_delta(fit1, xvar = "elevation", model = 2, gg = TRUE)
visreg_delta(fit1, xvar = "gsr", model = 2, gg = TRUE)
visreg_delta(fit1, xvar = "bare", model = 2, gg = TRUE)


# 予測値の不確実性の評価 -----------------------------------------------------------------
# qq plots
# residual
qq1 <- residuals(fit1)
qq1 <- qq1[is.finite(qq1)] # some Inf
qqnorm(qq1);qqline(qq1)

# simulation based residual
s_qq <- simulate(fit1, nsim = 500)
simulate(fit1, nsim = 300) %>% 
  dharma_residuals(fit1)


# ??
p2_sim     <- predict(fit1, newdata = grid2, type="response", nsim=500)
p_s$est_sd<- apply(p2_sim, 1, sd)
ggplot(p_s) + 
  geom_sf(aes(color = est_sd),pch=15,cex=0.5) +
  facet_wrap(~year) + 
  theme_minimal() +
  scale_color_gradient(low = "lightgrey", high = "red")



# 年トレンド -------------------------------------------------------------------
grid_yrs = replicate_df(grid2, "year", unique(df$year))
p_st = predict(fit1, newdata = grid_yrs, 
               return_tmb_object = TRUE)

index = get_index(p_st, bias_correct = T, area = rep(4, nrow(grid_yrs)))

ggplot(index, aes(year, est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey90") +
  geom_line(lwd = 1, colour = "grey30") +
  labs(x = "Year", y = "CPUE (N/m^2)")
