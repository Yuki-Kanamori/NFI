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
load("sugi_n_0.Rdata")
df = sugi_n_0
summary(df)
unique(df$type)
df = df %>% 
  filter(type == "人工林") %>%
  mutate(cpue = n/effort) %>% 
  select(year, lat, lon, n, cpue, species, slope, elevation) %>% 
  rename(spp = species)
summary(df)

df = df %>% na.omit()

# データの地図 ------------------------------------------------------------------
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = n), pch=15,cex=0.5) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
  # scale_color_gradient(low = "lightgrey", high = "red")


# 空間メッシュの作成 ---------------------------------------------------------------
mesh <- make_mesh(df, xy_cols = c("lat", "lon"), type = "kmeans", seed = 0, cutoff = 0.05, n_knots = 500)
plot(mesh, pch=1)



# フィッティング -----------------------------------------------------------------
fit1<- sdmTMB(
  n ~ s(elevation) + as.factor(year),
  data = df,
  mesh = mesh,
  family = poisson(),
  spatial = "on",
  reml=FALSE)

fit1
tidy(fit1, conf.int = TRUE)
tidy(fit1, effects = "ran_pars", conf.int = TRUE)
sanity(fit1)

# ggeffects::ggpredict(fit1, "elevation[0:1500, by = 100]") |> plot()


fit2<- sdmTMB(
  n ~ s(elevation) + as.factor(year),
  data = df,
  mesh = mesh,
  family = poisson(),
  spatial = "on",
  time = "year",
  spatiotemporal = "ar1",
  reml=FALSE)

fit2
tidy(fit2, conf.int = TRUE)
tidy(fit2, effects = "ran_pars", conf.int = TRUE)
sanity(fit2)

AIC(fit1)
AIC(fit2)

# 予測 ----------------------------------------------------------------------
df2 = df %>% mutate(lonlat = paste(lon, lat, sep = "_"))
grid = df2 %>% select(lonlat, lon, lat, elevation) %>% distinct(lonlat, .keep_all = T)

grid2 = NULL
for(year in unique(df$year)){
  grid_sub = data.frame(year, grid[, 2:4])
  grid2 = rbind(grid2, grid_sub)
}

grid2[1:2,]
p = predict(fit2, newdata = grid2, type = "response")
p[1:3, ]
p_s = st_as_sf(p, coords=c("lon", "lat"))
summary(p_s)
ggplot(p_s) + 
  geom_sf(aes(color = est), pch=15,cex=0.5) +
  facet_wrap(~year) + 
  theme_minimal() +
  scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
  # scale_color_gradient(low = "lightgrey", high = "red")

p_s %>% group_by(year) %>% summarize(mean = mean(est))



# 予測値の不確実性の評価 -----------------------------------------------------------------
p2_sim     <- predict(fit2, newdata = grid2, type="response", nsim=500)
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
  labs(x = "Year", y = "Biomass (kg)")
