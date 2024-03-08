# 0. Prepare the data -------------------------------------------
dirname = "/Users/Yuki/Dropbox/NFI"
setwd(dir = dirname)

# Packages
# require(TMB)
# require(VAST)
require(tidyverse)
require(sdmTMB)
library(sf)
library(sp)

# read the data
df = read.csv("sugi_n.csv", fileEncoding = "CP932")
summary(df)
df = df %>% 
  # filter(species == "スギ") %>% 
  mutate(cpue = n/effort, species = "スギ_天然林") %>% 
  select(year, lat, lon, n, cpue, species, slope, elevation) %>% 
  rename(spp = species)
summary(df)

df = df %>% na.omit()

# データの地図 ------------------------------------------------------------------
pcod_s <- st_as_sf(df, coords=c("lon", "lat"))
ggplot(pcod_s) + 
  geom_sf(aes(color = cpue)) +
  facet_wrap(~ year) + 
  theme_minimal() +
  scale_color_gradient(low = "lightgrey", high = "red")



# 空間メッシュの作成 ---------------------------------------------------------------
mesh <- make_mesh(df, xy_cols = c("lat", "lon"), cutoff = 0.05)
plot(mesh, pch=1)



# フィッティング -----------------------------------------------------------------
fit1<- sdmTMB(
  cpue ~ s(elevation) + as.factor(year),
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


# 予測 ----------------------------------------------------------------------
df2 = df %>% mutate(lonlat = paste(lon, lat, sep = "_"))
grid = df2 %>% select(lonlat, lon, lat, elevation) %>% distinct(lonlat, .keep_all = T)

grid2 = NULL
for(year in unique(df$year)){
  grid_sub = data.frame(year, grid[, 2:4])
  grid2 = rbind(grid2, grid_sub)
}

grid2[1:2,]
p = predict(fit1, newdata = grid2, type = "response")
p[1:3, ]
p_s = st_as_sf(p, coords=c("lon", "lat"))
ggplot(p_s) + 
  geom_sf(aes(color = est), pch=15,cex=0.5) +
  facet_wrap(~year) + 
  theme_minimal() +
  scale_color_gradient(low = "lightgrey", high = "red")
