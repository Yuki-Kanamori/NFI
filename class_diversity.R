require(tidyverse)
require(ggplot2)
require(maps)
require(mapdata)
require(mapproj)


# directory ---------------------------------------------------------------
dir = "/Users/Yuki/Dropbox/NFI"


# data --------------------------------------------------------------------
setwd(dir = dir)
load("tohoku_N.Rdata") # df_tohoku
head(df_tohoku)
unique(df_tohoku$cate)

df_tohoku$pref = ifelse(df_tohoku$pref == "福島県", "fukushima", ifelse(df_tohoku$pref == "宮城県", "miyagi", ifelse(df_tohoku$pref == "山形県", "yamagata", ifelse(df_tohoku$pref == "秋田県", "akita", ifelse(df_tohoku$pref == "岩手県", "iwate", ifelse(df_tohoku$pref == "青森県", "aomori", NA))))))

p_tohoku = df_tohoku %>% filter(n > 0) %>% group_by(pref, time) %>% count()
p_tohoku2 = df_tohoku %>% filter(n > 0) %>% group_by(pref, time, site_id) %>% count() %>% group_by(pref, time) %>% summarise(mean = mean(n))

p_tohoku3 = df_tohoku %>% filter(n > 0, cate == "small") %>% group_by(pref, time, site_id) %>% count() %>% group_by(pref, time) %>% summarise(mean = mean(n))
p_tohoku4 = df_tohoku %>% filter(n > 0) %>% group_by(pref, time, site_id, cate) %>% count() %>% group_by(pref, time, cate) %>% summarise(mean = mean(n))

# figure ------------------------------------------------------------------
g = ggplot(p_tohoku, aes(x = time, y = n, colour = pref))
p = geom_point()
l = geom_line()
labs = labs(x = "Time", y = "Number of species", colour = "Prefecture")
g+p+l+labs+theme_bw()

g = ggplot(p_tohoku2, aes(x = time, y = mean, colour = pref))
p = geom_point()
l = geom_line()
labs = labs(x = "Time", y = "Mean number of species", colour = "Prefecture")
g+p+l+labs+theme_bw()

g = ggplot(p_tohoku3, aes(x = time, y = mean, colour = pref))
p = geom_point()
l = geom_line()
labs = labs(x = "Time", y = "Mean number of species in small plots", colour = "Prefecture")
g+p+l+labs+theme_bw()

g = ggplot(p_tohoku4, aes(x = time, y = mean, colour = pref))
p = geom_point()
l = geom_line()
f = facet_wrap(~ cate, ncol = 1)
labs = labs(x = "Time", y = "Mean number of species", colour = "Prefecture")
g+p+l+f+labs+theme_bw()
