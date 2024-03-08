require(tidyverse)

setwd("/Users/Yuki/Dropbox/NFI")
all = read.csv("all.csv", fileEncoding = "CP932")
head(all, 3)
length(unique(all$species))
summary(all)
sp_list = all %>% distinct(species)
# write.csv(sp_list, "sp_list.csv", fileEncoding = "CP932", row.names = F)
sp_list = read.csv("sp_list.csv", fileEncoding = "CP932") %>% na.omit() %>% arrange(species2) 
sp_list = sp_list %>% mutate(n_sp = 1:nrow(sp_list))

all2 = left_join(all %>% filter(elevation > 0), sp_list, by = "species") 
ele = all2 %>% group_by(elevation, species2, n_sp) %>% count() %>% arrange(elevation, n) %>% na.omit()

# g = ggplot(ele %>% filter(species2 == "アカマツ"), aes(x = n))
# h = geom_histogram()
# g+h

mode = NULL
for(i in 1:nrow(sp_list)){
  d1 = ele %>% filter(n_sp == i)
  max = max(d1$n)
  d1 = d1 %>% mutate(nrow = nrow(d1)) %>% filter(n == max)
  mode = rbind(mode, d1)
}

summary(mode)
mode2 = mode %>% filter(nrow >= 100) %>% arrange(elevation) #nrow >= 100:  時空間的にな地点数．あまり小さいと解析できないと思ったから
summary(mode2)



# 高標高種の個体数の変化をみてみる --------------------------------------------------------
sp_list2 = mode2 %>% filter(elevation >= 1000)

high = NULL
for(i in sp_list2$species2){
  d1 = all2 %>% filter(species2 == i)
  high = rbind(high, d1)
}
length(unique(high$species2))
length(unique(sp_list2$species2))


h_trend = high %>% group_by(time, species2, type) %>% count()

g = ggplot(h_trend, aes(x = time, y = n, color = species2, group = species2))
l = geom_line()
f = facet_wrap(~ type, ncol = 3, scales = "free")
g+l+f+theme_bw()



# 低標高種の個体数の変化をみてみる --------------------------------------------------------
sp_list3 = mode2 %>% filter(between(elevation, 0, 500))

low = NULL
for(i in sp_list3$species2){
  d1 = all2 %>% filter(species2 == i)
  low = rbind(low, d1)
}
length(unique(low$species2))
length(unique(sp_list3$species2))


l_trend = low %>% group_by(time, species2, type) %>% count()

g = ggplot(l_trend, aes(x = time, y = n, color = species2, group = species2))
l = geom_line()
f = facet_wrap(~ type, ncol = 3, scales = "free")
g+l+f+theme_bw() + theme(legend.position = "none")

