require(tidyverse)


# (1) NFI種リストを読み込み
setwd("/Users/Yuki/Library/CloudStorage/Dropbox/")
spfile <- "splist.xlsx"   # ぬしさんのExcel
df <- readxl::read_excel(spfile, sheet = 1)

# (2) 学名（short表記）を取得
col_names = c("AccSpeciesID",	"AccSpeciesName",	"ObsNum",	"ObsGRNum",	"MeasNum",	"MeasGRNum",	"TraitNum")
list <- read.table("TryAccSpecies.txt",
                 header = TRUE,        # ファイル先頭行にヘッダーがない場合
                 sep = "\t",            # 区切り文字
                 quote = "",            # 引用符なし
                 col.names = col_names, # 列名を指定
                 fill = TRUE,           # 列数が揃わない行を埋める
                 stringsAsFactors = FALSE,
                 comment.char = "",     # コメント文字を無効化
                 na.strings = c("", "NA"))

colnames(df)
df = df %>% mutate(AccSpeciesName = scientificNameShort) %>% filter(AccSpeciesName != 9999)

df2 = left_join(df, list, by = "AccSpeciesName")
colSums(is.na(df2))
313/nrow(df2)

id = df2 %>% select(AccSpeciesID) %>% na.omit()
id_1 = id[1:1000, ]
id_2 = id[1001:1045, ]
paste(id_1$AccSpeciesID, collapse = ",")
paste(id_2$AccSpeciesID, collapse = ",")

write.csv(df2, "list_na.csv", fileEncoding = "CP932", row.names = FALSE)




# IDがNAだったものを再度やる -> TRYにデータなし -------------------------------------------------------------------
# 必要パッケージ
na = na_list %>% filter(is.na(AccSpeciesID))
colSums(is.na(na))
na = na %>% select("no", "japaneseName", "japaneseNameMatching", "scientificNameLong", "scientificNameShort", "normalized_candidate") %>% rename(AccSpeciesName = normalized_candidate)
na2 = left_join(na, list, by = "AccSpeciesName")
colSums(is.na(na2))
