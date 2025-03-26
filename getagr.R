require(tidyverse)
require(devtools)

install.packages('https://cran.r-project.org/src/contrib/Archive/tidync/tidync_0.3.0.tar.gz', repos=NULL, type='source') # NetCDFデータの利用のため
devtools::install_github("uribo/jpndistrict") # 国内の行政区画データの利用のため

devtools::install_github("KeachMurakami/agrmesh", upgrade = FALSE)
library(agrmesh)


# -------------------------------------------------------------------------
library(tidyverse)
library(jpndistrict)
library(agrmesh)


# data --------------------------------------------------------------------
# 取得可能なデータの種類
preview_dataset("daily")

# 取得可能な地理情報データ
# https://amu.rd.naro.go.jp/wiki_open/doku.php?id=geodata
print(preview_dataset("geo"), n = 100)
# point_altitude <-
#   fetch_amgsds(
#     lats = sites$lat, lons = sites$lon,    
#     elements = "altitude",                  
#     source = "geo"
#   )



# データのダウンロード --------------------------------------------------------------
for(i in 1980:2024){
  start = paste0(i, "-01-01")
  end = paste0(i, "-12-31")
  paths <-
    generate_path(
      times = ymd(c(start, end), tz = "Japan"),
      lats = c(24, 46), 
      lons = c(122, 146),
      element = "TMP_mea",
      source = "daily",
      is_clim = FALSE
    )
  setwd(dir = "/Users/Yuki/Dropbox/LST")
  dir.create(paste(i))
  outdir = paste0("/Users/Yuki/Dropbox/LST/", i)
  download_netcdf(amgsds_path = paths$complete, server = "amd.rd.naro.go.jp/opendap", outdir = outdir, .silent = FALSE)
}




area_daily_lst =
  fetch_amgsds(
    times = ymd(c("1980-01-01", "1980-01-02"), tz = "Japan"),
    lats = c(24, 46), lons = c(122, 146),
    elements = "TMP_mea",
    mode = "area",
    output = "array"
  )

# area_daily_rain <-
#   fetch_amgsds(
#     times = ymd(c("2023-01-10", "2023-01-11"), tz = "Japan"),
#     lats = c(41.5, 43.8), lons = c(140.2, 142.0),
#     elements = "APCP",
#     mode = "area",
#     output = "array"
#   )
