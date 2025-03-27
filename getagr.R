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
  # setwd(dir = "/Users/Yuki/Dropbox/LST")
  # dir.create(paste(i))
  outdir = paste0("/Users/Yuki/Dropbox/LST/")
  download_netcdf(amgsds_path = paths$complete, server = "amd.rd.naro.go.jp/opendap", outdir = outdir, .silent = FALSE)
}

area_daily_lst =
  fetch_amgsds(
    times = ymd(c("1980-01-01", "1980-01-01"), tz = "Japan"),
    lats = c(24, 46), lons = c(122, 146),
    elements = "TMP_mea",
    mode = "area",
    output = "tibble"
  )

lonlat = area_daily_lst %>% mutate(site_id = 1:nrow(area_daily_lst))

for(i in 1:nrow(lonlat)){
  lon = lonlat[i, "lon"]
  lat = lonlat[i, "lat"]

  temp = fetch_amgsds(
    times = ymd(c("1980-01-01", "2024-12-31"), tz = "Japan"),
    lats = lat$lat, lons = lon$lon,
    elements = "TMP_mea",
    output = "tibble"
  ) %>% mutate(site_id = i)
  
  setwd("/Users/Yuki/Dropbox/LST/1980-2024")
  file_name = paste0("TMP_mea_site", i, ".csv")
  write.csv(temp, file_name, row.names = FALSE)
}



setwd("/Users/Yuki/Dropbox/LST/mean")
