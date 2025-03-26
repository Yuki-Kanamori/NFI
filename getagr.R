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
area_daily_lst =
  fetch_amgsds(
    times = ymd(c("1980-01-01", "1989-12-31"), tz = "Japan"),
    lats = c(24, 46), lons = c(122, 146),
    elements = "TMP_mea",
    mode = "area",
    output = "tibble"
  )

# area_daily_rain <-
#   fetch_amgsds(
#     times = ymd(c("2023-01-10", "2023-01-11"), tz = "Japan"),
#     lats = c(41.5, 43.8), lons = c(140.2, 142.0),
#     elements = "APCP",
#     mode = "area",
#     output = "array"
#   )
