# =============================
# STI estimation from AccSpeciesName (list_na.csv) + GBIF + WorldClim v2.1
# =============================

# --- パッケージ & ネットワーク設定 ---
options(stringsAsFactors = FALSE)
options(timeout = 600)
options(download.file.method = "libcurl")

need <- c("rgbif")
new <- need[!need %in% installed.packages()[,"Package"]]
if(length(new)) install.packages(new, dependencies = TRUE)
library(rgbif)

# --- Aucuba japonica を10件だけ取れるか確認 ---
res_key <- rgbif::occ_search(taxonKey = 3033077, hasCoordinate = TRUE, limit = 10)
cat("Smoke nrow:", nrow(res_key$data), "\n")
if (nrow(res_key$data) == 0) {
  stop("GBIFから出現点が取得できません。ネットワーク(プロキシ/SSL)設定を確認してください。")
}
head(res_key$data[,c("species","decimalLongitude","decimalLatitude","year")])


need <- c("geodata","terra")
new <- need[!need %in% installed.packages()[,"Package"]]
if(length(new)) install.packages(new, dependencies = TRUE)
library(geodata); library(terra)

WC_RES <- "10m"   # 必要なら "2.5m" 等へ
res_num <- switch(WC_RES, "10m"=10, "5m"=5, "2.5m"=2.5, "30s"=0.5,
                  stop("WC_RES must be one of '10m','5m','2.5m','30s'"))

DIR_WC <- file.path(getwd(), "worldclim_bio"); if (!dir.exists(DIR_WC)) dir.create(DIR_WC, recursive = TRUE)

wc_stack <- geodata::worldclim_global(var = "bio", res = res_num, path = DIR_WC)
bio1    <- wc_stack[[1]]   # BIO1 (0.1℃刻み)
bio1_C  <- bio1 / 10       # ℃へ換算
cat("WorldClim BIO1 loaded. Resolution:", res(bio1_C)[1], "deg\n")



# ===== 設定 =====
SEED <- 42; set.seed(SEED)
GBIF_MAX_PER_SPECIES <- 800    # まずは少なめで接続確認
GBIF_PAGE_SIZE       <- 200
SLEEP_SEC_PER_PAGE   <- 0.2
BOOT_B               <- 300    # SEのブート回数（多すぎると重い）
DIR_CACHE <- file.path(getwd(), "cache_gbif"); if (!dir.exists(DIR_CACHE)) dir.create(DIR_CACHE, recursive = TRUE)



# ===== 必要パッケージ =====
need <- c("rgbif","dplyr","tidyr","stringr","readr","sf","progress")
new <- need[!need %in% installed.packages()[,"Package"]]
if(length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(need, library, character.only = TRUE))



# ===== 1) 種リスト読込・正規化（AccSpeciesName） =====
setwd("/Users/Yuki/Library/CloudStorage/Dropbox/")
df <- read.csv("list_na.csv", fileEncoding = "CP932", check.names = FALSE)
stopifnot("AccSpeciesName" %in% names(df))
norm_name <- function(x){
  x <- gsub("\u3000", " ", x)        # 全角スペース→半角
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- gsub("( cf\\.| aff\\.| sp\\.| spp\\.| var\\.| subsp\\.).*$", "", x, ignore.case = TRUE)
  x <- sub("^([A-Za-z\\.\\-]+\\s+[A-Za-z\\.\\-]+).*", "\\1", x)  # 先頭2語
  trimws(x)
}
species_vec <- unique(norm_name(df$AccSpeciesName))
species_vec <- species_vec[nchar(species_vec)>0]
cat("Unique species (normalized):", length(species_vec), "\n")



# ===== 2) GBIF name_backbone で taxonKey 取得 =====
`%||%` <- function(a,b) if(is.null(a)) b else a
normalize_name <- function(x){
  res <- try(rgbif::name_backbone(name = x, rank = "species"), silent = TRUE)
  if (inherits(res, "try-error") || length(res)==0)
    return(tibble::tibble(input_name=x, usageKey=NA_integer_, acceptedUsageKey=NA_integer_,
                          canonicalName=NA_character_, status=NA_character_))
  tibble::tibble(
    input_name = x,
    usageKey = res$usageKey %||% NA_integer_,
    acceptedUsageKey = res$acceptedUsageKey %||% NA_integer_,
    canonicalName = res$species %||% res$canonicalName %||% NA_character_,
    status = res$status %||% NA_character_
  )
}
name_map <- purrr::map_dfr(species_vec, normalize_name) |>
  dplyr::mutate(key_final = dplyr::if_else(!is.na(acceptedUsageKey), acceptedUsageKey, usageKey),
                name_final = canonicalName)
nohit <- name_map |> dplyr::filter(is.na(key_final))
if(nrow(nohit)>0) warning("GBIF一致なし（除外）: ", paste(nohit$input_name, collapse=" | "))
name_map_ok <- name_map |> dplyr::filter(!is.na(key_final))
cat("taxonKey resolved:", nrow(name_map_ok), "/", nrow(name_map), "\n")



# ===== 3) 出現点 取得（緩め設定：まず通す） =====
cache_file_key <- function(key) file.path(DIR_CACHE, paste0("key_", key, "_raw.rds"))

# 取得（ページング＋キャッシュ）。挙動はシンプル版に近いまま。
fetch_gbif_occ_by_key <- function(taxonKey,
                                  max_n = GBIF_MAX_PER_SPECIES,
                                  page_size = GBIF_PAGE_SIZE,
                                  sleep_sec = SLEEP_SEC_PER_PAGE) {
  cf <- file.path(DIR_CACHE, paste0("key_", taxonKey, "_raw.rds"))
  if (file.exists(cf)) return(readRDS(cf))
  
  out <- list(); n_downloaded <- 0; offset <- 0
  repeat {
    lim <- min(page_size, max_n - n_downloaded); if (lim <= 0) break
    res <- try(rgbif::occ_search(
      taxonKey = taxonKey,
      hasCoordinate = TRUE,  # まずは座標付きのみ
      limit = lim, offset = offset
    ), silent = TRUE)
    if (inherits(res, "try-error") || is.null(res$data) || nrow(res$data) == 0) break
    df <- res$data[, c("species","decimalLongitude","decimalLatitude","year")]
    names(df) <- c("species","lon","lat","year")
    df <- df[!is.na(df$lon) & !is.na(df$lat), ]
    # (0,0) を除去
    df <- df[!(abs(df$lon) < 1e-9 & abs(df$lat) < 1e-9), ]
    out[[length(out)+1]] <- df
    n_downloaded <- n_downloaded + nrow(df)
    offset <- offset + nrow(res$data)
    Sys.sleep(sleep_sec)
    if (nrow(res$data) < lim) break
  }
  
  if (length(out) == 0) return(NULL)
  all_df <- dplyr::bind_rows(out)
  saveRDS(all_df, cf)
  all_df
}



# ===== 5) STI 計算（ピクセル代表化 + ブートSE） =====
sti_from_points <- function(df_pts, b1_rast, boot_B=BOOT_B){
  if (is.null(df_pts) || nrow(df_pts)<5) return(NULL)
  temps <- terra::extract(b1_rast, cbind(df_pts$lon, df_pts$lat))[,1]
  df_pts$tempC <- temps
  df_pts <- df_pts[!is.na(df_pts$tempC),]
  if (nrow(df_pts)<5) return(NULL)
  cells <- terra::cellFromXY(b1_rast, cbind(df_pts$lon, df_pts$lat))
  dfpix <- dplyr::tibble(cell=cells, tempC=df_pts$tempC) |>
    dplyr::group_by(cell) |>
    dplyr::summarise(tempC=mean(tempC, na.rm=TRUE), .groups="drop") |>
    dplyr::filter(!is.na(tempC))
  if (nrow(dfpix)<5) return(NULL)
  sti_mean   <- mean(dfpix$tempC)
  sti_median <- median(dfpix$tempC)
  sti_p10    <- as.numeric(quantile(dfpix$tempC, 0.10))
  sti_p90    <- as.numeric(quantile(dfpix$tempC, 0.90))
  set.seed(SEED)
  bs <- replicate(boot_B, { samp <- sample(dfpix$tempC, size=nrow(dfpix), replace=TRUE); mean(samp) })
  list(n_used=nrow(dfpix), STI_mean=sti_mean, STI_median=sti_median,
       STI_p10=sti_p10, STI_p90=sti_p90, STI_SE=stats::sd(bs))
}

# ===== 6) ループ実行（まずは10種だけで動作確認推奨） =====
# subset_keys <- head(name_map_ok, 2)
# results <- vector("list", nrow(subset_keys))
# for (ii in seq_len(nrow(subset_keys))) {
#   key  <- subset_keys$key_final[ii]
#   in_nm <- subset_keys$input_name[ii]
#   ok_nm <- subset_keys$name_final[ii]
#   
#   df_raw <- fetch_gbif_occ_by_key(key, limit = 1000)
#   n_raw <- if (is.null(df_raw)) 0L else nrow(df_raw)
#   
#   if (n_raw < 5) {
#     results[[ii]] <- dplyr::tibble(input_name=in_nm, gbif_name=ok_nm, taxonKey=key,
#                                    n_raw=n_raw, n_used=0,
#                                    STI_mean=NA_real_, STI_median=NA_real_,
#                                    STI_p10=NA_real_, STI_p90=NA_real_, STI_SE=NA_real_)
#     next
#   }
#   
#   # 抽出→代表化→STI（③の関数を再利用）
#   pts <- df_raw
#   temps <- terra::extract(bio1_C, cbind(pts$lon, pts$lat))[,1]
#   pts$tempC <- temps
#   pts <- pts[!is.na(pts$tempC),]
#   if (nrow(pts) < 5) {
#     results[[ii]] <- dplyr::tibble(input_name=in_nm, gbif_name=ok_nm, taxonKey=key,
#                                    n_raw=n_raw, n_used=0,
#                                    STI_mean=NA_real_, STI_median=NA_real_,
#                                    STI_p10=NA_real_, STI_p90=NA_real_, STI_SE=NA_real_)
#     next
#   }
#   cells <- terra::cellFromXY(bio1_C, cbind(pts$lon, pts$lat))
#   dfpix <- dplyr::tibble(cell=cells, tempC=pts$tempC) |>
#     dplyr::group_by(cell) |>
#     dplyr::summarise(tempC=mean(tempC), .groups="drop") |>
#     dplyr::filter(!is.na(tempC))
#   
#   if (nrow(dfpix) < 5) {
#     results[[ii]] <- dplyr::tibble(input_name=in_nm, gbif_name=ok_nm, taxonKey=key,
#                                    n_raw=n_raw, n_used=0,
#                                    STI_mean=NA_real_, STI_median=NA_real_,
#                                    STI_p10=NA_real_, STI_p90=NA_real_, STI_SE=NA_real_)
#     next
#   }
#   
#   set.seed(42); B <- 300
#   STI_mean   <- mean(dfpix$tempC)
#   STI_median <- median(dfpix$tempC)
#   STI_p10    <- as.numeric(quantile(dfpix$tempC, 0.10))
#   STI_p90    <- as.numeric(quantile(dfpix$tempC, 0.90))
#   bs <- replicate(B, mean(sample(dfpix$tempC, size=nrow(dfpix), replace=TRUE)))
#   STI_SE <- sd(bs)
#   
#   results[[ii]] <- dplyr::tibble(
#     input_name=in_nm, gbif_name=ok_nm, taxonKey=key,
#     n_raw=n_raw, n_used=nrow(dfpix),
#     STI_mean=STI_mean, STI_median=STI_median,
#     STI_p10=STI_p10, STI_p90=STI_p90, STI_SE=STI_SE
#   )
# }
# 
# stis <- dplyr::bind_rows(results)
# print(stis)
# cat("\nSummary: n_used>=5 =", sum(stis$n_used>=5, na.rm=TRUE), " / ", nrow(stis), "\n")
# 
# # 保存（動作確認が取れたら全種に拡大）
# readr::write_csv(stis, paste0("STI_worldclim_", WC_RES, "_subset10.csv"))

subset_keys <- name_map_ok
out_path <- paste0("STI_worldclim_", WC_RES, "_all_progress.csv")
if (file.exists(out_path)) file.remove(out_path)

results <- vector("list", nrow(subset_keys))
for (ii in seq_len(nrow(subset_keys))) {
  key  <- subset_keys$key_final[ii]
  in_nm <- subset_keys$input_name[ii]
  ok_nm <- subset_keys$name_final[ii]
  
  # 取得
  df_raw <- fetch_gbif_occ_by_key(key, max_n = GBIF_MAX_PER_SPECIES)
  n_raw <- if (is.null(df_raw)) 0L else nrow(df_raw)
  
  # 抽出→代表化→STI
  if (!is.null(df_raw) && n_raw >= 5) {
    temps <- terra::extract(bio1_C, cbind(df_raw$lon, df_raw$lat))[,1]
    df_raw$tempC <- temps
    df_raw <- df_raw[!is.na(df_raw$tempC),]
    if (nrow(df_raw) >= 5) {
      cells <- terra::cellFromXY(bio1_C, cbind(df_raw$lon, df_raw$lat))
      dfpix <- dplyr::tibble(cell = cells, tempC = df_raw$tempC) |>
        dplyr::group_by(cell) |>
        dplyr::summarise(tempC = mean(tempC), .groups="drop") |>
        dplyr::filter(!is.na(tempC))
      if (nrow(dfpix) >= 5) {
        set.seed(42); B <- BOOT_B
        STI_mean   <- mean(dfpix$tempC)
        STI_median <- median(dfpix$tempC)
        STI_p10    <- as.numeric(quantile(dfpix$tempC, 0.10))
        STI_p90    <- as.numeric(quantile(dfpix$tempC, 0.90))
        bs <- replicate(B, mean(sample(dfpix$tempC, size=nrow(dfpix), replace=TRUE)))
        STI_SE <- sd(bs)
        
        row <- dplyr::tibble(
          input_name = in_nm, gbif_name = ok_nm, taxonKey = key,
          n_raw = n_raw, n_used = nrow(dfpix),
          STI_mean = STI_mean, STI_median = STI_median,
          STI_p10 = STI_p10, STI_p90 = STI_p90, STI_SE = STI_SE
        )
        results[[ii]] <- row
        readr::write_csv(row, out_path, append = TRUE)
        next
      }
    }
  }
  
  # ここに来たら NA を出力（追記保存）
  row <- dplyr::tibble(
    input_name = in_nm, gbif_name = ok_nm, taxonKey = key,
    n_raw = n_raw, n_used = 0,
    STI_mean = NA_real_, STI_median = NA_real_,
    STI_p10 = NA_real_, STI_p90 = NA_real_, STI_SE = NA_real_
  )
  results[[ii]] <- row
  readr::write_csv(row, out_path, append = TRUE)
}

stis <- dplyr::bind_rows(results)
readr::write_csv(stis, paste0("STI_worldclim_", WC_RES, "_all_final.csv"))

