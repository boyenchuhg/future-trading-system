# ==============================================================================
# 🚀 全自動台股 1 分 K 歷史資料庫更新腳本 (修正下標超出邊界 + 防閃退)
# ==============================================================================

library(httr)
library(jsonlite)
library(data.table)
library(lubridate)

# ---- 1. 參數與路徑設定 ----
API_KEY  <- "NDViMWJlZjItMWZkZi00YTQ4LWFlZWYtZTVlZjQ1MTI3MjQyIGRkZjliMzQwLTFlYTQtNDU1Mi1iMGRmLWU2YTk0MTEwNGRlOA=="
base_dir <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/整體基礎資料/CSV"

input_csv   <- file.path(base_dir, "股票代號對照表.csv")
history_rds <- file.path(base_dir, "All_Stocks_1min.rds")

# ---- 2. 讀取對照表過濾下市股票 ----
cat("📂 [1/5] 正在讀取股票對照表...\n")

df_symbols <- tryCatch(
  read.csv(input_csv, fileEncoding = "CP950", stringsAsFactors = FALSE),
  error = function(e) read.csv(input_csv, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
)

delisted_col   <- df_symbols[[3]]
is_active      <- is.na(delisted_col) | trimws(as.character(delisted_col)) == ""
active_symbols <- trimws(as.character(df_symbols[is_active, 1]))

cat(sprintf("✅ 成功鎖定 %d 檔正常上市櫃股票！\n\n", length(active_symbols)))

# ---- 3. 讀取既有 RDS 資料庫 ----
latest_dates <- list()
df_history   <- NULL

if (file.exists(history_rds)) {
  cat("📂 [2/5] 讀取歷史 RDS 資料庫中...\n")
  df_history <- readRDS(history_rds)
  setDT(df_history)
  
  latest_dt <- df_history[, .(last_date = as.character(as.Date(max(date)))), by = symbol]
  
  # 🔥 關鍵修復：轉為 List 結構，找不到的代號會安全回傳 NULL，不會再報下標超出邊界！
  latest_dates <- setNames(as.list(latest_dt$last_date), latest_dt$symbol)
  
  cat(sprintf("   └─ 成功解析 %s 筆歷史紀錄！\n\n", format(nrow(df_history), big.mark = ",")))
}

# ---- 4. 計算需要補抓的股票與日期 ----
today           <- Sys.Date()
thirty_days_ago <- today - days(30)
today_str       <- as.character(today)
tasks           <- list()

for (sym in active_symbols) {
  # 安全讀取，找不到即為 NULL
  last_date_str <- latest_dates[[sym]]
  
  if (!is.null(last_date_str)) {
    last_date <- as.Date(last_date_str)
    if (as.numeric(today - last_date) <= 1) next 
    
    fetch_start <- last_date + days(1)
    if (fetch_start < thirty_days_ago) fetch_start <- thirty_days_ago
  } else {
    fetch_start <- thirty_days_ago
  }
  
  tasks[[length(tasks) + 1]] <- list(symbol = sym, from = as.character(fetch_start), to = today_str)
}

# ---- 5. 執行 API 抓取 ----
if (length(tasks) == 0) {
  cat("🎉 [3/5] 所有未下市股票的資料皆已是最新的，無需抓取！\n")
} else {
  cat(sprintf("🚀 [3/5] 發現 %d 檔股票需要更新，開始發送 API 請求...\n\n", length(tasks)))
  new_list <- list()

  for (i in seq_along(tasks)) {
    tsk    <- tasks[[i]]
    sym    <- tsk$symbol
    f_from <- tsk$from
    f_to   <- tsk$to
    
    cat(sprintf("[%d/%d] 抓取 %s (%s~%s) ... ", i, length(tasks), sym, f_from, f_to))
    url <- sprintf("https://api.fugle.tw/marketdata/v1.0/stock/historical/candles/%s", sym)
    
    for (attempt in 1:3) {
      res <- tryCatch(
        GET(url, query = list(timeframe = "1", from = f_from, to = f_to), add_headers(`X-API-KEY` = API_KEY)),
        error = function(e) NULL
      )
      
      if (!is.null(res)) {
        status <- status_code(res)
        
        if (status == 200) {
          content_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
          if (!is.null(content_json$data) && length(content_json$data) > 0) {
            dt_temp <- as.data.table(content_json$data)
            dt_temp[, symbol := sym]
            
            # 只對當下剛抓取的新資料轉台灣時間
            dt_temp[, date := ymd_hms(as.character(date), tz = "UTC")]
            dt_temp[, date := with_tz(date, tzone = "Asia/Taipei")]
            
            setcolorder(dt_temp, c("symbol", setdiff(names(dt_temp), "symbol")))
            new_list[[length(new_list) + 1]] <- dt_temp
            cat(sprintf("✅ 新增 %d 筆\n", nrow(dt_temp)))
          } else {
            cat("⚠️ 無新資料\n")
          }
          break
        } else if (status == 404) {
          cat("⚠️ 無新資料 (休市/非交易日)\n")
          break
        } else if (status == 429) {
          cat("⏳ 觸發限速 (429)，自動暫停 60 秒...\n")
          Sys.sleep(60)
        } else {
          cat(sprintf("❌ HTTP 錯誤 (%d)\n", status))
          break
        }
      }
    }
    Sys.sleep(1.1)
  }

  # ---- 6. 安全合併與覆寫存檔 ----
  if (length(new_list) > 0) {
    cat("\n🔄 [4/5] 進行新舊資料安全合併...\n")
    df_new <- rbindlist(new_list, use.names = TRUE, fill = TRUE)
    
    df_final <- if (!is.null(df_history)) rbindlist(list(df_history, df_new), use.names = TRUE, fill = TRUE) else df_new
    df_final <- unique(df_final, by = c("symbol", "date"))
    setkey(df_final, symbol, date)
    
    rds_backup <- file.path(base_dir, "All_Stocks_1min_BACKUP.rds")
    if (file.exists(history_rds)) file.copy(history_rds, rds_backup, overwrite = TRUE)
	
	cat("💾 [5/5] 寫入 All_Stocks_1min.rds...\n")
    saveRDS(df_final, history_rds)
    
    cat("\n===================================================\n")
    cat("🎉 更新完成！歷史資料庫總筆數：", format(nrow(df_final), big.mark = ","), " 筆\n")
    cat("===================================================\n")
  } else {
    cat("\n💡 本次執行無新增資料，資料庫維持最新狀態。\n")
  }
}

# ---- 7. 防閃退鎖 ----
cat("\n===================================================\n")
cat("📌 程式執行完畢。請按任意鍵（或 Enter）關閉視窗...\n")
cat("===================================================\n")
if (.Platform$OS.type == "windows") {
  system("pause")
} else {
  invisible(readline())
}