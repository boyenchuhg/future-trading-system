library(data.table)
library(xts)

# --- 1. 設定路徑 (只需要設定一次，之後不用再改) ---
rds_dir    <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/評價函數/基礎資料/非線性規劃結果"
output_dir <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/評價函數/權重結果"
csv_source <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/評價函數/基礎資料/CSV/評價函數基礎資料(台股25策略).csv"

# --- 2. 讀取日曆 ---
P_data <- fread(csv_source)
all_dates <- as.Date(P_data[[1]])

# --- 3. 自動掃描資料夾內所有 RDS 檔（不管是checkpoint格式還是最終res格式都吃） ---
rds_files <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)

if (length(rds_files) == 0) {
  stop("❌ 在 rds_dir 裡找不到任何 .rds 檔案，請確認路徑。")
}

cat(sprintf("📋 找到 %d 個 RDS 檔案，開始批次轉換...\n\n", length(rds_files)))

for (rds_path in rds_files) {
  
  file_name <- basename(rds_path)
  prefix <- sub("\\.rds$", "", file_name)
  prefix <- sub("^deoptim_state_", "", prefix)
  
  cat("=======================================================\n")
  cat("📂 正在處理：", file_name, "\n")
  
  weight_out <- file.path(output_dir, paste0("portfolio_weights_", prefix, ".csv"))
  pnl_out    <- file.path(output_dir, paste0("portfolio_pnl_", prefix, ".csv"))
  
  if (file.exists(weight_out) && file.exists(pnl_out)) {
    cat("   ⏭️  已存在對應CSV，略過。（如果RDS有更新，先手動刪掉舊CSV再重跑即可）\n\n")
    next
  }
  
  state <- tryCatch(readRDS(rds_path), error = function(e) {
    cat("   ❌ 讀取失敗：", e$message, "\n")
    NULL
  })
  if (is.null(state)) next
  
  # --- 判斷這個RDS是「checkpoint格式」(weight_log) 還是「最終res格式」(weights_dt) ---
  w_dt <- NULL
  
  if (!is.null(state$weight_log) && length(state$weight_log) > 0) {
    # checkpoint 格式：weight_log 是原始 list，要自己 rbindlist
    cat("   📊 偵測到 checkpoint 格式 (weight_log)，處理權重表中...\n")
    w_dt <- rbindlist(lapply(state$weight_log, as.data.table), fill = TRUE)
    if (is.numeric(w_dt$date)) w_dt[, date := as.Date(date, origin = "1970-01-01")]
    w_dt[, DATE := format(as.Date(date), "%Y/%m/%d")]
    w_dt[, date := NULL]
    
  } else if (!is.null(state$weights_dt) && nrow(state$weights_dt) > 0) {
    # 最終res格式：weights_dt 已經是處理好的 data.table，但 DATE 欄位可能是數字(天數)而非日期字串
    cat("   📊 偵測到最終結果格式 (weights_dt)，處理權重表中...\n")
    w_dt <- copy(state$weights_dt)
    if (is.numeric(w_dt$DATE)) {
      w_dt[, DATE := as.Date(DATE, origin = "1970-01-01")]
    }
    w_dt[, DATE := format(as.Date(DATE), "%Y/%m/%d")]
    
  } else {
    cat("   ⚠️  這個RDS裡找不到 weight_log 或 weights_dt，略過權重輸出\n")
  }
  
  if (!is.null(w_dt)) {
    setorder(w_dt, DATE)
    cols <- c("DATE", setdiff(names(w_dt), "DATE"))
    setcolorder(w_dt, cols)
    fwrite(w_dt, weight_out)
    cat("   ✅ 權重 CSV 已產出：", basename(weight_out), "（", nrow(w_dt), "筆）\n")
  }
  
  # --- 產出損益 CSV (統一攤平成純數值向量，避免xts/matrix造成欄名跑掉變成PNL.V1) ---
  if (!is.null(state$port_pnl)) {
    cat("   📈 處理損益表中...\n")
    pnl_vec <- as.numeric(coredata(state$port_pnl))
    
    length(pnl_vec) <- length(all_dates)
    
    pnl_dt <- data.table(DATE = all_dates, PNL = pnl_vec)
    pnl_dt <- na.omit(pnl_dt)
    setorder(pnl_dt, DATE)
    pnl_dt[, DATE := format(as.Date(DATE), "%Y/%m/%d")]
    
    fwrite(pnl_dt, pnl_out)
    cat("   ✅ 損益 CSV 已產出：", basename(pnl_out), "（", nrow(pnl_dt), "筆）\n")
  } else {
    cat("   ⚠️  這個RDS沒有port_pnl，略過損益輸出\n")
  }
  
  cat("\n")
}

cat("🎯 全部搞定！請去「權重結果」資料夾收貨。\n")
