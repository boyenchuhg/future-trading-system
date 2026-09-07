# ===================================================================
# 🏆 滾動 2 年 Calmar 動態前 8 強等權重 (含卡瑪<0剔除機制)
# ===================================================================

rm(list = ls())
gc()

# ---- 1. 載入套件 ----
pkgs <- c("data.table", "xts", "lubridate")
for (p in pkgs) if (!require(p, character.only = TRUE)) install.packages(p)
library(data.table)
library(xts)
library(lubridate)

# ===================================================================
# 🎛️ 中央控制室 (路徑設定)
# ===================================================================
# 請確認您的輸入路徑是否正確
base_dir <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/評價函數/基礎資料"
input_dir  <- file.path(base_dir, "CSV")
output_dir <- file.path(base_dir, "../權重結果") 

# 若輸出資料夾不存在則自動建立
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 目標商品
target_markets <- c("台股25策略")
all_files <- list.files(path = input_dir, pattern = "評價函數基礎資料\\(.*\\)\\.csv", full.names = TRUE)

# 過濾取得目標檔案
target_files <- all_files[grepl("台股25策略", all_files)]

if (length(target_files) == 0) stop("❌ 找不到輸入檔案，請確認 CSV 是否放置在基礎資料/CSV 內！")

# ---- 工具函數 ----
mdd_points <- function(pnl) {
  if (length(pnl) == 0) return(0)
  equity <- cumsum(pnl)
  dd <- equity - cummax(equity)
  res <- abs(min(dd))
  if (res == 0) return(1e-6)
  return(res)
}

ann_pnl <- function(pnl, scale = 252) mean(pnl) * scale

# ===================================================================
# 🚀 執行主迴圈
# ===================================================================
for (file_path in target_files) {
  
  var_name <- sub(".*評價函數基礎資料\\((.*?)\\)\\.csv", "\\1", basename(file_path))
  cat("🚀 正在處理：", var_name, "\n")
  
  # 讀取每日損益資料
  dt <- fread(file_path)
  dt[, Date := as.Date(Date, format = "%Y/%m/%d")]
  setorder(dt, Date)
  
  strats <- setdiff(names(dt), "Date")
  P_xts <- xts(dt[, ..strats], order.by = dt$Date)
  dates <- index(P_xts)
  
  # 找出每月最後一個交易日做為換倉結算日
  ep <- endpoints(P_xts, on = "months")
  ep <- ep[ep > 0]
  
  rolling_years <- 2
  
  weight_log <- list()
  pnl_log <- list()
  
  cat("🔄 正在進行動態滾動運算...\n")
  
  # 迴圈：從有滿 2 年歷史資料的月份開始
  for (i in 1:(length(ep) - 1)) {
    curr_ep <- ep[i]
    next_ep <- ep[i+1]
    
    t_end_date <- dates[curr_ep]
    first_ok_date <- dates[1] %m+% years(rolling_years)
    
    if (t_end_date < first_ok_date) next
    
    # 🎯 框出過去兩年視窗
    window_start <- t_end_date %m-% years(rolling_years) + days(1)
    P_win <- P_xts[paste0(window_start, "/", t_end_date), ]
    
    # 1. 計算這兩年內單一策略的卡瑪比率 (總獲利 / 最大回撤)
    calmars <- sapply(strats, function(s) {
      pnl <- as.numeric(P_win[, s])
      tot <- sum(pnl)
      dd <- mdd_points(pnl)
      if (dd <= 0) return(-9999) # 避開完全沒交易或虧到死的策略
      return(tot / dd)
    })
    
    # 📢【新增規則 1】過濾出卡瑪 >= 0 的策略
    valid_calmars <- calmars[calmars >= 0]
    
    # 📢【新增規則 2】挑選前 8 強 (若符合條件不足 8 檔，則取實際數量)
    sorted_strats <- names(sort(valid_calmars, decreasing = TRUE))
    num_selected <- min(8, length(sorted_strats))
    
    # 3. 建立等權重向量並動態分配
    w <- rep(0, length(strats))
    names(w) <- strats
    
    if (num_selected > 0) {
      selected <- sorted_strats[1:num_selected]
      w[selected] <- 1.0 / num_selected  # 動態平均分配權重 (例如: 1/5, 1/8)
    }
    # 若 num_selected == 0 (全部策略都在虧損)，w 維持全 0，等同空手觀望
    
    # 記錄這個投資組合在過去兩年視窗的追蹤表現 (CSV 日誌用)
    port_win_pnl <- as.numeric(P_win %*% w)
    win_ann_ret <- ann_pnl(port_win_pnl)
    win_mdd <- mdd_points(port_win_pnl)
    win_calmar <- win_ann_ret / win_mdd
    
    log_entry <- as.list(w)
    log_entry$Calmar_Ratio <- win_calmar
    log_entry$Annualized_Return <- win_ann_ret
    log_entry$Max_Drawdown <- win_mdd
    # 格式化日期以符合 template
    log_entry$DATE <- as.character(t_end_date, format = "%Y/%m/%d")
    weight_log[[length(weight_log) + 1]] <- log_entry
    
    # 4. 套用權重至下一個月的每日真實績效 (Out-of-Sample)
    next_P <- P_xts[(curr_ep + 1):next_ep, ]
    next_pnl <- as.numeric(next_P %*% w)
    
    next_dates <- as.character(index(next_P), format = "%Y/%m/%d")
    
    pnl_df <- data.table(DATE = next_dates, PNL = next_pnl)
    pnl_log[[length(pnl_log) + 1]] <- pnl_df
  }
  
  # ===================================================================
  # 📢【新增規則】強制計算「最新一期」的實戰權重 (作為下個月下單依據)
  # ===================================================================
  cat("🔮 正在結算最新一期的下月實戰權重...\n")
  
  latest_date <- max(dates)
  latest_first_ok <- dates[1] %m+% years(rolling_years)
  
  if (latest_date >= latest_first_ok) {
    # 🎯 框出最新日期的過去兩年視窗
    latest_win_start <- latest_date %m-% years(rolling_years) + days(1)
    latest_P_win <- P_xts[paste0(latest_win_start, "/", latest_date), ]
    
    # 1. 計算卡瑪
    latest_calmars <- sapply(strats, function(s) {
      pnl <- as.numeric(latest_P_win[, s])
      tot <- sum(pnl)
      dd <- mdd_points(pnl)
      if (dd <= 0) return(-9999) 
      return(tot / dd)
    })
    
    # 2. 過濾與挑選前 8 強
    latest_valid_calmars <- latest_calmars[latest_calmars >= 0]
    latest_sorted <- names(sort(latest_valid_calmars, decreasing = TRUE))
    latest_num <- min(8, length(latest_sorted))
    
    # 3. 分配權重
    w_latest <- rep(0, length(strats))
    names(w_latest) <- strats
    if (latest_num > 0) {
      w_latest[latest_sorted[1:latest_num]] <- 1.0 / latest_num
    }
    
    # 記錄指標
    latest_port_pnl <- as.numeric(latest_P_win %*% w_latest)
    latest_ann_ret <- ann_pnl(latest_port_pnl)
    latest_mdd <- mdd_points(latest_port_pnl)
    latest_calmar <- latest_ann_ret / latest_mdd
    
    latest_log <- as.list(w_latest)
    latest_log$Calmar_Ratio <- latest_calmar
    latest_log$Annualized_Return <- latest_ann_ret
    latest_log$Max_Drawdown <- latest_mdd
    latest_log$DATE <- as.character(latest_date, format = "%Y/%m/%d")
    
    weight_log[[length(weight_log) + 1]] <- latest_log
  }
  
  # ===================================================================
  # 💾 整理並輸出結果
  # ===================================================================
  
  # 整理權重表
  final_weights <- rbindlist(weight_log, use.names = TRUE, fill = TRUE)
  setcolorder(final_weights, c("DATE", strats, "Calmar_Ratio", "Annualized_Return", "Max_Drawdown"))
  
  # 整理每日 PNL 表
  final_pnl <- rbindlist(pnl_log)
  
  # 建立匯出檔名
  date_str <- format(Sys.Date(), "%Y%m%d")
  w_out_file <- file.path(output_dir, paste0("portfolio_weights_ry2_DynamicTop8_EqWeight_", date_str, "(", var_name, ").csv"))
  p_out_file <- file.path(output_dir, paste0("portfolio_pnl_ry2_DynamicTop8_EqWeight_", date_str, "(", var_name, ").csv"))
  
  # 寫入 CSV
  fwrite(final_weights, w_out_file)
  fwrite(final_pnl, p_out_file)
  
  cat("✅ 處理完成！\n")
  cat("   📂 權重檔已儲存：", basename(w_out_file), "\n")
  cat("   📂 績效檔已儲存：", basename(p_out_file), "\n\n")
}