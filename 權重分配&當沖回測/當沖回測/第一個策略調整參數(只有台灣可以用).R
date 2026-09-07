library(data.table)
library(dplyr)

# ==============================================================================
# 0. 路徑與指定輸出目錄設定
# ==============================================================================
output_dir   <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/期貨日內/回測結果"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

tw_csv_path  <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/整體基礎資料/CSV/台灣期貨全資料/TAIEX_Futures_5min.csv"
jp_rds_path  <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/整體基礎資料/CSV/日本期貨/JP_Futures_5min_Clean.rds"

FEE_MICRO_TX <- 4.0; FEE_N225_MICRO <- 8.5; FEE_MINI_TOPIX <- 0.31

# 網格範圍 (突破門檻卡入 0.3% 領域下限，排除隨機雜訊)
breakout_vals   <- seq(0.003, 0.008, by = 0.001) 
vol_mult_vals   <- seq(1.0, 3.0, by = 0.2)
entry_time_vals <- c("09:00:00", "09:15:00", "09:30:00")

param_grid_s1 <- expand.grid(
  b_idx = seq_along(breakout_vals),
  v_idx = seq_along(vol_mult_vals),
  t_idx = seq_along(entry_time_vals),
  stringsAsFactors = FALSE
)
param_grid_s1$BreakoutPct <- breakout_vals[param_grid_s1$b_idx]
param_grid_s1$VolMult     <- vol_mult_vals[param_grid_s1$v_idx]
param_grid_s1$EntryTime   <- entry_time_vals[param_grid_s1$t_idx]

# ==============================================================================
# 1. S1 單一市場獨立算力與 3D 鄰域平滑模組
# ==============================================================================
run_S1_single_market <- function(dt, market_name, fee_pts, open_time = "08:45:00") {
  dt_is <- dt[Date >= "2023-01-01" & Date <= "2025-12-31"]
  if (nrow(dt_is) == 0) return(NULL)
  
  day_list <- split(dt_is, by = "Date")
  
  raw_res <- lapply(seq_len(nrow(param_grid_s1)), function(idx) {
    p <- param_grid_s1[idx, ]
    b_pct <- p$BreakoutPct; v_mult <- p$VolMult; e_time <- p$EntryTime
    trade_pnls <- c()
    
    for (day_data in day_list) {
      n_bars <- nrow(day_data)
      if (n_bars < 5) next
      
      open_day <- day_data[Time == open_time, Open][1]
      if (is.na(open_day)) open_day <- day_data$Open[1]
      long_trig <- open_day * (1 + b_pct); short_trig <- open_day * (1 - b_pct)
      
      vol_vec <- day_data$Volume
      vol_ma  <- shift(frollmean(vol_vec, n = 20, fill = NA), 1)
      vol_ma[is.na(vol_ma)] <- shift(cummean(vol_vec), 1)[is.na(vol_ma)]
      vol_ok_vec <- shift(vol_vec, 1) > v_mult * vol_ma
      
      time_vec  <- day_data$Time
      valid_idx <- which(time_vec >= e_time)
      if (length(valid_idx) == 0) next
      
      high_vec <- day_data$High; low_vec <- day_data$Low; close_vec <- day_data$Close
      
      for (i in valid_idx) {
        if (isTRUE(vol_ok_vec[i])) {
          if (high_vec[i] >= long_trig) {
            entry_p <- long_trig; sl_p <- open_day
            if (low_vec[i] <= sl_p) {
              exit_p <- sl_p
            } else if (i < n_bars) {
              rest_lows <- low_vec[(i+1):n_bars]
              sl_hits   <- which(rest_lows <= sl_p)
              exit_p    <- if (length(sl_hits) > 0) sl_p else close_vec[n_bars]
            } else {
              exit_p <- close_vec[n_bars]
            }
            trade_pnls <- c(trade_pnls, (exit_p - entry_p) - fee_pts); break
          } else if (low_vec[i] <= short_trig) {
            entry_p <- short_trig; sl_p <- open_day
            if (high_vec[i] >= sl_p) {
              exit_p <- sl_p
            } else if (i < n_bars) {
              rest_highs <- high_vec[(i+1):n_bars]
              sl_hits    <- which(rest_highs >= sl_p)
              exit_p     <- if (length(sl_hits) > 0) sl_p else close_vec[n_bars]
            } else {
              exit_p <- close_vec[n_bars]
            }
            trade_pnls <- c(trade_pnls, (entry_p - exit_p) - fee_pts); break
          }
        }
      }
    }
    
    if (length(trade_pnls) == 0) return(NULL)
    tot_trades <- length(trade_pnls)
    win_rate   <- round(sum(trade_pnls > 0) / tot_trades * 100, 1)
    net_pnl    <- round(sum(trade_pnls), 1)
    cum_pnl    <- cumsum(trade_pnls)
    mdd        <- round(abs(min(cum_pnl - cummax(cum_pnl))), 1)
    gp <- sum(trade_pnls[trade_pnls > 0]); gl <- abs(sum(trade_pnls[trade_pnls < 0]))
    pf <- ifelse(gl == 0, NA, round(gp / gl, 2))
    calmar <- ifelse(mdd == 0, NA, round((net_pnl / 3) / mdd, 2))
    
    return(data.table(
      Market = market_name, b_idx = p$b_idx, v_idx = p$v_idx, t_idx = p$t_idx,
      BreakoutPct = p$BreakoutPct, VolMult = p$VolMult, EntryTime = p$EntryTime,
      Trades = tot_trades, WinRate = win_rate, NetProfit_Pts = net_pnl, MDD_Pts = -mdd, PF = pf, Calmar_IS = calmar
    ))
  })
  
  dt_res <- rbindlist(raw_res)
  
  # 獨立對此單一市場進行 3D 鄰域平滑計算
  b_max <- max(dt_res$b_idx); v_max <- max(dt_res$v_idx); t_max <- max(dt_res$t_idx)
  
  smooth_res <- lapply(seq_len(nrow(dt_res)), function(i) {
    curr <- dt_res[i]
    neighbors <- dt_res[
      abs(b_idx - curr$b_idx) <= 1 &
      abs(v_idx - curr$v_idx) <= 1 &
      abs(t_idx - curr$t_idx) <= 1
    ]
    is_b <- (curr$b_idx == 1 | curr$b_idx == b_max) | 
            (curr$v_idx == 1 | curr$v_idx == v_max) | 
            (curr$t_idx == 1 | curr$t_idx == t_max)
    
    data.table(
      Market                = curr$Market,
      ParamID               = paste0(round(curr$BreakoutPct * 100, 1), "% | ", round(curr$VolMult, 1), "x | ", curr$EntryTime),
      BreakoutPct           = paste0(round(curr$BreakoutPct * 100, 1), "%"),
      VolMult               = paste0(round(curr$VolMult, 1), "x"),
      EntryTime             = curr$EntryTime,
      Trades                = curr$Trades,
      WinRate               = paste0(curr$WinRate, "%"),
      NetProfit_Pts         = curr$NetProfit_Pts,
      MDD_Pts               = curr$MDD_Pts,
      PF                    = curr$PF,
      Single_Peak_Calmar    = round(curr$Calmar_IS, 2),
      Plateau_Smooth_Calmar = round(mean(neighbors$Calmar_IS, na.rm = TRUE), 2),
      Worst_Neighbor_Calmar = round(min(neighbors$Calmar_IS, na.rm = TRUE), 2),
      Neighbor_Count        = nrow(neighbors),
      Is_Boundary           = is_b
    )
  })
  
  out_dt <- rbindlist(smooth_res)
  setorder(out_dt, -Plateau_Smooth_Calmar)
  return(out_dt)
}

# ==============================================================================
# 2. 嚴格載入資料與清洗 (去重 + 排序)
# ==============================================================================
cat("1/2 處理台灣微台指 (Micro TX)...\n")
dt_tw <- fread(tw_csv_path)
setnames(dt_tw, old = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"), 
         new = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"), skip_absent = TRUE)
dt_tw[, Time := format(as.POSIXct(Time, format="%H:%M:%S"), "%H:%M:%S")]
dt_tw <- dt_tw[Time >= "08:45:00" & Time <= "13:45:00"]
dt_tw[, Date := as.Date(Date, format = "%Y/%m/%d")]  # 🔧 [修正] 原本Date是字串"YYYY/MM/DD"，跟篩選邊界"YYYY-MM-DD"分隔符號不同，字串比較會出錯(2025年整年被誤篩掉)，改成正確的Date型態比較
dt_tw <- unique(dt_tw, by = c("Date", "Time"))
setorder(dt_tw, Date, Time)

s1_tw <- run_S1_single_market(dt_tw, "台灣微台指", FEE_MICRO_TX)

cat("2/2 處理日本微型與迷你期貨 (N225 Micro & Mini TOPIX)...\n")
dt_jp <- readRDS(jp_rds_path)
dt_jp <- unique(dt_jp, by = c("Symbol", "Date", "Time"))
setorder(dt_jp, Symbol, Date, Time)

s1_n225  <- run_S1_single_market(dt_jp[Symbol == "N225_Large"], "日經225微型", FEE_N225_MICRO)
s1_topix <- run_S1_single_market(dt_jp[Symbol == "TOPIX"], "東證期貨迷你", FEE_MINI_TOPIX)

# ==============================================================================
# 3. 匯出獨立總表
# ==============================================================================
master_s1_split <- rbind(s1_tw, s1_n225, s1_topix)
out_file_s1 <- file.path(output_dir, "S1_3Markets_Independent_Plateau_IS.csv")
fwrite(master_s1_split, out_file_s1, bom = TRUE)

cat("\n==============================================================================\n")
cat("【S1 成功存檔】已匯出至：\n", out_file_s1, "\n")
cat("==============================================================================\n\n")

cat("--- 【台灣微台指 S1 TOP 5 非邊界高原】 ---\n")
print(head(s1_tw[Is_Boundary == FALSE], 5))

cat("\n--- 【日經 225 微型 S1 TOP 5 非邊界高原】 ---\n")
print(head(s1_n225[Is_Boundary == FALSE], 5))

cat("\n--- 【東證期貨迷你 S1 TOP 5 非邊界高原】 ---\n")
print(head(s1_topix[Is_Boundary == FALSE], 5))
