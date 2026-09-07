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

# 門檻領域下限：0.3% ~ 0.8%
gap_vals      <- seq(0.003, 0.008, by = 0.001) 
box_end_vals  <- c("09:00:00", "09:15:00", "09:30:00")
vol_mult_vals <- seq(1.0, 3.0, by = 0.2)

param_grid_s2 <- expand.grid(
  g_idx = seq_along(gap_vals),
  b_idx = seq_along(box_end_vals),
  v_idx = seq_along(vol_mult_vals),
  stringsAsFactors = FALSE
)
param_grid_s2$GapPct  <- gap_vals[param_grid_s2$g_idx]
param_grid_s2$BoxEnd  <- box_end_vals[param_grid_s2$b_idx]
param_grid_s2$VolMult <- vol_mult_vals[param_grid_s2$v_idx]

# ==============================================================================
# 1. S2 單一市場獨立算力與 3D 鄰域平滑模組
# ==============================================================================
run_S2_single_market <- function(dt, market_name, fee_pts, open_time = "08:45:00") {
  dt_is <- dt[Date >= "2023-01-01" & Date <= "2025-12-31"]
  if (nrow(dt_is) == 0) return(NULL)
  
  day_list <- split(dt_is, by = "Date")
  dates <- names(day_list)
  if (length(dates) < 2) return(NULL)
  
  prev_closes <- sapply(seq_along(day_list), function(idx) {
    if (idx == 1) return(NA)
    day_list[[idx - 1]][.N, Close[1]]
  })
  
  raw_res <- lapply(seq_len(nrow(param_grid_s2)), function(idx) {
    p <- param_grid_s2[idx, ]; g_pct <- p$GapPct; b_end <- p$BoxEnd; v_mult <- p$VolMult
    trade_pnls <- c()
    
    for (d_idx in 2:length(day_list)) {
      day_data <- day_list[[d_idx]]
      prev_close <- prev_closes[d_idx]
      n_bars <- nrow(day_data)
      if (n_bars < 5 || is.na(prev_close)) next
      
      open_day <- day_data[Time == open_time, Open][1]
      if (is.na(open_day)) open_day <- day_data$Open[1]
      if (abs(open_day - prev_close) / prev_close < g_pct) next
      
      box_idx <- which(day_data$Time >= open_time & day_data$Time <= b_end)
      if (length(box_idx) == 0) next
      high_box <- max(day_data$High[box_idx], na.rm = TRUE)
      low_box  <- min(day_data$Low[box_idx], na.rm = TRUE)
      
      vol_vec <- day_data$Volume
      vol_ma  <- shift(frollmean(vol_vec, n = 20, fill = NA), 1)
      vol_ma[is.na(vol_ma)] <- shift(cummean(vol_vec), 1)[is.na(vol_ma)]
      vol_ok_vec <- shift(vol_vec, 1) > v_mult * vol_ma
      
      time_vec  <- day_data$Time
      entry_idx <- which(time_vec > b_end)
      if (length(entry_idx) == 0) next
      
      high_vec <- day_data$High; low_vec <- day_data$Low; close_vec <- day_data$Close
      
      for (i in entry_idx) {
        if (isTRUE(vol_ok_vec[i])) {
          if (high_vec[i] >= high_box) {
            entry_p <- high_box; sl_p <- open_day
            if (low_vec[i] <= sl_p) { exit_p <- sl_p } 
            else if (i < n_bars) {
              rest_lows <- low_vec[(i+1):n_bars]
              sl_hits   <- which(rest_lows <= sl_p)
              exit_p    <- if (length(sl_hits) > 0) sl_p else close_vec[n_bars]
            } else { exit_p <- close_vec[n_bars] }
            trade_pnls <- c(trade_pnls, (exit_p - entry_p) - fee_pts); break
          } else if (low_vec[i] <= low_box) {
            entry_p <- low_box; sl_p <- open_day
            if (high_vec[i] >= sl_p) { exit_p <- sl_p } 
            else if (i < n_bars) {
              rest_highs <- high_vec[(i+1):n_bars]
              sl_hits    <- which(rest_highs >= sl_p)
              exit_p     <- if (length(sl_hits) > 0) sl_p else close_vec[n_bars]
            } else { exit_p <- close_vec[n_bars] }
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
      Market = market_name, g_idx = p$g_idx, b_idx = p$b_idx, v_idx = p$v_idx,
      GapPct = p$GapPct, BoxEnd = p$BoxEnd, VolMult = p$VolMult,
      Trades = tot_trades, WinRate = win_rate, NetProfit_Pts = net_pnl, MDD_Pts = -mdd, PF = pf, Calmar_IS = calmar
    ))
  })
  
  dt_res <- rbindlist(raw_res)
  
  # 獨立進行單一市場 3D 平滑計算
  g_max <- max(dt_res$g_idx); b_max <- max(dt_res$b_idx); v_max <- max(dt_res$v_idx)
  
  smooth_res <- lapply(seq_len(nrow(dt_res)), function(i) {
    curr <- dt_res[i]
    neighbors <- dt_res[abs(g_idx - curr$g_idx) <= 1 & abs(b_idx - curr$b_idx) <= 1 & abs(v_idx - curr$v_idx) <= 1]
    is_b <- (curr$g_idx == 1 | curr$g_idx == g_max) | (curr$b_idx == 1 | curr$b_idx == b_max) | (curr$v_idx == 1 | curr$v_idx == v_max)
    
    data.table(
      Market                = curr$Market,
      ParamID               = paste0(round(curr$GapPct * 100, 1), "% | ", curr$BoxEnd, " | ", round(curr$VolMult, 1), "x"),
      GapPct                = paste0(round(curr$GapPct * 100, 1), "%"),
      BoxEnd                = curr$BoxEnd,
      VolMult               = paste0(round(curr$VolMult, 1), "x"),
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
# 2. 嚴格載入與去重排序
# ==============================================================================
cat("1/2 載入與清洗台灣期貨資料...\n")
dt_tw <- fread(tw_csv_path)
setnames(dt_tw, old = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"), 
         new = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"), skip_absent = TRUE)
dt_tw[, Time := format(as.POSIXct(Time, format="%H:%M:%S"), "%H:%M:%S")]
dt_tw <- dt_tw[Time >= "08:45:00" & Time <= "13:45:00"]
dt_tw[, Date := as.Date(Date, format = "%Y/%m/%d")]  # 🔧 [修正] 原本字串比較日期會誤篩掉2025年，改成正確Date型態比較
dt_tw <- unique(dt_tw, by = c("Date", "Time"))
setorder(dt_tw, Date, Time)

s2_tw <- run_S2_single_market(dt_tw, "台灣微台指", FEE_MICRO_TX)

cat("2/2 載入與清洗日本期貨資料...\n")
dt_jp <- readRDS(jp_rds_path)
dt_jp <- unique(dt_jp, by = c("Symbol", "Date", "Time"))
setorder(dt_jp, Symbol, Date, Time)

s2_n225  <- run_S2_single_market(dt_jp[Symbol == "N225_Large"], "日經225微型", FEE_N225_MICRO)
s2_topix <- run_S2_single_market(dt_jp[Symbol == "TOPIX"], "東證期貨迷你", FEE_MINI_TOPIX)

master_s2_split <- rbind(s2_tw, s2_n225, s2_topix)
out_file_s2 <- file.path(output_dir, "S2_3Markets_Independent_Plateau_IS.csv")
fwrite(master_s2_split, out_file_s2, bom = TRUE)

cat("【S2 成功存檔】三大市場獨立高原報表已存入 S2_3Markets_Independent_Plateau_IS.csv\n")
