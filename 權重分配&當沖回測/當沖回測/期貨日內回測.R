library(data.table)
library(dplyr)
library(ggplot2)
library(scales)

# ==============================================================================
# 0. 路徑與最小商品摩擦成本設定 (單位：純點數 Points)
# ==============================================================================
desktop_path <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/期貨日內/回測結果"
tw_csv_path  <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/整體基礎資料/CSV/台灣期貨全資料/TAIEX_Futures_5min.csv"
jp_rds_dir   <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/整體基礎資料/CSV/日本期貨"
jp_rds_path  <- file.path(jp_rds_dir, "JP_Futures_5min_Clean.rds")

if (!file.exists(jp_rds_path)) {
  rds_files <- list.files(jp_rds_dir, pattern = "\\.rds$", full.names = TRUE, ignore.case = TRUE)
  if (length(rds_files) > 0) jp_rds_path <- rds_files[1]
}

# --- 最小商品真實摩擦成本 (手續費 + 滑價) 寫死於此 ---
FEE_MICRO_TX   <- 4.0   # 微台指：規費 3.0 點 + 滑價 1.0 點 (1 Tick) = 4.0 點
FEE_N225_MICRO <- 8.5   # 日經微型：規費 3.5 點 + 滑價 5.0 點 (1 Tick) = 8.5 點
FEE_MINI_TOPIX <- 0.31  # 東證迷你：規費 0.06 點 + 滑價 0.25 點 (1 Tick) = 0.31 點

# ==============================================================================
# 1. 進階點數績效報表計算函數 (含近兩年卡馬比率)
# ==============================================================================
calc_performance_pts <- function(trades, market_name = "Market", strat_name = "Strategy") {
  if (is.null(trades) || nrow(trades) == 0) {
    return(data.table(
      Market = market_name, Strategy = strat_name, Trades = 0, WinRate = "0%", 
      NetProfit_Pts = 0, MDD_Pts = 0, PF = NA, Calmar_All = NA, Calmar_2Y = NA
    ))
  }
  
  trades[, Date := as.Date(Date)]
  setorder(trades, Date)
  
  trades[, CumPnL := cumsum(NetPnL)]
  trades[, Peak := cummax(CumPnL)]
  trades[, Drawdown := CumPnL - Peak]
  
  total_trades <- nrow(trades)
  wins         <- sum(trades$NetPnL > 0)
  win_rate     <- round(wins / total_trades * 100, 2)
  total_pnl    <- round(sum(trades$NetPnL), 1)
  mdd_all      <- round(abs(min(trades$Drawdown)), 1)
  
  gross_profit <- sum(trades$NetPnL[trades$NetPnL > 0])
  gross_loss   <- abs(sum(trades$NetPnL[trades$NetPnL < 0]))
  pf           <- ifelse(gross_loss == 0, NA, round(gross_profit / gross_loss, 2))
  
  years_all    <- as.numeric(max(trades$Date) - min(trades$Date)) / 365.25
  years_all    <- ifelse(years_all < 0.1, 0.1, years_all)
  annual_pnl   <- total_pnl / years_all
  calmar_all   <- ifelse(mdd_all == 0, NA, round(annual_pnl / mdd_all, 2))
  
  max_date     <- max(trades$Date)
  trades_2y    <- trades[Date >= (max_date - 730)]
  
  if (nrow(trades_2y) > 0) {
    trades_2y[, CumPnL_2y := cumsum(NetPnL)]
    trades_2y[, Peak_2y := cummax(CumPnL_2y)]
    trades_2y[, Drawdown_2y := CumPnL_2y - Peak_2y]
    
    pnl_2y       <- sum(trades_2y$NetPnL)
    mdd_2y       <- round(abs(min(trades_2y$Drawdown_2y)), 1)
    annual_2y    <- pnl_2y / 2
    calmar_2y    <- ifelse(mdd_2y == 0, NA, round(annual_2y / mdd_2y, 2))
  } else {
    calmar_2y    <- NA
  }
  
  return(data.table(
    Market        = market_name,
    Strategy      = strat_name,
    Trades        = total_trades,
    WinRate       = paste0(win_rate, "% (", wins, "/", total_trades - wins, ")"),
    NetProfit_Pts = total_pnl,
    MDD_Pts       = -mdd_all,
    PF            = pf,
    Calmar_All    = calmar_all,
    Calmar_2Y     = calmar_2y
  ))
}

# ==============================================================================
# 2. 四大策略 + 雙基準線點數算力模組
# ==============================================================================
run_S_Hold <- function(dt, fee_pts, open_time = "08:45:00") {
  trade_list <- list(); dates <- unique(dt$Date); if (length(dates) == 0) return(NULL)
  prev_close <- NA
  for (idx in seq_along(dates)) {
    d <- dates[idx]; day_data <- dt[Date == d]; if (nrow(day_data) < 1) next
    close_price <- day_data[.N, Close[1]]
    if (idx == 1) {
      open_price <- day_data[Time == open_time, Open][1]
      if (is.na(open_price)) open_price <- day_data$Open[1]
      net_pnl <- (close_price - open_price) - (fee_pts / 2)
    } else {
      net_pnl <- (close_price - prev_close)
    }
    prev_close <- close_price
    trade_list[[length(trade_list) + 1]] <- data.table(Date = d, Strategy = "S_Hold: 長期買持 (2023起)", NetPnL = net_pnl)
  }
  return(rbindlist(trade_list))
}

run_S0 <- function(dt, fee_pts, open_time = "08:45:00") {
  trade_list <- list(); dates <- unique(dt$Date)
  for (d in dates) {
    day_data <- dt[Date == d]; if (nrow(day_data) < 5) next
    open_price  <- day_data[Time == open_time, Open][1]
    if (is.na(open_price)) open_price <- day_data$Open[1]
    close_price <- day_data[.N, Close[1]]
    net_pnl <- (close_price - open_price) - fee_pts
    trade_list[[length(trade_list) + 1]] <- data.table(Date = d, Strategy = "S0: 當日買持 (Intraday B&H)", NetPnL = net_pnl)
  }
  return(rbindlist(trade_list))
}

run_S1 <- function(dt, fee_pts, open_time = "08:45:00", entry_time = "09:15:00") {
  trade_list <- list(); dates <- unique(dt$Date)
  for (d in dates) {
    day_data <- dt[Date == d]; if (nrow(day_data) < 5) next
    open_day <- day_data[Time == open_time, Open][1]
    if (is.na(open_day)) open_day <- day_data$Open[1]
    long_trigger  <- as.numeric(open_day * 1.005); short_trigger <- as.numeric(open_day * 0.995)
    
    day_data[, Vol_MA := shift(frollmean(Volume, n = 20, fill = NA), 1)]
    day_data[is.na(Vol_MA), Vol_MA := shift(cummean(Volume), 1)]
    day_data[, Vol_OK := shift(Volume, 1) > 1.5 * Vol_MA]
    
    entry_bars <- day_data[Time >= entry_time]
    for (i in seq_len(nrow(entry_bars))) {
      row <- entry_bars[i]
      if (row$High[1] >= long_trigger && isTRUE(row$Vol_OK[1])) {
        ex_p <- if (row$Low[1] <= open_day) open_day else {
          hit <- entry_bars[(i+1):.N][Low <= open_day]
          if (nrow(hit) > 0) open_day else entry_bars[.N, Close[1]]
        }
        net_pnl <- (ex_p - long_trigger) - fee_pts
        trade_list[[length(trade_list) + 1]] <- data.table(Date = d, Strategy = "S1: 0.5% 開盤突破", NetPnL = net_pnl); break
      } else if (row$Low[1] <= short_trigger && isTRUE(row$Vol_OK[1])) {
        ex_p <- if (row$High[1] >= open_day) open_day else {
          hit <- entry_bars[(i+1):.N][High >= open_day]
          if (nrow(hit) > 0) open_day else entry_bars[.N, Close[1]]
        }
        net_pnl <- (short_trigger - ex_p) - fee_pts
        trade_list[[length(trade_list) + 1]] <- data.table(Date = d, Strategy = "S1: 0.5% 開盤突破", NetPnL = net_pnl); break
      }
    }
  }
  return(rbindlist(trade_list))
}

run_S2 <- function(dt, fee_pts, open_time = "08:45:00", box_end = "09:15:00") {
  trade_list <- list(); dates <- unique(dt$Date); if (length(dates) < 2) return(NULL)
  for (idx in 2:length(dates)) {
    d_prev <- dates[idx - 1]; d_curr <- dates[idx]
    prev_close <- dt[Date == d_prev, Close[.N]]
    day_data   <- dt[Date == d_curr]
    if (nrow(day_data) < 5 || length(prev_close) == 0) next
    open_day   <- day_data[Time == open_time, Open][1]
    if (is.na(open_day)) open_day <- day_data$Open[1]
    gap_pct    <- abs(open_day - prev_close) / prev_close
    if (is.na(gap_pct) || gap_pct < 0.005) next
    
    box_30m  <- day_data[Time >= open_time & Time <= box_end]
    if (nrow(box_30m) == 0) next
    high_30m <- max(box_30m$High, na.rm = TRUE); low_30m <- min(box_30m$Low, na.rm = TRUE)
    
    day_data[, Vol_MA := shift(frollmean(Volume, n = 20, fill = NA), 1)]
    day_data[is.na(Vol_MA), Vol_MA := shift(cummean(Volume), 1)]
    day_data[, Vol_OK := shift(Volume, 1) > 1.5 * Vol_MA]
    
    entry_bars <- day_data[Time > box_end]
    for (i in seq_len(nrow(entry_bars))) {
      row <- entry_bars[i]
      if (row$High[1] >= high_30m && isTRUE(row$Vol_OK[1])) {
        ex_p <- if (row$Low[1] <= open_day) open_day else {
          hit <- entry_bars[(i+1):.N][Low <= open_day]
          if (nrow(hit) > 0) open_day else entry_bars[.N, Close[1]]
        }
        net_pnl <- (ex_p - high_30m) - fee_pts
        trade_list[[length(trade_list) + 1]] <- data.table(Date = d_curr, Strategy = "S2: 缺口+30m突破", NetPnL = net_pnl); break
      } else if (row$Low[1] <= low_30m && isTRUE(row$Vol_OK[1])) {
        ex_p <- if (row$High[1] >= open_day) open_day else {
          hit <- entry_bars[(i+1):.N][High >= open_day]
          if (nrow(hit) > 0) open_day else entry_bars[.N, Close[1]]
        }
        net_pnl <- (low_30m - ex_p) - fee_pts
        trade_list[[length(trade_list) + 1]] <- data.table(Date = d_curr, Strategy = "S2: 缺口+30m突破", NetPnL = net_pnl); break
      }
    }
  }
  return(rbindlist(trade_list))
}

run_S3 <- function(dt, fee_pts, open_time = "08:45:00", box_end = "08:55:00") {
  trade_list <- list(); dates <- unique(dt$Date)
  for (d in dates) {
    day_data <- dt[Date == d]; if (nrow(day_data) < 5) next
    day_data[, TypPrice := (High + Low + Close) / 3]
    day_data[, VWAP := cumsum(TypPrice * Volume) / cumsum(Volume)]
    box_15m <- day_data[Time >= open_time & Time <= box_end]
    if (nrow(box_15m) < 3) next
    high_15m <- max(box_15m$High, na.rm = TRUE); low_15m <- min(box_15m$Low, na.rm = TRUE); mid_15m <- (high_15m + low_15m) / 2
    
    day_data[, Vol_MA := shift(frollmean(Volume, n = 20, fill = NA), 1)]
    day_data[is.na(Vol_MA), Vol_MA := shift(cummean(Volume), 1)]
    day_data[, Vol_OK := shift(Volume, 1) > 1.5 * Vol_MA]
    
    entry_bars <- day_data[Time > box_end]
    long_t <- FALSE; short_t <- FALSE
    for (i in seq_len(nrow(entry_bars))) {
      row <- entry_bars[i]
      if (!long_t && row$High[1] >= high_15m && high_15m > row$VWAP[1] && isTRUE(row$Vol_OK[1])) {
        ex_p <- if (row$Low[1] <= mid_15m) mid_15m else {
          hit <- entry_bars[(i+1):.N][Low <= mid_15m]
          if (nrow(hit) > 0) mid_15m else entry_bars[.N, Close[1]]
        }
        net_pnl <- (ex_p - high_15m) - fee_pts
        trade_list[[length(trade_list) + 1]] <- data.table(Date = d, Strategy = "S3: Aziz 15m ORB", NetPnL = net_pnl)
        long_t <- TRUE
      } else if (!short_t && row$Low[1] <= low_15m && low_15m < row$VWAP[1] && isTRUE(row$Vol_OK[1])) {
        ex_p <- if (row$High[1] >= mid_15m) mid_15m else {
          hit <- entry_bars[(i+1):.N][High >= mid_15m]
          if (nrow(hit) > 0) mid_15m else entry_bars[.N, Close[1]]
        }
        net_pnl <- (low_15m - ex_p) - fee_pts
        trade_list[[length(trade_list) + 1]] <- data.table(Date = d, Strategy = "S3: Aziz 15m ORB", NetPnL = net_pnl)
        short_t <- TRUE
      }
    }
  }
  return(rbindlist(trade_list))
}

run_S4 <- function(dt, fee_pts, box_start = "09:15:00", box_end = "12:00:00", entry_end = "13:15:00") {
  trade_list <- list(); dates <- unique(dt$Date)
  for (d in dates) {
    day_data <- dt[Date == d]; if (nrow(day_data) < 5) next
    box_mid <- day_data[Time >= box_start & Time <= box_end]
    if (nrow(box_mid) == 0) next
    high_mid <- max(box_mid$High, na.rm = TRUE); low_mid <- min(box_mid$Low, na.rm = TRUE); mid_mid <- (high_mid + low_mid) / 2
    
    day_data[, Vol_MA := shift(frollmean(Volume, n = 20, fill = NA), 1)]
    day_data[is.na(Vol_MA), Vol_MA := shift(cummean(Volume), 1)]
    day_data[, Vol_OK := shift(Volume, 1) > 1.5 * Vol_MA]
    
    entry_bars <- day_data[Time >= box_end & Time <= entry_end]
    for (i in seq_len(nrow(entry_bars))) {
      row <- entry_bars[i]
      if (row$High[1] >= high_mid && isTRUE(row$Vol_OK[1])) {
        ex_p <- if (row$Low[1] <= mid_mid) mid_mid else {
          hit <- day_data[Time > row$Time][Low <= mid_mid]
          if (nrow(hit) > 0) mid_mid else day_data[.N, Close[1]]
        }
        net_pnl <- (ex_p - high_mid) - fee_pts
        trade_list[[length(trade_list) + 1]] <- data.table(Date = d, Strategy = "S4: 午盤尾盤動能", NetPnL = net_pnl); break
      } else if (row$Low[1] <= low_mid && isTRUE(row$Vol_OK[1])) {
        ex_p <- if (row$High[1] >= mid_mid) mid_mid else {
          hit <- day_data[Time > row$Time][High >= mid_mid]
          if (nrow(hit) > 0) mid_mid else day_data[.N, Close[1]]
        }
        net_pnl <- (low_mid - ex_p) - fee_pts
        trade_list[[length(trade_list) + 1]] <- data.table(Date = d, Strategy = "S4: 午盤尾盤動能", NetPnL = net_pnl); break
      }
    }
  }
  return(rbindlist(trade_list))
}

# ==============================================================================
# 3. 核心處理函數：產出點數圖表 (cairo_pdf) 與 每日點數 PnL CSV
# ==============================================================================
process_market_points <- function(dt_sub, market_name, fee_pts, pdf_filename, csv_filename) {
  th <- run_S_Hold(dt_sub, fee_pts)
  t0 <- run_S0(dt_sub, fee_pts)
  t1 <- run_S1(dt_sub, fee_pts)
  t2 <- run_S2(dt_sub, fee_pts)
  t3 <- run_S3(dt_sub, fee_pts)
  t4 <- run_S4(dt_sub, fee_pts)
  
  all_trades <- rbind(th, t0, t1, t2, t3, t4, fill = TRUE)
  all_trades[, Date := as.Date(Date)]
  all_trades[, Market := market_name]
  all_trades[, Fee_Pts_Used := fee_pts]
  
  setorder(all_trades, Strategy, Date)
  all_trades[, CumPnL_Pts := cumsum(NetPnL), by = Strategy]
  setnames(all_trades, old = "NetPnL", new = "NetPnL_Pts")
  
  setcolorder(all_trades, c("Market", "Date", "Strategy", "NetPnL_Pts", "CumPnL_Pts", "Fee_Pts_Used"))
  
  # 導出單一市場 CSV
  csv_path <- file.path(desktop_path, csv_filename)
  fwrite(all_trades, csv_path, bom = TRUE)
  cat(sprintf("【成功導出 CSV】%s 每日點數 PnL 已存至：%s\n", market_name, csv_path))
  
  # 繪製點數 PnL 圖表
  p <- ggplot(all_trades, aes(x = Date, y = CumPnL_Pts, color = Strategy)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = c(
      "S_Hold: 長期買持 (2023起)" = "#222222",
      "S0: 當日買持 (Intraday B&H)" = "#888888",
      "S1: 0.5% 開盤突破"          = "#E41A1C",
      "S2: 缺口+30m突破"           = "#377EB8",
      "S3: Aziz 15m ORB"           = "#4DAF4A",
      "S4: 午盤尾盤動能"           = "#984EA3"
    )) +
    labs(
      title = paste0(market_name, " - 各策略資金累積點數曲線 (Points)"),
      subtitle = sprintf("2023-2026 日盤歷史數據 (包含手續費+滑價共扣除 %s 點)", fee_pts),
      x = "日期",
      y = "累積淨損益 (點數 / Points)",
      color = "策略類型"
    ) +
    theme_minimal(base_size = 14, base_family = "Microsoft JhengHei") +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      legend.position = "bottom"
    )
  
  pdf_path <- file.path(desktop_path, pdf_filename)
  ggsave(pdf_path, plot = p, width = 11, height = 7, device = cairo_pdf)
  cat(sprintf("【成功導出 PDF】%s 點數 PnL 圖表已存至：%s\n", market_name, pdf_path))
  
  setnames(all_trades, old = "NetPnL_Pts", new = "NetPnL")
  res_table <- rbind(
    calc_performance_pts(all_trades[Strategy == "S_Hold: 長期買持 (2023起)"], market_name, "S_Hold: 長期買持 (2023起)"),
    calc_performance_pts(all_trades[Strategy == "S0: 當日買持 (Intraday B&H)"], market_name, "S0: 當日買持 (Intraday B&H)"),
    calc_performance_pts(all_trades[Strategy == "S1: 0.5% 開盤突破"], market_name, "S1: 0.5% 開盤突破"),
    calc_performance_pts(all_trades[Strategy == "S2: 缺口+30m突破"], market_name, "S2: 缺口+30m突破"),
    calc_performance_pts(all_trades[Strategy == "S3: Aziz 15m ORB"], market_name, "S3: Aziz 15m ORB"),
    calc_performance_pts(all_trades[Strategy == "S4: 午盤尾盤動能"], market_name, "S4: 午盤尾盤動能")
  )
  
  return(list(Summary = res_table, Trades = all_trades))
}

# ==============================================================================
# 4. 啟動三大微型商品同步運算
# ==============================================================================
cat("==============================================================================\n")
cat("          開始執行三大微型商品 (真實摩擦點數) 一鍵總分析...\n")
cat("==============================================================================\n\n")

# 1. 台灣微台指
cat("1/3 正在處理：台灣微台指 (Micro TX) - 扣除摩擦 4.0 點...\n")
dt_tw <- fread(tw_csv_path)
setnames(dt_tw, old = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"), 
         new = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"), skip_absent = TRUE)
dt_tw[, Time := format(as.POSIXct(Time, format="%H:%M:%S"), "%H:%M:%S")]
dt_tw <- dt_tw[Time >= "08:45:00" & Time <= "13:45:00"]
dt_tw <- unique(dt_tw, by = c("Date", "Time"))
setorder(dt_tw, Date, Time)

out_tw <- process_market_points(dt_tw, "台灣微台指 (Micro TX)", FEE_MICRO_TX, "Taiwan_Micro_TX_PnL_Pts.pdf", "Daily_PnL_Taiwan_Micro_TX_Pts.csv")

# 2. 日本微型/迷你期貨
dt_jp <- readRDS(jp_rds_path)
dt_jp <- unique(dt_jp, by = c("Symbol", "Date", "Time"))
setorder(dt_jp, Symbol, Date, Time)

cat("\n2/3 正在處理：日經 225 微型 (N225 Micro) - 扣除摩擦 8.5 點...\n")
out_n225 <- process_market_points(dt_jp[Symbol == "N225_Large"], "日經 225 微型 (N225 Micro)", FEE_N225_MICRO, "Japan_N225_Micro_PnL_Pts.pdf", "Daily_PnL_Japan_N225_Micro_Pts.csv")

cat("\n3/3 正在處理：東證期貨 迷你 (Mini TOPIX) - 扣除摩擦 0.31 點...\n")
out_topix <- process_market_points(dt_jp[Symbol == "TOPIX"], "東證期貨 迷你 (Mini TOPIX)", FEE_MINI_TOPIX, "Japan_Mini_TOPIX_Pts.pdf", "Daily_PnL_Japan_Mini_TOPIX_Pts.csv")

# 彙整三合一 master 數據檔
master_summary <- rbind(out_tw$Summary, out_n225$Summary, out_topix$Summary)
master_trades  <- rbind(out_tw$Trades, out_n225$Trades, out_topix$Trades, fill = TRUE)

master_csv_path <- file.path(desktop_path, "Daily_PnL_All_Micro_Markets_Master.csv")
fwrite(master_trades, master_csv_path, bom = TRUE)

cat("\n==============================================================================\n")
cat("       三大微型商品四大策略 ＋ 雙基準線 ＋ 卡馬比率 (真實摩擦點數) 總報表\n")
cat("==============================================================================\n")
print(master_summary)