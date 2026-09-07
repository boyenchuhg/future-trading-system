# ==============================================================================
# 期交所逐筆成交資料 → 5分K 聚合程式
# 用途：自行切出5分K，與XQ資料比對驗證，作為即時切K邏輯的基礎
# ==============================================================================
library(data.table)

# ---- 使用者設定 ----
tick_dir   <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/期貨日內/回測資料"
output_dir <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/期貨日內/回測資料"

PRODUCT      <- "TX"          # 商品代號：TX=大台, MTX=小台, TMF=微台
SESSION_FROM <- "084500"      # 日盤開始
SESSION_TO   <- "134500"      # 日盤結束
BAR_MINUTES  <- 5             # K棒週期(分鐘)
CONTRACT_RULE <- "front"      # "front"=前月法(最近未到期月份) / "volume"=最大量法
HALVE_VOLUME <- TRUE          # 成交數量(B+S)是雙邊計數，除以2還原真實量

# ==============================================================================
read_one_day <- function(fpath) {
  # 期交所檔案為Big5編碼，欄名為中文；改用「位置」指定欄位，避免編碼問題
  dt <- tryCatch(
    fread(fpath, encoding = "unknown", showProgress = FALSE),
    error = function(e) { cat("  讀取失敗:", basename(fpath), "\n"); return(NULL) }
  )
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  if (ncol(dt) < 8) { cat("  欄位數不足，略過:", basename(fpath), "\n"); return(NULL) }
  
  setnames(dt, 1:8, c("TradeDate","Product","ContractMonth","TradeTime",
                      "Price","QtyBS","NearPrice","FarPrice"))
  
  dt[, Product       := trimws(as.character(Product))]
  dt[, ContractMonth := trimws(as.character(ContractMonth))]
  dt[, NearPrice     := trimws(as.character(NearPrice))]
  dt[, FarPrice      := trimws(as.character(FarPrice))]
  
  # (1) 指定商品
  dt <- dt[Product == PRODUCT]
  if (nrow(dt) == 0) return(NULL)
  
  # (2) 排除價差交易：近月/遠月價格有值者為日曆價差單，其成交價非真實指數點位
  dt <- dt[NearPrice %in% c("-","") & FarPrice %in% c("-","")]
  if (nrow(dt) == 0) return(NULL)
  
  # (3) 排除週合約（到期月份含 "W"），只保留月合約
  dt <- dt[!grepl("W", ContractMonth, ignore.case = TRUE)]
  if (nrow(dt) == 0) return(NULL)
  
  # (4) 型別轉換
  dt[, TradeDate := as.integer(TradeDate)]
  dt[, TradeTime := sprintf("%06d", as.integer(TradeTime))]
  dt[, Price     := as.numeric(Price)]
  dt[, QtyBS     := as.numeric(QtyBS)]
  dt <- dt[!is.na(Price) & !is.na(QtyBS) & Price > 0]
  
  # (5) 只留日盤時段（此舉同時排除夜盤，夜盤成交日期為前一日）
  #     注意：上界為「含」13:45:00，該筆為收盤集合競價，需納入最後一根K棒，
  #     否則收盤價與成交量會與XQ不符。
  dt <- dt[TradeTime >= SESSION_FROM & TradeTime <= SESSION_TO]
  if (nrow(dt) == 0) return(NULL)
  
  # (6) 成交量還原：期交所欄位為 B+S 雙邊計數
  if (HALVE_VOLUME) dt[, Qty := QtyBS / 2] else dt[, Qty := QtyBS]
  
  dt[, .(TradeDate, ContractMonth, TradeTime, Price, Qty)]
}

pick_contract <- function(dt_day) {
  # 從當日所有月合約中挑出要用的那一個
  if (CONTRACT_RULE == "volume") {
    # 最大量法：流動性最高的月份
    vol_by_ct <- dt_day[, .(V = sum(Qty)), by = ContractMonth]
    target <- vol_by_ct[which.max(V), ContractMonth]
    return(dt_day[ContractMonth == target])
  }
  
  # 前月法：單純取最近（數值最小）的未到期月份，結算日不提前換月。
  # 【結算日行為】大台月合約於第三個星期三 13:30 停止交易，該日僅切出 57 根K棒
  # （最後一根為 13:25），此與 XQ 一致，屬正常現象，不需補齊為 60 根。
  months <- sort(unique(dt_day$ContractMonth))
  dt_day[ContractMonth == months[1]]
}

aggregate_bars <- function(dt_day) {
  # 時間 → 當日分鐘數 → 落入哪一根K棒（左閉右開，以起始時間標記）
  hh <- as.integer(substr(dt_day$TradeTime, 1, 2))
  mm <- as.integer(substr(dt_day$TradeTime, 3, 4))
  mins <- hh * 60 + mm
  
  base_min <- as.integer(substr(SESSION_FROM,1,2)) * 60 + as.integer(substr(SESSION_FROM,3,4))
  end_min  <- as.integer(substr(SESSION_TO,1,2))   * 60 + as.integer(substr(SESSION_TO,3,4))
  max_idx  <- (end_min - base_min) %/% BAR_MINUTES - 1   # 日盤 08:45~13:45 → 0..59
  dt_day[, BarIdx := (mins - base_min) %/% BAR_MINUTES]
  # 13:45:00 的收盤集合競價會落在 BarIdx=60，需歸入最後一根（13:40）
  dt_day[BarIdx > max_idx, BarIdx := max_idx]
  
  bars <- dt_day[, .(
    Open     = Price[1],
    High     = max(Price),
    Low      = min(Price),
    Close    = Price[.N],
    Volume   = sum(Qty),
    Contract = ContractMonth[1]
  ), by = .(TradeDate, BarIdx)]
  
  # 還原K棒的起始時間標籤
  bars[, BarStartMin := base_min + BarIdx * BAR_MINUTES]
  bars[, Time := sprintf("%02d:%02d:00", BarStartMin %/% 60, BarStartMin %% 60)]
  bars[, Date := as.Date(as.character(TradeDate), format = "%Y%m%d")]
  
  setorder(bars, Date, BarIdx)
  bars[, .(Date, Time, Open, High, Low, Close, Volume, Contract)]
}

# ==============================================================================
files <- list.files(tick_dir, pattern = "^Daily_.*\\.csv$", full.names = TRUE)
cat(sprintf("找到 %d 個逐筆檔案\n", length(files)))
if (length(files) == 0) stop("找不到 Daily_*.csv，請確認 tick_dir 路徑")

all_bars <- list()
for (f in files) {
  raw <- read_one_day(f)
  if (is.null(raw) || nrow(raw) == 0) next
  
  # 一個檔案理論上只含一個日盤交易日，但仍逐日處理以策安全
  for (d in unique(raw$TradeDate)) {
    dt_day <- raw[TradeDate == d]
    dt_day <- pick_contract(dt_day)
    if (nrow(dt_day) == 0) next
    all_bars[[length(all_bars) + 1]] <- aggregate_bars(dt_day)
  }
  cat(".")
}
cat("\n")

result <- rbindlist(all_bars)
setorder(result, Date, Time)

# 加上與現有回測檔一致的 Datetime 欄位
result[, Datetime := as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")]
result[, DateStr  := format(Date, "%Y/%m/%d")]
out <- result[, .(Datetime, Date = DateStr, Time, Open, High, Low, Close, Volume, Contract)]

out_file <- file.path(output_dir, sprintf("TAIEX_Futures_%dmin_FromTick.csv", BAR_MINUTES))
fwrite(out, out_file, bom = TRUE)

cat(sprintf("\n【完成】共 %d 根K棒，涵蓋 %d 個交易日\n", nrow(out), uniqueN(out$Date)))
cat(sprintf("日期範圍：%s ~ %s\n", min(out$Date), max(out$Date)))
cat(sprintf("輸出：%s\n", out_file))

cat("\n--- 每日K棒數量檢查（日盤正常為60根；結算日近月13:30停止交易，57根屬正常）---\n")
bar_count <- out[, .N, by = Date]
short_days <- bar_count[N != 60]
if (nrow(short_days) == 0) {
  cat("所有交易日皆為60根\n")
} else {
  print(short_days)
  cat("※ 若上列日期為每月第三個星期三（結算日），57根為正確結果。\n")
}

cat("\n--- 合約換月紀錄（確認結算日是否正確換月）---\n")
roll <- out[, .(Contract = Contract[1], Bars = .N), by = Date]
roll[, Changed := Contract != shift(Contract, 1)]
print(roll[Changed == TRUE])
cat(sprintf("共發生 %d 次換月\n", sum(roll$Changed, na.rm = TRUE)))

cat("\n--- 前3根與後3根 ---\n")
print(head(out, 3)); print(tail(out, 3))
