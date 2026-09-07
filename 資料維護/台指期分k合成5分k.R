library(dplyr)
library(lubridate)

# ==========================================
# 1. 載入 1 分 K 資料庫
# ==========================================
path_rds <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/整體基礎資料/CSV/台灣期貨全資料/TAIEX_Futures_1min.rds"

cat("📖 正在載入 1 分 K 資料庫...\n")
df_1min <- readRDS(path_rds)

# ==========================================
# 2. 刪除無量資料，並合成 5 分 K
# ==========================================
cat("⚡ 開始清理無量資料並合成 5 分 K...\n")

df_5min <- df_1min %>%
  filter(!is.na(Volume) & Volume > 0) %>%
  mutate(Datetime_5m = floor_date(Datetime, "5 mins")) %>%
  group_by(Datetime_5m) %>%
  summarise(
    Date   = first(Date),
    Time   = format(first(Datetime_5m), "%H:%M:%S"),
    Open   = first(Open),
    High   = max(High),
    Low    = min(Low),
    Close  = last(Close),
    Volume = sum(Volume),
    .groups = "drop"
  ) %>%
  rename(Datetime = Datetime_5m)

# ==========================================
# 3. 存成 CSV 檔 (排斥 RDS，改存 CSV)
# ==========================================
save_5m_csv <- "C:/Users/user/OneDrive - 財團法人台灣綜合研究院/個人工作/114年/安格/數據分析/分析工作/整體基礎資料/CSV/台灣期貨全資料/TAIEX_Futures_5min.csv"

write.csv(df_5min, save_5m_csv, row.names = FALSE)

cat(sprintf("\n🎉 搞定！已成功將 %d 筆 5 分 K 資料寫入 CSV 檔。\n", nrow(df_5min)))
cat("📁 檔案位置：TAIEX_Futures_5min.csv\n")