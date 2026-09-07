print("DEBUG: [1] 正在載入 Python 模組...")
import os
import glob
import datetime
import pandas as pd
import xlwings as xw
import time

print("DEBUG: [2] 模組載入完成，正在讀取設定...")

# ==============================================================================
# ⚙️ 設定區
# ==============================================================================
BASE_DIR = r"C:\Users\user\OneDrive - 財團法人台灣綜合研究院\個人工作\114年\安格\數據分析\分析工作\整體基礎資料\EXCEL"
CSV_DIR = r"C:\Users\user\OneDrive - 財團法人台灣綜合研究院\個人工作\114年\安格\數據分析\分析工作\評價函數\基礎資料\CSV"

EXCEL_NAME = "TQT-2.xlsm"
CSV_NAME = "評價函數基礎資料(台股25策略).csv"

# 📅 起始日期
START_DATE = datetime.date(2007, 8, 1)

# 💎 策略設定 (修正回：使用您辛苦整理的「固定數字」)
# 格式: ("策略名稱", 固定分母數值, "分子欄位")
STRATEGY_CONFIG = [
    ("QS",   1,   "K"),
    ("DM",   3,   "K"),
    ("SD",   2,   "K"),
    ("HY",   2,   "I"),
    ("RS",   2,   "L"),
    ("IA",   1,   "L"),
    ("GT",   3,   "P"),
    ("KD",   1,   "L"),
    ("K",    2.5, "I"),  # 注意這裡有小數點
    ("ZO",   2,   "M"),
    ("XX",   2,   "P"),
    ("Bi",   1,   "U"),
    # 注意：以下順序已更新
    ("BS",   1,   "H"),
    ("BH",   1,   "G"),
    ("SS",   1,   "H"),
    ("QF",   1,   "I"),
    ("EF",   1,   "I"),
    ("T10",  3,   "O"),
    ("Pig",  3,   "J"),
    ("Zii",  1,   "K"),
    ("ZW",   3,   "O"),
    ("PCR",  1,   "H"),
    ("PO",   1,   "K"),
    ("SPC",  1,   "H"),
    ("T5",   1,   "M")
]
print(f"DEBUG: [3] 設定讀取完畢，目標策略數: {len(STRATEGY_CONFIG)}")

# ==============================================================================

def find_file(directory, filename):
    target = os.path.join(directory, filename)
    if os.path.exists(target): return target
    files = glob.glob(os.path.join(directory, "**", filename), recursive=True)
    return files[0] if files else None

def kill_excel():
    try:
        os.system("taskkill /f /im excel.exe >nul 2>&1")
        time.sleep(1)
    except: pass

def main_execution():
    print("DEBUG: [4] 進入主程式邏輯...")
    kill_excel()
    
    excel_path = find_file(BASE_DIR, EXCEL_NAME)
    csv_path = find_file(CSV_DIR, CSV_NAME)

    if not excel_path:
        print(f"❌ 嚴重錯誤: 找不到 Excel 檔案!\n路徑: {BASE_DIR}")
        return
    
    if not csv_path:
        print(f"⚠️ 找不到舊 CSV，準備建立新檔案: {CSV_NAME}")
        csv_path = os.path.join(CSV_DIR, CSV_NAME)

    print(f"DEBUG: [5] 找到 Excel: {excel_path}")

    app = xw.App(visible=False, add_book=False)
    app.display_alerts = False
    all_data = {}
    success_count = 0

    try:
        print("DEBUG: [6] 正在開啟 Excel (這可能需要幾秒鐘)...")
        wb = app.books.open(excel_path, read_only=True, ignore_read_only_recommended=True)
        sheet_map = {s.name.strip().lower(): s for s in wb.sheets}
        print("DEBUG: [7] Excel 開啟成功，開始計算...")

        for strategy_name, manual_denom, num_col in STRATEGY_CONFIG:
            clean_name = strategy_name.strip()
            ws = sheet_map.get(clean_name.lower())
            
            if not ws:
                print(f"❌ 缺頁: [{clean_name}]")
                continue

            try:
                # 找最後一列
                last_row = ws.range(f"A{ws.cells.last_cell.row}").end('up').row
                if last_row < 4: 
                    print(f"⚠️  [{clean_name}] 資料不足")
                    continue
                
                # --- 直接使用您設定的數字 ---
                if manual_denom == 0:
                    print(f"❌ 錯誤: [{clean_name}] 分母設定為 0")
                    continue

                # --- 抓取損益並計算 ---
                dates = ws.range(f"A4:A{last_row}").value
                profits = ws.range(f"{num_col}4:{num_col}{last_row}").value
                
                series = {}
                for d, p in zip(dates, profits):
                    if isinstance(d, datetime.datetime): d = d.date()
                    if isinstance(d, datetime.date) and isinstance(p, (int, float)):
                        # 🔥 核心運算：損益 / 固定數字
                        series[d] = round(p / manual_denom, 4)
                
                if series:
                    all_data[clean_name] = series
                    print(f"✅ [{clean_name}] OK (除以 {manual_denom})")
                    success_count += 1
                else:
                    print(f"⚠️  [{clean_name}] 無有效數據")
            except Exception as e:
                print(f"❌ [{clean_name}] 發生錯誤: {e}")

        # --- 存檔 ---
        if all_data:
            print(f"DEBUG: [8] 數據抓取完成，正在寫入 CSV ({len(all_data)} 個策略)...")
            df = pd.DataFrame(all_data)
            df.index.name = "Date"
            df.sort_index(inplace=True)
            
            # 日期裁切
            if START_DATE:
                df = df[df.index >= START_DATE]
            
            df.fillna(0, inplace=True)
            df.reset_index(inplace=True)

            # 🔥 刪除最後一日資料
            if len(df) > 0:
                last_date = df.iloc[-1]['Date']
                print(f"✂️  自動刪除最後一日資料: {last_date}")
                df = df.iloc[:-1]

            # 排序欄位
            cols = ["Date"] + [c[0] for c in STRATEGY_CONFIG if c[0] in df.columns]
            df = df[cols]
            
            df.to_csv(csv_path, index=False, encoding='utf-8-sig')
            print(f"🎉 成功建立 CSV: {csv_path}")
        else:
            print("❌ 沒有任何數據被抓到！")

    except Exception as e:
        print(f"❌ 發生未知錯誤: {e}")
        import traceback
        traceback.print_exc()
    finally:
        try:
            wb.close()
            app.quit()
        except: pass

# ==============================================================================
# 🔥 啟動開關 (絕對不會少)
# ==============================================================================
if __name__ == "__main__":
    print("DEBUG: [START] 程式啟動確認！")
    main_execution()
    print("DEBUG: [END] 程式執行結束！")