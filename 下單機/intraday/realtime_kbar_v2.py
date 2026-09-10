# -*- coding: utf-8 -*-
"""
即時切K模組 v2 - 元大行情API (YUANTAQUOTE.YuantaQuoteCtrl.1)
================================================================
COM 綁定方式已改為與使用者驗證過的「查詢.py」一致的寫法：
    comtypes + windll.atl.AtlAxCreateControlEx（不是 win32com.client）
這點在 v1 版本猜錯了，元大行情API.pdf 文件本身也沒有寫清楚，
以下差異是從「查詢.py」的真實可動程式碼比對得知：

  1. 所有 On... event callback 的第一個參數是 `this`，不能省略
  2. OnGetMktAll 結尾多一個 `ReqType`（文件19個欄位 + this + ReqType）
  3. OnMktStatusChange 簽名是 (this, Status, Msg, ReqType)，文件只寫兩個參數
  4. AddMktReg 實際是 4 參數：(Symbol, Mode, ReqType, 0)，文件寫成 2 參數
  5. SetMktLogon 實際是 6 參數：(User, Pass, IP, PORT, 通道別, 0)，
     且「查詢.py」呼叫了兩次（port 80 通道1、port 82 通道2）—
     這兩個尾端參數的確切意義未經文件證實，目前僅依樣照做，
     不代表已理解其語意，若之後要精簡連線邏輯需另外找元大確認
  6. 不需要 pythoncom.PumpWaitingMessages()，改用
     self.root.update() + time.sleep() 的迴圈（ATL控制項掛在真實HWND下，
     tkinter 自己的 update() 就會處理 Windows 訊息）

⚠️ 仍未被任何驗證過的來源解決的兩個未知數（沿用交接摘要§四）：
   1. MatchTime 實際字串格式 —「查詢.py」完全沒用到 MatchTime，幫不上忙
   2. TolMatchQty 是否需要 ÷2 才對齊期交所逐筆檔案的雙邊計數規則
   兩者仍只能開盤後用診斷輸出實測確認，見 _parse_match_time() 與
   on_tick() 內的註解。
"""

import os
import csv
import time
import tkinter as tk
from datetime import datetime, time as dtime
from typing import Optional

from ctypes import byref, POINTER, windll
from comtypes import IUnknown, GUID
from comtypes.client import GetBestInterface, GetEvents

from strategy_common_v2 import Bar1Min as Bar
from strategies import S1State, S2State, S3State, S4State, EXIT_TIME as STRATEGY_EXIT_TIME
import strategies as strategies_module   # 需要這個才能動態覆寫strategies.EXIT_TIME（結算日用）
from persistence import load_or_create_state, save_state, setup_system_logger, state_file_path

windll.atl.AtlAxWinInit()

# ============================================================
# 設定 —— 帳密依需求寫死（這是要送單的自動化系統，不做互動式輸入）
# ⚠️ 裸奔提醒（使用者已知情，僅留紀錄）：
#    建議至少做到 (1) 檔案權限只留自己帳號可讀
#                (2) 若這支程式進版本控制，記得 .gitignore 排除
# ============================================================
USER_ID_FUT = "請填入身分證字號"
PASSWORD = "請填入密碼"
API_QUOTE_IP = "apiquote.yuantafutures.com.tw"   # 沿用「查詢.py」的位址
API_ORDER_IP = "api.yuantafutures.com.tw"        # Ord元件用，來源：查詢.py/Forder.py

# ============================================================
# 委託參數 —— 依《元大BToCAPI格式.pdf》文件+Forder.py驗證過的語法
# ============================================================
ORD_OFFSET_ENTRY = "0"   # 新倉 —— 使用者已跟元大確認：填'2'(當沖)會被強制沖銷，不要
ORD_OFFSET_EXIT = "1"    # 平倉
ORD_PRICE_TYPE = "M"    # 市價（使用者確認：下單時就用市價單）
ORD_COND = "I"          # IOC（沿用Forder.py的市價單慣例）
ORD_PRICE = ""           # 市價單不用填價格

TARGET_BHNO = "P00"       # 2026/09/09使用者指定寫死，不再從AccList自動抓第一筆
TARGET_ACNO = "2922633"

BAR_MINUTES = 1    # 2026/09/09改成1分K，依《S1_S4_下單機開發規格書》2.1節
SESSION_START = dtime(8, 45, 0)
SESSION_END = dtime(13, 45, 0)     # 含（13:45:00 集合競價併入13:44那根，1分K下的最後一根）
LOG_DIR = "./realtime_logs"
DIAG_TICK_LIMIT = 50               # 開盤後前N筆tick印出原始值供人工核對

# ============================================================
# 四策略參數 —— 依《S1_S4_下單機開發規格書》(2026-09)定案值
# ============================================================
S1_QTY = 2
S2_QTY = 2
S3_QTY = 1
S4_QTY = 1
# 最壞情況（四策略同一天同方向全數觸發）：S1+S2+S3+S4 = 6口同時在場

S1_BREAKOUT_PCT = 0.004
S1_VOL_MULT = 4.5
S1_VOL_WINDOW_MIN = 100
S1_ENTRY_TIME = dtime(9, 15, 0)

S2_GAP_PCT = 0.007
S2_VOL_MULT = 4.5
S2_VOL_WINDOW_MIN = 150
S2_ENTRY_TIME = dtime(9, 15, 0)   # 同時也是08:45~09:15箱體的結束時間

S3_VOL_MULT = 4.5
S3_VOL_WINDOW_MIN = 75
S3_BOX_END = dtime(8, 55, 0)      # 箱體僅08:45~08:55（10分鐘）

S4_VOL_MULT = 4.5
S4_VOL_WINDOW_MIN = 150
S4_BOX_START = dtime(9, 15, 0)
S4_BOX_END = dtime(10, 30, 0)
S4_ENTRY_START = dtime(10, 30, 0)
S4_ENTRY_END = dtime(13, 30, 0)

# ============================================================
# 下單模式開關 —— 之後接上SendOrderF時，統一用這個開關控制
# ⚠️ 這是唯一控制「會不會真的送單」的地方，寫成獨立常數方便一眼看到，
#    不要散落在程式碼各處各自判斷。預設SAFE_MODE=True（只印出、不送單），
#    要真的送單必須手動把這裡改成False，不能用參數/環境變數之類的方式
#    意外被改動——這種等級的開關，改動應該要明顯、留痕跡。
# ============================================================
SAFE_MODE = True   # True=只印出「本來要下單」不送單 / False=真的呼叫SendOrderF

os.makedirs(LOG_DIR, exist_ok=True)


# ============================================================
# 近月合約代碼自動產生 —— 前月法，結算日不換月
# ⚠️ 這裡不再沿用「查詢.py」的 day>18 近似規則。那支程式只是看行情，
#    差一兩天沒關係；這支要送單，差一天就會抓錯合約下錯單，不能將就。
#
# 正確規則（交接摘要§二已強調過的既有結論，且已用網路搜尋確認
# 「每個月第三個星期三為月結算日」這條在2026年仍成立，來源：udn.com）：
#   - 結算日「當天」仍算前月合約，不換月
#   - 結算日「隔天」才換到下月合約
#
# ⚠️ 唯一無法用程式自動處理的例外：若當月第三個星期三剛好卡到國定假日，
#    期交所會另行公告調整結算日，這個程式算不出來，需要人工對照期交所
#    月曆確認（尤其是過年、國慶前後的月份）。
# ============================================================
def get_third_wednesday(year: int, month: int):
    import calendar
    c = calendar.Calendar()
    wednesdays = [d for d in c.itermonthdates(year, month)
                  if d.month == month and d.weekday() == 2]  # 2 = Wednesday
    return wednesdays[2]


def get_current_fut_symbol(prefix: str = "TMF", today=None) -> str:
    today = today or datetime.now().date()
    year, month = today.year, today.month
    settlement = get_third_wednesday(year, month)

    if today <= settlement:
        target_year, target_month = year, month
    else:
        target_month = month + 1
        target_year = year
        if target_month > 12:
            target_month = 1
            target_year += 1

    month_char = "ABCDEFGHIJKL"[target_month - 1]
    year_char = str(target_year)[-1]
    return f"{prefix}{month_char}{year_char}"


SYMBOL = get_current_fut_symbol("TXF")   # 2026/09/09再次更正：策略判斷(箱體/VWAP/開盤價/
                                          # 停損/跳空)全部盯大台，這個變數代表「監控/註冊報價」的商品
TRADE_SYMBOL = get_current_fut_symbol("TMF")   # 實際下單交易的商品是微台，跟判斷邏輯脫鉤
                                                 # ⚠️ 微台完全不用訂閱報價，只在真正送單時用到這個代碼


# ============================================================
# 前一交易日收盤價 —— S2缺口判斷需要
# ⚠️ 依賴前一天自己存的CSV完不完整；如果前一天最後一段剛好斷線，
#    讀到的可能不是真正13:40那根，而是斷線前的殘缺資料。
#    另外準備 RefPri（來自OnGetMktAll，開盤就有）當作交叉比對，
#    但RefPri是否精確等於「前一交易日日盤收盤價」尚未驗證過，
#    下次開盤務必人工比對這兩個數字是否一致。
# ============================================================
def find_previous_day_close(symbol: str, today=None) -> Optional[float]:
    today = today or datetime.now().date()
    candidates = []
    if not os.path.isdir(LOG_DIR):
        return None
    for fname in os.listdir(LOG_DIR):
        prefix = f"{symbol}_"
        suffix = "_realtime.csv"
        if fname.startswith(prefix) and fname.endswith(suffix):
            date_str = fname[len(prefix):-len(suffix)]
            try:
                file_date = datetime.strptime(date_str, "%Y%m%d").date()
            except ValueError:
                continue
            if file_date < today:
                candidates.append((file_date, fname))
    if not candidates:
        print(f"[前日收盤] 找不到 {symbol} 更早日期的CSV，無法取得前日收盤價")
        return None
    candidates.sort()
    latest_date, latest_fname = candidates[-1]
    path = os.path.join(LOG_DIR, latest_fname)
    try:
        with open(path, encoding="utf-8-sig") as f:
            rows = list(csv.DictReader(f))
        if not rows:
            print(f"[前日收盤] {latest_fname} 是空檔案")
            return None
        last_row = rows[-1]
        close = float(last_row["close"])
        last_bucket = last_row["bucket_start"]
        print(f"[前日收盤] 讀取 {latest_fname}，最後一根bucket_start={last_bucket}，"
              f"close={close}")
        if last_bucket != "13:40":
            print(f"[前日收盤][警告] 最後一根不是13:40，前一天資料可能不完整，"
                  f"這個收盤價可能不可靠，務必人工核對")
        return close
    except Exception as e:
        print(f"[前日收盤] 讀取失敗：{e}")
        return None


PREV_DAY_CLOSE = find_previous_day_close(SYMBOL)


# ============================================================
# 結算日特殊處理 —— 2026/09/09新增
# ⚠️ 交接摘要裡本來就記過「前月法，結算日不換月（結算日近月13:30停止交易，
#    該日僅57根，屬正常）」，但這個規則之前只用在R回測的資料清洗，
#    從沒被搬進即時系統。結算日當天近月合約13:30就停止交易，如果還用平常
#    日的SESSION_END(13:45)/EXIT_TIME(13:43:50)，會導致：
#    (1) 13:30後根本沒有新tick進來，K棒/策略邏輯會卡在那裡，
#        原本13:45才觸發的「強制寫出最後一根K棒」機制也不會被觸發
#    (2) 13:43:50的強制平倉，同樣因為沒有tick進來而永遠不會執行，
#        部位可能就這樣掛著沒平倉
#    這裡偵測今天是不是結算日，是的話把收盤時間跟強制平倉時間都提前。
# ============================================================
_settlement_day = get_third_wednesday(datetime.now().year, datetime.now().month)
IS_SETTLEMENT_DAY = (datetime.now().date() == _settlement_day)

if IS_SETTLEMENT_DAY:
    SESSION_END = dtime(13, 30, 0)
    strategies_module.EXIT_TIME = dtime(13, 28, 50)   # 提前平倉緩衝，比照平常日70秒的精神
    print(f"[結算日] 今天({datetime.now().date()})是本月結算日，"
          f"近月合約13:30提前停止交易，收盤/強制平倉時間已對應調整為13:30/13:28:50")
else:
    print(f"[結算日] 今天({datetime.now().date()})不是結算日"
          f"（本月結算日為{_settlement_day}），沿用平常日13:45/13:43:50")


# ============================================================
# 統一送單進出口 —— 四策略共用，由SAFE_MODE控制
# ============================================================
def place_order(strategy_name: str, direction: str, qty: int, price: float, event_type: str) -> str:
    """
    回傳值是委託單號。
    ⚠️ 2026/09/09：策略判斷全部盯大台(SYMBOL)，但實際下單商品固定是TRADE_SYMBOL(微台)，
       兩者是脫鉤的——這裡收到的price是大台觸發價，不是微台的成交價，只能當作
       「觸發當下大台在哪個位置」的紀錄，不能當作微台的委託價；因為是市價單，
       委託價欄位本身送出時就是空字串，price主要拿去記錄在CSV跟log裡供事後檢討。

    direction/event_type的組合決定買賣別(B/S)：
      entry + long  -> B（買進開多）
      entry + short -> S（賣出開空）
      exit  + long  -> S（賣出平掉多單）
      exit  + short -> B（買進平掉空單）
    """
    is_buy = (direction == "long") == (event_type == "entry")
    act = "B" if is_buy else "S"
    act_cn = "買" if is_buy else "賣"

    if SAFE_MODE:
        print(f"[模擬模式] 不送單（SAFE_MODE=True），"
              f"若為真實模式將送出：[{strategy_name}] {event_type} {act_cn}({act}) "
              f"{TRADE_SYMBOL} {qty}口 市價單（觸發時大台價位={price}）")
        return f"SIM_{strategy_name}_{event_type}_{datetime.now():%H%M%S}"

    if _ord_com is None or _target_account is None:
        print(f"[真實下單失敗] ⚠️ 下單元件尚未登入成功或帳戶尚未選定，"
              f"無法送出：[{strategy_name}] {event_type} {act_cn} {qty}口")
        return f"FAILED_NOT_READY_{strategy_name}_{event_type}"

    try:
        bhno = _target_account["bhno"]
        acno = _target_account["acno"]
        suba = _target_account["suba"]
        offset = ORD_OFFSET_ENTRY if event_type == "entry" else ORD_OFFSET_EXIT
        ret = _ord_com.SendOrderF(
            "01", "0", bhno, acno, suba, "",
            act, TRADE_SYMBOL, ORD_PRICE, str(qty),
            offset, ORD_PRICE_TYPE, ORD_COND, "", ""
        )
        print(f"[真實下單] [{strategy_name}] {event_type} {act_cn}({act}) "
              f"{TRADE_SYMBOL} {qty}口 市價單 帳戶{acno} -> API回傳：{ret}")
        # ret格式依《元大BToCAPI格式.pdf》：委託流水號|錯誤代碼|錯誤訊息，用水管符號分隔
        order_no = str(ret).split('|')[0] if ret else f"UNKNOWN_{strategy_name}_{event_type}"
        return order_no
    except Exception as e:
        print(f"[真實下單例外] [{strategy_name}] {event_type} 送單失敗：{e}")
        return f"EXCEPTION_{strategy_name}_{event_type}"



class KBarBuilder:
    """
    分K規則（沿用交接摘要§三驗證規則，改成1分K版本）：
    - 左閉右開，以起始時間標記（08:45起，每1分鐘一根）
    - 13:45:00 收盤集合競價併入 13:44 那根（1分K下的最後一根）
    - 盤前（TolMatchQty == -1）與盤後一律不切K
    - 同一批tick同步餵給四支策略狀態機（B案：不等K棒收完才判斷）
    """

    def __init__(self, bar_minutes: int = BAR_MINUTES):
        self.bar_minutes = bar_minutes
        self.current_bucket: Optional[dtime] = None
        self.current_bar: Optional[Bar] = None
        self.finished_bars = []
        self.last_tol_match_qty: Optional[int] = None
        self.diag_count = 0
        self.session_closed_flushed = False

        # ------------------------------------------------------------
        # 四策略狀態機整合：JSON狀態檔/LOG都在這裡初始化，
        # 斷線重開機時 load_or_create_state 會自動讀回今天已存在的狀態。
        # ------------------------------------------------------------
        self.logger = setup_system_logger(LOG_DIR)

        # 2026/09/09新增：偵測「今天是不是重開機」——用JSON狀態檔案在讀取之前
        # 存不存在來判斷。存在=今天已經跑過(不管是正常運作中被關掉、還是當機)，
        # 現在是重開；不存在=今天第一次啟動。使用者決定：只要是重開機，
        # 不管什麼原因，統計資料(箱體/均量/VWAP)都無法恢復，一律當天不再開
        # 新倉，跟資料缺口用同一個開關，邏輯統一——已有部位不受影響，
        # 停損/強制平倉照常監控。
        is_restart_today = os.path.exists(state_file_path(LOG_DIR))

        self.state = load_or_create_state(LOG_DIR, ["S1", "S2", "S3", "S4"])

        if is_restart_today and not self.state.get("data_gap_today"):
            self.state["data_gap_today"] = True
            save_state(LOG_DIR, self.state)
            print("[重開機保護] 偵測到今天已經有狀態檔(重開機)，統計資料(箱體/均量/"
                  "VWAP)無法恢復，四策略今天起停止開新倉。已有部位不受影響，"
                  "停損/強制平倉照常監控。")
            self.logger.warning("偵測到重開機，data_gap_today設為True，今天停止開新倉")
        elif self.state.get("data_gap_today"):
            self.logger.warning("狀態檔顯示今天已經發生過資料缺口，新倉維持關閉")

        self.s1 = S1State(breakout_pct=S1_BREAKOUT_PCT, entry_time=S1_ENTRY_TIME,
                           qty=S1_QTY, vol_mult=S1_VOL_MULT, vol_window_min=S1_VOL_WINDOW_MIN,
                           log_dir=LOG_DIR, state=self.state, symbol=TRADE_SYMBOL,
                           place_order_fn=place_order, logger=self.logger)
        self.s2 = S2State(gap_pct=S2_GAP_PCT, entry_time=S2_ENTRY_TIME, prev_close=PREV_DAY_CLOSE,
                           qty=S2_QTY, vol_mult=S2_VOL_MULT, vol_window_min=S2_VOL_WINDOW_MIN,
                           log_dir=LOG_DIR, state=self.state, symbol=TRADE_SYMBOL,
                           place_order_fn=place_order, logger=self.logger)
        self.s3 = S3State(qty=S3_QTY, vol_mult=S3_VOL_MULT, vol_window_min=S3_VOL_WINDOW_MIN,
                           box_end=S3_BOX_END,
                           log_dir=LOG_DIR, state=self.state, symbol=TRADE_SYMBOL,
                           place_order_fn=place_order, logger=self.logger)
        self.s4 = S4State(box_start=S4_BOX_START, box_end=S4_BOX_END,
                           entry_start=S4_ENTRY_START, entry_end=S4_ENTRY_END,
                           qty=S4_QTY, vol_mult=S4_VOL_MULT, vol_window_min=S4_VOL_WINDOW_MIN,
                           log_dir=LOG_DIR, state=self.state, symbol=TRADE_SYMBOL,
                           place_order_fn=place_order, logger=self.logger)
        self.strategies = [self.s1, self.s2, self.s3, self.s4]

    def bucket_for(self, t: dtime) -> Optional[dtime]:
        if t < SESSION_START or t > SESSION_END:
            return None
        if t == SESSION_END:
            # 13:45:00 集合競價併入最後一根1分K（13:44那根）
            last_start = (SESSION_END.hour * 60 + SESSION_END.minute) - self.bar_minutes
            h, m = divmod(last_start, 60)
            return dtime(h, m, 0)
        start_total = SESSION_START.hour * 60 + SESSION_START.minute
        t_total = t.hour * 60 + t.minute
        elapsed = t_total - start_total
        bucket_idx = elapsed // self.bar_minutes
        bucket_total = start_total + bucket_idx * self.bar_minutes
        h, m = divmod(bucket_total, 60)
        return dtime(h, m, 0)

    def on_tick(self, match_time_raw: str, match_price_raw: str,
                tol_match_qty_raw: str, match_qty_raw: str, open_pri_raw: str = ""):
        try:
            tol_qty = int(tol_match_qty_raw)
        except (TypeError, ValueError):
            return
        if tol_qty == -1:   # 已確認：盤前資料
            return

        t = self._parse_match_time(match_time_raw)
        if t is None:
            return

        # 成交量：用 TolMatchQty 累計差值（不用 MatchQty 累加）
        # 已用夜盤實測資料驗證：delta 與同筆 MatchQty 逐一對得上，
        # 代表即時API的 TolMatchQty 是單邊累加，不需要像期交所逐筆檔案那樣÷2。
        is_new_tick = self.last_tol_match_qty is None or tol_qty != self.last_tol_match_qty
        if self.last_tol_match_qty is None:
            delta_qty = 0
        else:
            delta_qty = tol_qty - self.last_tol_match_qty
            if delta_qty < 0:
                print(f"[警告] TolMatchQty 倒退：{self.last_tol_match_qty} -> "
                      f"{tol_qty}，重置基準（可能跨日或連線重置）")
                delta_qty = 0
        self.last_tol_match_qty = tol_qty

        if is_new_tick and self.diag_count < DIAG_TICK_LIMIT:
            print(f"[診斷] raw_time={match_time_raw!r} parsed={t} "
                  f"tol_qty={tol_qty} delta={delta_qty} match_qty={match_qty_raw!r} "
                  f"price={match_price_raw!r}")
            self.diag_count += 1

        try:
            price = float(match_price_raw)
        except (TypeError, ValueError):
            return

        # 開盤價：直接用元大官方的OpenPri，不用再等第一根K棒收盤才推算
        # （2026/09/10發現：用K棒推算，如果程式沒有剛好在08:45連上，
        #  會抓到「程式開始收tick後第一筆」的價格，不是真正開盤價；
        #  就算準時開，08:45那一刻連線不穩一樣會抓錯，且沒有警告）
        try:
            open_price = float(open_pri_raw)
            if open_price > 0:
                for strat in self.strategies:
                    strat.set_day_open(open_price)
        except (TypeError, ValueError):
            pass

        # 四策略訊號判斷：跟切K棒共用同一筆tick，不等K棒收完（B案即時觸發）
        for strat in self.strategies:
            strat.on_tick(t, price, delta_qty)

        bucket = self.bucket_for(t)
        if bucket is None:
            # t >= SESSION_END(13:45)時代表盤後不切K，但這樣一來最後一根K棒
            # 永遠不會因為「換下一個bucket」被觸發寫出，會卡在記憶體裡遺失。
            # 這裡在偵測到收盤後的第一筆tick時，強制把手上還沒寫出的
            # 最後一根K棒flush掉，只做一次。
            if not self.session_closed_flushed and self.current_bar is not None:
                self.finished_bars.append(self.current_bar)
                self._flush_bar(self.current_bar)
                for strat in self.strategies:
                    strat.on_bar_close(self.current_bar)
                print(f"[收盤] 已強制寫出最後一根K棒：{self.current_bar.bucket_start.strftime('%H:%M')}")
                self.session_closed_flushed = True
            return

        if self.current_bucket != bucket:
            if self.current_bar is not None:
                self.finished_bars.append(self.current_bar)
                self._flush_bar(self.current_bar)
                for strat in self.strategies:
                    strat.on_bar_close(self.current_bar)

                # 偵測缺根（斷線交界處資料不完整）：新舊bucket時間差理論上應該
                # 剛好是bar_minutes，差更多代表中間缺了至少一根。一旦偵測到，
                # 整天永久關閉全部四策略的新倉，不管時間、不管之後有沒有恢復
                # 正常——單向開關，只會關不會重開，寫進共用的JSON狀態檔，
                # 四支策略下次檢查entry時都會讀到同一個旗標。
                prev_total = self.current_bucket.hour * 60 + self.current_bucket.minute
                new_total = bucket.hour * 60 + bucket.minute
                if new_total - prev_total > self.bar_minutes:
                    if not self.state.get("data_gap_today"):
                        print(f"[資料缺口] {self.current_bucket.strftime('%H:%M')} -> "
                              f"{bucket.strftime('%H:%M')} 中間缺K棒，四策略今天起停止開新倉")
                        self.logger.warning(f"資料缺口 {self.current_bucket} -> {bucket}，"
                                             f"四策略今天起停止開新倉")
                    self.state["data_gap_today"] = True
                    save_state(LOG_DIR, self.state)

            self.current_bucket = bucket
            self.current_bar = Bar(bucket_start=bucket)

        self.current_bar.update(price, delta_qty)

    def _parse_match_time(self, raw: str) -> Optional[dtime]:
        """
        [待驗證] 假設前6碼為 HHMMSS，忽略其餘碼數。
        「查詢.py」沒用到這欄位，無從比對，開盤後務必看診斷輸出核對。
        """
        if not raw:
            return None
        raw = raw.strip()
        if len(raw) >= 6:
            try:
                h = int(raw[0:2])
                m = int(raw[2:4])
                s = int(raw[4:6])
                return dtime(h, m, s)
            except ValueError:
                return None
        return None

    def _flush_bar(self, bar: Bar):
        path = os.path.join(LOG_DIR, f"{SYMBOL}_{datetime.now():%Y%m%d}_realtime.csv")
        write_header = not os.path.exists(path)
        with open(path, "a", newline="", encoding="utf-8-sig") as f:
            w = csv.writer(f)
            if write_header:
                w.writerow(["bucket_start", "open", "high", "low", "close", "volume", "n_ticks"])
            w.writerow([bar.bucket_start.strftime("%H:%M"), bar.open, bar.high,
                        bar.low, bar.close, bar.volume, bar.n_ticks])

    def force_flush_at_close(self):
        """
        2026/09/09新增：保險機制，用主迴圈的本機時間定期檢查，不依賴tick觸發。
        原本「強制寫出最後一根K棒」只在收到一筆時間晚於SESSION_END的tick時才
        會觸發，但如果交易在收盤那一刻就完全停止、之後再也沒有任何tick進來
        （結算日13:30整這種情況特別可能發生），這個機制永遠不會被觸發，
        最後一根K棒會遺失。改成不管有沒有tick，主迴圈定期呼叫這個方法，
        只要本機時間已經過了收盤，就強制寫出，不等tick。
        """
        if self.session_closed_flushed or self.current_bar is None:
            return
        now_t = datetime.now().time()
        if now_t > SESSION_END:
            self.finished_bars.append(self.current_bar)
            self._flush_bar(self.current_bar)
            for strat in self.strategies:
                strat.on_bar_close(self.current_bar)
            print(f"[收盤-保險機制] 本機時間已過收盤，強制寫出最後一根K棒："
                  f"{self.current_bar.bucket_start.strftime('%H:%M')}（未依賴tick觸發）")
            self.logger.info(f"保險機制強制flush最後一根K棒 {self.current_bar.bucket_start}")
            self.session_closed_flushed = True

    def force_exit_if_needed(self):
        """
        2026/09/09新增：跟force_flush_at_close同樣理由的保險機制，而且這個更重要——
        原本的強制平倉(13:43:50或結算日13:28:50)完全依賴tick觸發(strategy.on_tick
        裡的t>=EXIT_TIME判斷)，如果那個時間點之後剛好沒有任何tick進來，
        持倉可能就這樣掛著，完全沒有被平掉，比K棒遺失嚴重得多。
        用本機時間當保險，不管有沒有tick，只要偵測到已經過了強制平倉時間、
        還有策略持倉未平，就用「目前已知最新價格」強制平倉。
        """
        now_t = datetime.now().time()
        if now_t < strategies_module.EXIT_TIME:
            return
        fallback_price = self.current_bar.close if self.current_bar is not None else PREV_DAY_CLOSE
        if fallback_price is None:
            return  # 連個參考價都沒有，沒辦法強制平倉，只能等tick自然觸發
        for strat in self.strategies:
            has_position = getattr(strat, "position", None) is not None
            if has_position:
                print(f"[平倉-保險機制] {strat.name} 本機時間已過強制平倉時間但仍有持倉，"
                      f"用最新已知價格{fallback_price}強制平倉（未依賴tick觸發）")
                self.logger.warning(f"[{strat.name}] 保險機制強制平倉，"
                                     f"price={fallback_price}（非即時tick價格，是最後已知價）")
                strat._exit(fallback_price, now_t, "本機時間保險機制強制平倉(無tick觸發)")


# ============================================================
# COM 事件接收 —— 綁定方式照抄「查詢.py」驗證過的寫法
# ============================================================
# ============================================================
# 下單元件全域狀態 —— place_order()是模組層級函式(不是方法)，
# 要能送單需要拿到Ord COM物件跟目標帳戶資訊，用模組全域變數保存，
# 由YuantaOrdEvents.OnLogonS在登入成功時設定。
# ============================================================
_ord_com = None            # Ord COM物件本身
_target_account = None     # dict: {"bhno":..., "acno":..., "suba":...}
_account_list_raw = []     # 除錯用，保留登入回傳的完整帳號清單


class YuantaOrdEvents:
    """
    下單元件事件 —— 語法照抄查詢.py/Forder.py驗證過的寫法。
    OnLogonS回傳的AccList格式："市場別-分公司-帳號-子帳號-姓名;..."（分號分隔多筆）
    """
    def __init__(self, parent):
        self.parent = parent

    def OnLogonS(self, this, TLinkStatus, AccList, Casq, Cast):
        global _ord_com, _target_account, _account_list_raw
        if str(TLinkStatus) == "2":
            raw = str(AccList)
            accounts = [a for a in raw.split(';') if a]
            _account_list_raw = accounts
            print(f"[下單] 登入成功，帳號清單：{accounts}")

            # 2026/09/09 改成比對指定帳號(TARGET_BHNO/TARGET_ACNO)，
            # 不再自動抓清單第一筆——使用者已明確指定要用哪個帳戶。
            matched = None
            for acc in accounts:
                parts = acc.split('-')
                if len(parts) >= 3 and parts[1] == TARGET_BHNO and parts[2] == TARGET_ACNO:
                    matched = parts
                    break
            if matched:
                _target_account = {"bhno": matched[1], "acno": matched[2],
                                    "suba": matched[3] if len(matched) > 3 else ""}
                print(f"[下單] 已選定目標帳戶：分公司={_target_account['bhno']} "
                      f"帳號={_target_account['acno']} 子帳={_target_account['suba']}")
            else:
                print(f"[下單][錯誤] 登入回傳的帳號清單裡，找不到指定帳戶"
                      f"（分公司={TARGET_BHNO} 帳號={TARGET_ACNO}）！"
                      f"下單會持續失敗直到這個問題解決，請確認TARGET_BHNO/TARGET_ACNO設定"
                      f"是否正確，或這個帳戶今天是否真的有登入。")
            _ord_com = self.parent.ord
        else:
            print(f"[下單] 登入失敗 TLinkStatus={TLinkStatus}")

    def OnOrdRptF(self, *args):
        # 簽名未經文件證實，用*args防止參數數量不符時崩潰（沿用Forder.py的防禦寫法）
        print(f"[委託回報] {args}")

    def OnOrdMatF(self, *args):
        # 成交回報，簽名同樣未經逐一驗證，先印出原始內容，
        # 之後要精確解析欄位時再依實際印出的內容反推位置
        print(f"[成交回報] {args}")


class YuantaQuoteEvents:
    def __init__(self, parent):
        self.parent = parent

    def OnMktStatusChange(self, this, Status, Msg, ReqType):
        print(f"[連線狀態] Status={Status} Msg={Msg} ReqType={ReqType}")
        if self.parent.circuit_broken:
            return
        key = str(ReqType)
        if str(Status) == "2":
            self.parent.login_retry_count[key] = 0
            self.parent.reg_retry_count[key] = 0
            self._register(ReqType)
        elif str(Status) in ("-2", "-1"):
            if self._trip_circuit_if_needed():
                return
            count = self.parent.login_retry_count.get(key, 0) + 1
            self.parent.login_retry_count[key] = count
            if key == "1" and count <= 8:
                print(f"[登入重試 ReqType={ReqType} {count}/8] "
                      f"Status={Status}({Msg})，1.5秒後重新登入")
                self.parent.root.after(1500, lambda: self.parent.relogin(ReqType))
            elif key == "1":
                print(f"[登入放棄 ReqType={ReqType}] 已重試8次仍失敗，"
                      f"建議手動重開程式，或找元大確認帳號連線狀態。")

    def _trip_circuit_if_needed(self) -> bool:
        """
        ⚠️ 2026/09/05 新增：總量煞車。
        今天實測發現，登入重試(port80)跟註冊重試(port82)兩條線同時
        頻繁重試時，觸發了 Fatal Python error(GIL相關的直譯器崩潰)，
        懷疑是短時間內COM呼叫太密集、互相疊加造成的。
        不管是哪一種重試，全部加總只要在這次執行期間超過15次，
        就整個停止重試(不代表程式關閉，只是不再自動嘗試連線)，
        改成印出明確訊息要求手動重開，比讓它繼續瘋狂重試更安全。
        這不保證能完全避免那個崩潰(底層GIL問題很難遠端百分之百診斷)，
        但大幅降低短時間內COM呼叫的密集度，是目前能做的最直接緩解。
        """
        self.parent.total_failure_count += 1
        if self.parent.total_failure_count > 15:
            self.parent.circuit_broken = True
            print("=" * 50)
            print("[全域煞車] 短時間內連線/註冊失敗總次數過多，已停止自動重試。")
            print("這是為了避免頻繁重試觸發的底層錯誤，請手動關閉並重新執行本程式。")
            print("=" * 50)
            return True
        return False

    def _register(self, req_type):
        try:
            self.parent.quote.AddMktReg(self.parent.symbol, 4, int(req_type), 0)
            print(f"[註冊] 已送出 {self.parent.symbol} 註冊要求 (ReqType={req_type})")
        except Exception as e:
            print(f"[註冊失敗] {e}")

    def OnRegError(self, *args):
        # 簽名未經文件證實，用*args防止參數數量不符時崩潰。
        # 依實測比對PDF的RegErrCode表：(Symbol, Mode, ErrorCode, ReqType)
        # ErrorCode: 1=商品錯誤 2=模式錯誤 3=連線未完成
        print(f"[註冊錯誤事件] {args}")
        if self.parent.circuit_broken:
            return
        if len(args) >= 4:
            symbol, mode, err_code, req_type = args[0], args[1], args[2], args[3]
            if str(err_code) == "3":
                if self._trip_circuit_if_needed():
                    return
                key = str(req_type)
                count = self.parent.reg_retry_count.get(key, 0) + 1
                self.parent.reg_retry_count[key] = count
                if count <= 5:
                    # ⚠️ 2026/09/05 改回用 root.after 延遲重試（不是立即同步呼叫），
                    # 降低短時間內COM呼叫過於密集的風險，跟登入重試的節奏錯開，
                    # 避免兩條線的重試同時擠在同一瞬間發生。
                    print(f"[註冊重試 ReqType={req_type} {count}/5] "
                          f"ErrorCode=3(連線未完成)，0.8秒後重試")
                    self.parent.root.after(800, lambda: self._register(req_type))
                else:
                    print("=" * 50)
                    print(f"[註冊放棄 ReqType={req_type}] 已重試5次仍失敗。")
                    print("=" * 50)

    def OnGetMktAll(self, this, symbol, RefPri, OpenPri, HighPri, LowPri, UpPri, DnPri,
                     MatchTime, MatchPri, MatchQty, TolMatchQty,
                     BestBuyQty, BestBuyPri, BestSellQty, BestSellPri,
                     FDBPri, FDBQty, FDSPri, FDSQty, ReqType):
        try:
            symbol = str(symbol).strip()
            if symbol != self.parent.symbol:
                return

            # ⚠️ 2026/09/04 新增：RefPri(參考價) vs 前一天CSV算出來的收盤價
            # 只印一次做交叉比對，尚未驗證兩者是否精確相等，需人工核對。
            if not self.parent.ref_pri_checked:
                try:
                    ref_pri_val = float(RefPri)
                    if ref_pri_val > 0:
                        self.parent.ref_pri_checked = True
                        if PREV_DAY_CLOSE is not None:
                            diff = ref_pri_val - PREV_DAY_CLOSE
                            print(f"[前日收盤比對] RefPri={ref_pri_val} vs "
                                  f"CSV前日收盤={PREV_DAY_CLOSE} 差={diff:+.1f} "
                                  f"{'（一致）' if abs(diff) < 0.01 else '（不一致，需人工確認原因）'}")
                        else:
                            print(f"[前日收盤比對] RefPri={ref_pri_val}（沒有CSV前日收盤可比對）")
                except (TypeError, ValueError):
                    pass

            self.parent.kbar_builder.on_tick(MatchTime, MatchPri, TolMatchQty, MatchQty, OpenPri)
        except Exception as e:
            print(f"[OnGetMktAll例外] {e}")


class RealtimeQuoteReceiver:
    def __init__(self, symbol: str):
        self.symbol = symbol
        self.kbar_builder = KBarBuilder()
        self.is_running = True
        self.reg_retry_count = {}  # per-channel(ReqType)各自獨立計數
        self.ref_pri_checked = False   # RefPri vs 前日收盤 只比對一次
        self.login_retry_count = {}  # per-channel(ReqType)登入重試計數
        self._login_ports = {"1": "80", "2": "82"}  # ReqType對照的port
        self.total_failure_count = 0  # 全域煞車用的總失敗次數
        self.circuit_broken = False   # 一旦超過門檻就永久停止重試

        # 沿用查詢.py：用一個極小的 tkinter frame 取得 HWND 供 AtlAx 掛載
        self.root = tk.Tk()
        self.root.attributes("-toolwindow", True)
        self.root.geometry("1x1+-2000+-2000")   # 移到螢幕外，但不能用 withdraw()
        self.com_cage = tk.Frame(self.root, width=1, height=1)
        self.com_cage.pack()
        self.com_cage.update_idletasks()
        self.hwnd = self.com_cage.winfo_id()

        self.root.after(500, self._init_ord)

    def _init_ord(self):
        try:
            Iwindow_o, Icontrol_o, Ievent_o = POINTER(IUnknown)(), POINTER(IUnknown)(), POINTER(IUnknown)()
            windll.atl.AtlAxCreateControlEx(
                "Yuanta.YuantaOrdCtrl.1", self.hwnd, None,
                byref(Iwindow_o), byref(Icontrol_o), byref(GUID()), Ievent_o
            )
            self.ord = GetBestInterface(Icontrol_o)
            self.ord_events = GetEvents(self.ord, YuantaOrdEvents(self))
            self.ord.SetFutOrdConnection(USER_ID_FUT, PASSWORD, API_ORDER_IP, '80')
            print("[初始化] 已送出 SetFutOrdConnection")
        except Exception as e:
            print(f"[Ord初始化失敗] {e}")
        self.root.after(3000, self._init_quote)

    def _init_quote(self):
        Iwindow, Icontrol, Ievent = POINTER(IUnknown)(), POINTER(IUnknown)(), POINTER(IUnknown)()
        windll.atl.AtlAxCreateControlEx(
            "YUANTAQUOTE.YuantaQuoteCtrl.1", self.hwnd, None,
            byref(Iwindow), byref(Icontrol), byref(GUID()), Ievent
        )
        self.quote = GetBestInterface(Icontrol)
        self.quote_events = GetEvents(self.quote, YuantaQuoteEvents(self))
        self.quote.SetMktLogon(USER_ID_FUT, PASSWORD, API_QUOTE_IP, '80', 1, 0)
        self.quote.SetMktLogon(USER_ID_FUT, PASSWORD, API_QUOTE_IP, '82', 2, 0)
        print("[初始化] 已送出 SetMktLogon")

    def relogin(self, req_type):
        """針對單一通道重新登入（登入失敗Status=-2時使用）"""
        port = self._login_ports.get(str(req_type))
        if port is None:
            return
        try:
            self.quote.SetMktLogon(USER_ID_FUT, PASSWORD, API_QUOTE_IP, port, int(req_type), 0)
            print(f"[重新登入] 已送出 ReqType={req_type} (port={port})")
        except Exception as e:
            print(f"[重新登入失敗] {e}")

    def run(self):
        # ⚠️ 2026/09/04 新增心跳輸出：使用者今天多次誤以為「畫面安靜=程式壞了」，
        # 實際上前50筆診斷輸出印完之後、沒觸發S1訊號時，程式本來就會安靜，
        # 這種安靜跟真的斷線卡死，畫面上看起來一模一樣，容易搞混、製造焦慮。
        # 改成固定每60秒印一行心跳，不管有沒有事發生都會跳出來，
        # 讓「安靜」跟「卡死」看得出差別。
        last_heartbeat = time.time()

        while self.is_running:
            try:
                if not self.root.winfo_exists():
                    break
                self.root.update()

                now = time.time()
                if now - last_heartbeat >= 300:
                    bar = self.kbar_builder.current_bar
                    price_str = f"{bar.close}" if bar is not None else "尚無資料"
                    ticks_str = f"{bar.n_ticks}" if bar is not None else "0"
                    print(f"[心跳] {datetime.now():%H:%M:%S} 程式運行中，"
                          f"最新價格={price_str}，本根K棒已收{ticks_str}筆tick")
                    last_heartbeat = now

                # 2026/09/09新增：收盤保險機制，不依賴tick觸發，每次迴圈都檢查
                # （成本很低，只是幾個時間比較，不會拖慢主迴圈）
                self.kbar_builder.force_exit_if_needed()
                self.kbar_builder.force_flush_at_close()
            except tk.TclError:
                break
            time.sleep(0.02)


if __name__ == "__main__":
    receiver = RealtimeQuoteReceiver(SYMBOL)
    receiver.run()
