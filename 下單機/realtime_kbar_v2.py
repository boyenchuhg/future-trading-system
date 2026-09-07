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
from dataclasses import dataclass
from datetime import datetime, time as dtime
from typing import Optional

from ctypes import byref, POINTER, windll
from comtypes import IUnknown, GUID
from comtypes.client import GetBestInterface, GetEvents

windll.atl.AtlAxWinInit()

# ============================================================
# 設定 —— 帳密依需求寫死（這是要送單的自動化系統，不做互動式輸入）
# ⚠️ 裸奔提醒（使用者已知情，僅留紀錄）：
#    建議至少做到 (1) 檔案權限只留自己帳號可讀
#                (2) 若這支程式進版本控制，記得 .gitignore 排除
# ============================================================
USER_ID_FUT = ""
PASSWORD = ""
API_QUOTE_IP = "apiquote.yuantafutures.com.tw"   # 沿用「查詢.py」的位址
API_ORDER_IP = "api.yuantafutures.com.tw"        # Ord元件用，來源：查詢.py/Forder.py

BAR_MINUTES = 5
SESSION_START = dtime(8, 45, 0)
SESSION_END = dtime(13, 45, 0)     # 含（13:45:00 集合競價併入13:40那根）
LOG_DIR = "./realtime_logs"
DIAG_TICK_LIMIT = 50               # 開盤後前N筆tick印出原始值供人工核對

# ============================================================
# S1 開盤突破 —— 參數依《參數定案版》docx定案值
# ============================================================
S1_BREAKOUT_PCT = 0.004    # 突破開盤價 ±0.4%
S1_VOL_MULT = 2.0          # 前一根K棒量 > 2.0x 均量
S1_ENTRY_TIME = dtime(9, 15, 0)
S1_EXIT_TIME = dtime(13, 44, 0)    # 收盤強制平倉

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


SYMBOL = get_current_fut_symbol("TMF")


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
# K棒累加器（與 v1 相同，邏輯未變，只有連線方式改了）
# ============================================================
@dataclass
class Bar:
    bucket_start: dtime
    open: Optional[float] = None
    high: Optional[float] = None
    low: Optional[float] = None
    close: Optional[float] = None
    volume: int = 0
    n_ticks: int = 0

    def update(self, price: float, qty: int):
        if self.open is None:
            self.open = price
        self.high = price if self.high is None else max(self.high, price)
        self.low = price if self.low is None else min(self.low, price)
        self.close = price
        self.volume += qty
        self.n_ticks += 1


# ============================================================
# S1 開盤突破 —— 訊號邏輯（目前只印出「本來要下單」，不真的送單）
# 規則：09:15後，價格突破開盤價±0.4%，前一根K棒量>2.0x均量，觸價進場
#      停損：回觸當日開盤價；13:45市價強制平倉；每日最多1次
# ============================================================
class S1State:
    """
    S1 開盤突破策略狀態機。
    ⚠️ 2026/09/04 依策略端規則更新：
      1. 從「停損一次就整天結束」改成「最多可進場2筆」——第一筆停損出場後，
         允許同一天內再找第二筆（同向再觸發 或 對向反手，哪個先發生就做哪個，
         互斥不會兩者都發生）。第二筆的進出場邏輯完全比照第一筆。
         上限嚴格鎖在2筆，即使理論上可能出現更多次觸發也不再繼續尋找。
      2. 新增「箱體零範圍跳過」：當天目前為止最高價=最低價（跌停/漲停鎖死、
         完全無波動）時，不進場，避免無意義的重複觸發-停損循環。
         S4遇過1天內重複觸發4~5次的案例，S1/S2目前沒實際遇過但先一併防範。
    """
    MAX_TRADES_PER_DAY = 2

    def __init__(self):
        self.day_open: Optional[float] = None
        self.day_high: Optional[float] = None
        self.day_low: Optional[float] = None
        self.volumes: list = []          # 已收盤K棒的量，供滾動均量使用
        self.position: Optional[str] = None   # None / 'long' / 'short'
        self.trade_count = 0             # 今天已完成幾筆（進場+出場算一筆）
        self.done_today = False
        self.entry_price: Optional[float] = None
        self.data_gap_today = False      # 今天資料有沒有缺過根，缺過就永久True

    def on_bar_close(self, bar: Bar):
        if self.day_open is None:
            self.day_open = bar.open   # 第一根K棒的開盤價 = 當日開盤價
        self.day_high = bar.high if self.day_high is None else max(self.day_high, bar.high)
        self.day_low = bar.low if self.day_low is None else min(self.day_low, bar.low)
        self.volumes.append(bar.volume)

    def _vol_ok(self) -> bool:
        n = len(self.volumes)
        if n == 0:
            return False
        window = self.volumes[-20:] if n >= 20 else self.volumes[:]
        ma = sum(window) / len(window)
        return self.volumes[-1] > S1_VOL_MULT * ma

    def _zero_range_today(self) -> bool:
        """當天目前為止最高=最低（鎖死無波動），回傳True代表要跳過進場"""
        if self.day_high is None or self.day_low is None:
            return False
        return (self.day_high - self.day_low) == 0

    def on_tick(self, t: dtime, price: float):
        if self.day_open is None or self.done_today:
            return

        if self.position is None:
            if t < S1_ENTRY_TIME or t >= S1_EXIT_TIME:
                return
            if self.data_gap_today:
                return
            if self._zero_range_today():
                return
            if not self._vol_ok():
                return
            upper = self.day_open * (1 + S1_BREAKOUT_PCT)
            lower = self.day_open * (1 - S1_BREAKOUT_PCT)
            if price >= upper:
                self._enter("long", price, t)
            elif price <= lower:
                self._enter("short", price, t)
        else:
            if t >= S1_EXIT_TIME:
                self._exit(price, t, f"{S1_EXIT_TIME.strftime('%H:%M')}收盤強制平倉")
                return
            if self.position == "long" and price <= self.day_open:
                self._exit(price, t, "回觸開盤價停損")
            elif self.position == "short" and price >= self.day_open:
                self._exit(price, t, "回觸開盤價停損")

    def _enter(self, direction: str, price: float, t: dtime):
        self.position = direction
        self.entry_price = price
        side = "買進(做多)" if direction == "long" else "賣出(做空)"
        nth = self.trade_count + 1
        print(f"[S1訊號] {t} 【本來要下單】第{nth}筆 {side} 觸價進場 @ {price} "
              f"(開盤價{self.day_open}, 突破門檻{S1_BREAKOUT_PCT*100:.1f}%)")

    def _exit(self, price: float, t: dtime, reason: str):
        pnl_dir = 1 if self.position == "long" else -1
        pnl = (price - self.entry_price) * pnl_dir if self.entry_price else 0
        self.trade_count += 1
        print(f"[S1訊號] {t} 【本來要下單】第{self.trade_count}筆 出場平倉 @ {price} "
              f"({reason})，估計損益(未含手續費)={pnl:+.1f}點")
        self.position = None
        if self.trade_count >= self.MAX_TRADES_PER_DAY:
            self.done_today = True


class KBarBuilder:
    """
    分K規則（沿用交接摘要§三的七項驗證規則，適用於即時資料的部分）：
    - 左閉右開，以起始時間標記（08:45起，每5分鐘一根）
    - 13:45:00 收盤集合競價併入 13:40 那根
    - 盤前（TolMatchQty == -1）與盤後一律不切K
    """

    def __init__(self, bar_minutes: int = BAR_MINUTES):
        self.bar_minutes = bar_minutes
        self.current_bucket: Optional[dtime] = None
        self.current_bar: Optional[Bar] = None
        self.finished_bars = []
        self.last_tol_match_qty: Optional[int] = None
        self.diag_count = 0
        self.session_closed_flushed = False
        self.s1 = S1State()   # S1訊號邏輯，跟切K棒共用同一批tick

    def bucket_for(self, t: dtime) -> Optional[dtime]:
        if t < SESSION_START or t > SESSION_END:
            return None
        if t == SESSION_END:
            return dtime(13, 40, 0)
        start_total = SESSION_START.hour * 60 + SESSION_START.minute
        t_total = t.hour * 60 + t.minute
        elapsed = t_total - start_total
        bucket_idx = elapsed // self.bar_minutes
        bucket_total = start_total + bucket_idx * self.bar_minutes
        h, m = divmod(bucket_total, 60)
        return dtime(h, m, 0)

    def on_tick(self, match_time_raw: str, match_price_raw: str,
                tol_match_qty_raw: str, match_qty_raw: str):
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
        # （雙線 SetMktLogon 會造成同一筆成交重複觸發 callback、tol_qty 不變，
        #  這裡用差值法天然濾掉重複，delta 會是 0，不會重複計量）
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

        # 診斷模式：只在「真的有新成交」時印出，過濾掉雙線重複的callback
        if is_new_tick and self.diag_count < DIAG_TICK_LIMIT:
            print(f"[診斷] raw_time={match_time_raw!r} parsed={t} "
                  f"tol_qty={tol_qty} delta={delta_qty} match_qty={match_qty_raw!r} "
                  f"price={match_price_raw!r}")
            self.diag_count += 1

        try:
            price = float(match_price_raw)
        except (TypeError, ValueError):
            return

        # S1訊號判斷：跟切K棒共用同一筆tick，不等K棒收完（B案即時觸發）
        self.s1.on_tick(t, price)

        bucket = self.bucket_for(t)
        if bucket is None:
            # ⚠️ 2026/09/04 修正bug：t >= SESSION_END(13:45)時bucket_for()
            # 回傳None，代表盤後不切K，但這樣一來最後一根K棒(13:40這根)
            # 永遠不會因為「換下一個bucket」被觸發寫出，會卡在記憶體裡
            # 直到程式關閉就直接遺失，完全沒有落地保存過。
            # 這裡在偵測到收盤後的第一筆tick時，強制把手上還沒寫出的
            # 最後一根K棒flush掉，只做一次。
            if not self.session_closed_flushed and self.current_bar is not None:
                self.finished_bars.append(self.current_bar)
                self._flush_bar(self.current_bar)
                self.s1.on_bar_close(self.current_bar)
                print(f"[收盤] 已強制寫出最後一根K棒：{self.current_bar.bucket_start.strftime('%H:%M')}")
                self.session_closed_flushed = True
            return

        if self.current_bucket != bucket:
            if self.current_bar is not None:
                self.finished_bars.append(self.current_bar)
                self._flush_bar(self.current_bar)
                self.s1.on_bar_close(self.current_bar)

                # ⚠️ 2026/09/04 新增：偵測缺根（斷線交界處資料不完整）。
                # 判斷方式：新bucket跟舊bucket之間的時間差，理論上應該
                # 剛好是5分鐘（BAR_MINUTES），如果差更多，代表中間缺了
                # 至少一根。一旦偵測到，整天永久關閉S1新倉，不管時間、
                # 不管之後有沒有恢復正常——單向開關，只會關不會重開，
                # 避免去判斷「現在算不算恢復」這種沒完沒了的邏輯。
                prev_total = self.current_bucket.hour * 60 + self.current_bucket.minute
                new_total = bucket.hour * 60 + bucket.minute
                if new_total - prev_total > self.bar_minutes:
                    if not self.s1.data_gap_today:
                        print(f"[資料缺口] {self.current_bucket.strftime('%H:%M')} -> "
                              f"{bucket.strftime('%H:%M')} 中間缺K棒，S1今天起停止開新倉")
                    self.s1.data_gap_today = True

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


# ============================================================
# COM 事件接收 —— 綁定方式照抄「查詢.py」驗證過的寫法
# ============================================================
class YuantaQuoteEvents:
    def __init__(self, parent):
        self.parent = parent

    def OnMktStatusChange(self, this, Status, Msg, ReqType):
        print(f"[連線狀態] Status={Status} Msg={Msg} ReqType={ReqType}")
        key = str(ReqType)
        if str(Status) == "2":
            # ⚠️ 2026/09/04 新增：成功連線後重置這個通道的重試計數。
            # 之前發現斷線重連幾輪後，即使每輪都只需要2-3次就恢復，
            # 累加下來還是會超過8次上限、被誤判永久放棄。連線成功
            # 代表這輪的問題已經解決，計數器歸零重新開始算。
            self.parent.login_retry_count[key] = 0
            self.parent.reg_retry_count[key] = 0
            self._register(ReqType)
        elif str(Status) in ("-2", "-1"):
            # ⚠️ 2026/09/04 修正漏洞：原本只處理 Status=-2(無登入權限)，
            # 完全沒處理 Status=-1(行情連線結束)。代表如果程式已經連線
            # 成功、資料正常跑一陣子後中途斷線(顯示-1)，之前的程式碼
            # 什麼都不會做，不會嘗試重連，只會停在那裡不動。
            # 現在兩種狀態都視為「需要重新登入」來處理。
            count = self.parent.login_retry_count.get(key, 0) + 1
            self.parent.login_retry_count[key] = count
            if key == "1" and count <= 8:
                print(f"[登入重試 ReqType={ReqType} {count}/8] "
                      f"Status={Status}({Msg})，1.5秒後重新登入")
                self.parent.root.after(1500, lambda: self.parent.relogin(ReqType))
            elif key == "1":
                print(f"[登入放棄 ReqType={ReqType}] 已重試8次仍失敗，"
                      f"建議手動重開程式，或找元大確認帳號連線狀態。")

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
        # ⚠️ 2026/09/04 修正bug：原本reg_retry_count是兩條線(port80/82)共用
        # 的單一計數器，導致其中一條線先用完5次額度後，另一條線根本沒機會
        # 重試就被判定放棄。改成per-channel(用dict，key是req_type)各自獨立
        # 計數，兩條線互不影響。
        print(f"[註冊錯誤事件] {args}")
        if len(args) >= 4:
            symbol, mode, err_code, req_type = args[0], args[1], args[2], args[3]
            if str(err_code) == "3":
                key = str(req_type)
                count = self.parent.reg_retry_count.get(key, 0) + 1
                self.parent.reg_retry_count[key] = count
                if count <= 5:
                    print(f"[註冊重試 ReqType={req_type} {count}/5] "
                          f"ErrorCode=3(連線未完成)，立即重試")
                    self._register(req_type)
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

            self.parent.kbar_builder.on_tick(MatchTime, MatchPri, TolMatchQty, MatchQty)
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
        # ⚠️ 2026/09/04 新增診斷：查詢.py跟避險.py都是「先建立Ord(下單元件)
        # 並登入，等3秒，才建立Quote(報價元件)」，我們的程式先前完全沒碰
        # Ord。懷疑ErrorCode=3(連線未完成)講的是整個帳號session(可能包含
        # Ord)還沒建立完成，不是Quote本身的問題。這裡只是登入Ord建立
        # session，不會送單。
        try:
            Iwindow_o, Icontrol_o, Ievent_o = POINTER(IUnknown)(), POINTER(IUnknown)(), POINTER(IUnknown)()
            windll.atl.AtlAxCreateControlEx(
                "Yuanta.YuantaOrdCtrl.1", self.hwnd, None,
                byref(Iwindow_o), byref(Icontrol_o), byref(GUID()), Ievent_o
            )
            self.ord = GetBestInterface(Icontrol_o)
            self.ord.SetFutOrdConnection(USER_ID_FUT, PASSWORD, API_ORDER_IP, '80')
            print("[初始化] 已送出 SetFutOrdConnection（Ord，僅登入不下單）")
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
            except tk.TclError:
                break
            time.sleep(0.02)


if __name__ == "__main__":
    receiver = RealtimeQuoteReceiver(SYMBOL)
    receiver.run()
