# -*- coding: utf-8 -*-
import datetime
import json
import math
import os
import sys
import time
import tkinter as tk
from ctypes import POINTER, windll, byref  # 修正 1: 補上 byref 匯入
from urllib.parse import quote

from comtypes import GUID, IUnknown
from comtypes.client import GetBestInterface, GetEvents
import pandas as pd
import requests

# 設定路徑
sys.path.append(r"C:\Users\user\OneDrive - 財團法人台灣綜合研究院")
from my_config import *

# ==============================================================================
# 🟢 頂端核心設定區
# ==============================================================================
WAIT_FOR_TIME = True  # True: 實戰模式 (08:45) | False: 立刻測試
SAFE_MODE = False  # False: 真槍實彈下單 | True: 安全模式(不送單)

# 下單參數
ORD_PRICE_TYPE = "M"  # 'L-限價','M-市價','P-範圍市價'
ORD_PRICE = ""  # Pril: 價格不填東西 (空字串)
ORD_OFFSET = " "  # '0-新倉','1-平倉','2-當沖',' -自動'
ORD_COND = "I"  # 'R-ROD','F-FOK','I-IOC'

# --- 額度控制參數 ---
MAX_QTY_PER_ORDER = 20  # 單筆最大下單口數上限

USER_ID = "身分證1"
USER_PWD = "密碼1"

USER3_ID = "身分證2"
USER3_PWD = "密碼2"

# ==============================================================================
# ⚙️ 系統核心與 Line 設定
# ==============================================================================
LINE_ACCESS_TOKEN = "zpGmtvDxAj+G6SCTsPlX3/W+8wdBmbxoZtZR6AUyGgmRGZlnwhTa8H/TolFOEtKqB0tJD6tpU5vdJHhJ7tfyjsDuKMgPrqm/S1ntqM9YqwvEyxJhsdS9XDPOjfb9YrpTzEHNHIQXoftcDU8j80/g7wdB04t89/1O/w1cDnyilFU="
LINE_USER_ID = "U8b4d9a2afe1be5096981d96aef1c101d"

SUFFIX = "ABCDEFGHIJKL"[int(YEAR_MONTH[4:]) - 1] + YEAR_MONTH[3]
TX_SYM = "TXF" + SUFFIX
MTX_SYM = "MXF" + SUFFIX
TMF_SYM = "TMF" + SUFFIX

API_QUOTE_IP = "apiquote.yuantafutures.com.tw"
API_ORDER_IP = "api.yuantafutures.com.tw"

full_log_box = []
windll.atl.AtlAxWinInit()


def sync_log(msg):
    print(msg)
    full_log_box.append(msg)


def send_line_final():
    if not full_log_box:
        return
    final_msg = "\n".join(full_log_box)
    try:
        requests.post(
            "https://api.line.me/v2/bot/message/push",
            headers={
                "Content-Type": "application/json",
                "Authorization": f"Bearer {LINE_ACCESS_TOKEN}",
            },
            json={
                "to": LINE_USER_ID,
                "messages": [{"type": "text", "text": final_msg}],
            },
        )
    except:
        pass


# ==============================================================================
# 🌎 美股數據智慧抓取工具 (自動對齊紐約時區 + 1h 補洞備援與狀態回傳)
# ==============================================================================
def fetch_yf(sym, interval="1d", host="query1"):
    cb = int(time.time())
    url = f"https://{host}.finance.yahoo.com/v8/finance/chart/{quote(sym)}"
    res = requests.get(
        url,
        # 修正 3: range 從 1mo 拉長到 2mo，就算某天格子在來源端被漏掉，
        # 後面重新請求時也有更多歷史資料可以做完整比對，不會因為視窗太窄而看不出破洞
        params={"range": "2mo", "interval": interval, "_": cb},
        headers={"User-Agent": "Mozilla/5.0"},
        timeout=10,
    )
    res.raise_for_status()
    j = res.json()["chart"]["result"][0]

    ts = pd.to_datetime(j["timestamp"], unit="s", utc=True).tz_convert(
        "America/New_York"
    )
    df = pd.DataFrame(
        {"Close": j["indicators"]["quote"][0]["close"]}, index=ts
    ).dropna()

    if interval == "1h":
        df["Date"] = df.index.date
        df = df.groupby("Date").last()
        df.index = pd.to_datetime(df.index)
    else:
        df.index = df.index.normalize().tz_localize(None)
    return df["Close"]


def fetch_yf_robust(sym, interval="1d"):
    """修正 4: 針對『雅虎財經出包』(某主機瞬斷/單天格子缺漏) 做重試 + 雙主機備援。
    依序嘗試 query1 / query2，每台主機重試 2 次，全部失敗才真正拋錯。"""
    last_err = None
    for host in ("query1", "query2"):
        for attempt in range(2):
            try:
                return fetch_yf(sym, interval, host=host)
            except Exception as e:
                last_err = e
                time.sleep(1)
    raise RuntimeError(f"抓取 {sym}({interval}) 失敗，已嘗試雙主機皆無法取得: {last_err}")


def get_us_data_smart():
    """修正 5 (核心修正): 舊邏輯只比對『最後一筆日期』，抓不到『中間某天格子整個消失』
    的狀況 (例如 8/28 被漏掉、但 8/29 之後又正常出現，最後一筆日期看起來沒有落後)。
    改成用日期聯集合並 (combine_first)：只要 1d 資料裡任何一天缺格，
    就用當天的 1h 收盤價補上，不再只看尾端。"""
    idx = {"COMP": "^IXIC", "SPX": "^GSPC"}
    df_1d = pd.concat(
        {k: fetch_yf_robust(v, "1d") for k, v in idx.items()}, axis=1
    )
    df_1h = pd.concat(
        {k: fetch_yf_robust(v, "1h") for k, v in idx.items()}, axis=1
    )

    combined = df_1d.combine_first(df_1h).sort_index()

    # 找出哪些日期是靠 1h 補上的 (1d 原本沒有這個格子)
    filled_dates = combined.index.difference(df_1d.index)
    is_fallback = len(filled_dates) > 0

    return combined, is_fallback, filled_dates


class YuantaOrdEvents(object):

    def __init__(self, parent, is_acc3=False):
        self.parent = parent
        self.is_acc3 = is_acc3

    def OnLogonS(self, this, TLinkStatus, AccList, Casq, Cast):
        if str(TLinkStatus) == "2":
            acc_list = [a for a in AccList.split(";") if a]
            if self.is_acc3:
                self.parent.account_list3 = acc_list
                self.parent.ord3_logged_in = True
                print(f"✅ [下單-帳號3] 登入成功 (帳號數: {len(acc_list)})")
            else:
                self.parent.account_list = acc_list
                self.parent.ord_logged_in = True
                print(
                    f"✅ [下單-主帳號] 登入成功 (帳號數: {len(self.parent.account_list)})"
                )
        else:
            tag = "帳號3" if self.is_acc3 else "主帳號"
            print(
                f"❌ [下單-{tag}] 登入失敗，元大狀態碼: {TLinkStatus} (請檢查帳密或API權限)"
            )

    def OnOrdRptF(self, *args):
        try:
            raw_bs = str(args[11]).strip()
            side = (
                "買"
                if raw_bs == "B"
                else ("賣" if raw_bs == "S" else f"({raw_bs})")
            )
            sync_log(
                f"📢 [回報] {side} {args[10]} | 狀:{args[6]} | 訊:{args[-3]}"
            )
        except:
            pass


class YuantaQuoteEvents(object):

    def __init__(self, parent):
        self.parent = parent

    def OnMktStatusChange(self, this, Status, Msg, ReqType):
        if str(Status) == "2":
            self.parent.quote_logged_in = True
            print("✅ [行情] 登入成功，訂閱中...")
            self.parent.subscribe_quote()

    def OnGetMktAll(
        self,
        this,
        symbol,
        RefPri,
        OpenPri,
        HighPri,
        LowPri,
        UpPri,
        DnPri,
        MatchTime,
        MatchPri,
        MatchQty,
        TolMatchQty,
        BestBuyQty,
        BestBuyPri,
        BestSellQty,
        BestSellPri,
        FDBPri,
        FDBQty,
        FDSPri,
        FDSQty,
        ReqType,
    ):
        if (
            WAIT_FOR_TIME
            and datetime.datetime.now() < self.parent.warmup_time
        ):
            return
        if not self.parent.strategy_triggered:
            try:
                open_val = float(OpenPri)
                if open_val > 0:
                    if symbol == MTX_SYM:
                        self.parent.open_mxf = open_val
                    elif symbol == TMF_SYM:
                        self.parent.open_tmf = open_val

                    if self.parent.open_mxf > 0 and self.parent.open_tmf > 0:
                        self.parent.strategy_triggered = True
                        sync_log(
                            f"🎯 開盤價 MXF:{self.parent.open_mxf}, TMF:{self.parent.open_tmf}"
                        )
                        self.parent.execute_strategy()
            except:
                pass


class TradeBot:

    def __init__(self):
        (
            self.account_list,
            self.account_list3,
            self.strategy_triggered,
        ) = ([], [], False)
        self.open_mxf, self.open_tmf = 0.0, 0.0
        (
            self.ord_logged_in,
            self.quote_logged_in,
            self.ord3_logged_in,
        ) = (False, False, False)
        self.last_heartbeat, self.rs_base_value = time.time(), 0
        self.rs_parts = (0, 0)
        now = datetime.datetime.now()
        self.warmup_time = now.replace(
            hour=8, minute=44, second=58, microsecond=0
        )
        self.timeout_time = now.replace(
            hour=8, minute=45, second=10, microsecond=0
        )
        self.root = tk.Tk()
        self.root.withdraw()
        self.hwnd = self.root.winfo_id()
        self.init_drivers()

    def init_drivers(self):
        try:
            Iwin_ord, Ictrl_ord, Ievt_ord = (
                POINTER(IUnknown)(),
                POINTER(IUnknown)(),
                POINTER(IUnknown)(),
            )
            windll.atl.AtlAxCreateControlEx(
                "Yuanta.YuantaOrdCtrl.1",
                self.hwnd,
                None,
                byref(Iwin_ord),
                byref(Ictrl_ord),
                byref(GUID()),
                Ievt_ord,
            )
            self.Ord = GetBestInterface(Ictrl_ord)
            self.OrdCon = GetEvents(self.Ord, YuantaOrdEvents(self))

            self.frame3 = tk.Frame(self.root)
            self.frame3.pack()
            Iwin3, Ictrl3, Ievt3 = (
                POINTER(IUnknown)(),
                POINTER(IUnknown)(),
                POINTER(IUnknown)(),
            )
            windll.atl.AtlAxCreateControlEx(
                "Yuanta.YuantaOrdCtrl.1",
                self.frame3.winfo_id(),
                None,
                byref(Iwin3),
                byref(Ictrl3),
                byref(GUID()),
                Ievt3,
            )
            self.Ord3 = GetBestInterface(Ictrl3)
            self.OrdCon3 = GetEvents(
                self.Ord3, YuantaOrdEvents(self, is_acc3=True)
            )

            Iwin_quote, Ictrl_quote, Ievt_quote = (
                POINTER(IUnknown)(),
                POINTER(IUnknown)(),
                POINTER(IUnknown)(),
            )
            windll.atl.AtlAxCreateControlEx(
                "YUANTAQUOTE.YuantaQuoteCtrl.1",
                self.hwnd,
                None,
                byref(Iwin_quote),
                byref(Ictrl_quote),
                byref(GUID()),
                Ievt_quote,
            )
            self.Quote = GetBestInterface(Ictrl_quote)
            self.QuoteCon = GetEvents(self.Quote, YuantaQuoteEvents(self))
        except Exception as e:
            print(f"❌ 元件錯誤: {e}")

    def pre_calculate_rs(self):
        sync_log("🌎 美股 RS 計算中...")
        try:
            # 1. 抓取智慧補洞數據與警報狀態 (修正 5: 現在會回傳「哪些日期是補的」)
            df_all, is_fallback, filled_dates = get_us_data_smart()
            df = df_all.tail(5)

            # 🚨【警報提示】只要有任何一天是靠 1h 補洞，就示警並寫入 Line 紀錄
            if is_fallback:
                sync_log("⚠️ --------------------------------- ⚠️")
                sync_log("⚠️ 【數據警報】Yahoo 1d 官方日線有格子缺漏！")
                filled_str = ", ".join(
                    d.strftime("%Y-%m-%d") for d in filled_dates
                )
                sync_log(f"⚠️ 已用 [1h 小時 K 線] 補齊缺漏日期: {filled_str}")
                sync_log("⚠️ --------------------------------- ⚠️")
            else:
                sync_log(
                    f"✅ 數據源正常: 採用 Yahoo 1d 官方日線 (最新: {df.index[-1].strftime('%Y-%m-%d')})"
                )

            nas = df["COMP"]
            spx = df["SPX"]

            if len(nas) < 5 or len(spx) < 5:
                sync_log("❌ 資料不足 5 筆，無法計算！")
                self.rs_base_value = 0
                self.rs_parts = (0, 0)
                return

            # 修正 6 (安全防呆): 就算合併補洞後，最近 5 筆裡若仍有 NaN
            # (代表 1d 跟 1h 兩邊都抓不到那天)，絕對不要用壞資料硬算，
            # 直接中止並示警，避免重演「整個計算都毀了」卻沒發現。
            if nas.isna().any() or spx.isna().any():
                bad_dates = df.index[nas.isna() | spx.isna()]
                bad_str = ", ".join(d.strftime("%Y-%m-%d") for d in bad_dates)
                sync_log(f"❌ 資料仍有缺格 (1d/1h 皆無法取得): {bad_str}，停止計算避免算錯！")
                self.rs_base_value = 0
                self.rs_parts = (0, 0)
                return

            # 2. H2 與 H3 計算
            sync_log(
                f"🔍 [H2] Nas({nas.iloc[-1]:.2f}/{nas.iloc[-4]:.2f}) vs Spx({spx.iloc[-1]:.2f}/{spx.iloc[-4]:.2f})"
            )
            h2 = math.log(nas.iloc[-1] / nas.iloc[-4]) - math.log(
                spx.iloc[-1] / spx.iloc[-4]
            )
            sync_log(f"   👉 H2 = {h2:.6f}")

            sync_log(
                f"🔍 [H3] Nas({nas.iloc[-2]:.2f}/{nas.iloc[-5]:.2f}) vs Spx({spx.iloc[-2]:.2f}/{spx.iloc[-5]:.2f})"
            )
            h3 = math.log(nas.iloc[-2] / nas.iloc[-5]) - math.log(
                spx.iloc[-2] / spx.iloc[-5]
            )
            sync_log(f"   👉 H3 = {h3:.6f}")

            part1 = -1 if h2 >= h3 else 1
            part2 = 1 if h2 > 0 else -1
            self.rs_parts = (part1, part2)
            self.rs_base_value = part1 + part2

            sync_log("-" * 19)
            sync_log(f"🧮 判斷: {part1} + {part2}")
            sync_log(
                f"✅ RS = {self.rs_base_value} (最新交易日: {df.index[-1].strftime('%Y-%m-%d')})"
            )
            sync_log("-" * 19)

        except Exception as e:
            sync_log(f"❌ RS 計算失敗! 原因: {e}")
            self.rs_base_value = 0
            self.rs_parts = (0, 0)

    def login_check(self):
        if (
            self.ord_logged_in
            and self.quote_logged_in
            and self.ord3_logged_in
        ):
            return
        try:
            if not self.ord_logged_in and self.Ord is not None:
                self.Ord.SetFutOrdConnection(
                    USER_ID, USER_PWD, API_ORDER_IP, "80"
                )
            if not self.ord3_logged_in and self.Ord3 is not None:
                self.Ord3.SetFutOrdConnection(
                    USER3_ID, USER3_PWD, API_ORDER_IP, "80"
                )
            if not self.quote_logged_in and self.Quote is not None:
                self.Quote.SetMktLogon(
                    USER_ID, USER_PWD, API_QUOTE_IP, "80", 1, 0
                )
        except:
            pass

    def subscribe_quote(self):
        if self.Quote is None:
            return
        try:
            self.Quote.AddMktReg(MTX_SYM, 2, 1, 0)
            self.Quote.AddMktReg(TMF_SYM, 2, 1, 0)
        except:
            pass

    def execute_strategy(self):
        ia_base = 1 if self.open_mxf >= self.open_tmf else -1
        w = RATIO * 5

        # 1. 安格
        ia_p_a = IA_VAL_ALGO * ia_base
        rs_p_a = RS_VAL_ALGO * self.rs_base_value
        total_a = QTY_ALGO + ia_p_a + rs_p_a
        vol_a = int(abs(total_a) + 0.5)
        if vol_a > MAX_QTY_PER_ORDER:
            vol_a = MAX_QTY_PER_ORDER
        act_a = "買" if total_a >= 0 else "賣"

        # 2. 鄭
        z_base = (STOCK_ALGO * w) - STOCK_ZHENG
        ia_p_z = IA_VAL_ALGO * ia_base * w
        rs_p_z = RS_VAL_ALGO * self.rs_base_value * w
        total_z = z_base + ia_p_z + rs_p_z
        vol_z = int(abs(total_z) + 0.5)
        if vol_z > MAX_QTY_PER_ORDER:
            vol_z = MAX_QTY_PER_ORDER
        act_z = "買" if total_z >= 0 else "賣"

        # 3. 模擬
        ia_p_r = IA_VAL_REPORT * ia_base
        rs_p_r = RS_VAL_REPORT * self.rs_base_value
        total_r = QTY_REPORT + ia_p_r + rs_p_r
        vol_r = int(abs(total_r) + 0.5)
        if vol_r > MAX_QTY_PER_ORDER:
            vol_r = MAX_QTY_PER_ORDER
        act_r = "買" if total_r >= 0 else "賣"

        sync_log("📊 [今日報告]")
        p1, p2 = getattr(self, "rs_parts", (0, 0))
        sync_log(
            f" {self.open_mxf} vs {self.open_tmf} -> IA:{ia_base} | RS:{self.rs_base_value} ({p1}{p2:+d})"
        )
        sync_log("-" * 19)

        sync_log(
            f"🏢 安格: {QTY_ALGO:.2f} + ({ia_p_a:.2f}) + ({rs_p_a:.2f})"
        )
        sync_log(
            f"   = {total_a:.2f} -> {act_a}{vol_a} (大:{vol_a//4}|小:{vol_a%4})"
        )

        sync_log(
            f"👤 鄭: {z_base:.2f} + ({ia_p_z:.2f}) + ({rs_p_z:.2f})"
        )
        sync_log(
            f"   = {total_z:.2f} -> {act_z}{vol_z} (小:{vol_z//5}|微:{vol_z%5})"
        )

        sync_log(
            f"📝 模擬: {QTY_REPORT:.2f} + ({ia_p_r:.2f}) + ({rs_p_r:.2f})"
        )
        sync_log(
            f"   = {total_r:.2f} -> {act_r}{vol_r} (大:{vol_r//4}|小:{vol_r%4})"
        )
        sync_log("-" * 19)

        if not SAFE_MODE:
            # 1. 主帳號群 (安格 & 鄭)
            if len(self.account_list) >= 2:
                acc_z, acc_a = self.account_list[0].split(
                    "-"
                ), self.account_list[1].split("-")
                self.send_order(
                    acc_a,
                    TX_SYM,
                    "B" if "買" in act_a else "S",
                    vol_a // 4,
                    "安格",
                )
                self.send_order(
                    acc_a,
                    MTX_SYM,
                    "B" if "買" in act_a else "S",
                    vol_a % 4,
                    "安格",
                )
                self.send_order(
                    acc_z,
                    MTX_SYM,
                    "B" if "買" in act_z else "S",
                    vol_z // 5,
                    "鄭",
                )
                self.send_order(
                    acc_z,
                    TMF_SYM,
                    "B" if "買" in act_z else "S",
                    vol_z % 5,
                    "鄭",
                )
            elif len(self.account_list) == 1:
                acc_a = self.account_list[0].split("-")
                self.send_order(
                    acc_a,
                    TX_SYM,
                    "B" if "買" in act_a else "S",
                    vol_a // 4,
                    "安格",
                )
                self.send_order(
                    acc_a,
                    MTX_SYM,
                    "B" if "買" in act_a else "S",
                    vol_a % 4,
                    "安格",
                )
            else:
                sync_log("❌ 主帳號尚未取得，跳過安格與鄭的下單")

            # 2. 第三帳號 (模擬)
            if len(self.account_list3) >= 1:
                acc_r = self.account_list3[0].split("-")
                self.send_order(
                    acc_r,
                    TX_SYM,
                    "B" if "買" in act_r else "S",
                    vol_r // 4,
                    "模擬",
                    ord_driver=self.Ord3,
                )
                self.send_order(
                    acc_r,
                    MTX_SYM,
                    "B" if "買" in act_r else "S",
                    vol_r % 4,
                    "模擬",
                    ord_driver=self.Ord3,
                )
            else:
                sync_log("❌ 帳號 3 尚未取得，跳過模擬帳戶下單")
        else:
            sync_log("⚠️ 安全模式: 未送實單")

        print("⏳ 下單完成，等待成交回報中")
        time.sleep(15)

        sync_log("-" * 19)
        sync_log("🔌 正在切斷元大 API 連線")

        self.Ord = None
        self.Ord3 = None
        self.Quote = None
        send_line_final()

        input("\n✅ 程式執行完畢，Line 已發送。請按 Enter 鍵關閉視窗...")
        sys.exit(0)

    def send_order(self, acc_data, sym, act, qty, owner, ord_driver=None):
        if ord_driver is None:
            ord_driver = self.Ord

        if ord_driver is None:
            sync_log(f"❌ [送單] 無可用的下單驅動，跳過 {owner}")
            return

        if not acc_data or len(acc_data) < 3:
            sync_log(f"❌ [送單] 帳號格式錯誤: {acc_data}，跳過 {owner}")
            return

        bhno = acc_data[1] if len(acc_data) > 1 else ""
        acno = acc_data[2] if len(acc_data) > 2 else ""
        suba = acc_data[3] if len(acc_data) > 3 else ""

        qty = abs(int(float(qty)))
        if qty > MAX_QTY_PER_ORDER:
            qty = MAX_QTY_PER_ORDER
        if qty <= 0:
            return

        act_cn = "買" if act == "B" else "賣"
        sync_log(f"🚀 送單: [{owner}] {sym} {act_cn} {qty}")
        try:
            ret = ord_driver.SendOrderF(
                "01",
                "0",
                bhno,
                acno,
                suba,
                "",
                act,
                sym,
                ORD_PRICE,
                str(qty),
                ORD_OFFSET,
                ORD_PRICE_TYPE,
                ORD_COND,
                "",
                "",
            )
            sync_log(f"   API 回報: {ret}")
        except Exception as e:
            sync_log(f"❌ [送單] 錯誤: {e}")

    def main_loop(self):
        self.pre_calculate_rs()
        self.login_check()

        init_delay = time.time()
        while time.time() - init_delay < 1.0:
            self.root.update()
            time.sleep(0.02)

        while True:
            now = datetime.datetime.now()

            if time.time() - self.last_heartbeat > 5:
                self.login_check()
                self.last_heartbeat = time.time()

            if not WAIT_FOR_TIME and not self.strategy_triggered:
                self.login_check()
                self.strategy_triggered = True
                self.execute_strategy()

            if (
                WAIT_FOR_TIME
                and now >= self.timeout_time
                and not self.strategy_triggered
            ):
                self.login_check()
                self.strategy_triggered = True
                self.execute_strategy()

            self.root.update()
            time.sleep(
                0.001 if not WAIT_FOR_TIME or now >= self.warmup_time else 0.1
            )


if __name__ == "__main__":
    try:
        TradeBot().main_loop()
    except Exception as e:
        print(f"❌ 程式崩潰: {e}")
        import traceback

        traceback.print_exc()
        input("按 Enter 鍵結束...")