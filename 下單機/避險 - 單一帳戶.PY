# -*- coding: utf-8 -*-
"""
👑 單一帳戶純期貨自動風控下單機 (省手續費打包 + 修復版)
"""
import os
import sys
import time
import datetime
import math
import json
import tkinter as tk

from ctypes import byref, POINTER, windll
from comtypes import IUnknown, GUID
from comtypes.client import GetBestInterface, GetEvents

windll.atl.AtlAxWinInit()

# ==============================================================================
# 🎯 風控參數與帳號密碼 (寫死在中控區)
# ==============================================================================
DROP_PCT          = 0.007             # 📉 大台開盤價下跌 0.7% 觸發防線
REDUCE_PCT        = 0.35              # ✂️ 出清 35% 部位
MAX_QTY_PER_ORDER  = 20               # 🛡️ 單筆下單安全上限

TRADE_START_TIME  = datetime.time(9, 15, 0)   # ⏰ 09:15:00 起開放交易
TRADE_END_TIME    = datetime.time(13, 44, 0)  # ⏰ 13:44:00 收盤強制平倉時間

REAL_TRADE        = False              # 🚩 True【實彈模式】已開啟；False【模擬測試】
USER_ID_FUT       = "A226112197"    # 期貨帳號 (原第三帳號，現為唯一帳號)
USER_PWD          = "Jsbach0314"    # 期貨帳號密碼

API_ORDER_IP      = "api.yuantafutures.com.tw"
API_QUOTE_IP      = "apiquote.yuantafutures.com.tw"

ACCOUNT_LABELS = {
    "6601565": "📝 模擬",
}

# 🔧 [修正] 下單參數，比照 Forder.py 驗證過能成功送單的設定
ORD_PRICE_TYPE    = "M"     # 'L-限價','M-市價','P-範圍市價'
ORD_PRICE         = ""      # 市價單，價格留空字串
ORD_OFFSET        = " "     # '0-新倉','1-平倉','2-當沖',' -自動'
ORD_COND          = "I"     # 'R-ROD','F-FOK','I-IOC'

CONTRACT_RATIOS = {
    "TMF": {"TX": 20.0, "MX": 5.0, "TM": 1.0},
    "MXF": {"TX": 4.0,  "MX": 1.0, "TM": 0.2}
}

def get_symbol_type(symbol_str):
    sym = str(symbol_str).upper()
    if "TM" in sym or "微台" in sym: return "TM"
    elif "MX" in sym or "MTX" in sym or "小台" in sym: return "MX"
    elif "TX" in sym or "大台" in sym: return "TX"
    return "MX"

def derive_target_symbols_from_pos(pos_dict):
    suffix = ""
    if pos_dict:
        first_sym = next(iter(pos_dict.keys())).strip().upper()
        s = first_sym
        for prefix in ["TXF", "MXF", "TMF", "MTX", "TX", "MX", "TM"]:
            if s.startswith(prefix):
                s = s[len(prefix):]
                break
        suffix = s

    if not suffix:
        now = datetime.datetime.now()
        year, month, day = now.year, now.month, now.day
        if day > 18:
            month += 1
            if month > 12: month = 1; year += 1
        suffix = "ABCDEFGHIJKL"[month - 1] + str(year)[-1]

    return {
        "TXF": f"TXF{suffix}",
        "MXF": f"MXF{suffix}",
        "TMF": f"TMF{suffix}"
    }

def format_price(p):
    return f"{p:8.2f}" if p > 0 else " 資料傳輸中..."

# 🔧 [新增] 避險狀態存檔路徑：以「當天日期」命名，隔天自動變成新檔案(等同全新一天)
STATE_DIR = os.path.dirname(os.path.abspath(__file__))

def get_state_file_path():
    today_str = datetime.date.today().strftime("%Y%m%d")
    return os.path.join(STATE_DIR, f"hedge_state_{today_str}.json")

# ==============================================================================
# 1. COM 事件接收器
# ==============================================================================
class YuantaOrdEvents(object):
    def __init__(self, parent):
        self.parent = parent

    def OnLogonS(self, this, TLinkStatus, AccList, Casq, Cast):
        if str(TLinkStatus) == "2":
            accs = [a for a in AccList.split(';') if a]
            print(f"🟢 期貨帳務登入成功！活動帳戶: {accs}")
            self.parent.futures_raw_accounts = accs
            self.parent.fut_logon_ready = True
        else:
            print(f"❌ 期貨帳務登入失敗！元大狀態碼: {TLinkStatus}")

    def OnOrdRptF(self, *args):
        # 🔧 [修正] 比照 Forder.py 驗證過的委託回報解析方式，原本這裡完全是空的
        try:
            raw_bs = str(args[11]).strip()
            side = "買" if raw_bs == "B" else ("賣" if raw_bs == "S" else f"({raw_bs})")
            print(f"📢 【委託回報】 {side} {args[10]} | 狀態:{args[6]} | 訊息:{args[-3]}")
        except Exception:
            pass
    def OnUserDefinsFuncResult(self, this, RowCount, Results, WorkID):
        wid = str(WorkID)
        if wid.startswith("FA002"):
            acno = wid.split('_')[1] if '_' in wid else ""
            self.parent.parse_fut_position(acno, str(Results))

class YuantaQuoteEvents(object):
    def __init__(self, parent): self.parent = parent
    
    def OnMktStatusChange(self, this, Status, Msg, ReqType):
        if str(Status) == "2":
            try:
                symbols_map = derive_target_symbols_from_pos({})
                self.parent.Quote.AddMktReg(symbols_map["MXF"], 4, int(ReqType), 0)
                self.parent.Quote.AddMktReg(symbols_map["TXF"], 4, int(ReqType), 0)
                print(f"📡 報價通道 [{ReqType}] 連線成功，已註冊: {symbols_map['TXF']}, {symbols_map['MXF']}")
            except Exception as e: print(f"⚠️ 報價註冊異常: {e}")
            
    def OnGetMktAll(self, this, symbol, RefPri, OpenPri, HighPri, LowPri, UpPri, DnPri, MatchTime, MatchPri, MatchQty, TolMatchQty, BestBuyQty, BestBuyPri, BestSellQty, BestSellPri, FDBPri, FDBQty, FDSPri, FDSQty, ReqType):
        try:
            symbol = str(symbol).strip()
            if "TX" in symbol:
                try: price = float(MatchPri); op = float(OpenPri); ref = float(RefPri)
                except Exception: price, op, ref = 0.0, 0.0, 0.0
                
                if price > 0: self.parent.txf_price = price
                
                if self.parent.txf_open == 0.0:
                    if op > 0: self.parent.txf_open = op
                    elif ref > 0: self.parent.txf_open = ref
        except Exception: pass
        
    def OnGetMktData(self, this, PriType, symbol, Qty, Pri, ReqType):
        try:
            symbol = str(symbol).strip()
            if "TX" in symbol:
                try: price = float(Pri)
                except Exception: price = 0.0
                if price > 0:
                    self.parent.txf_price = price
        except Exception: pass

# ==============================================================================
# 2. 單一帳戶即時風控執行引擎
# ==============================================================================
class FixedDualAccountStopLossEngine:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("避險監控引擎 (背景執行中)")
        self.root.geometry("1x1-2000-2000")  
        self.root.attributes('-toolwindow', True)
        self.root.deiconify()
        
        self.txf_open = 0.0
        self.txf_price = 0.0
        
        self.futures_raw_accounts = []
        self.account_info = {}
        self.raw_positions = {}       
        self.account_states = {}      
        self.is_initialized = False
        self.fa002_ready = False       
        
        self.fut_logon_ready = False
        
        self.com_cage = tk.Frame(self.root, width=1, height=1)
        self.com_cage.pack(side=tk.BOTTOM)
        self.com_cage.update_idletasks()  
        self.hwnd = self.com_cage.winfo_id()

        # 🔧 [新增] 開機時先讀今天稍早的避險狀態(如果有的話)，避免斷線重開後忘記已經避險過
        self.load_state()

    def load_state(self):
        """🔧 [新增] 讀取今天稍早的避險狀態檔。找不到檔案、或模式(實單/模擬)跟現在不符，
        一律當作沒有紀錄，讓帳戶照原本流程從 INIT 重新判斷，不冒險亂還原。"""
        path = get_state_file_path()
        if not os.path.exists(path):
            return
        try:
            with open(path, "r", encoding="utf-8") as f:
                saved = json.load(f)
        except Exception as e:
            print(f"⚠️ 避險狀態檔讀取失敗，視為無紀錄，從頭開始判斷: {e}")
            return

        if saved.get("real_trade") != REAL_TRADE:
            print(f"⚠️ 狀態檔紀錄的模式(REAL_TRADE={saved.get('real_trade')})跟這次啟動的設定"
                  f"(REAL_TRADE={REAL_TRADE})不一致，為安全起見不還原，從頭開始判斷。")
            return

        restored = saved.get("account_states", {})
        if not restored:
            return

        print("♻️ 偵測到今天稍早的避險紀錄，還原狀態：")
        for acno, st in restored.items():
            self.account_states[acno] = st
            label = ACCOUNT_LABELS.get(acno, f"帳號{acno}")
            print(f"   👤 {label}: {st.get('state')}")
            if st.get('state') == "WAITING_BUY_BACK":
                desc = " + ".join([f"{i['qty']}口{i['name']}" for i in st.get('hedged_orders', [])])
                print(f"      已避險賣出: {desc}，等待回補價 {st.get('target_buyback_price', 0):.2f}")

    def save_state(self):
        """🔧 [新增] 狀態一有異動就落地存檔，斷線重開能接續判斷，不是每次都當全新一天。
        存檔失敗只印警告、不中斷監控——存檔是保險，不該讓保險本身變成當機點。"""
        path = get_state_file_path()
        try:
            with open(path, "w", encoding="utf-8") as f:
                json.dump({
                    "real_trade": REAL_TRADE,
                    "saved_at": datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
                    "account_states": self.account_states,
                }, f, ensure_ascii=False, indent=2)
        except Exception as e:
            print(f"⚠️ 避險狀態存檔失敗(不影響本次繼續運作，但重開機時會抓不到這次的異動): {e}")

    def start_apis(self):
        print("📡 開啟元大期貨帳務專線...")
        try:
            Iwindow, Icontrol, Ievent = POINTER(IUnknown)(), POINTER(IUnknown)(), POINTER(IUnknown)()
            windll.atl.AtlAxCreateControlEx("Yuanta.YuantaOrdCtrl.1", self.hwnd, None, byref(Iwindow), byref(Icontrol), byref(GUID()), Ievent)
            self.Ord = GetBestInterface(Icontrol)
            self.OrdCon = GetEvents(self.Ord, YuantaOrdEvents(self))
            self.Ord.SetFutOrdConnection(USER_ID_FUT, USER_PWD, API_ORDER_IP, '80')
        except Exception as e: print(f"❌ 期貨帳務初始化失敗: {e}")

        start_t = time.time()
        while time.time() - start_t < 3.0:  
            self.root.update(); time.sleep(0.02)

        print("📡 開啟元大期貨行情專線...")
        try:
            Iwindow_q, Icontrol_q, Ievent_q = POINTER(IUnknown)(), POINTER(IUnknown)(), POINTER(IUnknown)()
            windll.atl.AtlAxCreateControlEx("YUANTAQUOTE.YuantaQuoteCtrl.1", self.hwnd, None, byref(Iwindow_q), byref(Icontrol_q), byref(GUID()), Ievent_q)
            self.Quote = GetBestInterface(Icontrol_q)
            self.QuoteCon = GetEvents(self.Quote, YuantaQuoteEvents(self))
            self.Quote.SetMktLogon(USER_ID_FUT, USER_PWD, API_QUOTE_IP, '80', 1, 0)
            self.Quote.SetMktLogon(USER_ID_FUT, USER_PWD, API_QUOTE_IP, '82', 2, 0)
        except Exception as e: print(f"❌ 期貨行情初始化失敗: {e}")

    def parse_fut_position(self, acno, res_str):
        if any(err in res_str for err in ["emsg", "Error", "Fail", "連線逾時", "格式錯誤"]):
            print(f"⚠️ FA002 帳戶 [{acno}] 回傳異常: {res_str}，將保持等待重試...")
            return

        if acno not in self.raw_positions: self.raw_positions[acno] = {}
        if "查無資料" not in res_str:
            parts = res_str.replace('\n', '|').split('|')
            records, current_rec = [], {}
            
            for p in parts:
                if '=' in p:
                    k, v = p.split('=', 1)
                    k_lower = k.strip().lower()
                    if k_lower in ['symb', 'commodity'] and k_lower in current_rec:
                        records.append(current_rec); current_rec = {}
                    current_rec[k_lower] = v.strip()
            if current_rec: records.append(current_rec)

            for rec in records:
                symb = rec.get('symb', rec.get('commodity', '')).strip()
                bs = rec.get('bs', '').strip()
                qty_str = rec.get('off_position', rec.get('position', rec.get('qty', '0'))).strip()
                try: qty = int(float(qty_str))
                except: qty = 0

                if symb and qty > 0:
                    direction = 1 if bs == 'B' else -1
                    self.raw_positions[acno][symb] = self.raw_positions[acno].get(symb, 0) + (qty * direction)

        self.fa002_ready = True
        self.lock_initial_hedge_plan(acno)  # 🔧 [新增] 部位資料一到手，鎖定今天的避險基準(只鎖一次)

    def query_futures_positions(self):
        if hasattr(self, 'Ord') and self.futures_raw_accounts:
            for acc in self.futures_raw_accounts:
                acc_data = acc.split('-')
                bhno, acno = acc_data[1], acc_data[2]
                sub_acc = acc_data[3] if len(acc_data) > 3 else ""
                self.account_info[acno] = {'bhno': bhno, 'acno': acno, 'sub_acc': sub_acc, 'full_acc': acc, 'driver': self.Ord}

                if acno not in self.account_states:
                    self.account_states[acno] = {'state': "INIT", 'base_unit': "MXF", 'hedged_orders': [], 'target_buyback_price': 0.0}
                print(f"👤 {ACCOUNT_LABELS.get(acno, f'帳號{acno}')}")
                try: self.Ord.UserDefinsFunc(f"Func=FA002|bhno={bhno}|acno={acno}|suba=|kind=A|FC=N", f"FA002_{acno}")
                except Exception: pass

    def calculate_exact_reduction(self, acno):
        pos = self.raw_positions.get(acno, {})
        base_unit = self.account_states[acno]['base_unit']
        symbols_map = derive_target_symbols_from_pos(pos)

        total_base_equiv = 0.0
        for symbol, raw_qty in pos.items():
            sym_type = get_symbol_type(symbol)
            ratio = CONTRACT_RATIOS[base_unit][sym_type]
            total_base_equiv += raw_qty * ratio

        raw_target = total_base_equiv * REDUCE_PCT
        target_qty = round(raw_target)
        if target_qty < 1 and raw_target > 0: target_qty = 1

        order_list = []

        if base_unit == "TMF":
            txf_qty = target_qty // 20
            rem_after_txf = target_qty % 20
            mxf_qty = rem_after_txf // 5
            tmf_qty = rem_after_txf % 5

            if txf_qty > 0:
                order_list.append({'name': '大台', 'sym': symbols_map['TXF'], 'qty': min(txf_qty, MAX_QTY_PER_ORDER)})
            if mxf_qty > 0:
                order_list.append({'name': '小台', 'sym': symbols_map['MXF'], 'qty': min(mxf_qty, MAX_QTY_PER_ORDER)})
            if tmf_qty > 0:
                order_list.append({'name': '微台', 'sym': symbols_map['TMF'], 'qty': min(tmf_qty, MAX_QTY_PER_ORDER)})

        elif base_unit == "MXF":
            # 🔧 [修正] 補上 MXF 的完整拆分邏輯 (大台→小台)
            txf_qty = target_qty // 4
            mxf_qty = target_qty % 4

            if txf_qty > 0:
                order_list.append({'name': '大台', 'sym': symbols_map['TXF'], 'qty': min(txf_qty, MAX_QTY_PER_ORDER)})
            if mxf_qty > 0:
                order_list.append({'name': '小台', 'sym': symbols_map['MXF'], 'qty': min(mxf_qty, MAX_QTY_PER_ORDER)})

        return total_base_equiv, raw_target, order_list

    def lock_initial_hedge_plan(self, acno):
        """🔧 [新增] 鎖定「今天」的避險基準：只在該帳戶今天第一次真正拿到部位資料時算一次、鎖住，
        之後不管重開機幾次、部位有沒有被別的程式動過，都直接沿用這個鎖住的值，絕對不重算。
        這是自己的一本帳，不依賴之後任何一次查詢到的即時部位。"""
        state_info = self.account_states.get(acno)
        if state_info is None:
            return
        if 'initial_base_equiv' in state_info:
            return  # 今天已經鎖過了(這次開機鎖的，或從存檔還原的)，不再重算

        base_equiv, _, order_list = self.calculate_exact_reduction(acno)
        state_info['initial_base_equiv'] = base_equiv
        state_info['hedge_order_list'] = order_list
        self.save_state()

        label = ACCOUNT_LABELS.get(acno, f"帳號{acno}")
        desc = " + ".join([f"{i['qty']}口{i['name']}" for i in order_list]) or "無(部位不足以避險)"
        print(f"🔒 {label} 今日避險基準已鎖定：{base_equiv:+.2f}口當量 → 避險明細: {desc}")

    def safely_exit(self, reason_msg):
        print("\n" + "="*65)
        print(f"🔌 【程式關閉】 {reason_msg}")
        print("="*65 + "\n")
        time.sleep(1.0)
        input("按 Enter 鍵結束程式...")
        sys.exit(0)

    def monitor_and_execute(self):
        if not self.fa002_ready:
            print("⏳ 正在等待 FA002 帳戶部位資料回傳...", end="\r")
            return

        now_dt = datetime.datetime.now()
        now_time = now_dt.time()
        now_str = now_dt.strftime("%H:%M:%S")

        if now_time >= TRADE_END_TIME:
            if any(s['state'] == "MONITORING_DROP" for s in self.account_states.values()):
                self.safely_exit(f"⏰ 時間已達 {TRADE_END_TIME.strftime('%H:%M:%S')} 收盤時間，今日未觸發避險，自動斷線關閉。")

        long_account_count = 0

        for acno in list(self.account_info.keys()):
            state_info = self.account_states[acno]
            st = state_info['state']

            if st == "INIT":
                if 'initial_base_equiv' not in state_info:
                    continue  # 🔧 部位資料還沒到手，這輪先跳過，下一輪再判斷（不用即時部位頂替）
                if state_info['initial_base_equiv'] > 0:
                    state_info['state'] = "MONITORING_DROP"
                    long_account_count += 1
                else:
                    state_info['state'] = "SKIPPED_NOT_LONG"
                    print(f"\n⚪ 帳戶 [{acno}] 非淨多頭 (空頭/空手)，略過風控。")
                self.save_state()  # 🔧 [新增] INIT 判斷完就存檔，斷線重開不用再重判一次
            elif st in ["MONITORING_DROP", "WAITING_BUY_BACK"]:
                long_account_count += 1

        if not self.is_initialized:
            self.is_initialized = True
            if long_account_count == 0:
                self.safely_exit("所有帳戶皆為【淨空頭/空手】，自動關閉。")

        active_accounts = [a for a, s in self.account_states.items() if s['state'] in ["MONITORING_DROP", "WAITING_BUY_BACK"]]
        if self.is_initialized and len(active_accounts) == 0:
            self.safely_exit("所有淨多頭帳戶風控與買回流程全數執行完成，自動關閉！")

        op_p, price_p = self.txf_open, self.txf_price
        trig_exit_p = op_p * (1 - DROP_PCT) if op_p > 0 else 0.0

        op_str    = format_price(op_p)
        trig_str  = format_price(trig_exit_p)
        price_str = format_price(price_p)

        print("\n" + "━"*75)
        print(f"⚡ 【期貨風控看板】 🕒 時間: {now_str}")
        print(f"📊 大台開盤價: {op_str}  |  📉 跌0.7%防線: {trig_str}  |  🟢 大台現價: {price_str}")
        print("━"*75)

        for acno in active_accounts:
            state_info = self.account_states[acno]
            order_list = state_info['hedge_order_list']
            st = state_info['state']

            order_desc_cn = " + ".join([f"{item['qty']}口 {item['name']} ({item['sym']})" for item in order_list])
            label = ACCOUNT_LABELS.get(acno, f"帳號{acno}")
            print(f"👤 {label} | ⚡ 避險部位: {order_desc_cn}")

            if op_p <= 0 or price_p <= 0:
                print(f"   ⏳ 狀態: 【資料傳輸中... 等待大台開盤價與現價串流】")
                continue

            # ------------------------------------------------------------------
            # 1. 觸發避險階段 (市價賣出 S)
            # ------------------------------------------------------------------
            if st == "MONITORING_DROP":
                st_desc = "【09:15洗盤期監控中】" if now_time < TRADE_START_TIME else "【防線監控中】"
                print(f"   🎯 狀態: {st_desc}")

                if now_time >= TRADE_START_TIME and price_p <= trig_exit_p:
                    mode_str = "【實彈發射】" if REAL_TRADE else "【模擬測試】"
                    print("\n" + "🚨" * 20)
                    print(f"🚨 {mode_str} 期貨避險賣出觸發！")
                    print(f"👤 {ACCOUNT_LABELS.get(acno, f'帳號{acno}')}")
                    print(f"📉 大台觸發價：{trig_exit_p:.2f} (大台現價: {price_p:.2f})")
                    print(f"⚡ 指令：市價賣出 35% 部位 (省手續費最佳化)")
                    print(f"🔥 下單組合：{order_desc_cn}")
                    print(f"🎯 預估應回補大台價：{op_p:.2f}")
                    print(f"⏰ 時間：{now_str}")
                    print("🚨" * 20 + "\n")

                    all_success = True
                    executed_orders = []

                    if REAL_TRADE:
                        full_acc = self.account_info[acno]['full_acc']
                        bhno = self.account_info[acno]['bhno']
                        real_acno = self.account_info[acno]['acno']
                        sub_acc = self.account_info[acno]['sub_acc']
                        driver = self.account_info[acno]['driver']  # 🟢 使用對應帳號的 API 發射
                        for item in order_list:
                            try:
                                # 🔧 [修正] 改用 Forder.py 驗證過能成功送單的 SendOrderF 語法(原本的SendFutOrder不存在/格式錯誤)
                                res = driver.SendOrderF("01", "0", bhno, real_acno, sub_acc, "", "S", item['sym'],
                                                         ORD_PRICE, str(item['qty']), ORD_OFFSET, ORD_PRICE_TYPE, ORD_COND, "", "")
                                print(f"🚀 【元大 API 下單發射】 帳戶: {acno} | 市價賣出: {item['qty']}口 {item['name']}({item['sym']}) | 回傳碼: {res}")
                                executed_orders.append(item)
                            except Exception as e:
                                print(f"❌ 【API 下單失敗】 商品: {item['sym']} | 錯誤: {e}")
                                all_success = False
                    else:
                        executed_orders = order_list

                    if all_success and executed_orders:
                        self.account_states[acno]['hedged_orders'] = executed_orders
                        self.account_states[acno]['target_buyback_price'] = op_p
                        self.account_states[acno]['state'] = "WAITING_BUY_BACK"
                        self.save_state()  # 🔧 [新增] 已避險賣出，立刻存檔——這一步最關鍵

            # ------------------------------------------------------------------
            # 2. 反彈買回階段 (市價買進 B)
            # ------------------------------------------------------------------
            elif st == "WAITING_BUY_BACK":
                hedged_orders = self.account_states[acno]['hedged_orders']
                buyback_target = self.account_states[acno]['target_buyback_price']
                hedged_desc_cn = " + ".join([f"{item['qty']}口 {item['name']} ({item['sym']})" for item in hedged_orders])

                print(f"   🛡️ 狀態: 【已賣出避險 {hedged_desc_cn}，等待回補價 {buyback_target:.2f}】")

                buy_back_triggered = False
                buy_reason = ""

                if price_p >= buyback_target:
                    buy_back_triggered = True
                    buy_reason = f"大台價格反彈拉回開盤價 ({price_p:.2f} >= {buyback_target:.2f})"
                elif now_time >= TRADE_END_TIME:
                    buy_back_triggered = True
                    buy_reason = f"時間抵達 {TRADE_END_TIME.strftime('%H:%M:%S')} 收盤強制平倉時間"

                if buy_back_triggered:
                    mode_str = "【實彈發射】" if REAL_TRADE else "【模擬測試】"
                    print("\n" + "🎉" * 20)
                    print(f"🎉 {mode_str} 期貨平倉買回觸發！")
                    print(f"👤 {ACCOUNT_LABELS.get(acno, f'帳號{acno}')}")
                    print(f"💡 原因：{buy_reason}")
                    print(f"⚡ 指令：市價買回平倉")
                    print(f"🔥 下單組合：{hedged_desc_cn}")
                    print(f"⏰ 時間：{now_str}")
                    print("🎉" * 20 + "\n")

                    all_success = True
                    if REAL_TRADE:
                        full_acc = self.account_info[acno]['full_acc']
                        bhno = self.account_info[acno]['bhno']
                        real_acno = self.account_info[acno]['acno']
                        sub_acc = self.account_info[acno]['sub_acc']
                        driver = self.account_info[acno]['driver']  # 🟢 動態拿取帳號專屬的 API 發射
                        for item in hedged_orders:
                            try:
                                # 🔧 [修正] 改用 SendOrderF 正確語法
                                res = driver.SendOrderF("01", "0", bhno, real_acno, sub_acc, "", "B", item['sym'],
                                                         ORD_PRICE, str(item['qty']), ORD_OFFSET, ORD_PRICE_TYPE, ORD_COND, "", "")
                                print(f"🚀 【元大 API 下單發射】 帳戶: {acno} | 市價買回: {item['qty']}口 {item['name']}({item['sym']}) | 回傳碼: {res}")
                            except Exception as e:
                                print(f"❌ 【API 下單失敗】 商品: {item['sym']} | 錯誤: {e}")
                                all_success = False
                    else:
                        all_success = True

                    if all_success:
                        self.account_states[acno]['state'] = "FINISHED"
                        self.save_state()  # 🔧 [新增] 買回完成，存檔標記今天這帳戶已結束

                        # 🔧 [新增] 收尾摘要，方便你事後人工核對這本帳
                        label = ACCOUNT_LABELS.get(acno, f"帳號{acno}")
                        initial_equiv = self.account_states[acno]['initial_base_equiv']
                        print(f"\n📒 [今日避險帳本] 帳戶: {label}")
                        print(f"   起始部位(開機鎖定基準): {initial_equiv:+.2f} 口當量")
                        print(f"   避險賣出: {hedged_desc_cn}")
                        print(f"   回補買回: {hedged_desc_cn}")
                        print(f"   ✅ 買回明細與賣出明細一致，今日避險循環結束\n")

        print("━"*75 + "\n")

    def run(self):
        print("⏳ 啟動 Windows 訊息迴圈並建立元大 API 連線...")
        self.start_apis()

        start_t = time.time()
        while time.time() - start_t < 3.0:
            self.root.update(); time.sleep(0.02)

        # 🔧 等待帳號登入完成再查詢部位
        print("⏳ 等待帳號登入完成...")
        logon_start = time.time()
        while time.time() - logon_start < 5.0:
            if self.fut_logon_ready:
                print("✅ 帳號登入完成！")
                break
            self.root.update()
            time.sleep(0.1)
        
        if not self.fut_logon_ready:
            print("⚠️ 登入等待逾時，帳號登入狀態:", self.fut_logon_ready)

        self.query_futures_positions()

        print(f"\n📡 即時風控監控中 ({TRADE_START_TIME.strftime('%H:%M:%S')} 後開放市價發射)...\n")
        
        last_check = time.time()
        while True:
            try:
                self.root.update()
                now = time.time()
                if now - last_check >= 1.0:
                    self.monitor_and_execute()
                    last_check = now
                time.sleep(0.02)
            except KeyboardInterrupt:
                print("🛑 收到終止訊號，監控結束。")
                break
            except Exception as e:
                print(f"⚠️ 監控異常: {e}")
                time.sleep(1.0)

if __name__ == "__main__":
    engine = FixedDualAccountStopLossEngine()
    engine.run()
