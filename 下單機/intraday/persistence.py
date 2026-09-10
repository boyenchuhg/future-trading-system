# -*- coding: utf-8 -*-
"""
三檔案架構 —— JSON狀態檔 / 系統LOG / 交易對帳本CSV
================================================================
採用使用者提供的Gemini規格書設計，2026/09/09整合進主系統。
三個檔案各自的用途（不要混用）：
  1. daytrade_state_YYYYMMDD.json —— 給程式自己讀，斷線/崩潰重開機用來
     恢復記憶，防止重複進場或重複平倉。
  2. daytrade_system_YYYYMMDD.log —— 給工程排查用，記錄連線事件、錯誤。
  3. trade_ledger_YYYYMMDD.csv —— 給人看的對帳本，utf-8-sig編碼方便Excel開啟。
"""
import os
import json
import csv
import logging
from datetime import datetime
from typing import Optional

BOT_ID = "DAY_TRADE_BOT"


# ============================================================
# 系統LOG —— 給工程排查用
# ============================================================
def setup_system_logger(log_dir: str) -> logging.Logger:
    os.makedirs(log_dir, exist_ok=True)
    path = os.path.join(log_dir, f"daytrade_system_{datetime.now():%Y%m%d}.log")
    logger = logging.getLogger("daytrade")
    logger.setLevel(logging.DEBUG)
    if not logger.handlers:  # 避免重複執行main()時重複加handler
        fh = logging.FileHandler(path, encoding="utf-8")
        fh.setFormatter(logging.Formatter(
            fmt="[%(asctime)s.%(msecs)03d] [%(levelname)s] [%(module)s] %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S"
        ))
        logger.addHandler(fh)
        # 同時印到終端機，方便即時盯著看
        sh = logging.StreamHandler()
        sh.setFormatter(logging.Formatter("[%(levelname)s] %(message)s"))
        logger.addHandler(sh)
    return logger


# ============================================================
# JSON狀態檔 —— 給程式自己讀，防重複進場/平倉、斷線重開機恢復記憶
# ============================================================
def state_file_path(log_dir: str) -> str:
    return os.path.join(log_dir, f"daytrade_state_{datetime.now():%Y%m%d}.json")


def load_or_create_state(log_dir: str, strategy_names: list) -> dict:
    """程式啟動時呼叫。已存在當天的狀態檔就讀回來（斷線重開機的情境），
    不存在就建立一份全新的（今天第一次啟動）。"""
    path = state_file_path(log_dir)
    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            return json.load(f)

    state = {
        "bot_id": BOT_ID,
        "date": datetime.now().strftime("%Y-%m-%d"),
        "strategies": {},
    }
    for name in strategy_names:
        state["strategies"][name] = {
            "status": "INIT",          # INIT -> IN_POSITION -> FINISHED
            "trade_used": False,       # S1/S2/S4用：今天這1筆額度用掉了沒
            "long_used": False,        # S3用：多單額度用掉了沒
            "short_used": False,       # S3用：空單額度用掉了沒
            "virtual_pos": 0,          # 目前虛擬部位，+qty=多單 -qty=空單 0=空手
            "entry_price": None,
            "day_open": None,
            "orders": [],              # [{time, action, side, qty, price, order_no}]
        }
    save_state(log_dir, state)
    return state


def save_state(log_dir: str, state: dict):
    path = state_file_path(log_dir)
    tmp_path = path + ".tmp"
    # 先寫暫存檔再改名，避免寫到一半程式崩潰導致JSON檔案本身壞掉讀不回來
    with open(tmp_path, "w", encoding="utf-8") as f:
        json.dump(state, f, ensure_ascii=False, indent=2)
    os.replace(tmp_path, path)


# ============================================================
# 交易對帳本CSV —— 給人看，utf-8-sig方便Excel開啟不亂碼
# ============================================================
CSV_HEADER = ["交易時間", "下單機識別", "動作類別", "買賣別", "商品代碼",
              "數量(口)", "委託/成交價", "API單號", "執行後虛擬部位", "備註"]


def append_trade_ledger(log_dir: str, action: str, side: str, symbol: str,
                         qty: int, price: float, order_no: str,
                         virtual_pos_after: int, note: str = ""):
    path = os.path.join(log_dir, f"trade_ledger_{datetime.now():%Y%m%d}.csv")
    write_header = not os.path.exists(path)
    with open(path, "a", newline="", encoding="utf-8-sig") as f:
        w = csv.writer(f)
        if write_header:
            w.writerow(CSV_HEADER)
        w.writerow([
            datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            BOT_ID, action, side, symbol, qty, price,
            order_no, virtual_pos_after, note,
        ])
