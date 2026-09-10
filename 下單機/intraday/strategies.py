# -*- coding: utf-8 -*-
"""
S1~S4 策略狀態機 —— 依《S1_S4_下單機開發規格書》(2026-09) 實作
================================================================
四支共用的機制：
  - 持續監看每一筆tick（B案，不等K棒收完才判斷）
  - 統計量（均量/箱體/VWAP）用1分K聚合（見strategy_common_v2.py）
  - 進出場都經過JSON狀態檔同步（斷線重開機防重複）+ CSV對帳本記錄
  - SAFE_MODE開關統一控制（見persistence/主程式）
  - 13:43:50強制平倉（Gemini規格書：提早70秒，預留流動性與API重試緩衝）

⚠️ 2026/09/09 重要更正：舊版S1的「停損後最多可進場2筆」規則，
   已被最新《S1_S4_下單機開發規格書》取代——S1/S2/S4現在都是
   「每日限額1筆，多單或空單擇一，先觸發者為準」，不再有二次進場。
   S3維持原本「多空各限1次」不變（本來就是獨立判斷）。
"""
from datetime import time as dtime
from typing import Optional, Callable
from strategy_common_v2 import Bar1Min, RollingVolumeTracker, BoxTracker, VWAPTracker
from persistence import save_state, append_trade_ledger

EXIT_TIME = dtime(13, 43, 50)  # 四策略共用：當沖強制平倉時間（Gemini規格書）


class StrategyBase:
    """
    S1/S2/S4共用骨架：單方向擇一觸發、單一停損參考價、每日限額1筆。
    S3因為多空獨立判斷、停損定義也不同，不繼承這個，另外寫。
    """
    name = "BASE"

    def __init__(self, qty: int, vol_mult: float, vol_window_min: int,
                 log_dir: str, state: dict, symbol: str,
                 place_order_fn: Callable, logger):
        self.qty = qty
        self.vol_mult = vol_mult
        self.vol_tracker = RollingVolumeTracker(vol_window_min)
        self.log_dir = log_dir
        self.state = state  # 整個JSON狀態dict的參照，各策略各自讀寫自己那把key
        self.symbol = symbol
        self.place_order_fn = place_order_fn
        self.logger = logger
        self.day_open: Optional[float] = None

        # 從JSON狀態檔恢復（斷線重開機情境）
        s = self.state["strategies"][self.name]
        self.trade_used = s["trade_used"]
        self.position: Optional[str] = "long" if s["virtual_pos"] > 0 else (
            "short" if s["virtual_pos"] < 0 else None)
        self.entry_price = s["entry_price"]
        if s["day_open"] is not None:
            self.day_open = s["day_open"]
        if self.trade_used or self.position is not None:
            self.logger.info(f"[{self.name}] 從狀態檔恢復：trade_used={self.trade_used} "
                              f"position={self.position} entry_price={self.entry_price}")

    def on_bar_close(self, bar: Bar1Min):
        if self.day_open is None:
            self.day_open = bar.open
            self._sync_state(day_open=self.day_open)
        self.vol_tracker.on_bar_close(bar.volume)

    def _stop_price(self) -> float:
        """S1/S2用開盤價；S4覆寫這個方法改用箱體中線"""
        return self.day_open

    def _entry_window_ok(self, t: dtime) -> bool:
        raise NotImplementedError

    def _check_breakout(self, price: float) -> Optional[str]:
        """回傳 'long' / 'short' / None，由子類實作各自的門檻定義"""
        raise NotImplementedError

    def on_tick(self, t: dtime, price: float, delta_qty: int = 0):
        # delta_qty參數是為了跟S3(需要VWAP用到即時量)呼叫介面一致，
        # S1/S2/S4用不到，接收但忽略。
        if self.day_open is None or self.trade_used:
            return

        if self.position is None:
            if self.state.get("data_gap_today", False):
                return
            if not self._entry_window_ok(t):
                return
            if not self.vol_tracker.vol_ok(self.vol_mult):
                return
            direction = self._check_breakout(price)
            if direction:
                self._enter(direction, price, t)
        else:
            if t >= EXIT_TIME:
                self._exit(price, t, "13:43:50強制平倉")
                return
            stop = self._stop_price()
            if self.position == "long" and price <= stop:
                self._exit(price, t, "回觸停損價")
            elif self.position == "short" and price >= stop:
                self._exit(price, t, "回觸停損價")

    def _enter(self, direction: str, price: float, t: dtime):
        self.position = direction
        self.entry_price = price
        side_cn = "買" if direction == "long" else "賣"
        self.logger.info(f"[{self.name}] {t} 進場 {direction} {self.qty}口 @ {price}")
        print(f"[{self.name}訊號] {t} 【本來要下單】{side_cn} {self.qty}口 觸價進場 @ {price}")
        vpos = self.qty if direction == "long" else -self.qty
        order_no = self.place_order_fn(self.name, direction, self.qty, price, "entry")
        self._record_order(t, "entry", side_cn, price, order_no)
        self._sync_state(status="IN_POSITION", virtual_pos=vpos, entry_price=price)
        append_trade_ledger(self.log_dir, "進場", side_cn, self.symbol, self.qty,
                             price, order_no, vpos, f"{self.name}觸價進場(大台價位，實際交易微台)")

    def _exit(self, price: float, t: dtime, reason: str):
        pnl_dir = 1 if self.position == "long" else -1
        pnl = (price - self.entry_price) * pnl_dir if self.entry_price else 0
        side_cn = "賣" if self.position == "long" else "買"  # 平倉方向跟進場相反
        closing_direction = self.position  # 送單前先記下來，place_order_fn需要知道平的是long還是short才能算B/S
        self.logger.info(f"[{self.name}] {t} 出場 @ {price} ({reason}) pnl={pnl:+.1f}點/口")
        print(f"[{self.name}訊號] {t} 【本來要下單】出場平倉 @ {price} "
              f"({reason})，估計損益(未含手續費)={pnl:+.1f}點/口")
        self.trade_used = True
        self.position = None
        order_no = self.place_order_fn(self.name, closing_direction, self.qty, price, "exit")
        self._record_order(t, "exit", side_cn, price, order_no)
        self._sync_state(status="FINISHED", virtual_pos=0, trade_used=True, entry_price=None)
        append_trade_ledger(self.log_dir, "平倉", side_cn, self.symbol, self.qty,
                             price, order_no, 0, f"{self.name} {reason}(大台價位，實際交易微台)")

    def _record_order(self, t: dtime, action: str, side_cn: str, price: float, order_no: str):
        s = self.state["strategies"][self.name]
        s["orders"].append({
            "time": t.strftime("%H:%M:%S"), "action": action, "side": side_cn,
            "qty": self.qty, "price": price, "order_no": order_no,
        })

    def _sync_state(self, **kwargs):
        s = self.state["strategies"][self.name]
        s.update(kwargs)
        save_state(self.log_dir, self.state)


class S1State(StrategyBase):
    """開盤突破：09:15起，突破開盤價±0.4%，停損=開盤價"""
    name = "S1"

    def __init__(self, breakout_pct: float, entry_time: dtime, **kwargs):
        super().__init__(**kwargs)
        self.breakout_pct = breakout_pct
        self.entry_time = entry_time

    def _entry_window_ok(self, t: dtime) -> bool:
        return self.entry_time <= t < EXIT_TIME

    def _check_breakout(self, price: float) -> Optional[str]:
        upper = self.day_open * (1 + self.breakout_pct)
        lower = self.day_open * (1 - self.breakout_pct)
        if price >= upper:
            return "long"
        if price <= lower:
            return "short"
        return None


class S2State(StrategyBase):
    """缺口+箱體：跳空≥0.7%才觀察，突破08:45-09:15箱體，停損=開盤價"""
    name = "S2"

    def __init__(self, gap_pct: float, entry_time: dtime, prev_close: Optional[float], **kwargs):
        super().__init__(**kwargs)
        self.gap_pct = gap_pct
        self.entry_time = entry_time
        self.prev_close = prev_close
        self.box = BoxTracker(dtime(8, 45), self.entry_time)
        self.disabled_today = False
        s = self.state["strategies"][self.name]
        self.disabled_today = s.get("disabled_today", False)

    def on_bar_close(self, bar: Bar1Min):
        first_bar = self.day_open is None
        super().on_bar_close(bar)
        if first_bar and not self.disabled_today:
            if self.prev_close is None:
                self.logger.warning(f"[{self.name}] 沒有前日收盤價，無法判斷跳空，今天停用")
                self.disabled_today = True
                self._sync_state(disabled_today=True)
            else:
                gap = abs(self.day_open - self.prev_close) / self.prev_close
                if gap < self.gap_pct:
                    self.logger.info(f"[{self.name}] 跳空{gap*100:.2f}%未達{self.gap_pct*100:.1f}%門檻，今天停用")
                    self.disabled_today = True
                    self._sync_state(disabled_today=True)
                else:
                    self.logger.info(f"[{self.name}] 跳空{gap*100:.2f}%達標，正常觀察")
        self.box.on_bar_close(bar)

    def _entry_window_ok(self, t: dtime) -> bool:
        if self.disabled_today or not self.box.ready:
            return False
        return self.entry_time <= t < EXIT_TIME

    def _check_breakout(self, price: float) -> Optional[str]:
        if price >= self.box.high:
            return "long"
        if price <= self.box.low:
            return "short"
        return None


class S3State:
    """
    ORB+VWAP：08:45-08:55箱體，方向要跟VWAP一致，停損=箱體中線。
    多空獨立判斷，各限1次（跟S1/S2/S4的StrategyBase骨架不一樣，獨立寫）。
    """
    name = "S3"

    def __init__(self, qty: int, vol_mult: float, vol_window_min: int,
                 box_end: dtime, log_dir: str, state: dict, symbol: str,
                 place_order_fn: Callable, logger):
        self.qty = qty
        self.vol_mult = vol_mult
        self.vol_tracker = RollingVolumeTracker(vol_window_min)
        self.box = BoxTracker(dtime(8, 45), box_end)
        self.box_end = box_end
        self.vwap = VWAPTracker()
        self.log_dir = log_dir
        self.state = state
        self.symbol = symbol
        self.place_order_fn = place_order_fn
        self.logger = logger

        s = self.state["strategies"][self.name]
        self.long_used = s["long_used"]
        self.short_used = s["short_used"]
        self.position: Optional[str] = "long" if s["virtual_pos"] > 0 else (
            "short" if s["virtual_pos"] < 0 else None)
        self.entry_price = s["entry_price"]
        if self.long_used or self.short_used or self.position is not None:
            self.logger.info(f"[{self.name}] 從狀態檔恢復：long_used={self.long_used} "
                              f"short_used={self.short_used} position={self.position}")

    def on_bar_close(self, bar: Bar1Min):
        self.vol_tracker.on_bar_close(bar.volume)
        self.box.on_bar_close(bar)
        self.vwap.on_bar_close(bar)

    def on_tick(self, t: dtime, price: float, delta_qty: int = 0):
        self.vwap.on_tick(price, delta_qty)

        if self.position is None:
            if t < self.box_end or t >= EXIT_TIME or not self.box.ready:
                return
            if self.state.get("data_gap_today", False):
                return
            if self.long_used and self.short_used:
                return  # 兩個方向都用過了，今天結束
            if not self.vol_tracker.vol_ok(self.vol_mult):
                return
            vwap_now = self.vwap.vwap_live
            if vwap_now is None:
                return
            if (not self.long_used and price >= self.box.high
                    and self.box.high > vwap_now):
                self._enter("long", price, t)
            elif (not self.short_used and price <= self.box.low
                  and self.box.low < vwap_now):
                self._enter("short", price, t)
        else:
            if t >= EXIT_TIME:
                self._exit(price, t, "13:43:50強制平倉")
                return
            stop = self.box.mid
            if self.position == "long" and price <= stop:
                self._exit(price, t, "回觸箱體中線停損")
            elif self.position == "short" and price >= stop:
                self._exit(price, t, "回觸箱體中線停損")

    def _enter(self, direction: str, price: float, t: dtime):
        self.position = direction
        self.entry_price = price
        side_cn = "買" if direction == "long" else "賣"
        self.logger.info(f"[{self.name}] {t} 進場 {direction} {self.qty}口 @ {price}")
        print(f"[{self.name}訊號] {t} 【本來要下單】{side_cn} {self.qty}口 觸價進場 @ {price}")
        vpos = self.qty if direction == "long" else -self.qty
        order_no = self.place_order_fn(self.name, direction, self.qty, price, "entry")
        self._record_order(t, "entry", side_cn, price, order_no)
        self._sync_state(status="IN_POSITION", virtual_pos=vpos, entry_price=price)
        append_trade_ledger(self.log_dir, "進場", side_cn, self.symbol, self.qty,
                             price, order_no, vpos, f"{self.name}觸價進場({direction})(大台價位，實際交易微台)")

    def _exit(self, price: float, t: dtime, reason: str):
        pnl_dir = 1 if self.position == "long" else -1
        pnl = (price - self.entry_price) * pnl_dir if self.entry_price else 0
        side_cn = "賣" if self.position == "long" else "買"
        closing_direction = self.position
        self.logger.info(f"[{self.name}] {t} 出場 @ {price} ({reason}) pnl={pnl:+.1f}點/口")
        print(f"[{self.name}訊號] {t} 【本來要下單】出場平倉 @ {price} "
              f"({reason})，估計損益(未含手續費)={pnl:+.1f}點/口")
        if self.position == "long":
            self.long_used = True
        else:
            self.short_used = True
        self.position = None
        status = "FINISHED" if (self.long_used and self.short_used) else "IN_POSITION"
        # 註：status用IN_POSITION代表「還有一個方向的額度沒用完，可能還會再動」，
        # 不完全符合原本INIT->IN_POSITION->FINISHED的字面意義，但S3本來就是特例，
        # 用long_used/short_used這兩個旗標才是真正判斷依據，status只是輔助顯示。
        order_no = self.place_order_fn(self.name, closing_direction, self.qty, price, "exit")
        self._record_order(t, "exit", side_cn, price, order_no)
        self._sync_state(status=status, virtual_pos=0, entry_price=None,
                          long_used=self.long_used, short_used=self.short_used)
        append_trade_ledger(self.log_dir, "平倉", side_cn, self.symbol, self.qty,
                             price, order_no, 0, f"{self.name} {reason}(大台價位，實際交易微台)")

    def _record_order(self, t: dtime, action: str, side_cn: str, price: float, order_no: str):
        s = self.state["strategies"][self.name]
        s["orders"].append({
            "time": t.strftime("%H:%M:%S"), "action": action, "side": side_cn,
            "qty": self.qty, "price": price, "order_no": order_no,
        })

    def _sync_state(self, **kwargs):
        s = self.state["strategies"][self.name]
        s.update(kwargs)
        save_state(self.log_dir, self.state)


class S4State(StrategyBase):
    """午盤動能：09:15-10:30收框，10:30-13:30進場，停損=箱體中線"""
    name = "S4"

    def __init__(self, box_start: dtime, box_end: dtime,
                 entry_start: dtime, entry_end: dtime, **kwargs):
        super().__init__(**kwargs)
        self.box = BoxTracker(box_start, box_end)
        self.entry_start = entry_start
        self.entry_end = entry_end

    def on_bar_close(self, bar: Bar1Min):
        super().on_bar_close(bar)
        self.box.on_bar_close(bar)

    def _stop_price(self) -> float:
        return self.box.mid

    def _entry_window_ok(self, t: dtime) -> bool:
        if not self.box.ready:
            return False
        return self.entry_start <= t < self.entry_end

    def _check_breakout(self, price: float) -> Optional[str]:
        if price >= self.box.high:
            return "long"
        if price <= self.box.low:
            return "short"
        return None
