# -*- coding: utf-8 -*-
"""
共用策略元件 —— S1~S4四策略共用，2026/09/09依《S1_S4_下單機開發規格書》重寫
================================================================
跟舊版(strategy_common.py)的差異：
  1. 統計聚合改成1分K（規格書2.1節明定），不是5分K
  2. RollingVolumeTracker改成「可指定窗口分鐘數」，不再寫死20根，
     因為四策略窗口不同（S1:100分鐘 S2:150 S3:75 S4:150）
  3. 進出場觸發改成「持續監看每一筆tick」，不等K棒收盤才判斷
     （規格書2節明定：這是B案觸價即時觸發，不是A案等K棒收完）
"""
from dataclasses import dataclass
from datetime import time as dtime
from typing import Optional, List


@dataclass
class Bar1Min:
    """1分鐘K棒"""
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


class RollingVolumeTracker:
    """
    滾動均量，窗口以「分鐘數」指定（1分K下=根數）。
    規則沿用交接摘要精確定義：V[i-1] > 倍數 × MA[i]，
    視窗結束於i-1（含），i-1的根數 < 窗口分鐘數時取全部平均。
    """
    def __init__(self, window_minutes: int):
        self.window_minutes = window_minutes
        self.volumes: List[int] = []

    def on_bar_close(self, volume: int):
        self.volumes.append(volume)

    def vol_ok(self, mult: float) -> bool:
        n = len(self.volumes)
        if n == 0:
            return False
        window = self.volumes[-self.window_minutes:] if n >= self.window_minutes else self.volumes[:]
        ma = sum(window) / len(window)
        return self.volumes[-1] > mult * ma


class BoxTracker:
    """箱體高低點，累積指定時間區間內的高低"""
    def __init__(self, start: dtime, end: dtime):
        self.start = start
        self.end = end
        self.high: Optional[float] = None
        self.low: Optional[float] = None
        self.finalized = False

    def on_bar_close(self, bar: Bar1Min):
        if self.finalized:
            return
        if self.start <= bar.bucket_start <= self.end:
            self.high = bar.high if self.high is None else max(self.high, bar.high)
            self.low = bar.low if self.low is None else min(self.low, bar.low)
            if bar.bucket_start == self.end:
                self.finalized = True

    @property
    def mid(self) -> Optional[float]:
        if self.high is None or self.low is None:
            return None
        return (self.high + self.low) / 2

    @property
    def ready(self) -> bool:
        return self.finalized and self.high is not None and self.low is not None


class VWAPTracker:
    """
    VWAP，自08:45累積不重置，典型價=(H+L+C)/3。
    支援即時tick更新（vwap_live，含當根K棒進行中的部分成交量）
    與完整K棒收盤更新（vwap_closed）兩種。S3用vwap_live（B案）。
    """
    def __init__(self):
        self.cum_pv = 0.0
        self.cum_v = 0
        self._cur_pv = 0.0
        self._cur_v = 0

    def on_bar_close(self, bar: Bar1Min):
        typ = (bar.high + bar.low + bar.close) / 3
        vol = bar.volume if bar.volume > 0 else 0
        self.cum_pv += typ * vol
        self.cum_v += vol
        self._cur_pv = 0.0
        self._cur_v = 0

    def on_tick(self, price: float, delta_qty: int):
        if delta_qty <= 0:
            return
        self._cur_pv += price * delta_qty
        self._cur_v += delta_qty

    @property
    def vwap_live(self) -> Optional[float]:
        total_v = self.cum_v + self._cur_v
        if total_v == 0:
            return None
        return (self.cum_pv + self._cur_pv) / total_v
