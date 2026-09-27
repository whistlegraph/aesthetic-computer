#!/usr/bin/env python3
"""Mac built-in display entrance cue. Probe is silent; run needs a future local epoch.

DisplayServices signatures: https://github.com/nriley/brightness/blob/master/brightness.c
This helper does not start audio. The full-system conductor owns authorization,
clock skew correction, and sending cancellation to every participating Mac.
"""
import argparse
import ctypes as c
import json
import math
import os
from pathlib import Path
import signal
import time


class Display:
    def __init__(self):
        self.cg = c.CDLL('/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics')
        self.ds = c.CDLL('/System/Library/PrivateFrameworks/DisplayServices.framework/DisplayServices')
        self.cg.CGGetOnlineDisplayList.argtypes = [c.c_uint32, c.POINTER(c.c_uint32), c.POINTER(c.c_uint32)]
        self.cg.CGDisplayIsBuiltin.argtypes = [c.c_uint32]
        ids, count = (c.c_uint32 * 16)(), c.c_uint32()
        if self.cg.CGGetOnlineDisplayList(16, ids, c.byref(count)):
            raise RuntimeError('Cannot enumerate displays')
        builtin = [d for d in ids[:count.value] if self.cg.CGDisplayIsBuiltin(d)]
        if len(builtin) != 1:
            raise RuntimeError('Expected exactly one built-in singer display')
        self.id = builtin[0]
        self.ds.DisplayServicesGetBrightness.argtypes = [c.c_uint32, c.POINTER(c.c_float)]
        self.ds.DisplayServicesSetBrightness.argtypes = [c.c_uint32, c.c_float]

    def get(self):
        value = c.c_float()
        if self.ds.DisplayServicesGetBrightness(self.id, c.byref(value)):
            raise RuntimeError('Brightness read failed')
        if not math.isfinite(value.value) or not 0 <= value.value <= 1:
            raise RuntimeError('Invalid brightness reading')
        return value.value

    def set(self, value):
        if not math.isfinite(value) or not 0 <= value <= 1:
            raise ValueError('Brightness must be 0..1')
        if self.ds.DisplayServicesSetBrightness(self.id, value):
            raise RuntimeError('Brightness write failed')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('command', choices=['probe', 'run'])
    p.add_argument('--start-epoch', type=float)
    p.add_argument('--duration', type=float, default=64 * 60 / 88)
    p.add_argument('--dim', type=float, default=.28)
    p.add_argument('--bright', type=float, default=.85)
    p.add_argument('--cancel-file', type=Path)
    p.add_argument('--status-file', type=Path)
    a = p.parse_args()
    display = Display()
    original = display.get()
    if a.command == 'probe':
        # Exercise the hardware write without changing the visible level.
        display.set(original)
        measured = display.get()
        if abs(measured - original) > .02:
            raise RuntimeError('Brightness write/read verification failed')
        print(json.dumps(dict(ready=True, displayId=display.id, brightness=measured,
                             observedAt=time.time(), pid=os.getpid(), audioStarted=False)))
        return
    if (a.start_epoch is None or not math.isfinite(a.start_epoch)
            or not 3 <= a.start_epoch - time.time() <= 120):
        p.error('run requires a local-clock start epoch 3..120 seconds ahead')
    if not math.isfinite(a.duration) or not 0 < a.duration <= 600:
        p.error('duration must be finite, 0..600 seconds')
    if not all(math.isfinite(v) and .1 <= v <= 1 for v in [a.dim, a.bright]):
        p.error('brightness levels must be .1..1')
    if a.cancel_file is None or a.status_file is None:
        p.error('run requires unique --cancel-file and --status-file paths')
    if a.cancel_file.exists() or a.status_file.exists():
        p.error('use fresh run paths; an existing cancellation must never be ignored')
    cancelled = False

    def cancel(*_):
        nonlocal cancelled
        cancelled = True

    def status(phase, **extra):
        data = dict(phase=phase, startEpoch=a.start_epoch, originalBrightness=original,
                    displayId=display.id, pid=os.getpid(), observedAt=time.time(), **extra)
        temp = a.status_file.with_suffix('.tmp')
        temp.write_text(json.dumps(data))
        temp.replace(a.status_file)

    signal.signal(signal.SIGTERM, cancel)
    signal.signal(signal.SIGINT, cancel)
    # Freeze the wall-to-monotonic mapping before the entrance.
    downbeat = time.monotonic() + (a.start_epoch - time.time())
    status('armed')
    previous = original
    try:
        while not cancelled and not a.cancel_file.exists():
            t = time.monotonic() - downbeat
            if t >= a.duration:
                break
            target = original
            if -.9 <= t < -.3:
                x = (t + .9) / .6
                target = original + (a.dim - original) * (x * x * (3 - 2 * x))
            elif -.3 <= t < 0:
                target = a.dim
            elif t >= 0:
                x = min(1, t / .55)
                target = a.dim + (a.bright - a.dim) * (x * x * (3 - 2 * x))
            if abs(target - previous) > .001:
                display.set(target)
                previous = target
            time.sleep(1 / 30)
    finally:
        display.set(original)
        status('restored', cancelled=cancelled or a.cancel_file.exists(), brightness=display.get())


if __name__ == '__main__':
    main()
