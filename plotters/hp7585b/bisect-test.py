#!/usr/bin/env python3
"""Send progressively-larger slabs of the slide to find which section
triggers E2. After each slab we read OE; — first non-zero reading
identifies the offending section."""

from __future__ import annotations

import os
import sys
import time
from pathlib import Path

import serial

XON, XOFF = 0x11, 0x13

SECTIONS = [
    ("init",          ["IN;DT\x03;DI1,0;LT;SP1;VS25;FS2;\n"]),
    ("title",         ["SI1.476,2.214;PA-7969,3500;LBnotepat.com keymap\x03\n"]),
    ("divider",       ["PU;PA-7567,3050;PD;PA7567,3050;PU;\n"]),
    ("piano-naturals",[]),  # filled below
    ("piano-sharps",  []),
    ("qwerty-row1",   []),
    ("qwerty-row2",   []),
    ("qwerty-row3",   []),
    ("connectors",    []),
    ("tagline",       []),
    ("urls",          []),
    ("park",          ["LT;PU;PA0,0;SP0;\n"]),
]

# Reuse the actual HPGL we generate by parsing the existing file into the
# section buckets. Simpler: just send the file in halves first.
FILE = Path("/Users/jas/aesthetic-computer/plotters/hp7585b/notepat-slide.hpgl")
data = FILE.read_text()
lines = data.splitlines(keepends=True)

# Split lines into sections by content heuristics.
naturals, sharps, q1, q2, q3, conns, tag, urls, post = [], [], [], [], [], [], [], [], []
phase = "header"
for ln in lines:
    if ln.startswith("IN;") or ln.startswith("PU;PA-7567"):
        continue  # init/divider already in SECTIONS
    if "LBnotepat.com keymap" in ln:
        continue
    # piano naturals: y range 1200..2500, the "labelled" lines
    if "LB" in ln and "1370" in ln and len(ln.split("LB")[-1]) <= 3:
        naturals.append(ln); continue
    # piano sharps: double_rect calls — y at 1700..2500 and 1760..2440
    if ("EA" in ln) and ("1700" in ln or "1760" in ln or "2500" in ln) and "PA-" in ln and "1200" not in ln:
        sharps.append(ln); continue
    # QWERTY row 1 ytop=800
    if "EA" in ln and "800" in ln and "-100" in ln:
        q1.append(ln); continue
    if "LB" in ln and "200" in ln and "y" in "":
        q1.append(ln); continue
    # ... fall back: dump remaining into post
    post.append(ln)

# This heuristic is rough — instead just split file into 4 quarters.
quarters = [data[i:i+len(data)//4] for i in range(0, len(data), len(data)//4)]


def open_port():
    s = serial.Serial("/dev/cu.usbserial-110", 9600, bytesize=8,
                      parity=serial.PARITY_NONE, stopbits=1,
                      xonxoff=False, rtscts=True, dsrdtr=True, timeout=0.5)
    s.setDTR(True); s.setRTS(True)
    return s


def ask(ser, cmd):
    ser.reset_input_buffer()
    ser.write(cmd); ser.flush()
    deadline = time.time() + 1.0
    buf = bytearray()
    while time.time() < deadline:
        c = ser.read(64)
        if c:
            for b in c:
                if b not in (XON, XOFF):
                    buf.append(b)
            if b"\n" in buf or b"\r" in buf:
                break
        else:
            time.sleep(0.05)
    return buf.decode("ascii", errors="replace").strip()


def send_slow(ser, payload: bytes):
    """Send with 256-byte frames + 2ms/byte + 30ms/frame."""
    i = 0
    while i < len(payload):
        # cut on ; or ETX boundary, max 256
        end = min(i + 256, len(payload))
        if end < len(payload):
            cut = payload.rfind(b";", i, end + 1)
            if cut > i:
                end = cut + 1
        chunk = payload[i:end]
        for b in chunk:
            ser.write(bytes([b]))
            time.sleep(0.002)
        ser.flush()
        time.sleep(0.030)
        i = end


def main():
    ser = open_port()
    try:
        # always start with init + clear error
        print("clearing error...")
        ask(ser, b"OE;")
        time.sleep(0.5)
        cumulative = b""
        for idx, q in enumerate(quarters, 1):
            print(f"\n=== sending quarter {idx}/{len(quarters)} ({len(q)} bytes) ===")
            cumulative += q.encode("ascii")
            # we resend cumulative each time so plotter state is consistent
            # but that's expensive — instead just send the new piece.
            send_slow(ser, q.encode("ascii"))
            time.sleep(2.0)
            err = ask(ser, b"OE;")
            print(f"  OE; -> {err}")
            if err.strip() and err.strip() != "0":
                print(f"  *** ERROR after quarter {idx} ***")
                # save the quarter to disk for inspection
                Path(f"/tmp/quarter-{idx}.hpgl").write_text(q)
                print(f"  payload saved to /tmp/quarter-{idx}.hpgl")
                # park the pen
                ask(ser, b"PU;PA0,0;SP0;")
                return 1
        print("\nall quarters clean")
        ask(ser, b"PU;PA0,0;SP0;")
    finally:
        ser.close()
    return 0


if __name__ == "__main__":
    sys.exit(main())
