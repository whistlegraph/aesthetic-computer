#!/usr/bin/env python3
"""
Generate a wordmark plot fitted to the live plotter limits.

Queries OH;/OP; off the plotter, then composes:
    - frame at 92% of the P1/P2 box
    - big title (HP-GL LB), centered horizontally, set above mid-line
    - subtitle (smaller LB), centered horizontally, set below mid-line

Output goes to stdout (or --out file) as HPGL, ready to feed to serial-tx.py.

Usage:
    wordmark.py --port /dev/cu.usbserial-110 --title "AESTHETIC COMPUTER" \
        --subtitle "HP 7585B  ·  FIRST PLOT  ·  2026-04-30" --out plot.hpgl
"""

from __future__ import annotations

import argparse
import os
import sys

import serial

XON, XOFF = 0x11, 0x13


def _ask(ser: serial.Serial, cmd: bytes) -> str:
    import time

    ser.reset_input_buffer()
    ser.write(cmd)
    ser.flush()
    deadline = time.time() + 1.0
    buf = bytearray()
    while time.time() < deadline:
        chunk = ser.read(64)
        if chunk:
            for b in chunk:
                if b not in (XON, XOFF):
                    buf.append(b)
            if b"\n" in buf or b"\r" in buf:
                break
        else:
            time.sleep(0.05)
    return buf.decode("ascii", errors="replace").strip()


def _ints(s: str) -> list[int]:
    out = []
    for tok in s.replace(",", " ").split():
        try:
            out.append(int(tok))
        except ValueError:
            pass
    return out


def query(port: str, baud: int) -> tuple[tuple[int, int, int, int], tuple[int, int]]:
    ser = serial.Serial(
        port=port,
        baudrate=baud,
        bytesize=serial.EIGHTBITS,
        parity=serial.PARITY_NONE,
        stopbits=serial.STOPBITS_ONE,
        xonxoff=False,
        rtscts=True,
        dsrdtr=True,
        timeout=0.5,
    )
    ser.setDTR(True)
    ser.setRTS(True)
    try:
        pp = _ints(_ask(ser, b"OP;"))
        of = _ints(_ask(ser, b"OF;"))
    finally:
        ser.close()
    if len(pp) != 4 or len(of) < 2:
        raise SystemExit("plotter did not respond to OP;/OF;")
    return (pp[0], pp[1], pp[2], pp[3]), (of[0], of[1])


def _label_width_mm(text: str, char_cm_w: float) -> float:
    # HP-GL default char spacing: cell width = 1.5 * char_width.
    return len(text) * char_cm_w * 1.5 * 10.0


def fit_size(text: str, max_mm: float, target_mm: float) -> float:
    """Pick char width in cm so the label is at most max_mm wide and ideally
    target_mm. Return char width (cm)."""
    spacing = 1.5
    cw_cm = target_mm / (len(text) * spacing * 10.0)
    if _label_width_mm(text, cw_cm) > max_mm:
        cw_cm = max_mm / (len(text) * spacing * 10.0)
    return cw_cm


def build(
    p1p2: tuple[int, int, int, int],
    factor: tuple[int, int],
    title: str,
    subtitle: str,
    pen_title: int = 1,
    pen_sub: int = 1,
    pen_frame: int = 1,
) -> bytes:
    x1, y1, x2, y2 = p1p2
    fx, fy = factor

    # 92% frame
    fx1 = int(x1 * 0.92)
    fy1 = int(y1 * 0.92)
    fx2 = int(x2 * 0.92)
    fy2 = int(y2 * 0.92)

    # available width for text in mm, leaving 5% inside frame
    avail_w_mm = (fx2 - fx1) / fx * 0.90
    avail_h_mm = (fy2 - fy1) / fy

    # title: ~80% of width, height ~ 35% of avail height
    title_cw_cm = fit_size(title, avail_w_mm, avail_w_mm * 0.85)
    title_ch_cm = title_cw_cm * 1.6  # taller than wide for presence

    # subtitle: smaller — height roughly 25-30% of title
    sub_cw_cm = fit_size(subtitle, avail_w_mm * 0.7, avail_w_mm * 0.45)
    sub_ch_cm = sub_cw_cm * 1.4

    title_w_units = int(_label_width_mm(title, title_cw_cm) * fx / 10)
    sub_w_units = int(_label_width_mm(subtitle, sub_cw_cm) * fx / 10)

    title_h_units = int(title_ch_cm * 10 * fy)
    sub_h_units = int(sub_ch_cm * 10 * fy)

    # vertical layout: title above midline, subtitle below
    gap = int(avail_h_mm * 0.05 * fy)
    title_y = gap // 2
    sub_y = -title_h_units // 2 - gap - sub_h_units

    # …actually simpler: title baseline above center, subtitle baseline below.
    title_baseline = int(avail_h_mm * 0.10 * fy)
    sub_baseline = -int(avail_h_mm * 0.10 * fy) - sub_h_units

    cmds: list[str] = []
    cmds.append("IN;DT\x03;")  # default terminator = ETX (0x03)
    cmds.append("DI1,0;")  # horizontal labels

    # Frame
    cmds.append(f"SP{pen_frame};")
    cmds.append(
        f"PA{fx1},{fy1};PD;PA{fx2},{fy1};PA{fx2},{fy2};"
        f"PA{fx1},{fy2};PA{fx1},{fy1};PU;"
    )

    # Title
    cmds.append(f"SP{pen_title};")
    cmds.append(f"SI{title_cw_cm:.3f},{title_ch_cm:.3f};")
    cmds.append(f"PA{-title_w_units // 2},{title_baseline};")
    cmds.append(f"LB{title}\x03")

    # Subtitle
    cmds.append(f"SP{pen_sub};")
    cmds.append(f"SI{sub_cw_cm:.3f},{sub_ch_cm:.3f};")
    cmds.append(f"PA{-sub_w_units // 2},{sub_baseline};")
    cmds.append(f"LB{subtitle}\x03")

    # park
    cmds.append("PU;PA0,0;SP0;")

    return ("\n".join(cmds) + "\n").encode("ascii")


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument(
        "--port",
        default=os.environ.get("HP7585B_TTY", ""),
        help="serial device (defaults to $HP7585B_TTY)",
    )
    p.add_argument("--baud", type=int, default=9600)
    p.add_argument("--title", default="AESTHETIC COMPUTER")
    p.add_argument("--subtitle", default="HP 7585B")
    p.add_argument("--out", help="write HPGL here (default stdout)")
    p.add_argument("--pen-title", type=int, default=1)
    p.add_argument("--pen-sub", type=int, default=1)
    p.add_argument("--pen-frame", type=int, default=1)
    args = p.parse_args()
    if not args.port:
        p.error("no --port and HP7585B_TTY not set")

    p1p2, factor = query(args.port, args.baud)
    sys.stderr.write(
        f"plotter P1/P2 = {p1p2}, factor = {factor}\n"
    )
    hpgl = build(
        p1p2,
        factor,
        title=args.title,
        subtitle=args.subtitle,
        pen_title=args.pen_title,
        pen_sub=args.pen_sub,
        pen_frame=args.pen_frame,
    )
    if args.out:
        with open(args.out, "wb") as f:
            f.write(hpgl)
        sys.stderr.write(f"wrote {len(hpgl)} bytes to {args.out}\n")
    else:
        sys.stdout.buffer.write(hpgl)
    return 0


if __name__ == "__main__":
    sys.exit(main())
