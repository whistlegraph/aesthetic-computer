#!/usr/bin/env python3
"""
Probe the HP 7585B for its actual limits and emit a usable test pattern.

The plotter knows what sheet is loaded and what hard-clip limits apply far
better than any TOML guess. This script:

  1. opens the serial port,
  2. sends OE; OH; OP; OF; OS; (all read-only, no movement),
  3. parses the responses,
  4. prints a human summary,
  5. optionally writes a hand-rolled HPGL test pattern sized to fit *inside*
     the reported P1/P2 area with margin (`--write-test out.hpgl`),
  6. optionally writes a vpype paper profile snippet to merge into vpype.toml
     (`--write-toml`).

Usage:
  query-limits.py --port /dev/cu.usbserial-110
  query-limits.py --port /dev/cu.usbserial-110 --write-test safe.hpgl
  query-limits.py --port /dev/cu.usbserial-110 --write-toml > paper.toml
"""

from __future__ import annotations

import argparse
import os
import sys
import time

import serial

XON, XOFF = 0x11, 0x13


def _ask(ser: serial.Serial, cmd: bytes, settle_s: float = 1.0) -> str:
    ser.reset_input_buffer()
    ser.write(cmd)
    ser.flush()
    deadline = time.time() + settle_s
    buf = bytearray()
    while time.time() < deadline:
        chunk = ser.read(64)
        if chunk:
            for b in chunk:
                if b not in (XON, XOFF):
                    buf.append(b)
            # plotter terminates output strings with CR / LF; bail early
            if b"\n" in buf or b"\r" in buf:
                break
        else:
            time.sleep(0.05)
    return buf.decode("ascii", errors="replace").strip()


def _ints(s: str) -> list[int]:
    out: list[int] = []
    for tok in s.replace(",", " ").split():
        try:
            out.append(int(tok))
        except ValueError:
            pass
    return out


def _decode_status(byte: int) -> str:
    flags = []
    if byte & 0x01:
        flags.append("pen-down")
    if byte & 0x02:
        flags.append("p1p2-changed")
    if byte & 0x04:
        flags.append("digitized-point")
    if byte & 0x08:
        flags.append("initialized")
    if byte & 0x10:
        flags.append("ready")
    if byte & 0x20:
        flags.append("error")
    return ",".join(flags) or "<none>"


def query(port: str, baud: int) -> dict:
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
        info = {}
        info["id"] = _ask(ser, b"OI;")
        info["error_raw"] = _ask(ser, b"OE;")
        info["hardclip_raw"] = _ask(ser, b"OH;")
        info["p1p2_raw"] = _ask(ser, b"OP;")
        info["factor_raw"] = _ask(ser, b"OF;")
        info["status_raw"] = _ask(ser, b"OS;")
    finally:
        ser.close()

    info["error"] = _ints(info["error_raw"])[0] if _ints(info["error_raw"]) else None
    hc = _ints(info["hardclip_raw"])
    pp = _ints(info["p1p2_raw"])
    of = _ints(info["factor_raw"])
    st = _ints(info["status_raw"])
    if len(hc) == 4:
        info["hardclip"] = (hc[0], hc[1], hc[2], hc[3])
    if len(pp) == 4:
        info["p1p2"] = (pp[0], pp[1], pp[2], pp[3])
    if len(of) >= 2:
        info["factor"] = (of[0], of[1])
    if st:
        info["status"] = st[0]
        info["status_flags"] = _decode_status(st[0])
    return info


def summarize(info: dict) -> str:
    lines = []
    lines.append(f"identifier:   {info.get('id', '?')}")
    err = info.get("error")
    lines.append(f"error reg:    {err}  {'(clear)' if err == 0 else '(SET — investigate)'}")
    if "hardclip" in info:
        x1, y1, x2, y2 = info["hardclip"]
        fx, fy = info.get("factor", (40, 40))
        lines.append(
            f"hard clip:    X={x1}..{x2}  Y={y1}..{y2}  units"
            f"  ({(x2 - x1) / fx:.1f} x {(y2 - y1) / fy:.1f} mm)"
        )
    if "p1p2" in info:
        x1, y1, x2, y2 = info["p1p2"]
        fx, fy = info.get("factor", (40, 40))
        lines.append(
            f"P1/P2:        ({x1},{y1}) ({x2},{y2})  units"
            f"  ({(x2 - x1) / fx:.1f} x {(y2 - y1) / fy:.1f} mm)"
        )
    if "factor" in info:
        fx, fy = info["factor"]
        lines.append(f"factor:       {fx}, {fy} units/mm  (= {1000 / fx:.3f} um/unit)")
    if "status" in info:
        lines.append(f"status:       0x{info['status']:02X}  [{info['status_flags']}]")
    return "\n".join(lines)


def build_test_hpgl(p1p2: tuple[int, int, int, int], pen: int = 1) -> bytes:
    """Hand-rolled test pattern sized to fit inside P1/P2 with 5% margin.

    Frame + diagonals + centered crosshair + centered circle. All coordinates
    are clipped to (P1+5%) .. (P2-5%) so we cannot run off-limit.
    """
    x1, y1, x2, y2 = p1p2
    mx = int((x2 - x1) * 0.05)
    my = int((y2 - y1) * 0.05)
    fx1, fy1 = x1 + mx, y1 + my
    fx2, fy2 = x2 - mx, y2 - my
    cx, cy = (x1 + x2) // 2, (y1 + y2) // 2
    # circle radius = 35% of the smaller axis
    r = int(min((fx2 - fx1), (fy2 - fy1)) * 0.35 / 2)

    cmds = []
    cmds.append(f"IN;SP{pen};")
    # frame
    cmds.append(
        f"PA{fx1},{fy1};PD;PA{fx2},{fy1};PA{fx2},{fy2};"
        f"PA{fx1},{fy2};PA{fx1},{fy1};PU;"
    )
    # diagonals
    cmds.append(
        f"PA{fx1},{fy1};PD;PA{fx2},{fy2};PU;"
        f"PA{fx1},{fy2};PD;PA{fx2},{fy1};PU;"
    )
    # crosshair
    cmds.append(
        f"PA{fx1},{cy};PD;PA{fx2},{cy};PU;"
        f"PA{cx},{fy1};PD;PA{cx},{fy2};PU;"
    )
    # circle
    cmds.append(f"PA{cx},{cy};CI{r};PU;")
    # park
    cmds.append("PA0,0;SP0;")
    return ("\n".join(cmds) + "\n").encode("ascii")


def build_toml(info: dict, name: str = "current") -> str:
    if "hardclip" not in info or "factor" not in info:
        return "# could not query plotter — no profile emitted\n"
    x1, y1, x2, y2 = info["hardclip"]
    fx, fy = info["factor"]
    paper_x_mm = (x2 - x1) / fx
    paper_y_mm = (y2 - y1) / fy
    return f"""# Generated from a live OH; query against the connected 7585B.
# This reflects whatever sheet is currently loaded — re-run query-limits.py
# whenever you change paper.

[[device.hp7585b.paper]]
name = "{name}"
paper_size = ["{paper_x_mm:.2f}mm", "{paper_y_mm:.2f}mm"]
x_range = [{x1}, {x2}]
y_range = [{y1}, {y2}]
y_axis_up = true
origin_location = ["{paper_x_mm / 2:.2f}mm", "{paper_y_mm / 2:.2f}mm"]
final_pu_params = "0,0"
info = "auto-generated from plotter OH; query"
"""


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument(
        "--port",
        default=os.environ.get("HP7585B_TTY", ""),
        help="serial device (defaults to $HP7585B_TTY)",
    )
    p.add_argument("--baud", type=int, default=9600)
    p.add_argument(
        "--write-test",
        metavar="OUT.HPGL",
        help="write a hand-rolled, in-bounds test plot",
    )
    p.add_argument(
        "--write-toml",
        action="store_true",
        help="emit a vpype paper profile to stdout based on the current sheet",
    )
    p.add_argument(
        "--pen", type=int, default=1, help="pen number for the test plot"
    )
    args = p.parse_args()

    if not args.port:
        p.error("no --port and HP7585B_TTY not set")

    info = query(args.port, args.baud)
    print(summarize(info), file=sys.stderr)

    if args.write_test:
        if "p1p2" not in info:
            print("no P1/P2 reading — refusing to generate test", file=sys.stderr)
            return 2
        with open(args.write_test, "wb") as f:
            f.write(build_test_hpgl(info["p1p2"], pen=args.pen))
        print(f"wrote {args.write_test}", file=sys.stderr)

    if args.write_toml:
        sys.stdout.write(build_toml(info))

    return 0


if __name__ == "__main__":
    sys.exit(main())
