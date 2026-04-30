#!/usr/bin/env python3
"""
HP 7585B serial driver with monitoring + safe chunking.

- Xon/Xoff hardware flow control (kernel-level via pyserial).
- Chunks on HP-GL `;` boundaries, max 256 bytes/frame (the plotter's
  documented per-frame ceiling). Never splits mid-command.
- Per-byte delay (default 1 ms) to keep tiny machines happy.
- Reader thread prints anything the plotter sends back, prefixed [<-].
- Raw mode (`-c "OI;"`) for ad-hoc commands; file mode (`-f file.hpgl`)
  for prepared plots.

usage:
  serial-tx.py --port /dev/cu.usbserial-110 -c "OI;" --listen 2
  serial-tx.py --port /dev/cu.usbserial-110 -f test-print.hpgl
"""

from __future__ import annotations

import argparse
import os
import sys
import threading
import time
from pathlib import Path

import serial


XON = 0x11
XOFF = 0x13


def _reader(
    ser: serial.Serial, stop: threading.Event, can_send: threading.Event
) -> None:
    while not stop.is_set():
        try:
            data = ser.read(64)
        except serial.SerialException:
            return
        if not data:
            continue

        # Strip Xon/Xoff out of the user-visible stream and act on them.
        passthrough = bytearray()
        for b in data:
            if b == XOFF:
                if can_send.is_set():
                    sys.stdout.write("[<-] <Xoff>\n")
                    sys.stdout.flush()
                can_send.clear()
            elif b == XON:
                if not can_send.is_set():
                    sys.stdout.write("[<-] <Xon>\n")
                    sys.stdout.flush()
                can_send.set()
            else:
                passthrough.append(b)
        if passthrough:
            try:
                text = bytes(passthrough).decode("ascii", errors="replace")
            except Exception:
                text = repr(bytes(passthrough))
            sys.stdout.write(f"[<-] {text}")
            sys.stdout.flush()


def _chunks(payload: bytes, limit: int) -> list[bytes]:
    """Split payload into <= `limit`-byte frames on `;` boundaries.

    Walks forward, taking as many full HP-GL commands as fit before each
    cut. Falls back to a hard split if a single command exceeds `limit`
    (rare — long PA polylines from vpype can do this).
    """
    out: list[bytes] = []
    i = 0
    n = len(payload)
    while i < n:
        end = min(i + limit, n)
        if end < n:
            cut = payload.rfind(b";", i, end + 1)
            if cut > i:
                end = cut + 1  # include the ';'
        out.append(payload[i:end])
        i = end
    return out


def send(
    port: str,
    payload: bytes,
    baud: int,
    frame_limit: int,
    byte_delay_ms: float,
    chunk_delay_ms: float,
    listen_after_s: float,
) -> None:
    ser = serial.Serial(
        port=port,
        baudrate=baud,
        bytesize=serial.EIGHTBITS,
        parity=serial.PARITY_NONE,
        stopbits=serial.STOPBITS_ONE,
        xonxoff=False,  # plotter is in hardwire-handshake mode
        rtscts=True,    # honor the plotter's CTS line
        dsrdtr=True,    # assert DTR; honor DSR
        timeout=0.5,
        write_timeout=30,
    )
    # Make sure modem control lines are asserted from our side.
    ser.setDTR(True)
    ser.setRTS(True)
    stop = threading.Event()
    can_send = threading.Event()
    can_send.set()  # start in "go" state; the plotter Xoffs us when full
    reader = threading.Thread(
        target=_reader, args=(ser, stop, can_send), daemon=True
    )
    reader.start()

    def _wait_xon() -> None:
        while not can_send.wait(timeout=0.5):
            if stop.is_set():
                return

    try:
        # drain anything already buffered (boot banner, prior responses)
        ser.reset_input_buffer()

        frames = _chunks(payload, frame_limit)
        total = len(payload)
        sent = 0
        byte_pause = byte_delay_ms / 1000.0
        chunk_pause = chunk_delay_ms / 1000.0

        for idx, frame in enumerate(frames, 1):
            _wait_xon()
            sys.stdout.write(
                f"[->] frame {idx}/{len(frames)} ({len(frame)} B, "
                f"{sent}/{total} sent)\n"
            )
            sys.stdout.flush()
            if byte_pause > 0:
                for b in frame:
                    _wait_xon()
                    ser.write(bytes([b]))
                    time.sleep(byte_pause)
            else:
                ser.write(frame)
            ser.flush()
            sent += len(frame)
            if chunk_pause > 0 and idx < len(frames):
                time.sleep(chunk_pause)

        sys.stdout.write(f"[->] done, {sent} B sent\n")
        sys.stdout.flush()

        if listen_after_s > 0:
            time.sleep(listen_after_s)
    finally:
        stop.set()
        time.sleep(0.1)
        ser.close()


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument(
        "--port",
        default=os.environ.get("HP7585B_TTY", ""),
        help="serial device (defaults to $HP7585B_TTY)",
    )
    p.add_argument("--baud", type=int, default=9600)
    p.add_argument(
        "--frame-bytes",
        type=int,
        default=256,
        help="max bytes per frame (plotter limit; default 256)",
    )
    p.add_argument(
        "--byte-delay-ms",
        type=float,
        default=2.0,
        help="delay between bytes in ms (default 2.0; set 0 to disable)",
    )
    p.add_argument(
        "--chunk-delay-ms",
        type=float,
        default=10.0,
        help="delay between frames in ms (default 10)",
    )
    p.add_argument(
        "--listen",
        type=float,
        default=1.0,
        metavar="SECONDS",
        help="seconds to keep listening after the last byte is sent",
    )
    src = p.add_mutually_exclusive_group(required=True)
    src.add_argument("-c", "--command", help='inline HP-GL, e.g. "OI;"')
    src.add_argument("-f", "--file", help="path to .hpgl file")

    args = p.parse_args()
    if not args.port:
        p.error("no --port and HP7585B_TTY not set")
    if not Path(args.port).exists():
        p.error(f"{args.port} does not exist")

    if args.command is not None:
        payload = args.command.encode("ascii")
    else:
        payload = Path(args.file).read_bytes()

    send(
        port=args.port,
        payload=payload,
        baud=args.baud,
        frame_limit=args.frame_bytes,
        byte_delay_ms=args.byte_delay_ms,
        chunk_delay_ms=args.chunk_delay_ms,
        listen_after_s=args.listen,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
