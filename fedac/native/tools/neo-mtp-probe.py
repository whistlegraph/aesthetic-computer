#!/usr/bin/env python3
"""Bounded T8140 MTP inspection through an already-running m1n1 proxy.

Default: read status registers without consuming mailbox or HID data.
--hello: start the MTP CPU if stopped, consume at most 32 mailbox messages,
and look for RTKit HELLO. No HELLO reply, endpoint startup, DMA allocation,
DART/DAPF programming, GPIO access, or HID command is performed. Restore
CPU_CONTROL if changed; reboot the target after this mode before Linux.

Register evidence: Blueberry macOS 27.0.1 AppleASCWrapV6::_runCPU,
_getOutboxEmpty, _outbox and AppleDockChannel::_readBytesGated. RTKit
message fields: Asahi m1n1 proxyclient/m1n1/fw/asc/mgmt.py.
This is a firmware-startup experiment, not a keyboard driver or key test.
"""

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import sys
import time


ASC = 0x314600000
FIFO = 0x314B34000
RUN = 1 << 4
EMPTY = 1 << 17
M1N1_SHA256 = "780096243a690a09b8e033df25789da0a5b921511f242a0865f1e6712387f933"
IDENTITY_OFFSET = 0x165B0
IDENTITY_BYTES = bytes.fromhex("880700b008d547f9")
RAM_WORKAROUND = bytes.fromhex("00008052c0035fd6")
REGISTERS = {
    "cpu_control": ASC + 0x44,
    "v6_idle_status": ASC + 0x8000,
    "inbox_control": ASC + 0x8110,
    "outbox_control": ASC + 0x8114,
    "fifo_tx_free": FIFO + 0x14,
    "fifo_rx_count": FIFO + 0x2C,
}


class MMIO:
    def __init__(self, proxy, report):
        self.p = proxy
        self.report = report

    def access(self, operation, address, *values):
        # GET_EXC_COUNT returns AND clears the counter. A guarded read can
        # return a marker instead of raising, so check every MMIO operation.
        if self.p.get_exc_count():
            raise RuntimeError("Unaccounted m1n1 exception before MMIO")
        result = getattr(self.p, operation)(address, *values)
        count = self.p.get_exc_count()
        if count:
            raise RuntimeError(f"{operation} at {address:#x}: {count} MMIO fault(s)")
        return result

    def read32(self, address):
        return self.access("read32", address)

    def read64(self, address):
        return self.access("read64", address)

    def write_control(self, value):
        # This tool has exactly one writable register; no arbitrary writes.
        row = {"address": hex(ASC + 0x44), "value": hex(value), "verified": False}
        self.report["writes"].append(row)
        self.access("write32", ASC + 0x44, value)
        actual = self.read32(ASC + 0x44)
        row["readback"] = hex(actual)
        row["verified"] = actual == value
        if not row["verified"]:
            raise RuntimeError("CPU_CONTROL write did not read back exactly")


def inspect(mmio, report):
    report["registers"] = {}
    for name, address in REGISTERS.items():
        value = mmio.read32(address)
        report["registers"][name] = {"address": hex(address), "value": hex(value)}
    report["result"] = "snapshot_only"


def hello(mmio, report, seconds):
    original = mmio.read32(ASC + 0x44)
    changed = not bool(original & RUN)
    report["cpu_was_running"] = not changed
    report["reboot_required"] = True  # Reading mailbox data consumes messages.
    report["messages"] = []
    try:
        if changed:
            mmio.write_control(original | RUN)
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline and len(report["messages"]) < 32:
            if mmio.read32(ASC + 0x8114) & EMPTY:
                time.sleep(0.02)
                continue
            msg0 = mmio.read64(ASC + 0x8830)
            msg1 = mmio.read64(ASC + 0x8838)
            endpoint = msg1 & 0xFF
            msgtype = (msg0 >> 52) & 0xFF
            report["messages"].append({
                "word0": hex(msg0), "word1": hex(msg1),
                "endpoint": endpoint, "type": msgtype,
            })
            if endpoint == 0 and msgtype == 1:
                low, high = msg0 & 0xFFFF, (msg0 >> 16) & 0xFFFF
                if not (0 < low <= high):
                    raise RuntimeError("Malformed RTKit HELLO version range")
                report["hello"] = {"minimum_version": low, "maximum_version": high}
                report["result"] = "hello_received"
                return
        report["result"] = "no_hello"
    finally:
        if changed:
            try:
                mmio.write_control(original)
                report["cpu_control_restored"] = True
            except Exception as exc:
                report["cpu_control_restored"] = False
                report["restore_error"] = str(exc)
                raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--m1n1", type=Path, required=True, help="verified m1n1 source/build directory")
    parser.add_argument("--device", default=os.environ.get("M1N1DEVICE"))
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--hello", action="store_true", help="start/consume HELLO; target reboot required afterward")
    parser.add_argument("--seconds", type=float, default=5.0, help="HELLO poll duration, 0.1..15 seconds")
    args = parser.parse_args()
    if not args.device or not 0.1 <= args.seconds <= 15:
        parser.error("provide --device (or M1N1DEVICE) and --seconds in 0.1..15")
    raw = (args.m1n1 / "build/m1n1.bin").read_bytes()
    if hashlib.sha256(raw).hexdigest() != M1N1_SHA256:
        parser.error("unverified m1n1 build; re-check proxy ABI and identity before use")

    report = {
        "time": datetime.now(timezone.utc).isoformat(),
        "mode": "hello" if args.hello else "snapshot",
        "m1n1_sha256": M1N1_SHA256,
        "writes": [], "reboot_required": False, "result": "not_started",
    }
    iface = None
    try:
        sys.path.insert(0, str(args.m1n1 / "proxyclient"))
        from m1n1.proxy import UartInterface, M1N1Proxy
        from m1n1.proxyutils import ProxyUtils, bootstrap_port

        # Do not import m1n1.setup: it also resets the PMU panic counter.
        iface = UartInterface(args.device)
        iface.dev.write_timeout = 3
        p = M1N1Proxy(iface)
        bootstrap_port(iface, p)
        if p.get_chipid() != 0x8140:
            raise RuntimeError("Target is not T8140")
        if iface.readmem(p.get_base() + IDENTITY_OFFSET, 8) not in (IDENTITY_BYTES, RAM_WORKAROUND):
            raise RuntimeError("Running m1n1 identity bytes do not match")
        u = ProxyUtils(p)
        mtp = u.adt["/arm-io/mtp"]
        dc = u.adt["/arm-io/dockchannel-mtp"]
        if "iop,ascwrap-v6" not in mtp.compatible or mtp.get_reg(0) != (ASC, 0x88000):
            raise RuntimeError("Unexpected MTP firmware register map")
        if "dockchannel,t8002" not in dc.compatible or dc.get_reg(3) != (FIFO, 0x1000):
            raise RuntimeError("Unexpected MTP FIFO register map")
        report["chip"] = "0x8140"
        mmio = MMIO(p, report)
        inspect(mmio, report)
        if args.hello:
            hello(mmio, report, args.seconds)
    except Exception as exc:
        report["result"] = "error"
        report["error"] = f"{type(exc).__name__}: {exc}"
    finally:
        if iface is not None:
            iface.dev.close()
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report, indent=2))
    return 0 if report["result"] in ("snapshot_only", "hello_received") else 1


if __name__ == "__main__":
    sys.exit(main())
