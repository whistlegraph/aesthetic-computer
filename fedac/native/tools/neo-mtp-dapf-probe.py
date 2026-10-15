#!/usr/bin/env python3
"""T8140 MTP firmware-startup probe with the MTP bus firewall (DAPF) in scope.

Context: m1n1 e798b719's dapf_init_all walks /arm-io/dart-aop first and
faults there (its reg index 1 is a GAPF/PS_PROT bank on this firmware), so
/arm-io/dart-mtp's real DAPF bank (reg index 1, 0x314810000) was never
programmed on any boot so far, and the earlier HELLO probe ran with the MTP
firmware's bus windows (nub-gpio, aop-gpio, AOP ranges) still closed.

Modes, all through an already-running m1n1 proxy, all bounded:
  default   read-only: ASC status, DART-mtp stream 0 (TCR/TTBR/ERROR),
            firmware IOVA 0x1000000 translation, DAPF bank contents, and
            the first bytes of the firmware __TEXT segment in SRAM.
  --dapf    program ONLY /arm-io/dart-mtp's DAPF via m1n1's own
            proxy call dapf_init(path) (index 1 inside m1n1), then read the
            bank back and compare it with the ADT dapf-instance-0 entries.
            Nothing touches dart-aop or the shared GAPF at 0x3082a4000.
  --hello   set CPU_CONTROL.RUN, poll the outbox for RTKit HELLO (bounded),
            restore CPU_CONTROL. Combine with --dapf to test the hypothesis.

Every MMIO access runs under m1n1's exception guard (SKIP|SILENT) and is
checked with get_exc_count, so a faulting read is reported instead of
rebooting the target. Reboot the target before Linux after --dapf/--hello.
No HELLO ack, no endpoint start, no DMA, no HID traffic, no GPIO.
"""

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import struct
import sys
import time

ASC = 0x314600000
DART = 0x314800000
DAPF = 0x314810000
FIFO = 0x314B34000
FW_TEXT_PA = 0x314C00000
FW_TEXT_IOVA = 0x1000000
FW_TEXT_SIZE = 0x52000
RUN = 1 << 4
EMPTY = 1 << 17
M1N1_SHA256 = "780096243a690a09b8e033df25789da0a5b921511f242a0865f1e6712387f933"
IDENTITY_OFFSET = 0x165B0
IDENTITY_BYTES = bytes.fromhex("880700b008d547f9")
RAM_WORKAROUND = bytes.fromhex("00008052c0035fd6")
DAPF_ENTRY = struct.Struct("<QQIII20sBBBB")  # m1n1 dapf_t8110_config, 52 bytes
ASC_REGS = {
    "cpu_control": ASC + 0x44,
    "cpu_status_legacy_0x48": ASC + 0x48,
    "v6_idle_status": ASC + 0x8000,
    "inbox_control": ASC + 0x8110,
    "outbox_control": ASC + 0x8114,
    "fifo_tx_free": FIFO + 0x14,
    "fifo_rx_count": FIFO + 0x2C,
}
DART_REGS = {
    "params0": DART + 0x000, "params4": DART + 0x004,
    "params8": DART + 0x008, "paramsC": DART + 0x00C,
    "error": DART + 0x100, "error_addr_lo": DART + 0x170,
    "error_addr_hi": DART + 0x174, "protect": DART + 0x200,
    "enable_streams0": DART + 0xC00,
    "tcr0": DART + 0x1000, "ttbr0": DART + 0x1400,
}


def parse_dapf_entries(blob):
    if len(blob) % DAPF_ENTRY.size:
        raise ValueError(f"dapf-instance-0 length {len(blob)} is not a t8110a multiple")
    out = []
    for start, end, r20, unk1, r4, _unk2, unk3, r0h, r0l, unk4 in DAPF_ENTRY.iter_unpack(blob):
        out.append({"start": start, "end": end, "r20": r20, "r4": r4,
                    "r0": (r0h << 4) | r0l})
    return out


def compare_bank(entries, bank):
    """bank: list of dicts read from hardware (r0, r4, start, end, r20)."""
    mismatches = []
    for i, (want, got) in enumerate(zip(entries, bank)):
        for key in ("r0", "r4", "start", "end", "r20"):
            # The DAPF hardware rounds END up to a 4-byte boundary (observed
            # 0x310824068 -> 0x31082406b on Frisbee); accept that.
            if key == "end" and (want[key] | 3) == got[key]:
                continue
            if want[key] != got[key]:
                mismatches.append({"entry": i, "field": key,
                                   "want": hex(want[key]), "got": hex(got[key])})
    return mismatches


class MMIO:
    def __init__(self, proxy, report, guard):
        self.p = proxy
        self.report = report
        self.guard = guard
        self.faults = 0

    def _check(self, what, address):
        count = self.p.get_exc_count()
        if count:
            self.faults += count
            self.report.setdefault("faults", []).append({"op": what, "address": hex(address), "count": count})
            raise RuntimeError(f"{what} at {address:#x}: {count} MMIO fault(s)")

    def read32(self, address):
        self.p.set_exc_guard(self.guard)
        try:
            value = self.p.read32(address)
        finally:
            self._check("read32", address)
        return value

    def read64(self, address):
        self.p.set_exc_guard(self.guard)
        try:
            value = self.p.read64(address)
        finally:
            self._check("read64", address)
        return value

    def write32_verified(self, address, value, label):
        row = {"label": label, "address": hex(address), "value": hex(value), "verified": False}
        self.report["writes"].append(row)
        self.p.set_exc_guard(self.guard)
        try:
            self.p.write32(address, value)
        finally:
            self._check("write32", address)
        actual = self.read32(address)
        row["readback"] = hex(actual)
        row["verified"] = actual == value
        if not row["verified"]:
            raise RuntimeError(f"{label}: wrote {value:#x}, read back {actual:#x}")


def snapshot(mmio, report, iface):
    regs = {}
    for name, address in ASC_REGS.items():
        regs[name] = {"address": hex(address), "value": hex(mmio.read32(address))}
    report["asc"] = regs
    dart = {}
    for name, address in DART_REGS.items():
        try:
            dart[name] = {"address": hex(address), "value": hex(mmio.read32(address))}
        except RuntimeError as exc:
            dart[name] = {"address": hex(address), "error": str(exc)}
    tcr = int(dart["tcr0"]["value"], 16) if "value" in dart["tcr0"] else None
    ttbr = int(dart["ttbr0"]["value"], 16) if "value" in dart["ttbr0"] else None
    if tcr is not None:
        dart["tcr0_decoded"] = {
            "translate_enable": bool(tcr & 1), "bypass_dart": bool(tcr & 2),
            "bypass_dapf": bool(tcr & 4), "four_levels": bool(tcr & 8),
            "remap_en": bool(tcr & 0x80), "remap": (tcr >> 8) & 0xF,
        }
    if ttbr is not None:
        dart["ttbr0_decoded"] = {"valid": bool(ttbr & 1), "table_pa": hex((ttbr >> 2) << 14)}
    report["dart_mtp"] = dart
    # Firewall bank, read-only.
    bank = []
    for i in range(report["dapf_entry_count"]):
        base = DAPF + 0x40 * i
        try:
            bank.append({
                "r0": mmio.read32(base + 0x00), "r4": mmio.read32(base + 0x04),
                "start": mmio.read64(base + 0x08), "end": mmio.read64(base + 0x10),
                "r20": mmio.read32(base + 0x20),
            })
        except RuntimeError as exc:
            report["dapf_bank_error"] = str(exc)
            break
    report["dapf_bank"] = [{k: hex(v) for k, v in row.items()} for row in bank]
    report["dapf_bank_matches_adt"] = (
        len(bank) == report["dapf_entry_count"]
        and not compare_bank(report["_dapf_entries"], bank))
    # Firmware __TEXT head in SRAM (guarded).
    try:
        mmio.p.set_exc_guard(mmio.guard)
        head = iface.readmem(FW_TEXT_PA, 64)
        mmio._check("readmem", FW_TEXT_PA)
        report["fw_text_head"] = head.hex()
        report["fw_text_head_nonzero"] = any(head)
    except Exception as exc:
        report["fw_text_head_error"] = str(exc)


def translate_firmware(u, report):
    """Read-only DART page walk of the firmware stream (sid 0)."""
    try:
        from m1n1.hw.dart import DART as DARTObj
        dart = DARTObj.from_adt(u, "/arm-io/dart-mtp")
        ranges = dart.iotranslate(0, FW_TEXT_IOVA, FW_TEXT_SIZE)
        report["fw_text_translation"] = [
            {"pa": hex(pa) if pa is not None else None, "size": hex(size)} for pa, size in ranges]
        report["fw_text_translation_ok"] = bool(ranges) and ranges[0][0] == FW_TEXT_PA and all(
            pa is not None for pa, _ in ranges)
    except Exception as exc:
        report["fw_text_translation_error"] = f"{type(exc).__name__}: {exc}"


def program_dapf(p, mmio, report):
    report["reboot_required"] = True
    report["writes"].append({"label": "proxy dapf_init(/arm-io/dart-mtp)", "address": hex(DAPF),
                             "value": "m1n1 dapf_init_t8110a sequence", "verified": None})
    p.set_exc_guard(mmio.guard)
    ret = p.dapf_init("/arm-io/dart-mtp")
    count = p.get_exc_count()
    report["dapf_init_return"] = ret
    if count:
        report.setdefault("faults", []).append({"op": "dapf_init", "address": hex(DAPF), "count": count})
        raise RuntimeError(f"dapf_init faulted {count} time(s)")
    if ret != 0:
        raise RuntimeError(f"dapf_init returned {ret}")
    bank = []
    for i in range(report["dapf_entry_count"]):
        base = DAPF + 0x40 * i
        bank.append({
            "r0": mmio.read32(base + 0x00), "r4": mmio.read32(base + 0x04),
            "start": mmio.read64(base + 0x08), "end": mmio.read64(base + 0x10),
            "r20": mmio.read32(base + 0x20),
        })
    mism = compare_bank(report["_dapf_entries"], bank)
    report["dapf_bank_after"] = [{k: hex(v) for k, v in row.items()} for row in bank]
    report["dapf_mismatches"] = mism
    report["writes"][-1]["verified"] = not mism
    if mism:
        raise RuntimeError(f"DAPF bank readback differs from ADT in {len(mism)} field(s)")


def hello(mmio, report, seconds):
    original = mmio.read32(ASC + 0x44)
    changed = not bool(original & RUN)
    report["cpu_was_running"] = not changed
    report["reboot_required"] = True
    report["messages"] = []
    try:
        if changed:
            mmio.write32_verified(ASC + 0x44, original | RUN, "CPU_CONTROL.RUN")
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline and len(report["messages"]) < 32:
            if mmio.read32(ASC + 0x8114) & EMPTY:
                time.sleep(0.02)
                continue
            msg0 = mmio.read64(ASC + 0x8830)
            msg1 = mmio.read64(ASC + 0x8838)
            endpoint = msg1 & 0xFF
            msgtype = (msg0 >> 52) & 0xFF
            report["messages"].append({"word0": hex(msg0), "word1": hex(msg1),
                                       "endpoint": endpoint, "type": msgtype})
            if endpoint == 0 and msgtype == 1:
                low, high = msg0 & 0xFFFF, (msg0 >> 16) & 0xFFFF
                report["hello"] = {"minimum_version": low, "maximum_version": high}
                report["result"] = "hello_received"
                return
        report["result"] = "no_hello"
    finally:
        after = {}
        for name in ("cpu_control", "cpu_status_legacy_0x48", "v6_idle_status", "outbox_control"):
            try:
                after[name] = hex(mmio.read32(ASC_REGS[name]))
            except RuntimeError as exc:
                after[name] = str(exc)
        for name in ("error", "error_addr_lo", "error_addr_hi"):
            try:
                after["dart_" + name] = hex(mmio.read32(DART_REGS[name]))
            except RuntimeError as exc:
                after["dart_" + name] = str(exc)
        report["after_run"] = after
        if changed:
            try:
                mmio.write32_verified(ASC + 0x44, original, "CPU_CONTROL restore")
                report["cpu_control_restored"] = True
            except Exception as exc:
                report["cpu_control_restored"] = False
                report["restore_error"] = str(exc)
                raise


def selftest():
    blob = DAPF_ENTRY.pack(0x30828C000, 0x30828C05F, 1, 0, 0, b"\0" * 20, 0, 3, 1, 0) * 2
    entries = parse_dapf_entries(blob)
    assert entries[0] == {"start": 0x30828C000, "end": 0x30828C05F, "r20": 1, "r4": 0, "r0": 0x31}
    bank = [dict(e) for e in entries]
    assert compare_bank(entries, bank) == []
    bank[1]["end"] = 0
    assert compare_bank(entries, bank) == [{"entry": 1, "field": "end", "want": "0x30828c05f", "got": "0x0"}]
    try:
        parse_dapf_entries(b"\0" * 53)
    except ValueError:
        pass
    else:
        raise AssertionError("length check failed")
    print("selftest ok")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--m1n1", type=Path, help="verified m1n1 source/build directory")
    parser.add_argument("--device", default=os.environ.get("M1N1DEVICE"))
    parser.add_argument("--output", type=Path)
    parser.add_argument("--dapf", action="store_true", help="program /arm-io/dart-mtp DAPF via m1n1 dapf_init")
    parser.add_argument("--hello", action="store_true", help="run the MTP CPU and wait for HELLO")
    parser.add_argument("--seconds", type=float, default=5.0, help="HELLO poll duration, 0.1..15 seconds")
    parser.add_argument("--selftest", action="store_true")
    args = parser.parse_args()
    if args.selftest:
        selftest()
        return 0
    if not (args.m1n1 and args.device and args.output) or not 0.1 <= args.seconds <= 15:
        parser.error("provide --m1n1, --device (or M1N1DEVICE), --output and --seconds in 0.1..15")
    raw = (args.m1n1 / "build/m1n1.bin").read_bytes()
    if hashlib.sha256(raw).hexdigest() != M1N1_SHA256:
        parser.error("unverified m1n1 build; re-check proxy ABI and identity before use")

    report = {
        "time": datetime.now(timezone.utc).isoformat(),
        "mode": "+".join(m for m, on in (("dapf", args.dapf), ("hello", args.hello)) if on) or "snapshot",
        "m1n1_sha256": M1N1_SHA256,
        "writes": [], "reboot_required": False, "result": "not_started",
    }
    iface = None
    try:
        sys.path.insert(0, str(args.m1n1 / "proxyclient"))
        from m1n1.proxy import UartInterface, M1N1Proxy, GUARD
        from m1n1.proxyutils import ProxyUtils, bootstrap_port

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
        dart = u.adt["/arm-io/dart-mtp"]
        dc = u.adt["/arm-io/dockchannel-mtp"]
        if "iop,ascwrap-v6" not in mtp.compatible or mtp.get_reg(0) != (ASC, 0x88000):
            raise RuntimeError("Unexpected MTP firmware register map")
        if "dart,t8110" not in dart.compatible or dart.get_reg(0) != (DART, 0xC000) or dart.get_reg(1) != (DAPF, 0x4000):
            raise RuntimeError("Unexpected dart-mtp register map")
        if "dockchannel,t8002" not in dc.compatible or dc.get_reg(3) != (FIFO, 0x1000):
            raise RuntimeError("Unexpected MTP FIFO register map")
        # m1n1's dapf_init uses the raw property; fetch the same bytes from the ADT blob.
        blob = u.get_adt()
        key = b"dapf-instance-0".ljust(32, b"\0")
        # Locate the dart-mtp node's property by matching its first entry start.
        entries = None
        pos = 0
        while True:
            pos = blob.find(key, pos)
            if pos < 0:
                break
            size = struct.unpack_from("<I", blob, pos + 32)[0] & 0x7FFFFFFF
            data = blob[pos + 36:pos + 36 + size]
            if size % DAPF_ENTRY.size == 0 and struct.unpack_from("<Q", data)[0] == 0x30828C000:
                entries = parse_dapf_entries(data)
                break
            pos += 36 + size
        if not entries or len(entries) != 10:
            raise RuntimeError("Could not locate dart-mtp dapf-instance-0 (10 entries) in the ADT")
        report["_dapf_entries"] = entries
        report["dapf_entry_count"] = len(entries)
        report["dapf_adt_entries"] = [{k: hex(v) for k, v in e.items()} for e in entries]
        if any(not (0x308000000 <= e["start"] < 0x320000000) for e in entries):
            raise RuntimeError("Unexpected DAPF window outside the AOP/nub range; refusing")
        report["chip"] = "0x8140"
        guard = GUARD.SKIP | GUARD.SILENT
        mmio = MMIO(p, report, guard)
        snapshot(mmio, report, iface)
        translate_firmware(u, report)
        report["result"] = "snapshot_only"
        if args.dapf:
            program_dapf(p, mmio, report)
            report["result"] = "dapf_programmed"
        if args.hello:
            hello(mmio, report, args.seconds)
    except Exception as exc:
        report["result"] = "error"
        report["error"] = f"{type(exc).__name__}: {exc}"
    finally:
        try:
            if iface is not None:
                p.set_exc_guard(0)
        except Exception:
            pass
        if iface is not None:
            iface.dev.close()
        report.pop("_dapf_entries", None)
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report, indent=2))
    return 0 if report["result"] in ("snapshot_only", "dapf_programmed", "hello_received") else 1


if __name__ == "__main__":
    sys.exit(main())
