#!/usr/bin/env python3
"""T8140 AOP (audio coprocessor) probe through the m1n1 proxy, bounded.

On Frisbee every audio output (Leap Speaker / MAX98360, CS42L83 headphone
codec) is an "iop-adma-stream" owned by the AOP's AudioService, so first
sound needs the AOP up and its EPIC audio service enumerated.  This tool:

  default   read-only snapshot: AOP CPU_CONTROL/idle, outbox control, nub
            boot-args span, dart-aop params/protect, GAPF enable/lock regs.
  --boot    bring the AOP up the way m1n1's experiments/aop_audio.py does
            (DART for /arm-io/dart-aop, AOPClient, optional boot-args
            tweaks, CPU RUN, RTKit negotiation), start its EPIC endpoints
            and log every service/device announcement for --listen seconds.
            No audio commands are sent; no DAPF/GAPF writes; MMIO faults are
            marked (not fatal) and counted.  Reboot the target afterwards.

m1n1 is used as a library only (no checkout edits).
"""

import argparse
from datetime import datetime, timezone
import hashlib
import io
import json
import os
from pathlib import Path
import sys
import time

M1N1_SHA256 = "780096243a690a09b8e033df25789da0a5b921511f242a0865f1e6712387f933"


class Tee(io.TextIOBase):
    def __init__(self, *streams):
        self.streams = streams

    def write(self, s):
        for st in self.streams:
            st.write(s)
            st.flush()
        return len(s)

    def flush(self):
        for st in self.streams:
            st.flush()


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--m1n1", type=Path, required=True)
    parser.add_argument("--device", default=os.environ.get("M1N1DEVICE"))
    parser.add_argument("--output", type=Path, required=True, help="JSON report")
    parser.add_argument("--log", type=Path, required=True, help="full transcript")
    parser.add_argument("--boot", action="store_true", help="start the AOP and enumerate EPIC services")
    parser.add_argument("--boot-seconds", type=float, default=15.0)
    parser.add_argument("--listen", type=float, default=20.0, help="seconds to collect announcements")
    parser.add_argument("--init-dart", action="store_true", help="reset dart-aop (M-series experiment behaviour)")
    parser.add_argument("--force", action="store_true",
                        help="halt and restart the AOP CPU if it is already running (WEDGES the AOP on Frisbee; needs a power cycle)")
    parser.add_argument("--resume", action="store_true",
                        help="AOP already running past HELLO: do not touch CPU_CONTROL, send the IOP power-state start and continue")
    parser.add_argument("--bootargs", action="store_true",
                        help="apply the m1n1 experiment boot-arg tweaks (p0CE/laCn/gila) if those keys exist")
    args = parser.parse_args()
    if not args.device:
        parser.error("provide --device or M1N1DEVICE")
    raw = (args.m1n1 / "build/m1n1.bin").read_bytes()
    if hashlib.sha256(raw).hexdigest() != M1N1_SHA256:
        parser.error("unverified m1n1 build")

    args.log.parent.mkdir(parents=True, exist_ok=True)
    logf = open(args.log, "w")
    sys.stdout = Tee(sys.__stdout__, logf)
    report = {"time": datetime.now(timezone.utc).isoformat(), "mode": "boot" if args.boot else "snapshot",
              "result": "not_started", "reboot_required": args.boot, "stages": [], "announcements": [],
              "faults": []}
    t0 = time.monotonic()

    def stage(name, **kw):
        row = {"stage": name, "t": round(time.monotonic() - t0, 3), **kw}
        report["stages"].append(row)
        print(f"[stage] {row}")

    iface = None
    p = None
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
        u = ProxyUtils(p)
        aop = u.adt["/arm-io/aop"]
        nub = u.adt["/arm-io/aop/iop-aop-nub"]
        dart_node = u.adt["/arm-io/dart-aop"]
        asc, asc_size = aop.get_reg(0)
        dart, _ = dart_node.get_reg(0)
        if asc != 0x310600000 or dart != 0x310fc0000:
            raise RuntimeError("Unexpected AOP register map")
        p.set_exc_guard(GUARD.SKIP | GUARD.SILENT)

        def faults(where):
            n = p.get_exc_count()
            if n:
                report["faults"].append({"where": where, "count": n})
                print(f"!! {n} MMIO fault(s) during {where}")
            return n

        def r32(a):
            v = p.read32(a)
            return None if faults(f"read32 {a:#x}") else v

        snap = {
            "cpu_control": r32(asc + 0x44),
            "cpu_status_0x48": r32(asc + 0x48),
            "v6_idle_status": r32(asc + 0x8000),
            "inbox_control": r32(asc + 0x8110),
            "outbox_control": r32(asc + 0x8114),
            "dart_params0": r32(dart + 0x0),
            "dart_params4": r32(dart + 0x4),
            "dart_protect": r32(dart + 0x200),
            "dart_tcr0": r32(dart + 0x1000),
            "dart_ttbr0": r32(dart + 0x1400),
            "dart_enable_streams0": r32(dart + 0xc00),
            "gapf_enable_0x310828020": r32(0x310828020),
            "gapf_lock_0x310828024": r32(0x310828024),
        }
        region_base = getattr(nub, "region_base", None)
        snap["nub_region_base"] = hex(region_base) if region_base else None
        snap["nub_firmware_name"] = getattr(nub, "firmware_name", None)
        if region_base:
            off = r32(region_base + 0x22c)
            size = r32(region_base + 0x230)
            snap["bootargs_offset"] = off
            snap["bootargs_size"] = size
            if off and size and 0 < size < 0x2000 and not faults("bootargs span"):
                blob = iface.readmem(region_base + off, size)
                try:
                    from m1n1.fw.aop.base import AOPBootargs
                    ba = AOPBootargs(blob)
                    snap["bootargs"] = {k: v.hex() for k, v in ba.items()}
                except Exception as exc:
                    snap["bootargs_error"] = str(exc)
                    snap["bootargs_raw"] = blob.hex()
        report["snapshot"] = {k: (hex(v) if isinstance(v, int) else v) for k, v in snap.items()}
        stage("snapshot", cpu_control=report["snapshot"]["cpu_control"],
              idle=report["snapshot"]["v6_idle_status"])
        report["result"] = "snapshot_only"

        if args.boot:
            if snap["cpu_control"] is not None and snap["cpu_control"] & 0x10 and not args.resume:
                if not args.force:
                    raise RuntimeError("AOP CPU already running; reboot the target first or pass --force")
                # A previous bounded attempt left the firmware stalled before
                # "startup complete".  Halt the CPU and start it again.
                p.write32(asc + 0x44, 0)
                time.sleep(0.5)
                stage("cpu_halted", cpu_control=hex(p.read32(asc + 0x44)), idle=hex(p.read32(asc + 0x8000)))
            from m1n1.hw.dart import DART as DARTObj
            from m1n1.fw.aop.client import AOPClient

            # Frisbee's iBoot leaves dart-aop configured (stream 0 TTBR valid,
            # streams 0/1/3/8-11/16/17 enabled, protect 0x1e7, GAPF locked).
            # Resetting it like the M-series experiment would throw away the
            # firmware's own mappings, so only --init-dart does that.
            dartobj = DARTObj.from_adt(u, "/arm-io/dart-aop")
            if args.init_dart:
                dartobj.initialize()
            faults("dart init")
            stage("dart_ready", tcr0=hex(p.read32(dart + 0x1000)), ttbr0=hex(p.read32(dart + 0x1400)))

            client = AOPClient(u, "/arm-io/aop", dartobj)
            client.verbose = 3
            client.allow_phys = True
            if args.bootargs:
                # This firmware's nub header has no boot-args offset at +0x22c;
                # find the "GKTS" block in the firmware region instead and patch
                # the keys Asahi/m1n1 set on M-series: p0CE (EC0p, power config,
                # t6020 value), laCn (nCal) = 0; gila/tPOA only if present.
                try:
                    import struct as _s
                    region, region_size = aop.get_reg(2)
                    # Bulk proxy reads of this region before the AOP is powered
                    # took SErrors in m1n1 (wide loads on device memory); 32-bit
                    # reads there work.  Copy with memcpy32 into proxy heap and
                    # read the DRAM copy.  GKTS sits around +0x618f8.
                    span = 0x70000
                    heap = u.malloc(span)
                    p.memcpy32(heap, region, span)
                    if faults("memcpy32 region"):
                        raise RuntimeError("region copy faulted")
                    blob = iface.readmem(heap, span)
                    idx = blob.find(b"GKTS")
                    if idx < 0:
                        raise RuntimeError("GKTS boot-args block not found")
                    off = idx
                    entries = {}
                    while off + 8 <= len(blob):
                        key = blob[off:off + 4]
                        size = _s.unpack_from("<I", blob, off + 4)[0]
                        if not all(32 <= c < 127 for c in key) or size > 0x1000:
                            break
                        entries[key.decode()] = (off + 8, size)
                        off += 8 + size
                    want = {"p0CE": 0x010000000000, "laCn": 0x0, "gila": 0x40, "tPOA": 0}
                    patched = {}
                    for key, val in want.items():
                        if key not in entries:
                            continue
                        eoff, size = entries[key]
                        old = blob[eoff:eoff + size]
                        new = val.to_bytes(size, "little")
                        if old != new:
                            for i, b in enumerate(new):
                                p.write8(region + eoff + i, b)
                            back = bytes(p.read8(region + eoff + i) for i in range(size))
                            patched[key] = {"old": old.hex(), "new": new.hex(), "readback": back.hex()}
                    if faults("bootargs write"):
                        raise RuntimeError("boot-args write faulted")
                    stage("bootargs_updated", block=hex(region + idx), entries=len(entries), patched=patched)
                    report["bootargs_keys"] = sorted(entries)
                except Exception as exc:
                    stage("bootargs_skipped", error=str(exc))
            # This firmware does not advertise the crashlog/syslog endpoints in
            # its endpoint map but still talks on them (it hands over its own
            # crashlog-region as a physical buffer on EP 1 before "startup
            # complete").  Register the handlers up front so those messages
            # are answered instead of logged as unknown.
            for epno in (1, 2):
                if epno not in client.epmap and epno in client.epcls:
                    client.add_ep(epno, client.epcls[epno](client, epno))
            def post_stall_dump(tag):
                """What is the AOP waiting for?  Read its OS_LOG ring, the
                __SHARED block it handed us, and the exclave mailbox regs."""
                out = {}
                try:
                    os_log = iface.readmem(0x10005dc0000, 0x37000)  # __OS_LOG (DRAM)
                    (args.output.parent / f"frisbee-aop-oslog-{tag}.bin").write_bytes(os_log)
                    import re as _re
                    strings = [s.decode() for s in _re.findall(rb"[\x20-\x7e]{8,}", os_log)]
                    out["oslog_strings"] = strings[-60:]
                    out["oslog_nonzero_bytes"] = sum(1 for b in os_log if b)
                except Exception as exc:
                    out["oslog_error"] = str(exc)
                try:
                    shared = iface.readmem(0x10001934000, 0x18000)  # __SHARED (-> 0x1004005c000)
                    (args.output.parent / f"frisbee-aop-shared-{tag}.bin").write_bytes(shared)
                    out["shared_nonzero_bytes"] = sum(1 for b in shared if b)
                    out["shared_head"] = shared[:64].hex()
                except Exception as exc:
                    out["shared_error"] = str(exc)
                mb = 0x35067c000
                regs = {}
                for name, off in (("a2i_ctrl", 0x110), ("i2a_ctrl", 0x114), ("i2a_recv0", 0x830),
                                  ("i2a_recv1", 0x838), ("r0", 0x0), ("r4", 0x4), ("r8", 0x8), ("r40", 0x40), ("r44", 0x44)):
                    v = p.read32(mb + off)
                    n = p.get_exc_count()
                    regs[name] = hex(v) if not n else f"fault({n})"
                out["exclave_mailbox"] = regs
                report[f"post_stall_{tag}"] = out
                print(f"[post-stall {tag}] exclave mailbox {regs}")
                print(f"[post-stall {tag}] oslog strings: {out.get('oslog_strings', [])[-25:]}")

            if args.resume:
                # The AOP is already past HELLO from an earlier bounded run and
                # is waiting for the IOP power-state message; re-create the
                # endpoint handlers it already advertised and carry on.
                for epno in (4, 8):
                    if epno not in client.epmap and epno in client.epcls:
                        client.add_ep(epno, client.epcls[epno](client, epno))
                stage("resume", cpu_control=hex(p.read32(asc + 0x44)))
            else:
                post_stall_dump("pre")  # baseline before RUN, for diffing
                # Match the experiment: make sure the outbox is enabled/empty.
                client.asc.OUTBOX_CTRL.val = 0x20001
                client.asc.CPU_CONTROL.set(RUN=1)
                stage("cpu_run")
            try:
                # AOP firmware waits for SetIOPPower(0x220) ("start via message",
                # what StandardASC.start() and Linux apple_rtkit_wake send)
                # before it reports IOP/AP power state 0x20.
                client.mgmt.start()
                try:
                    client.mgmt.wait_boot(args.boot_seconds)
                except Exception:
                    post_stall_dump("stall")
                    raise
                stage("rtkit_booted", endpoints=sorted(hex(k) for k in client.epmap))
            finally:
                report["dart_after"] = {"error": hex(p.read32(dart + 0x100)),
                                        "error_addr_lo": hex(p.read32(dart + 0x170)),
                                        "error_addr_hi": hex(p.read32(dart + 0x174)),
                                        "cpu_control": hex(p.read32(asc + 0x44)),
                                        "idle": hex(p.read32(asc + 0x8000))}
                print(f"[dart] {report['dart_after']}")
            faults("rtkit_boot")

            # Collect EPIC announcements from every endpoint.
            deadline = time.monotonic() + args.listen
            seen = set()
            while time.monotonic() < deadline:
                client.work_pending()
                for epno, ep in list(client.epmap.items()):
                    for attr in dir(ep):
                        srv = getattr(ep, attr, None)
                        if hasattr(srv, "ready") and hasattr(srv, "NAME") and (epno, attr) not in seen:
                            seen.add((epno, attr))
                            props = getattr(srv, "props", None)
                            try:
                                props = json.loads(json.dumps(props, default=str))
                            except Exception:
                                props = str(props)
                            row = {"endpoint": hex(epno), "service": attr, "name": srv.NAME, "props": props}
                            report["announcements"].append(row)
                            print(f"[announce] {row}")
                time.sleep(0.01)
            stage("listen_done", services=len(report["announcements"]))
            faults("listen")
            report["result"] = "aop_booted" if report["announcements"] else "aop_booted_no_services"
    except Exception as exc:
        import traceback
        traceback.print_exc()
        report["result"] = "error"
        report["error"] = f"{type(exc).__name__}: {exc}"
    finally:
        try:
            if p is not None:
                report["final_faults"] = p.get_exc_count()
                p.set_exc_guard(0)
        except Exception:
            pass
        if iface is not None:
            iface.dev.close()
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
        print(f"[result] {report['result']} -> {args.output}")
        sys.stdout = sys.__stdout__
        logf.close()
    return 0 if report["result"] in ("snapshot_only", "aop_booted") else 1


if __name__ == "__main__":
    sys.exit(main())
