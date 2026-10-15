#!/usr/bin/env python3
"""T8140 MTP RTKit bring-up and keyboard HID capture through the m1n1 proxy.

Builds on neo-mtp-dapf-probe.py (HELLO confirmed once dart-mtp's DAPF is
programmed). This tool, in order and all bounded:
  1. programs /arm-io/dart-mtp's DAPF via the proxy's dapf_init(path);
  2. wraps dart-mtp as the MTP data DART on stream 0 (Frisbee mapper-mtp
     reg = 0; the M2 reference used stream 1), low IOVA range 0x8000..0x100000
     so nothing collides with the firmware's own 0x1000000.. window;
  3. opens DockChannel channel 0 (irq bank reg 1, config reg 2, data reg 3);
  4. starts the MTP CPU and runs m1n1's StandardASC RTKit negotiation
     (HELLO/ack, endpoint map, crashlog/syslog/... buffers) with a deadline;
  5. runs the m1n1 MTP transport protocol until the "keyboard" interface
     reports ready and its HID descriptor is captured (deadline);
  6. listens for HID reports for --listen seconds so a person can press
     keys on the target; every packet is logged.
GPIO requests from the firmware are logged and acknowledged without an SMC
client (no SMC traffic, no GPIO toggles) unless --smc is given.
Reboot the target before Linux afterwards. m1n1 is used as a library only.
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

ASC = 0x314600000
DART = 0x314800000
DAPF = 0x314810000
M1N1_SHA256 = "780096243a690a09b8e033df25789da0a5b921511f242a0865f1e6712387f933"
IDENTITY_OFFSET = 0x165B0
IDENTITY_BYTES = bytes.fromhex("880700b008d547f9")
RAM_WORKAROUND = bytes.fromhex("00008052c0035fd6")


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
    parser.add_argument("--boot-seconds", type=float, default=15.0)
    parser.add_argument("--init-seconds", type=float, default=30.0, help="wait for keyboard interface")
    parser.add_argument("--listen", type=float, default=20.0, help="seconds to capture HID reports")
    parser.add_argument("--smc", action="store_true", help="also bring up the SMC client for GPIO requests")
    parser.add_argument("--wait-for", default="keyboard")
    parser.add_argument("--notify", action="store_true", help="macOS notification when the listen window opens")
    args = parser.parse_args()
    if not args.device:
        parser.error("provide --device or M1N1DEVICE")
    raw = (args.m1n1 / "build/m1n1.bin").read_bytes()
    if hashlib.sha256(raw).hexdigest() != M1N1_SHA256:
        parser.error("unverified m1n1 build")

    args.log.parent.mkdir(parents=True, exist_ok=True)
    logf = open(args.log, "w")
    sys.stdout = Tee(sys.__stdout__, logf)
    report = {"time": datetime.now(timezone.utc).isoformat(), "result": "not_started",
              "reboot_required": True, "stages": [], "packets": [], "gpio_requests": [],
              "interfaces": {}}

    def stage(name, **kw):
        row = {"stage": name, "t": round(time.monotonic() - t0, 3), **kw}
        report["stages"].append(row)
        print(f"[stage] {row}")

    iface = None
    p = None
    t0 = time.monotonic()
    try:
        sys.path.insert(0, str(args.m1n1 / "proxyclient"))
        from m1n1.proxy import UartInterface, M1N1Proxy, GUARD
        from m1n1.proxyutils import ProxyUtils, bootstrap_port
        from m1n1.hw.dart import DART as DARTObj
        from m1n1.hw.dockchannel import DockChannel
        from m1n1.fw.asc import StandardASC
        from m1n1.fw import mtp as fwmtp

        iface = UartInterface(args.device)
        iface.dev.write_timeout = 3
        p = M1N1Proxy(iface)
        bootstrap_port(iface, p)
        if p.get_chipid() != 0x8140:
            raise RuntimeError("Target is not T8140")
        if iface.readmem(p.get_base() + IDENTITY_OFFSET, 8) not in (IDENTITY_BYTES, RAM_WORKAROUND):
            raise RuntimeError("Running m1n1 identity bytes do not match")
        u = ProxyUtils(p)
        mtp_node = u.adt["/arm-io/mtp"]
        dart_node = u.adt["/arm-io/dart-mtp"]
        dc_node = u.adt["/arm-io/dockchannel-mtp"]
        if mtp_node.get_reg(0) != (ASC, 0x88000) or dart_node.get_reg(1) != (DAPF, 0x4000):
            raise RuntimeError("Unexpected MTP register map")
        if dc_node.get_reg(1)[0] != 0x314B14000 or dc_node.get_reg(2)[0] != 0x314B30000:
            raise RuntimeError("Unexpected DockChannel register map")
        if p.read32(ASC + 0x44) & 0x10:
            raise RuntimeError("MTP CPU already running; reboot the target first")

        # Faults mark instead of rebooting; we check the counter at checkpoints.
        p.set_exc_guard(GUARD.SKIP | GUARD.SILENT)

        def faults(where):
            n = p.get_exc_count()
            if n:
                report.setdefault("faults", []).append({"where": where, "count": n})
                print(f"!! {n} MMIO fault(s) during {where}")
            return n

        ret = p.dapf_init("/arm-io/dart-mtp")
        if ret != 0 or faults("dapf_init"):
            raise RuntimeError(f"dapf_init returned {ret}")
        stage("dapf_programmed", entry0_start=hex(p.read64(DAPF + 8)))

        dart = DARTObj.from_adt(u, "/arm-io/dart-mtp", iova_range=(0x8000, 0x100000))
        stage("dart_ready", tcr0=hex(p.read32(DART + 0x1000)), ttbr0=hex(p.read32(DART + 0x1400)))

        dc = DockChannel(u, dc_node.get_reg(1)[0], dc_node.get_reg(2)[0], 1)
        drained = 0
        while dc.rx_count and drained < 4096:
            dc.read(dc.rx_count)
            drained += 1
        stage("dockchannel_ready", tx_free=dc.tx_free, rx_count=dc.rx_count)

        mtp = StandardASC(u, ASC, dart, stream=0)
        mtp.verbose = 3
        mtp.allow_phys = True

        smc = None
        if args.smc:
            from m1n1.fw.smc import SMCClient
            smc = SMCClient(u, u.adt["/arm-io/smc"].get_reg(0)[0])
            smc.start()
            smc.start_ep(0x20)
            smc.verbose = 0
            stage("smc_started")

        # Boot: RUN then bounded wait for the management handshake.
        mtp.asc.CPU_CONTROL.set(RUN=1)
        stage("cpu_run")
        mtp.mgmt.wait_boot(args.boot_seconds)
        stage("rtkit_booted", endpoints=sorted(hex(k) for k in mtp.epmap))
        faults("rtkit_boot")

        node = u.adt["/arm-io/dockchannel-mtp/mtp-transport"]
        proto = report  # for closure below

        class Comm(fwmtp.MTPCommInterface):
            def report(self, msg):
                parsed = fwmtp.NotificationMsg.parse(msg)
                if isinstance(parsed, fwmtp.GPIORequestMsg):
                    self.log(f"GPIO request (not executed): {parsed}")
                    proto["gpio_requests"].append({"device": parsed.device_id, "gpio": parsed.gpio_num,
                                                   "cmd": parsed.cmd, "args": parsed.args.hex()})
                    if self.proto.smc is not None and (parsed.device_id, parsed.gpio_num) in self.gpios:
                        return super().report(msg)
                    ack = fwmtp.GPIOAckMsg()
                    ack.unk = 0
                    ack.msg = parsed
                    self.device_control(ack)
                    return
                if isinstance(parsed, fwmtp.InitMsg):
                    rec = proto["interfaces"].setdefault(parsed.device_name, {"device_id": parsed.device_id, "blocks": []})
                    for blk in parsed.msg:
                        payload = blk.payload
                        if isinstance(payload, fwmtp.HIDDescriptor):
                            rec["hid_descriptor"] = bytes(payload.descriptor).hex()
                        elif isinstance(payload, fwmtp.GPIOInit):
                            rec["blocks"].append({"type": "gpio", "gpio_id": payload.gpio_id, "name": payload.gpio_name})
                            # Record only; the stock handler would look up the
                            # ADT function-* property and may raise for nodes
                            # without it.
                            try:
                                prop = getattr(self.proto.node[parsed.device_name],
                                               f"function-{payload.gpio_name}".replace("-", "_"))
                                import struct as _s
                                key = _s.pack(">I", prop.args[0]).decode("ascii")
                                self.gpios[(parsed.device_id, payload.gpio_id)] = key, prop.args[1]
                                rec["blocks"][-1]["smc_key"] = key
                            except Exception as exc:
                                rec["blocks"][-1]["lookup_error"] = str(exc)
                        else:
                            rec["blocks"].append({"type": blk.type, "payload": bytes(payload).hex()})
                    iface_obj = self.proto.get_interface(parsed.device_id, parsed.device_name)
                    self.log(f"Init block for {parsed.device_name} (id {parsed.device_id}), more={parsed.more_packets}")
                    if iface_obj is not None and not parsed.more_packets:
                        iface_obj.initialize()
                    return
                return super().report(msg)

        class Proto(fwmtp.MTPProtocol):
            INTERFACES = [Comm] + [c for c in fwmtp.MTPProtocol.INTERFACES if c.NAME != "comm"]

            def work(self):
                devid, pkt = self.read_pkt()
                proto["packets"].append({"t": round(time.monotonic() - t0, 3), "device": devid, "data": pkt.hex()})
                if devid in self.iface:
                    self.iface[devid].packet(pkt)
                else:
                    self.log(f"packet for unknown interface {devid}: {pkt.hex()}")

        mp = Proto(u, node, mtp, dc, smc)
        deadline = time.monotonic() + args.init_seconds
        target = args.wait_for
        while time.monotonic() < deadline:
            mtp.work_pending()
            if dc.rx_count:
                mp.work()
                continue
            obj = getattr(mp, target, None)
            if obj is not None and obj.initialized:
                break
            time.sleep(0.01)
        obj = getattr(mp, target, None)
        ready = obj is not None and obj.initialized
        stage("interface_wait_done", target=target, ready=ready,
              interfaces={k: v.get("device_id") for k, v in report["interfaces"].items()})
        faults("interface_init")
        if not ready:
            report["result"] = "interface_not_ready"
        else:
            report["result"] = "interface_ready"
            print(f"\n*** {target} ready. Press keys on the target for {args.listen:.0f} s ***\n")
            if args.notify:
                import subprocess
                subprocess.Popen(["/usr/bin/osascript", "-e",
                                  f'display notification "Press some keys on Frisbee now ({args.listen:.0f} s window)" with title "Claude: keyboard test" sound name "Glass"'])
            before = len(report["packets"])
            deadline = time.monotonic() + args.listen
            while time.monotonic() < deadline:
                mtp.work_pending()
                if dc.rx_count:
                    mp.work()
                else:
                    time.sleep(0.005)
            n = len(report["packets"]) - before
            stage("listen_done", packets=n)
            if n:
                report["result"] = "reports_received"
        faults("listen")
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
    return 0 if report["result"] in ("interface_ready", "reports_received") else 1


if __name__ == "__main__":
    sys.exit(main())
