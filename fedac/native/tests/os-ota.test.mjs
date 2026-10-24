// Regression for the OTA state machine in pieces/os.mjs: the frame in which
// the kernel download completes must not also judge the initramfs download,
// because `system` is a per-frame snapshot that still carries the kernel
// fetch's latched done/ok flags. On 2026-10-24 ac13 read -1 bytes for a file
// fetchBinary had just unlinked and failed every update as "truncated".
import { test } from "node:test";
import assert from "node:assert/strict";

async function rig() {
  const c = [0, 0, 0];
  globalThis.__theme = { update: () => ({ bg: c, fg: c, fgDim: c, fgMute: c, ok: c, warn: c, err: c }) }; // runtime global
  const os = await import(new URL(process.env.OS_PIECE || "../pieces/os.mjs", import.meta.url).href + "?" + Math.random());
  const calls = { fetch: [], fetchBinary: [], flash: [], post: [] };
  const sizes = {};
  const system = {
    version: "old-build 0000000000-2026-01-01T00:00", fetchPending: false, fetchResult: null, fetchError: "",
    fetchBinaryDone: false, fetchBinaryOk: false, fetchBinaryProgress: 0,
    flashTargets: [{ device: "/dev/nvme0n1p1", removable: false }], bootDevice: "/dev/nvme0n1p1",
    config: { handle: "oskie", claudeToken: "x" }, machineId: "test",
    fetch(u) { calls.fetch.push(u); system.fetchPending = true; },
    fetchBinary(u, dest) { calls.fetchBinary.push(dest); delete sizes[dest]; system.fetchBinaryDone = false; system.fetchBinaryOk = false; },
    fileSizeBytes(p) { return p in sizes ? sizes[p] : -1; },
    diskFreeBytes() { return 2e9; },
    flashUpdate(k, d, i) { calls.flash.push([k, d, i]); },
    fetchPost(u) { calls.post.push(u); return true; },
    jump() {},
    readFile(p) { return p === "/mnt/config.json" ? JSON.stringify({ handle: "oskie", token: "t" }) : ""; },
  };
  const noop = () => {};
  const paintApi = { wipe: noop, ink: noop, box: noop, line: noop, write: noop, screen: { width: 683, height: 384 }, system, wifi: { connected: true } };
  const frame = () => os.paint(paintApi);
  const key = (k) => os.act({ event: { is: (p) => p === "keyboard:down" || p === `keyboard:down:${k}` }, sound: { synth: noop }, system });
  os.boot({ system });
  return { os, system, calls, sizes, frame, key };
}

test("kernel-complete frame does not judge the initramfs with stale flags", async () => {
  const r = await rig();
  assert.equal(r.calls.fetch.length, 1, "version check fired at boot");
  r.system.fetchResult = "new-build 1111111111-2026-10-24T19:25\n15086592"; r.frame();
  r.system.fetchResult = null; r.system.fetchPending = false;
  r.key("y"); r.frame();
  assert.deepEqual(r.calls.fetchBinary, ["/tmp/vmlinuz.new"]);

  // Kernel lands: flags latch true, the file is its full size.
  r.sizes["/tmp/vmlinuz.new"] = 15086592;
  r.system.fetchBinaryDone = true; r.system.fetchBinaryOk = true; r.system.fetchBinaryProgress = 1;
  // The piece's fetchBinary mock clears the flags like the C side does, but
  // the frame that is already running keeps reading the old snapshot — so
  // re-latch them after the call, exactly what the device saw.
  const realFetchBinary = r.system.fetchBinary;
  r.system.fetchBinary = (u, d) => { realFetchBinary(u, d); r.system.fetchBinaryDone = true; r.system.fetchBinaryOk = true; };
  r.frame();
  assert.deepEqual(r.calls.fetchBinary, ["/tmp/vmlinuz.new", "/tmp/initramfs.cpio.gz.new"]);
  assert.equal(r.calls.post.length, 0, "no failure report was queued");
  assert.equal(r.calls.flash.length, 0, "and nothing flashed yet");

  // Next frame: the initramfs fetch is genuinely in flight.
  r.system.fetchBinaryDone = false; r.system.fetchBinaryOk = false; r.system.fetchBinaryProgress = 0.3;
  r.frame();
  assert.equal(r.calls.post.length, 0);

  // Initramfs lands at a sane size: flash starts with both files.
  r.sizes["/tmp/initramfs.cpio.gz.new"] = 422352789;
  r.system.fetchBinaryDone = true; r.system.fetchBinaryOk = true; r.system.fetchBinaryProgress = 1;
  r.frame();
  assert.deepEqual(r.calls.flash, [["/tmp/vmlinuz.new", "/dev/nvme0n1p1", "/tmp/initramfs.cpio.gz.new"]]);
  assert.equal(r.calls.post.length, 0);
});
