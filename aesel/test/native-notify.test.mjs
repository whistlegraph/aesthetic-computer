import test from "node:test";
import assert from "node:assert/strict";
import { notifyNative, notifyURL, terminalFocused, deviceReport } from "../src/native-notify.mjs";

// A fake exec: the front app's pid, whether the Aesel GUI runs, and a log.
function system({ frontPid = 999, gui = false } = {}) {
  const calls = [];
  const exec = async (file, args) => {
    calls.push([file, ...args]);
    if (file.endsWith("lsappinfo") && args[0] === "front") return "ASN:0x0-0x1234:\n";
    if (file.endsWith("lsappinfo") && args[0] === "info") return `"pid"=${frontPid}\n`;
    if (file.endsWith("lsappinfo") && args[0] === "find") return gui ? "ASN:0x0-0x99:\n" : "";
    if (file.endsWith("ps")) return "1\n";
    return "";
  };
  return { exec, calls };
}

test("a focused terminal gets no notification", async () => {
  assert.equal(await terminalFocused({ lsappinfo: system({ frontPid: 42 }).exec, chain: async () => new Set([42, 7]) }), true);
  assert.equal(await terminalFocused({ lsappinfo: system({ frontPid: 99 }).exec, chain: async () => new Set([42, 7]) }), false);
});

test("posts through the Aesel app when it runs, osascript otherwise; off when asked", async () => {
  const gui = system({ gui: true });
  assert.equal(await notifyNative({ title: "Aesel is done", body: "made a  palm\ntree", kind: "done" }, { platform: "darwin", env: {}, exec: gui.exec }), "aesel");
  const open = gui.calls.find(c => c[0] === "/usr/bin/open");
  assert.equal(open[1], "-g");
  const url = new URL(open[2]);
  assert.equal(url.protocol, "aesel:"); assert.equal(url.host, "notify");
  assert.equal(url.searchParams.get("body"), "made a palm tree");
  assert.equal(url.searchParams.get("from"), "tui");

  // A running Aesel.app that predates aesel:// cannot take the URL: fall back.
  const stale = system({ gui: true });
  const failingOpen = async (file, args) => file === "/usr/bin/open" ? null : stale.exec(file, args);
  assert.equal(await notifyNative({ title: "t" }, { platform: "darwin", env: {}, exec: failingOpen }), "osascript");

  const plain = system();
  assert.equal(await notifyNative({ title: 'say "hi"', body: "x" }, { platform: "darwin", env: {}, exec: plain.exec }), "osascript");
  const script = plain.calls.find(c => c[0] === "/usr/bin/osascript");
  assert.deepEqual(script.slice(-2), ['say "hi"', "x"], "title and body travel as argv, never inside the script");

  assert.equal(await notifyNative({ title: "t" }, { platform: "linux", env: {}, exec: plain.exec }), "off");
  assert.equal(await notifyNative({ title: "t" }, { platform: "darwin", env: { AESEL_NOTIFY: "0" }, exec: plain.exec }), "off");
});

test("notify URLs are bounded", () => {
  const url = new URL(notifyURL({ title: "t".repeat(500), body: "b".repeat(500) }));
  assert.equal(url.searchParams.get("title").length, 120);
  assert.equal(url.searchParams.get("body").length, 240);
});

test("the TUI reports as Aesel on this machine's platform", () => {
  const report = deviceReport("open", { version: "0.8.24", platform: "darwin" });
  assert.equal(report.app, "aesel"); assert.equal(report.platform, "mac"); assert.equal(report.label, "Aesel TUI");
  assert.match(report.deviceId, /^[0-9a-f-]{36}$/); assert.equal(report.version, "0.8.24");
  assert.equal(deviceReport("open", { version: "dev", platform: "linux" }).version, undefined);
});
