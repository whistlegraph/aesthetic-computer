#!/usr/bin/env node
// ac-dmx-cli — poke a USB DMX widget from the shell.
//
// Talks Enttec DMX USB Pro framing (the house DMXking DMX USB PRO, FTDI
// serial) — the same wire that Menu Band's DMXOut.swift and AC OS's
// system.dmxSend push, so whatever this proves on the bench is what the
// synth lane and the native piece will do. Node + stty only; no deps.
//
//   ac-dmx-cli list                       widgets, USB identity, who holds them
//   ac-dmx-cli info                       serial number + firmware + DMX timing
//   ac-dmx-cli set 510=255 511=0 512=0    channels (1-based; ranges 1-3=255)
//   ac-dmx-cli rgb 255 0 0 [--at 1]       3-channel fixture at an address
//   ac-dmx-cli fill 255                   every slot
//   ac-dmx-cli blackout                   every slot to 0
//   ac-dmx-cli sweep [--from 1 --to 512 --ms 300 --width 1]
//                                         walk one lit channel — find the
//                                         fixture's address by watching it
//   ac-dmx-cli cycle [--at 1 --ms 600]    red/green/blue/white/off — proves
//                                         channel order
//   ac-dmx-cli strobe [r g b] [--at 1 --hz 10 --duty 0.5]
//                                         hard on/off — how sharp can the
//                                         fixture flash?
//   ac-dmx-cli desk [--at 1,7,13 --names R,G,B,A,W,UV --slew 1 --link]
//                                         terminal fader desk: arrows move
//                                         sliders, moves glide, all live;
//                                         several fixtures on one screen
//   ac-dmx-cli stdin                      lines of `510=255 …` / `rgb r g b`
//                                         / `fill v` / `off`, held live
//   ac-dmx-cli raw <label> [hex…]         any Pro message; prints the reply
//
// Flags: --port /dev/cu.x (or AC_DMX_PORT)  --hold  --fps 25  --slots N  --json

import fs from "node:fs";
import { spawn, spawnSync } from "node:child_process";

const argv = process.argv.slice(2);
const flags = {};
const words = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const eq = a.indexOf("=");
    if (eq > 0) flags[a.slice(2, eq)] = a.slice(eq + 1);
    else if (i + 1 < argv.length && !argv[i + 1].startsWith("--") && !/^(hold|json)$/.test(a.slice(2)))
      flags[a.slice(2)] = argv[++i];
    else flags[a.slice(2)] = true;
  } else words.push(a);
}
const cmd = words.shift() || "help";
const num = (k, d) => (flags[k] === undefined ? d : Number(flags[k]));
const json = !!flags.json;

// ---------------------------------------------------------------- widgets

const PATTERNS = [/^cu\.usbserial/, /^cu\.SLAB_USBtoUART/, /^cu\.wchusbserial/];

function widgets() {
  return fs
    .readdirSync("/dev")
    .filter((n) => PATTERNS.some((p) => p.test(n)))
    .map((n) => "/dev/" + n);
}

// USB identity from ioreg, keyed by serial number (the FTDI port name
// carries the serial: cu.usbserial-<serial>).
function usbIdentities() {
  const out = spawnSync("ioreg", ["-p", "IOUSB", "-l", "-w0"], { encoding: "utf8" }).stdout || "";
  const ids = [];
  let cur = null;
  for (const line of out.split("\n")) {
    if (/^\s*\+-o /.test(line)) {
      if (cur) ids.push(cur);
      cur = {};
      continue;
    }
    if (!cur) continue;
    const m = line.match(/"(USB Product Name|USB Vendor Name|USB Serial Number|idVendor|idProduct)" = (?:"([^"]*)"|(\d+))/);
    if (m) cur[m[1]] = m[2] ?? Number(m[3]);
  }
  if (cur) ids.push(cur);
  return ids.filter((d) => d["USB Serial Number"]);
}

function identityFor(port) {
  const serial = (port.match(/-([A-Za-z0-9]+)$/) || [])[1];
  if (!serial) return null;
  const d = usbIdentities().find((x) => x["USB Serial Number"] === serial);
  if (!d) return null;
  const hex = (n) => "0x" + Number(n).toString(16).padStart(4, "0");
  return {
    vendor: d["USB Vendor Name"],
    product: d["USB Product Name"],
    serial,
    vid: hex(d.idVendor),
    pid: hex(d.idProduct),
  };
}

function holders(port) {
  const out = spawnSync("lsof", ["-n", "-P", "-F", "pc", port], { encoding: "utf8" }).stdout || "";
  const list = [];
  let pid = null;
  for (const line of out.split("\n")) {
    if (line[0] === "p") pid = Number(line.slice(1));
    else if (line[0] === "c" && pid) list.push({ pid, command: line.slice(1) });
  }
  return list;
}

function pickPort() {
  const want = flags.port || process.env.AC_DMX_PORT;
  if (want) {
    if (!fs.existsSync(want)) die(`no such port: ${want}`);
    return want;
  }
  const all = widgets();
  if (!all.length) die("no USB DMX widget found (looked for /dev/cu.usbserial*, SLAB_USBtoUART*, wchusbserial*)");
  if (all.length > 1) warn(`${all.length} widgets; using ${all[0]} (pick with --port)`);
  return all[0];
}

// ------------------------------------------------------------------ wire

// 7E label lenLo lenHi data… E7
function frame(label, data) {
  const n = data.length;
  return Buffer.concat([Buffer.from([0x7e, label, n & 0xff, n >> 8]), Buffer.from(data), Buffer.from([0xe7])]);
}

// Label 6 "send DMX": start code 0 + slots. Pro widgets latch the last
// frame and keep the line refreshed themselves, so one write lights a
// fixture; a hold loop just keeps it honest.
//
// Frame SIZE is the whole game on this link. The widget drains its serial
// side far slower than we can write, so 518-byte full-universe frames at
// 30 fps pile up in the kernel, and then every close() or open() of the
// port waits behind the backlog — that was the 2026-09-18 "wedge". So a
// frame carries only up to the highest channel touched this run (DMX
// minimum 24), which for a par at address 1 is 30 bytes instead of 518.
// `--slots N` forces a size (512 = classic full universe).
// Measured 2026-09-18 with tcdrain on the DMXking ultraDMX Micro: ~9.5 KB/s,
// a 115200-baud link. A 518-byte frame drains in ~55 ms (18 fps ceiling);
// a 30-byte one in ~3 ms. Loops never sleep less than a frame's drain time
// (plus a little slack) so the kernel queue can't grow.
const LINK_BYTES_PER_S = 9000;
const framePeriod = (fps, bytes) => Math.max(1000 / fps, (bytes / LINK_BYTES_PER_S) * 1000 * 1.15);
let highTouched = 24;
const touch = (ch) => { if (ch > highTouched) highTouched = Math.min(512, ch); };
function dmxFrame(universe) {
  const n = flags.slots ? Math.max(24, Math.min(512, num("slots", 512))) : highTouched;
  return frame(6, Buffer.concat([Buffer.from([0]), universe.subarray(0, n)]));
}

class Widget {
  constructor(port, fd) {
    this.port = port;
    this.fd = fd;
  }
  // Port setup. macOS resets a serial port's termios on its FIRST open, so
  // settings only stick while someone holds it — configure AFTER our own
  // open, not before. Raw 8N2 like DMXOut/js-bindings (Pro widgets ignore
  // baud), and -hupcl so close() leaves DTR up: the ultraDMX Micro stops
  // transmitting when DTR drops, which is why a one-shot `rgb` used to
  // light the pars and then let them fall dark the moment we exited.
  //
  // The probe first: stty's open() of the device can block forever in the
  // kernel behind a previous holder's undrained close (FTDI on macOS, seen
  // 2026-09-18). Nothing in userland clears that, and a stuck child can't
  // even be killed, so it runs detached under a timer and we report the
  // wedge and leave rather than hanging our own open() on it.
  static async open(port) {
    const probe = await new Promise((res) => {
      const c = spawn("stty", ["-f", port, "-a"], { stdio: "ignore", detached: true });
      const t = setTimeout(() => { c.unref(); res("wedged"); }, 2500);
      c.on("exit", (code) => { clearTimeout(t); res(code); });
      c.on("error", () => { clearTimeout(t); res(-1); });
    });
    if (probe === "wedged") die(`${port} is wedged — open() blocks in the kernel (a previous holder died mid-stream). Wait for it to drain, or unplug and replug the widget's USB cable.`);
    if (probe !== 0) die(`cannot open ${port}`);
    const fd = fs.openSync(port, "r+");
    const r = spawnSync("stty", ["-f", port, "raw", "115200", "cstopb", "-hupcl"], { encoding: "utf8", timeout: 2500 });
    if (r.status !== 0) { fs.closeSync(fd); die(`stty on ${port} failed: ${(r.stderr || "").trim()}`); }
    return new Widget(port, fd);
  }
  write(buf) {
    const t0 = Date.now();
    const n = fs.writeSync(this.fd, buf);
    if (n !== buf.length) die(`short write to ${this.port} (${n}/${buf.length}) — unplugged?`);
    const ms = Date.now() - t0;
    if (ms > 100 && !this.warnedSlow) {
      this.warnedSlow = true;
      warn(`write blocked ${ms} ms — the widget's link is behind; lower --fps or --slots (frame is ${buf.length} bytes)`);
    }
  }
  // Read until a complete 7E…E7 message arrives or the timeout lapses.
  // fs.read runs on the threadpool, so a widget that never answers only
  // costs us the timeout; process.exit at the end reaps the stuck read.
  async reply(ms = 600) {
    let buf = Buffer.alloc(0);
    const deadline = Date.now() + ms;
    while (Date.now() < deadline) {
      const chunk = await Promise.race([
        new Promise((res) => {
          const b = Buffer.alloc(1024);
          fs.read(this.fd, b, 0, b.length, null, (e, n) => res(e ? null : b.subarray(0, n)));
        }),
        new Promise((res) => setTimeout(() => res(null), Math.max(1, deadline - Date.now()))),
      ]);
      if (!chunk) break;
      buf = Buffer.concat([buf, chunk]);
      const msg = parseMessage(buf);
      if (msg) return msg;
    }
    return buf.length ? { raw: buf } : null;
  }
  async ask(label, data = [], ms) {
    this.write(frame(label, data));
    return this.reply(ms);
  }
  close() {
    try { fs.closeSync(this.fd); } catch {}
  }
}

function parseMessage(buf) {
  const s = buf.indexOf(0x7e);
  if (s < 0 || buf.length < s + 5) return null;
  const label = buf[s + 1];
  const len = buf[s + 2] | (buf[s + 3] << 8);
  if (buf.length < s + 4 + len + 1) return null;
  if (buf[s + 4 + len] !== 0xe7) return null;
  return { label, data: buf.subarray(s + 4, s + 4 + len) };
}

const hex = (b) => [...b].map((x) => x.toString(16).padStart(2, "0")).join(" ");

// -------------------------------------------------------------- universe

const universe = Buffer.alloc(512);

// `510=255`, `1-3=255`, `510:255`; channels are 1-based on the wire.
function applySpec(tok) {
  const m = tok.match(/^(\d+)(?:-(\d+))?[=:](\d+)$/);
  if (!m) throw new Error(`bad channel spec "${tok}" (want ch=val or a-b=val)`);
  const a = Number(m[1]), b = Number(m[2] ?? m[1]), v = Number(m[3]);
  if (a < 1 || b > 512 || a > b) throw new Error(`channel out of 1..512: "${tok}"`);
  if (v < 0 || v > 255) throw new Error(`value out of 0..255: "${tok}"`);
  universe.fill(v, a - 1, b);
  touch(b);
}

function applyLine(line) {
  const t = line.trim().split(/\s+/).filter(Boolean);
  if (!t.length) return;
  if (t[0] === "rgb") setRGB(num("at", 1), ...t.slice(1, 4).map(Number));
  else if (t[0] === "fill") { universe.fill(clamp(Number(t[1]))); touch(512); }
  else if (t[0] === "off" || t[0] === "blackout") universe.fill(0);
  else t.forEach(applySpec);
}

const clamp = (v) => Math.max(0, Math.min(255, Math.round(Number(v) || 0)));
function setRGB(at, r, g, b) {
  if (at < 1 || at > 510) die(`--at must be 1..510 for a 3-channel fixture (got ${at})`);
  universe[at - 1] = clamp(r);
  universe[at] = clamp(g);
  universe[at + 1] = clamp(b);
  touch(at + 2);
}

// ---------------------------------------------------------------- output

function die(msg) {
  console.error(`ac-dmx-cli: ${msg}`);
  process.exit(1);
}
const warn = (msg) => console.error(`ac-dmx-cli: ${msg}`);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function noteHolders(port) {
  const others = holders(port).filter((h) => h.pid !== process.pid);
  if (!others.length) return;
  const who = others.map((h) => `${h.command} (pid ${h.pid})`).join(", ");
  warn(`${who} also has ${port} open — its frames interleave with yours` +
       (others.some((h) => /menuband/i.test(h.command))
         ? "; Menu Band re-sends its own frame every 1s, so a held frame will flicker against it"
         : ""));
}

// Lit channels, runs of equal values folded: `1-64=255 510=40`.
function litSummary() {
  const lit = [];
  for (let i = 0; i < 512; i++) {
    if (!universe[i]) continue;
    let j = i;
    while (j + 1 < 512 && universe[j + 1] === universe[i]) j++;
    lit.push(j > i ? `${i + 1}-${j + 1}=${universe[i]}` : `${i + 1}=${universe[i]}`);
    i = j;
  }
  return lit.length ? lit.join(" ") : "all 0";
}

// Send the current universe; a couple of frames for a one-shot, or a
// refresh loop under --hold until Ctrl-C.
async function push(w, { hold = !!flags.hold, fps = num("fps", 25), label } = {}) {
  const pkt = dmxFrame(universe);
  if (!hold) {
    w.write(pkt);
    await sleep(40);
    w.write(pkt);
    console.log(`${label ?? "sent"}: ${litSummary()}`);
    return;
  }
  const real = (1000 / framePeriod(fps, dmxFrame(universe).length)).toFixed(0);
  console.log(`${label ?? "holding"}: ${litSummary()}  (${real} fps, Ctrl-C to stop; widget keeps the last frame)`);
  await holdLoop(w, fps);
}

let stopped = false;
process.on("SIGINT", () => { stopped = true; });

async function holdLoop(w, fps, onTick) {
  while (!stopped) {
    onTick?.();
    const pkt = dmxFrame(universe);
    w.write(pkt);
    await sleep(framePeriod(fps, pkt.length));
  }
}

// -------------------------------------------------------------- commands

async function cmdList() {
  const rows = widgets().map((port) => ({ port, usb: identityFor(port), holders: holders(port) }));
  if (json) return console.log(JSON.stringify(rows, null, 2));
  if (!rows.length) return console.log("no USB DMX widgets");
  for (const r of rows) {
    const id = r.usb ? `${r.usb.vendor} ${r.usb.product} (${r.usb.vid}:${r.usb.pid}, serial ${r.usb.serial})` : "unknown USB serial bridge";
    const held = r.holders.length ? r.holders.map((h) => `${h.command}:${h.pid}`).join(", ") : "free";
    console.log(`${r.port}\n  ${id}\n  held by: ${held}`);
  }
}

const bcdSerial = (d) => {
  // 4 bytes BCD, least significant first; FFFFFFFF = not programmed.
  if (d.length < 4) return null;
  if (d.every((b) => b === 0xff)) return "unset";
  return [...d.subarray(0, 4)].reverse().map((b) => b.toString(16).padStart(2, "0")).join("").replace(/^0+/, "") || "0";
};

async function cmdInfo() {
  const port = pickPort();
  noteHolders(port);
  const w = await Widget.open(port);
  const info = { port, usb: identityFor(port) };
  const serial = await w.ask(10);
  if (serial?.label === 10) info.serial = bcdSerial(serial.data);
  else info.serial = null;
  const params = await w.ask(3, [0, 0]);
  if (params?.label === 3 && params.data.length >= 5) {
    const d = params.data;
    info.firmware = `${d[1]}.${d[0]}`;
    info.breakTimeUs = +(d[2] * 10.67).toFixed(1);
    info.mabTimeUs = +(d[3] * 10.67).toFixed(1);
    info.refreshHz = d[4] === 0 ? "max (~40)" : d[4];
  }
  // Pro Mk2 / DMXking extension: label 77 = manufacturer, 78 = device, each
  // an ESTA id (LE u16) + ASCII name. A plain Pro stays silent; that's fine.
  const esta = (d) => (d.length >= 2 ? `0x${(d[0] | (d[1] << 8)).toString(16).padStart(4, "0")} ${d.subarray(2).toString("latin1")}` : hex(d));
  const mfr = await w.ask(77, [], 300);
  if (mfr?.label === 77) info.manufacturer = esta(mfr.data);
  const dev = await w.ask(78, [], 300);
  if (dev?.label === 78) info.device = esta(dev.data);
  w.close();
  if (json) return console.log(JSON.stringify(info, null, 2));
  console.log(`port      ${port}`);
  if (info.usb) console.log(`usb       ${info.usb.vendor} ${info.usb.product} ${info.usb.vid}:${info.usb.pid}`);
  console.log(`serial    ${info.serial ?? "no reply (label 10)"}`);
  if (info.firmware) {
    console.log(`firmware  ${info.firmware}`);
    console.log(`break     ${info.breakTimeUs} µs   mab ${info.mabTimeUs} µs   refresh ${info.refreshHz} Hz`);
  } else console.log("params    no reply (label 3)");
  if (info.manufacturer) console.log(`maker     ${info.manufacturer}`);
  if (info.device) console.log(`device    ${info.device}`);
}

async function cmdSet() {
  if (!words.length) die("set needs channel specs, e.g. set 510=255 511=0 512=0");
  try { words.forEach(applySpec); } catch (e) { die(e.message); }
  await withWidget((w) => push(w));
}

async function cmdRGB() {
  if (words.length < 3) die("rgb needs three values: rgb 255 0 0 [--at 510]");
  setRGB(num("at", 1), ...words.map(Number));
  await withWidget((w) => push(w, { label: `rgb @${num("at", 1)}` }));
}

async function cmdFill() {
  universe.fill(clamp(words[0] ?? 255));
  touch(512);
  await withWidget((w) => push(w));
}

async function cmdBlackout() {
  universe.fill(0);
  touch(512);   // a blackout means the whole line, whatever was lit before
  await withWidget((w) => push(w, { label: "blackout" }));
}

// Walk a lit window across the universe so the fixture announces its own
// address: hit Ctrl-C when it lights and the channel is on screen.
async function cmdSweep() {
  const from = num("from", 1), to = num("to", 512), ms = num("ms", 300);
  const width = num("width", 1), value = clamp(num("value", 255));
  if (from < 1 || to > 512 || from > to) die("--from/--to must satisfy 1 <= from <= to <= 512");
  await withWidget(async (w) => {
    console.log(`sweeping ${from}..${to}, ${width} wide at ${value}, ${ms} ms each (Ctrl-C when the fixture lights)`);
    let ch = from;
    while (!stopped && ch <= to) {
      universe.fill(0);
      universe.fill(value, ch - 1, Math.min(512, ch - 1 + width));
      touch(Math.min(512, ch - 1 + width));
      w.write(dmxFrame(universe));
      process.stdout.write(`\rchannel ${String(ch).padStart(3)}${width > 1 ? `-${Math.min(512, ch + width - 1)}` : ""}   `);
      await sleep(ms);
      if (!stopped) ch++;
    }
    console.log(stopped ? `\nstopped at channel ${ch} (still lit — \`ac-dmx-cli blackout\` to clear)` : "\nsweep done (last window still lit)");
  });
}

async function cmdCycle() {
  const at = num("at", 1), ms = num("ms", 600);
  const steps = [["red", 255, 0, 0], ["green", 0, 255, 0], ["blue", 0, 0, 255], ["white", 255, 255, 255], ["off", 0, 0, 0]];
  await withWidget(async (w) => {
    console.log(`cycling a 3-channel fixture at ${at}-${at + 2}, ${ms} ms per step (Ctrl-C to stop)`);
    let i = 0;
    while (!stopped) {
      const [name, r, g, b] = steps[i % steps.length];
      setRGB(at, r, g, b);
      w.write(dmxFrame(universe));
      process.stdout.write(`\r${name.padEnd(6)} ${at}=${r} ${at + 1}=${g} ${at + 2}=${b}   `);
      await sleep(ms);
      i++;
    }
    console.log("\nstopped");
  });
}

// Hard on/off at a rate — how sharp can this fixture flash? The widget
// refreshes the line at 40 Hz, so anything past 20 Hz smears into a dim
// hold; the fixture's own driver usually gives out before that.
async function cmdStrobe() {
  const at = num("at", 1), hz = num("hz", 10), duty = num("duty", 0.5);
  const [r, g, b] = words.length >= 3 ? words.map(Number) : [255, 255, 255];
  if (!(hz > 0 && hz <= 40)) die("--hz must be in (0, 40]");
  await withWidget(async (w) => {
    const period = 1000 / hz;
    setRGB(at, r, g, b);
    const minGap = framePeriod(1000, dmxFrame(universe).length);
    if (period * Math.min(duty, 1 - duty) < minGap) warn(`frame drains in ${minGap.toFixed(0)} ms — ${hz} Hz at this frame size will smear (--slots 24 helps)`);
    console.log(`strobing ${r},${g},${b} at ${at} — ${hz} Hz, duty ${duty}, on ${(period * duty).toFixed(0)} ms / off ${(period * (1 - duty)).toFixed(0)} ms (Ctrl-C to stop)`);
    let flashes = 0;
    while (!stopped) {
      setRGB(at, r, g, b);
      w.write(dmxFrame(universe));
      await sleep(period * duty);
      setRGB(at, 0, 0, 0);
      w.write(dmxFrame(universe));
      await sleep(period * (1 - duty));
      flashes++;
    }
    console.log(`stopped after ${flashes} flashes (fixture left dark)`);
  });
}

// A fader desk in the terminal: one slider per channel per fixture, all
// live at once so colours blend, with a slew time so a fader move glides
// the way a real crossfade does instead of snapping. `--at 1,7,13` puts
// several fixtures on one screen; link mode moves the same channel on all
// of them together.
async function cmdDesk() {
  const ats = String(flags.at ?? "1").split(",").map(Number).filter((x) => x >= 1);
  const names = String(flags.names || "R,G,B,A,W,UV").split(",").map((s) => s.trim()).filter(Boolean);
  const n = names.length;
  for (const at of ats) if (at + n - 1 > 512) die(`fixture at ${at} with ${n} channels runs past 512`);
  const fixtures = ats.map((at) => ({ at, target: new Array(n).fill(0), cur: new Array(n).fill(0) }));
  const slews = [0, 0.3, 1, 3, 8];              // seconds to ~95% of a move
  let slew = slews.indexOf(num("slew", 1)) >= 0 ? slews.indexOf(num("slew", 1)) : 2;
  let fsel = 0, sel = 0, link = ats.length > 1 && !!flags.link;
  const fps = num("fps", 25);

  const readline = await import("node:readline");
  if (!process.stdin.isTTY) die("desk needs a terminal (use `stdin` mode for piped control)");
  readline.emitKeypressEvents(process.stdin);
  process.stdin.setRawMode(true);
  process.stdin.resume();
  const clampV = (v) => Math.max(0, Math.min(255, Math.round(v)));
  // A move lands on the selected fader, or on that channel of every fixture in link mode.
  const nudge = (d) => { for (const f of link ? fixtures : [fixtures[fsel]]) f.target[sel] = clampV(f.target[sel] + d); };
  const put = (v) => { for (const f of link ? fixtures : [fixtures[fsel]]) f.target[sel] = clampV(v); };
  process.stdin.on("keypress", (ch, key) => {
    const k = key?.name;
    if ((key?.ctrl && k === "c") || k === "q") { stopped = true; return; }
    if (k === "left") { if (sel > 0) sel--; else if (fsel > 0) { fsel--; sel = n - 1; } }
    else if (k === "right" || k === "tab") { if (sel < n - 1) sel++; else if (fsel < fixtures.length - 1) { fsel++; sel = 0; } }
    else if (k === "up") nudge(key.shift ? 25 : 5);
    else if (k === "down") nudge(key.shift ? -25 : -5);
    else if (k === "pageup") nudge(25);
    else if (k === "pagedown") nudge(-25);
    else if (ch === "[") fsel = (fsel + fixtures.length - 1) % fixtures.length;
    else if (ch === "]") fsel = (fsel + 1) % fixtures.length;
    else if (ch >= "1" && ch <= "9" && Number(ch) <= n) sel = Number(ch) - 1;
    else if (ch === "0") put(0);
    else if (ch === "f") put(255);
    else if (ch === "F") for (const f of link ? fixtures : [fixtures[fsel]]) f.target.fill(255);
    else if (ch === "b") for (const f of fixtures) f.target.fill(0);
    else if (ch === "l") link = !link;
    else if (ch === "s") slew = (slew + 1) % slews.length;
    else if (ch === "S") slew = (slew + slews.length - 1) % slews.length;
  });

  await withWidget(async (w) => {
    let last = Date.now();
    const bar = (v) => "█".repeat(Math.round(v / 255 * 20)).padEnd(20, "░");
    while (!stopped) {
      const now = Date.now(), dt = (now - last) / 1000; last = now;
      const secs = slews[slew];
      const k = secs === 0 ? 1 : 1 - Math.exp(-dt / (secs / 3));
      for (const f of fixtures) {
        for (let i = 0; i < n; i++) {
          f.cur[i] += (f.target[i] - f.cur[i]) * k;
          if (Math.abs(f.target[i] - f.cur[i]) < 0.5) f.cur[i] = f.target[i];
          universe[f.at - 1 + i] = Math.round(f.cur[i]);
        }
        touch(f.at - 1 + n);
      }
      const pkt = dmxFrame(universe);
      w.write(pkt);
      const blocks = fixtures.map((f, fi) => {
        const head = `${fi === fsel ? "▶" : " "} fixture @${f.at}${fixtures.length > 1 ? `   [${fi + 1}/${fixtures.length}]` : ""}`;
        const rows = names.map((name, i) => {
          const on = fi === fsel && i === sel, linked = link && i === sel && fi !== fsel;
          return `  ${on ? "▶" : linked ? "·" : " "} ${i + 1} ${name.padEnd(3)} ${bar(f.cur[i])} ${String(Math.round(f.cur[i])).padStart(3)}` +
            (f.target[i] !== Math.round(f.cur[i]) ? ` → ${f.target[i]}` : "");
        });
        return [head, ...rows].join("\n");
      });
      process.stdout.write("\x1b[2J\x1b[H" +
        `ac-dmx-cli desk — ${w.port}   slew ${secs}s${fixtures.length > 1 ? `   link ${link ? "ON" : "off"}` : ""}\n\n` +
        blocks.join("\n\n") +
        `\n\n←/→ or 1-${n} pick   ↑/↓ ±5 (shift ±25)   0 zero   f full   F fixture full   b blackout all` +
        (fixtures.length > 1 ? `   [ ] fixture   l link` : "") + `   s/S slew   q quit\n`);
      await sleep(framePeriod(fps, pkt.length));
    }
    process.stdout.write("\x1b[2J\x1b[H");
    console.log(`desk closed — lights left at ${litSummary()}`);
  });
  process.stdin.setRawMode(false);
}

// Live universe fed by lines on stdin — the hook for scripts and agents.
async function cmdStdin() {
  await withWidget(async (w) => {
    const fps = num("fps", 25);
    let dirty = false;
    let eof = false;
    const rl = (await import("node:readline")).createInterface({ input: process.stdin });
    rl.on("line", (line) => {
      try { applyLine(line); dirty = true; } catch (e) { warn(e.message); }
    });
    rl.on("close", () => { eof = true; });
    console.error(`ac-dmx-cli: live on ${w.port} at ${fps} fps; lines: 510=255 …  rgb r g b [--at]  fill v  off`);
    const flush = () => { if (dirty) { console.log(litSummary()); dirty = false; } };
    while (!stopped && !eof) {
      const pkt = dmxFrame(universe);
      w.write(pkt);
      flush();
      await sleep(framePeriod(fps, pkt.length));
    }
    // EOF can land between ticks — push and report the final state anyway.
    w.write(dmxFrame(universe));
    flush();
  });
}

async function cmdRaw() {
  if (!words.length) die("raw needs a label: raw 10   |   raw 3 00 00   |   raw 6 00 ff ff ff");
  const label = Number(words[0]);
  if (!(label >= 0 && label <= 255)) die(`bad label ${words[0]}`);
  const data = words.slice(1).join("").replace(/[^0-9a-fA-F]/g, "");
  if (data.length % 2) die("hex data must be whole bytes");
  const bytes = Buffer.from(data, "hex");
  await withWidget(async (w) => {
    const pkt = frame(label, bytes);
    console.log(`> ${hex(pkt)}`);
    const r = await w.ask(label, bytes, num("ms", 600));
    if (!r) console.log("< (no reply)");
    else if (r.raw) console.log(`< partial: ${hex(r.raw)}`);
    else console.log(`< label ${r.label} len ${r.data.length}: ${hex(r.data) || "(empty)"}`);
  });
}

async function withWidget(fn) {
  const port = pickPort();
  noteHolders(port);
  const w = await Widget.open(port);
  try { await fn(w); } finally { w.close(); }
}

function help() {
  console.log(fs.readFileSync(new URL(import.meta.url), "utf8")
    .split("\n").slice(1, 33).map((l) => l.replace(/^\/\/ ?/, "")).join("\n"));
}

const commands = {
  list: cmdList, ls: cmdList, info: cmdInfo, set: cmdSet, rgb: cmdRGB, fill: cmdFill,
  blackout: cmdBlackout, off: cmdBlackout, sweep: cmdSweep, cycle: cmdCycle, strobe: cmdStrobe,
  desk: cmdDesk,
  stdin: cmdStdin, raw: cmdRaw, help, "--help": help, "-h": help,
};

const run = commands[cmd];
if (!run) die(`unknown command "${cmd}" (try: ac-dmx-cli help)`);
Promise.resolve().then(run).then(() => process.exit(0), (e) => die(e?.message || String(e)));
