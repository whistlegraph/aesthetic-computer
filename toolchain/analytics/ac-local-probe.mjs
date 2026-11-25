#!/usr/bin/env node
// AC local probe: what shares this Mac's room. Everything answering on the LAN
// (printers, speakers, phones, other Macs) and the Bluetooth devices paired to
// this machine. One snapshot per call, for the wallpaper's constellation
// (blueberry-wallpaper/Sources/Fishery.swift), carried as the `local` field of
// the fishery feed beside `infra` (ac-infra-probe.mjs).
//
//   - one ping per address on the default interface's subnet (/22 or smaller):
//     who is here. macOS 27 hides the ARP table's hardware addresses, so a
//     reply is the whole of what we know about an unnamed device.
//   - one round of multicast DNS (Bonjour), asked from a throwaway port so
//     every answer comes back to us alone: names, and what each thing is
//     from the services it offers (_ipp → printer, _raop → speaker …).
//   - `system_profiler SPBluetoothDataType`: paired devices, connected or not.
//
// Names, addresses and first/last-seen times stay on this machine
// (~/.local/share/blueberry-wallpaper/local-seen.json) and are never written
// into this repo.
//
//   node toolchain/analytics/ac-local-probe.mjs   one snapshot to stdout
import { execFile } from "node:child_process";
import dgram from "node:dgram";
import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { homedir, hostname, networkInterfaces, platform } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const SEEN = process.env.LOCAL_SEEN || join(homedir(), ".local/share/blueberry-wallpaper/local-seen.json");
// A device gone longer than this drops off the wallpaper (and out of the file after a month).
const KEEP_S = 24 * 3600;
const FORGET_S = 30 * 86400;
// A device first seen within this is new, once the network itself is older than an hour.
const NEW_S = 15 * 60;

const run = (bin, args, timeout = 8000) => new Promise(resolve =>
  execFile(bin, args, { timeout, maxBuffer: 16 * 1024 * 1024 }, (error, stdout) => resolve(error ? null : stdout)));

const OS = platform();

// 🧭 The interface the default route leaves by, and its subnet: macOS, Linux
// (jastow) and Windows each say it their own way.
async function lan() {
  let iface, gateway;
  if (OS === "darwin") {
    const route = await run("/sbin/route", ["-n", "get", "default"], 3000);
    iface = route?.match(/interface:\s*(\S+)/)?.[1];
    gateway = route?.match(/gateway:\s*(\d+\.\d+\.\d+\.\d+)/)?.[1];
  } else if (OS === "linux") {
    const route = await run("ip", ["-4", "route", "show", "default"], 3000);
    [, gateway, iface] = route?.match(/default via (\S+) dev (\S+)/) || [];
  } else if (OS === "win32") {
    const out = await run("powershell", ["-NoProfile", "-Command",
      "$r = Get-NetRoute -DestinationPrefix 0.0.0.0/0 | Sort-Object RouteMetric | Select-Object -First 1; \"$($r.NextHop) $((Get-NetAdapter -InterfaceIndex $r.ifIndex).Name)\""], 10000);
    [gateway, iface] = (out || "").trim().split(/\s+(.+)/);
  }
  const addr = (networkInterfaces()[iface] || []).find(a => a.family === "IPv4" && !a.internal);
  if (!iface || !gateway || !addr) return null;
  const ssid = await wifiName(iface);
  const bits = addr.cidr ? Number(addr.cidr.split("/")[1]) : 24;
  return { iface, gateway, self: addr.address, bits, ssid };
}

// 📛 The Wi-Fi network's name. macOS redacts it to scripts without Location
// access (the wallpaper app reads it through CoreWLAN instead); Linux and
// Windows just say.
async function wifiName(iface) {
  if (OS === "darwin") {
    const out = await run("/usr/sbin/ipconfig", ["getsummary", iface], 3000);
    const ssid = out?.match(/^\s*SSID : (.+)$/m)?.[1]?.trim();
    return ssid && ssid !== "<redacted>" ? ssid : "";
  }
  if (OS === "linux") {
    const out = await run("nmcli", ["-t", "-f", "active,ssid", "dev", "wifi"], 5000);
    return out?.split("\n").find(l => l.startsWith("yes:"))?.slice(4).trim() || "";
  }
  if (OS === "win32") {
    const out = await run("netsh", ["wlan", "show", "interfaces"], 5000);
    return out?.match(/^\s*SSID\s*: (.+)$/m)?.[1]?.trim() || "";
  }
  return "";
}

const toInt = ip => ip.split(".").reduce((n, o) => (n << 8 | Number(o)) >>> 0, 0);
const toIp = n => [24, 16, 8, 0].map(s => (n >>> s) & 255).join(".");

// 🏓 One ping each, 256 at a time, half a second to answer (a whole one on Linux,
// whose -W counts seconds).
const PING = ip => OS === "darwin" ? ["/sbin/ping", ["-c1", "-W500", "-t1", ip]]
  : OS === "win32" ? ["ping", ["-n", "1", "-w", "500", ip]] : ["ping", ["-c1", "-W1", ip]];
async function sweep(net) {
  // Up to a /22 (1022 addresses, ~8 s) is swept whole: mesh routers hand those
  // out, and an Ubuntu box that advertises no Bonjour service is only found by
  // its ping and reverse name. Anything wider sweeps just this machine's /24.
  const bits = net.bits < 22 ? 24 : net.bits;
  const base = toInt(net.self) & (~0 << (32 - bits)) >>> 0;
  const ips = [];
  for (let i = 1; i < 2 ** (32 - bits) - 1; i++) ips.push(toIp(base + i));
  const alive = new Map();
  let next = 0;
  await Promise.all(Array.from({ length: Math.min(256, ips.length) }, async () => {
    while (next < ips.length) {
      const ip = ips[next++];
      const out = await run(...PING(ip), 2500);
      const ms = out?.match(/time[=<]([\d.]+)/)?.[1];
      if (ms) alive.set(ip, Math.round(Number(ms)));
    }
  }));
  return alive;
}

// 📡 Multicast DNS, just enough of RFC 1035/6762 to read answers.
const T = { A: 1, PTR: 12, TXT: 16, SRV: 33 };

function encodeName(name) {
  const parts = name.replace(/\.$/, "").split(".").map(l => Buffer.from(l, "utf8"));
  return Buffer.concat([...parts.flatMap(p => [Buffer.from([p.length]), p]), Buffer.from([0])]);
}

function query(questions) {
  const head = Buffer.alloc(12);
  head.writeUInt16BE(questions.length, 4);
  return Buffer.concat([head, ...questions.map(([name, type]) => {
    const tail = Buffer.alloc(4);
    tail.writeUInt16BE(type, 0);
    tail.writeUInt16BE(1, 2);
    return Buffer.concat([encodeName(name), tail]);
  })]);
}

function readName(buf, at) {
  const labels = [];
  let end = -1;
  for (let hops = 0; hops < 64 && at < buf.length; hops++) {
    const len = buf[at];
    if (len === 0) { at += 1; break; }
    if ((len & 0xc0) === 0xc0) {
      if (end < 0) end = at + 2;
      at = ((len & 0x3f) << 8) | buf[at + 1];
      continue;
    }
    labels.push(buf.toString("utf8", at + 1, at + 1 + len));
    at += 1 + len;
  }
  return { name: labels.join("."), next: end < 0 ? at : end };
}

function parse(buf) {
  const records = [];
  try {
    const qd = buf.readUInt16BE(4), count = buf.readUInt16BE(6) + buf.readUInt16BE(8) + buf.readUInt16BE(10);
    let at = 12;
    for (let i = 0; i < qd; i++) at = readName(buf, at).next + 4;
    for (let i = 0; i < count && at < buf.length; i++) {
      const { name, next } = readName(buf, at);
      const type = buf.readUInt16BE(next), len = buf.readUInt16BE(next + 8), data = next + 10;
      const r = { name: name.toLowerCase(), raw: name, type };
      if (type === T.A) r.value = [...buf.subarray(data, data + 4)].join(".");
      else if (type === T.PTR) r.value = readName(buf, data).name;
      else if (type === T.SRV) r.value = readName(buf, data + 6).name.toLowerCase();
      else if (type === T.TXT) {
        r.value = {};
        for (let p = data; p < data + len;) {
          const s = buf.toString("utf8", p + 1, p + 1 + buf[p]);
          p += 1 + buf[p];
          const eq = s.indexOf("=");
          if (eq > 0) r.value[s.slice(0, eq).toLowerCase()] = s.slice(eq + 1);
        }
      }
      if (r.value !== undefined) records.push(r);
      at = data + len;
    }
  } catch {}
  return records;
}

// Services that say what a thing is, most telling first.
const KINDS = [
  ["printer", ["_ipp._tcp", "_ipps._tcp", "_printer._tcp", "_pdl-datastream._tcp", "_uscan._tcp", "_scanner._tcp"]],
  ["tv", ["_googlecast._tcp", "_androidtvremote2._tcp", "_mediaremotetv._tcp"]],
  ["speaker", ["_sonos._tcp", "_spotify-connect._tcp", "_raop._tcp", "_airplay._tcp"]],
  ["home", ["_hap._tcp", "_hap._udp", "_matter._tcp", "_homekit._tcp", "_hue._tcp"]],
  ["ac", ["_acmenuband._tcp"]],
  ["computer", ["_ssh._tcp", "_sftp-ssh._tcp", "_rfb._tcp", "_smb._tcp", "_afpovertcp._tcp"]],
  ["phone", ["_companion-link._tcp", "_apple-mobdev2._tcp", "_remotepairing._tcp"]],
];
const BROWSE = [...new Set(KINDS.flatMap(([, s]) => s))];

function fromModel(model = "") {
  if (/iphone/i.test(model)) return "phone";
  if (/ipad/i.test(model)) return "tablet";
  // Apple TVs give a board code (J305AP), as Macs give a model (Mac16,8).
  if (/appletv|^j\d{3}a/i.test(model)) return "tv";
  if (/^v\d?[a-z]{0,2}\d{2}[a-z]?\d?-|vizio|bravia|roku/i.test(model)) return "tv";
  if (/audioaccessory|homepod/i.test(model)) return "speaker";
  if (/mac|imac/i.test(model)) return "computer";
  if (/watch/i.test(model)) return "watch";
  return "";
}

// A phone's companion-link record is often named by a UUID: no name at all.
// Pairing records lead with a hardware address ("1e:f1:…@fe80::…"): no name either.
const useful = n => Boolean(n) && !/^[0-9a-f]{8}-[0-9a-f]{4}-/i.test(n) && !/^([0-9a-f]{1,2}:){5}[0-9a-f]{1,2}@/i.test(n);

// 📺 UPnP/SSDP: how consoles, TVs, routers and printers that keep off
// Bonjour (an Xbox) say their name. One M-SEARCH to the multicast group, then
// each answer's description XML (on the LAN, by its LOCATION) for its
// friendlyName, manufacturer and model.
async function ssdp(net) {
  if (!net) return new Map();
  const sock = dgram.createSocket({ type: "udp4", reuseAddr: true });
  const where = new Map();
  sock.on("message", (m, rinfo) => {
    const loc = String(m).match(/^location:\s*(\S+)/im)?.[1];
    if (loc && !where.has(rinfo.address)) where.set(rinfo.address, new Set());
    if (loc) where.get(rinfo.address).add(loc);
  });
  sock.on("error", () => {});
  await new Promise(r => sock.bind(0, r));
  try { sock.setMulticastInterface(net.self); } catch {}
  const ask = Buffer.from("M-SEARCH * HTTP/1.1\r\nHOST: 239.255.255.250:1900\r\nMAN: \"ssdp:discover\"\r\nMX: 2\r\nST: ssdp:all\r\n\r\n");
  sock.send(ask, 1900, "239.255.255.250");
  setTimeout(() => sock.send(ask, 1900, "239.255.255.250"), 400);
  await new Promise(r => setTimeout(r, 2500));
  sock.close();
  const tag = (xml, t) => xml.match(new RegExp(`<${t}>([^<]*)</${t}>`, "i"))?.[1]?.trim() || "";
  const out = new Map();
  await Promise.all([...where].map(async ([ip, locs]) => {
    for (const loc of [...locs].slice(0, 4)) {
      try {
        const u = new URL(loc);
        if (u.hostname !== ip) continue;
        const xml = await (await fetch(loc, { signal: AbortSignal.timeout(2500) })).text();
        const name = tag(xml, "friendlyName");
        if (!name) continue;
        out.set(ip, { name, maker: tag(xml, "manufacturer"), model: [tag(xml, "modelName"), tag(xml, "modelNumber")].filter(Boolean).join(" "),
          type: tag(xml, "deviceType") });
        return;
      } catch {}
    }
  }));
  return out;
}

// What the wallpaper prints: "Hirad’s MacBook Pro (2)" → "hirad mbp 2".
const MODELS = [[/macbook pro/i, "mbp"], [/macbook air/i, "mba"], [/macbook/i, "mb"], [/mac mini/i, "mini"],
  [/mac studio/i, "studio"], [/mac pro/i, "macpro"], [/imac/i, "imac"], [/iphone/i, "phone"], [/ipad/i, "ipad"],
  [/apple watch/i, "watch"], [/apple tv/i, "tv"], [/homepod/i, "homepod"]];
function short(name) {
  const m = name.match(/^(.+?)[’'`]s\s+(.+?)(?:\s*\((\d+)\))?$/);
  if (!m) return name;
  const what = MODELS.find(([re]) => re.test(m[2]))?.[1] ?? m[2].toLowerCase();
  return [m[1].toLowerCase(), what, m[3]].filter(Boolean).join(" ");
}

async function bonjour(net, alive) {
  if (!net) return [];
  const sock = dgram.createSocket({ type: "udp4", reuseAddr: true });
  const records = [];
  sock.on("message", m => records.push(...parse(m)));
  sock.on("error", () => {});
  await new Promise(r => sock.bind(0, r));
  try { sock.setMulticastInterface(net.self); } catch {}
  // Multicast drops packets: every question goes out twice, a quarter second apart.
  const ask = qs => {
    const send = () => { for (let i = 0; i < qs.length; i += 12) sock.send(query(qs.slice(i, i + 12)), 5353, "224.0.0.251"); };
    send();
    setTimeout(send, 250);
  };
  const wait = ms => new Promise(r => setTimeout(r, ms));
  const ofType = (type, pred = () => true) => records.filter(r => r.type === type && pred(r));

  // Round 1: every service type anyone offers, plus each live address's own name.
  ask([["_services._dns-sd._udp.local", T.PTR], ...BROWSE.map(s => [s + ".local", T.PTR]),
    ...[...alive.keys()].map(ip => [ip.split(".").reverse().join(".") + ".in-addr.arpa", T.PTR])]);
  await wait(900);
  // Round 2: the instances of each type of interest that turned up.
  const types = new Set(ofType(T.PTR, r => r.name === "_services._dns-sd._udp.local").map(r => r.value.toLowerCase().replace(/\.local$/, "")));
  ask([...types].filter(t => BROWSE.includes(t)).map(t => [t + ".local", T.PTR]));
  await wait(700);
  // Round 3: where each instance lives, and what each host says it is.
  const instances = ofType(T.PTR, r => BROWSE.some(s => r.name === s + ".local")).map(r => r.value);
  const hostsSoFar = new Set([...ofType(T.SRV).map(r => r.value), ...ofType(T.PTR, r => r.name.endsWith(".in-addr.arpa")).map(r => r.value.toLowerCase())]);
  ask([...instances.flatMap(i => [[i, T.SRV], [i, T.TXT]]),
    ...[...hostsSoFar].map(h => [h, T.A]),
    ...[...hostsSoFar].map(h => [h.replace(/\.local\.?$/, "") + "._device-info._tcp.local", T.TXT])]);
  await wait(900);
  // Round 4: addresses and models for hosts only round 3 revealed.
  const hosts = new Set([...ofType(T.SRV).map(r => r.value), ...hostsSoFar]);
  ask([...hosts].filter(h => !ofType(T.A, r => r.name === h).length).flatMap(h =>
    [[h, T.A], [h.replace(/\.local\.?$/, "") + "._device-info._tcp.local", T.TXT]]));
  await wait(600);
  sock.close();

  // Fold everything into one entry per host.
  const byHost = new Map();
  const entry = host => {
    host = host.toLowerCase().replace(/\.$/, "");
    if (!byHost.has(host)) byHost.set(host, { host, ips: new Set(), services: new Set(), names: {}, model: "" });
    return byHost.get(host);
  };
  for (const r of ofType(T.PTR, r => r.name.endsWith(".in-addr.arpa"))) {
    entry(r.value).ips.add(r.name.replace(/\.in-addr\.arpa$/, "").split(".").reverse().join("."));
  }
  for (const r of ofType(T.A)) if (byHost.has(r.name.replace(/\.$/, "")) || hosts.has(r.name)) entry(r.name).ips.add(r.value);
  for (const r of ofType(T.SRV)) {
    const service = BROWSE.find(s => r.name.endsWith("." + s + ".local"));
    if (!service) continue;
    const e = entry(r.value);
    e.services.add(service);
    // "AC1234@Kitchen" (AirPlay) and "RINCON_…@Office" (Sonos) mean "Kitchen" and "Office".
    e.names[service] ||= r.raw.slice(0, r.raw.length - service.length - 7).replace(/^[0-9A-Za-z_]{6,}@/, "");
  }
  for (const r of ofType(T.TXT, r => r.name.endsWith("._device-info._tcp.local"))) {
    const e = byHost.get(r.name.replace(/\._device-info\._tcp\.local$/, "") + ".local");
    if (e && r.value.model) e.model = r.value.model;
  }
  for (const r of ofType(T.TXT)) {
    const e = [...byHost.values()].find(e => [...e.services].some(s => r.name.endsWith("." + s + ".local")) && r.name.startsWith(String(Object.values(e.names)[0] || "\0").toLowerCase()));
    if (e && !e.model && (r.value.model || r.value.am || r.value.md)) e.model = r.value.model || r.value.am || r.value.md;
    if (e && !e.printer && (r.value.usb_mfg || r.value.ty || r.value.product)) e.printer = [r.value.usb_mfg, r.value.ty, r.value.product].filter(Boolean).join(" ");
  }

  // One machine can answer under two host names (an app's own record beside
  // the Mac's): the entries sharing an address become one, keeping the better-named.
  const entries = [...byHost.values()].filter(e => e.ips.size || e.services.size);
  for (const e of [...entries]) {
    const twin = entries.find(o => o !== e && [...o.ips].some(ip => e.ips.has(ip)));
    if (!twin) continue;
    const [keep, drop] = (twin.model || Object.keys(twin.names).length > Object.keys(e.names).length) ? [twin, e] : [e, twin];
    drop.services.forEach(s => keep.services.add(s));
    drop.ips.forEach(ip => keep.ips.add(ip));
    for (const [s, n] of Object.entries(drop.names)) keep.names[s] ||= n;
    keep.model ||= drop.model;
    entries.splice(entries.indexOf(drop), 1);
  }

  return entries.map(e => {
    const kind = KINDS.find(([, list]) => list.some(s => e.services.has(s)))?.[0];
    const modelKind = fromModel(e.model) || fromModel(e.host);
    // A Mac offers ssh, AirPlay and companion-link at once: its model settles it.
    const final = modelKind && (kind === "computer" || kind === "phone" || kind === "speaker" || !kind) ? modelKind : kind || "device";
    const preferred = ["_ipp._tcp", "_ipps._tcp", "_printer._tcp", "_googlecast._tcp", "_sonos._tcp", "_airplay._tcp",
      "_raop._tcp", "_companion-link._tcp", "_hap._tcp", "_rfb._tcp", "_smb._tcp"].map(s => e.names[s]).find(useful);
    const host = e.host.replace(/\.local$/, "");
    const name = (preferred || Object.values(e.names).find(useful) || (useful(host) ? host : final)).trim();
    const services = [...e.services].map(s => s.split(".")[0].slice(1));
    return { name, short: short(name), icon: vendor({ name, host: e.host, model: e.model, services, printer: e.printer }), host: e.host, ips: [...e.ips], kind: final, model: e.model, services: [...e.services].map(s => s.split(".")[0].slice(1)) };
  });
}

// 🎧 Paired Bluetooth devices.
const BT_KIND = { headphones: "headphones", headset: "headphones", speaker: "speaker", keyboard: "keyboard", mouse: "mouse",
  trackpad: "mouse", gamepad: "gamepad", joystick: "gamepad", phone: "phone", watch: "watch", computer: "computer" };

async function bluetooth() {
  if (OS === "linux") return bluetoothLinux();
  if (OS !== "darwin") return null;
  const out = await run("/usr/sbin/system_profiler", ["SPBluetoothDataType", "-json"], 15000);
  if (!out) return null;
  try {
    const d = JSON.parse(out).SPBluetoothDataType?.[0] || {};
    const list = (key, connected) => (d[key] || []).flatMap(o => Object.entries(o)).map(([name, v]) => {
      const minor = String(v.device_minorType || "").toLowerCase();
      const battery = Number(String(v.device_batteryLevelMain || v.device_batteryLevel || "").replace("%", "")) || null;
      const kind = Object.entries(BT_KIND).find(([k]) => minor.includes(k))?.[1] || "device";
      return { name, kind, connected, battery, icon: vendor({ name, vendorId: v.device_vendorID }) };
    });
    const on = d.controller_properties?.controller_state;
    return { power: on ? /on/i.test(on) : true, devices: [...list("device_connected", true), ...list("device_not_connected", false)] };
  } catch { return null; }
}

// bluetoothctl: paired devices, and which of them are connected.
async function bluetoothLinux() {
  const parse = out => (out || "").split("\n").map(l => l.match(/^Device ([0-9A-F:]{17}) (.+)$/i)).filter(Boolean).map(m => m[2].trim());
  const [paired, connected, show] = await Promise.all([run("bluetoothctl", ["devices", "Paired"], 5000),
    run("bluetoothctl", ["devices", "Connected"], 5000), run("bluetoothctl", ["show"], 5000)]);
  if (paired === null && show === null) return null;
  const on = new Set(parse(connected));
  return { power: !/Powered: no/.test(show || ""), devices: parse(paired).map(name => ({ name,
    kind: Object.entries(BT_KIND).find(([k]) => name.toLowerCase().includes(k))?.[1] || "device",
    connected: on.has(name), battery: null, icon: vendor({ name }) })) };
}

// 🏷 Whose a thing is, as a domain whose favicon the wallpaper badges it
// with: from its model, the services it offers, its name, or a Bluetooth
// vendor id. Empty when nothing says.
const BT_VENDORS = { "0x004c": "apple.com", "0x05ac": "apple.com", "0x045e": "microsoft.com", "0x2dc8": "8bitdo.com",
  "0x054c": "sony.com", "0x057e": "nintendo.com", "0x046d": "logitech.com", "0x18d1": "google.com", "0x04e8": "samsung.com",
  "0x1532": "razer.com", "0x2717": "mi.com", "0x0b05": "asus.com", "0x17ef": "lenovo.com", "0x03f0": "hp.com", "0x1915": "nordicsemi.com" };
const NAMED = [
  [/sonos|rincon/i, "sonos.com"], [/dream-?machine|unifi|ubnt|\budm\b|\busg\b/i, "ui.com"], [/eero/i, "eero.com"],
  [/netgear|orbi/i, "netgear.com"], [/tp-?link|deco|archer/i, "tp-link.com"], [/airpods|iphone|ipad|macbook|imac|mac ?mini|mac ?studio|apple ?tv|homepod|apple watch|magic (keyboard|mouse|trackpad)|\bneo\b/i, "apple.com"],
  [/8bitdo/i, "8bitdo.com"], [/xbox/i, "xbox.com"], [/microsoft/i, "microsoft.com"], [/dualsense|dualshock|playstation|\bps[45]\b/i, "playstation.com"],
  [/nintendo|joy-?con|switch pro/i, "nintendo.com"], [/galaxy|samsung/i, "samsung.com"], [/pixel|chromecast|nest|google/i, "google.com"],
  [/android/i, "android.com"], [/roku/i, "roku.com"], [/vizio|^v\d?[a-z]{0,2}\d{2}[a-z]?\d?-/i, "vizio.com"], [/echo|kindle|fire ?tv|amazon/i, "amazon.com"], [/\bring\b/i, "ring.com"],
  [/hp[ -]|officejet|laserjet|deskjet|envy/i, "hp.com"], [/brother/i, "brother.com"], [/canon/i, "canon.com"], [/epson/i, "epson.com"],
  [/bose/i, "bose.com"], [/beats/i, "beatsbydre.com"], [/jbl/i, "jbl.com"], [/sony|wh-1000|wf-1000/i, "sony.com"],
  [/logi|mx (master|keys|anywhere)/i, "logitech.com"], [/surface|desktop-|windows|\bmsft\b/i, "microsoft.com"],
  [/raspberry/i, "raspberrypi.com"], [/hue|philips/i, "philips-hue.com"], [/kindle/i, "amazon.com"],
];
const SERVICE_VENDOR = { sonos: "sonos.com", googlecast: "google.com", "androidtvremote2": "android.com", hue: "philips-hue.com",
  acmenuband: "aesthetic.computer", "companion-link": "apple.com", "apple-mobdev2": "apple.com", remotepairing: "apple.com",
  mediaremotetv: "apple.com", "spotify-connect": "spotify.com" };
function vendor({ name = "", host = "", model = "", services = [], vendorId = "", printer = "" } = {}) {
  if (/^(mac|iphone|ipad|appletv|audioaccessory|watch|j\d{3}a)/i.test(model)) return "apple.com";
  // The name before the vendor id: an 8BitDo pad in Xbox mode reports Microsoft's.
  for (const text of [printer, name, host, model]) {
    const hit = text && NAMED.find(([re]) => re.test(text));
    if (hit) return hit[1];
  }
  const id = String(vendorId).toLowerCase();
  if (BT_VENDORS[id]) return BT_VENDORS[id];
  for (const s of services) if (SERVICE_VENDOR[s]) return SERVICE_VENDOR[s];
  return "";
}

function readSeen() {
  try { return JSON.parse(readFileSync(SEEN, "utf8")); } catch { return { networks: {} }; }
}
function writeSeen(seen) {
  try {
    mkdirSync(dirname(SEEN), { recursive: true });
    const tmp = `${SEEN}.${process.pid}.tmp`;
    writeFileSync(tmp, JSON.stringify(seen));
    renameSync(tmp, SEEN);
  } catch {}
}

/** One snapshot. `state` carries the Bluetooth reading between calls (it is slow, so read once a minute). */
export async function probeLocal(state = {}) {
  const now = Math.round(Date.now() / 1000);
  const net = await lan();
  const btDue = !state.btAt || now - state.btAt >= 60;
  const [alive, bt] = await Promise.all([net ? sweep(net) : new Map(), btDue ? bluetooth() : state.bt]);
  if (btDue) { state.bt = bt; state.btAt = now; }
  const [found, upnp] = await Promise.all([bonjour(net, alive).catch(() => []), ssdp(net).catch(() => new Map())]);

  const self = hostname().toLowerCase().replace(/\.local$/, "");
  const seen = readSeen();
  const netKey = net ? `${net.iface}:${net.gateway}/${net.bits}` : "";
  const book = net ? (seen.networks[netKey] ||= { first: now, devices: {} }) : { first: now, devices: {} };

  const devices = [];
  const named = new Set();
  for (const f of found) {
    if (f.host.replace(/\.local$/, "") === self || f.ips.includes(net?.self)) continue;
    const ip = f.ips.find(i => alive.has(i)) || f.ips[0] || "";
    f.ips.forEach(i => named.add(i));
    const online = alive.has(ip) || f.services.length > 0;
    // The router is one star whatever it answers to this time.
    devices.push({ key: ip && ip === net?.gateway ? "gw" : "n:" + f.host, name: f.name, short: f.short, icon: f.icon, kind: f.kind, model: f.model, ip, ms: alive.get(ip) ?? null, online, services: f.services });
  }
  if (net && alive.has(net.gateway) && !named.has(net.gateway)) {
    devices.push({ key: "gw", name: "router", short: "router", kind: "router", ip: net.gateway, ms: alive.get(net.gateway), online: true, services: [] });
    named.add(net.gateway);
  }
  // An ISP's own gateway names its network after the ISP ("ATT2AWTpcr").
  const isp = [[/^att/i, "att.com"], [/xfinity|comcast/i, "xfinity.com"], [/spectrum/i, "spectrum.com"], [/verizon|fios/i, "verizon.com"],
    [/eero/i, "eero.com"], [/starlink/i, "starlink.com"], [/t-?mobile/i, "t-mobile.com"]].find(([re]) => re.test(net?.ssid || ""))?.[1] || "";
  for (const d of devices) if (d.ip === net?.gateway) Object.assign(d, { kind: "router", short: "router", icon: d.icon || isp });
  // What answered without a name keeps its address, so it can be remembered.
  // What UPnP named that Bonjour did not (an Xbox, a TV, the router's own name).
  for (const [ip, u] of upnp) {
    if (named.has(ip) || ip === net?.self) continue;
    const text = `${u.name} ${u.maker} ${u.model} ${u.type}`;
    const kind = ip === net?.gateway ? "router" : /xbox|playstation|nintendo/i.test(text) ? "console"
      : /mediarenderer|tv|roku|bravia|vizio|chromecast/i.test(text) ? "tv" : /printer/i.test(text) ? "printer"
      : /internetgatewaydevice|router|gateway/i.test(text) ? "router" : /zoneplayer|sonos|speaker/i.test(text) ? "speaker" : "device";
    devices.push({ key: "u:" + u.name.toLowerCase(), name: u.name, short: u.name, kind, model: u.model, icon: vendor({ name: text }),
      ip, ms: alive.get(ip) ?? null, online: alive.has(ip), services: ["upnp"] });
    named.add(ip);
  }
  // A named device that missed this round's Bonjour but answers at the
  // address it last had is still that device.
  for (const [ip, ms] of alive) {
    if (named.has(ip) || ip === net?.self) continue;
    const known = Object.entries(book.devices).find(([key, b]) => b.ip === ip && b.kind !== "unknown" && !devices.some(d => d.key === key));
    if (known) {
      const [key, b] = known;
      devices.push({ key, name: b.name, short: b.short || b.name, kind: b.kind, model: b.model || "", icon: b.icon || "", ip, ms, online: true, services: [] });
      continue;
    }
    devices.push({ key: "ip:" + ip, name: "." + ip.split(".")[3], short: "." + ip.split(".")[3], kind: "unknown", ip, ms, online: true, services: [] });
  }

  for (const d of devices) {
    const b = (book.devices[d.key] ||= { first: now, name: d.name, short: d.short, kind: d.kind });
    if (d.online) b.last = now;
    if (d.kind !== "unknown" && !(d.key === "gw" && d.name === "router" && b.name !== "router")) Object.assign(b, { name: d.name, short: d.short, kind: d.kind, model: d.model || b.model || "", icon: d.icon || b.icon || "" });
    d.icon ||= b.icon || "";
    if (d.ip) b.ip = d.ip;
    d.firstSeen = b.first;
    d.lastSeen = b.last || 0;
    d.new = now - book.first > 3600 && now - b.first < NEW_S;
  }
  // Remembered devices that did not answer this time, within a day.
  for (const [key, b] of Object.entries(book.devices)) {
    if (devices.some(d => d.key === key)) continue;
    if (now - (b.last || b.first) > FORGET_S) { delete book.devices[key]; continue; }
    if (now - (b.last || 0) > KEEP_S || b.kind === "unknown" || b.kind === "router" || !useful(b.name)) continue;
    devices.push({ key, name: b.name, short: b.short || b.name, kind: b.kind, icon: b.icon || "", ip: "", ms: null, online: false, services: [], firstSeen: b.first, lastSeen: b.last || 0, new: false });
  }
  if (net) writeSeen(seen);

  const order = { router: 0, printer: 1, tv: 2, console: 2, speaker: 3, home: 4, ac: 5, computer: 6, phone: 7, tablet: 8, watch: 9, device: 10, unknown: 11 };
  devices.sort((a, b) => b.online - a.online || b.new - a.new || (order[a.kind] ?? 10) - (order[b.kind] ?? 10) || a.name.localeCompare(b.name));
  return {
    observedAt: Date.now() / 1000,
    machine: hostname().replace(/\.local$/, "").toLowerCase(), os: OS,
    network: net ? { iface: net.iface, self: net.self, gateway: net.gateway, bits: net.bits, ssid: net.ssid, answered: alive.size } : null,
    devices,
    bluetooth: state.bt || null,
  };
}

// Run as a file, or piped over ssh to a machine that has no checkout:
//   ssh jastow node --input-type=module - --print < ac-local-probe.mjs
// --watch is for a Mac whose wallpaper feeder does not carry `local` (the
// Fuser minis): every 30 s it writes { local, sites } to room.json beside the
// feed, and the wallpaper reads that instead.
const main = process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1];
if (main && process.argv.includes("--watch")) {
  const out = join(homedir(), ".local/share/blueberry-wallpaper/room.json");
  const state = {}, room = { sites: [] };
  const { watchSites } = await import("./ac-sites-probe.mjs").catch(() => ({}));
  if (watchSites) void watchSites(room);
  for (;;) {
    const started = Date.now();
    try {
      const local = await probeLocal(state);
      mkdirSync(dirname(out), { recursive: true });
      writeFileSync(`${out}.tmp`, JSON.stringify({ local, sites: room.sites || [] }));
      renameSync(`${out}.tmp`, out);
    } catch (error) { console.error(new Date().toISOString(), String(error?.message || error).slice(0, 200)); }
    await new Promise(r => setTimeout(r, Math.max(5000, 30000 - (Date.now() - started))));
  }
} else if (process.argv.includes("--print") || main) {
  console.log(JSON.stringify(await probeLocal(), null, 2));
}
