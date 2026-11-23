#!/usr/bin/env node
// fetch-rockkit.mjs — a real miked acoustic rock kit, one-shots, into
// assets/rockkit/. A fixed list (no search, no credentials), so a re-run gets
// the same hits and skips any drum whose files are all already there.
// Writes assets/rockkit/SOURCES.json.
//
// Two sources, both Creative Commons Attribution 4.0 (commercial use OK,
// credit required, changes must be indicated):
//   DRSKit 2.1     — DrumGizmo (drumgizmo.org). A handcrafted DRSDrums kit
//                    (Jes Eiler) with Paiste Giant Beat / Formula 602 cymbals,
//                    13 mics: close, two overheads, two room mics "further back
//                    in the room". Everything except the rimshot.
//   CrocellKit 1.1 — DrumGizmo, sampled at JBOSound (Jacob Olsen's mics).
//                    Only the snare rimshot: DRSKit has none (its Snare_rim is
//                    a side stick, kept as rock-snare-sidestick).
// The kits are 2.8 GB and 5.5 GB zips. Nothing big is downloaded: the zip's
// central directory is read with HTTP range requests and only the chosen
// multichannel WAVs (one per hit, 13 or 15 mics as channels) are fetched.
//
// Each hit becomes rock-<drum>-...wav: a stereo mix (close + overheads + room,
// room pushed up) and, for the main hits, a -room.wav of the room pair alone,
// cut at the same sample so the two can be layered in phase. Leading silence
// is trimmed to 3 ms before the attack, the tail ends 66 dB under the peak with
// a 10 ms fade, and each drum family shares one gain so its loudest mix peaks
// at -1 dBFS and the softer layers keep their real level below it.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import zlib from "node:zlib";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../assets/rockkit");
const UA = "sound-fetch/1.0";
const MAX_BYTES = 120e6;       // everything kept
const MAX_MEMBER = 40e6;       // one multichannel hit
const LICENCE = "CC BY 4.0";
const LICENCE_URL = "https://creativecommons.org/licenses/by/4.0/";

const KITS = {
  drs: {
    zip: "https://drumgizmo.org/kits/DRSKit/DRSKit2_1.zip",
    root: "DRSKit",
    page: "https://drumgizmo.org/wiki/doku.php?id=kits:drskit",
    author: "DrumGizmo team (sampled by Deva & Lars Muldjord; kit by Jes Eiler, DRSDrums)",
    attribution: "Drum samples: DRSKit by the DrumGizmo team (drumgizmo.org), CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); mixed to stereo and trimmed.",
    room: ["AmbL", "AmbR"], oh: ["OHL", "OHR"],
  },
  crocell: {
    zip: "https://drumgizmo.org/kits/CrocellKit/CrocellKit1_1.zip",
    root: "CrocellKit",
    page: "https://drumgizmo.org/wiki/doku.php?id=kits:crocellkit",
    author: "DrumGizmo team (sampled at JBOSound with Jacob Olsen's mic setup)",
    attribution: "Drum samples: CrocellKit by the DrumGizmo team (drumgizmo.org), CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); mixed to stereo and trimmed.",
    room: ["AmbLeft", "AmbRight"], oh: ["OHLeft", "OHRight", "OHCenter"],
  },
};
const LICENCE_NOTE = "DrumGizmo kit page: \"When the samples are used in a composition, any context in which other credits (e.g. to performers) are presented must include an attribution.\"";

// Mix: dB gain and pan (-1 left .. +1 right, drummer's view) per mic.
// Room up for the classic-rock sound; close mics give the attack.
const MIC = {
  AmbL: [9, -1], AmbR: [9, 1], AmbLeft: [6, -1], AmbRight: [6, 1],
  OHL: [0, -0.7], OHR: [0, 0.7], OHLeft: [0, -0.7], OHRight: [0, 0.7], OHCenter: [-3, 0],
  Kdrum_back: [0, 0], Kdrum_front: [-3, 0],
  Snare_top: [0, 0], Snare_bottom: [-8, 0], SnareTop: [0, 0], SnareBottom: [-8, 0],
  Tom1: [0, -0.15], Tom2: [0, 0.25], Tom3: [0, 0.4],
  Hihat: [-6, -0.4], Ride: [-6, 0.4],
};

// A drum family: one instrument, its close mics, and the hits to keep.
// `hits` are [layer, sample number counted from the loudest (1 = loudest)].
const FAMILIES = [
  { name: "kick", kit: "drs", instr: "Kdrum_without_contact", close: ["Kdrum_back", "Kdrum_front"], room: true,
    desc: "Kick, beater off the head; DRSDrums kick, AKG D112 back + SM57 beater side",
    hits: [["v2", 1], ["v2", 2], ["v2", 3], ["v1", 12], ["v1", 13], ["v1", 14]], file: (l, t) => `rock-kick-${l}-${t}` },
  { name: "snare", kit: "drs", instr: "Snare", close: ["Snare_top", "Snare_bottom"], room: true,
    desc: "Snare, centre hit; SM57 top + bottom (bottom polarity matched to top)",
    hits: [["v2", 1], ["v2", 2], ["v2", 3], ["v2", 4], ["v1", 11], ["v1", 12], ["v1", 13], ["ghost", 24], ["ghost", 25], ["ghost", 26]],
    file: (l, t) => (l === "ghost" ? `rock-snare-ghost-${t}` : `rock-snare-${l}-${t}`) },
  { name: "snare-rim", kit: "crocell", instr: "SnareRimShot", close: ["SnareTop", "SnareBottom"], room: true,
    desc: "Snare rimshot (stick on head and rim at once), CrocellKit; Beta 56A top + MK-012 bottom",
    hits: [["v2", 1], ["v2", 2], ["v2", 3], ["v1", 6], ["v1", 7]], file: (l, t) => `rock-snare-rim-${l}-${t}` },
  { name: "snare-sidestick", kit: "drs", instr: "Snare_rim", close: ["Snare_top", "Snare_bottom"], room: false,
    desc: "Snare side stick (cross-stick rim click), DRSKit 'Snare_rim'",
    hits: [["v2", 1], ["v2", 2]], file: (l, t) => `rock-snare-sidestick-${t}` },
  { name: "tom-high", kit: "drs", instr: "Tom1", close: ["Tom1"], room: true,
    desc: "Rack tom (DRSKit Tom1, the one hanging tom); SM57",
    hits: [["v2", 1], ["v2", 2], ["v1", 9], ["v1", 10]], file: (l, t) => `rock-tom-high-${l}-${t}` },
  { name: "tom-mid", kit: "drs", instr: "Tom2", close: ["Tom2"], room: true,
    desc: "First floor tom (DRSKit Tom2); SM57",
    hits: [["v2", 1], ["v2", 2], ["v1", 10], ["v1", 11]], file: (l, t) => `rock-tom-mid-${l}-${t}` },
  { name: "tom-floor", kit: "drs", instr: "Tom3", close: ["Tom3"], room: true,
    desc: "Second, lowest floor tom (DRSKit Tom3); SM57",
    hits: [["v2", 1], ["v2", 2], ["v1", 10], ["v1", 11]], file: (l, t) => `rock-tom-floor-${l}-${t}` },
  { name: "crash", kit: "drs", instr: "Crash_left_shank", close: [], room: true,
    desc: "Crash, Paiste Giant Beat (left), hit with the shank of the stick",
    hits: [["v2", 1], ["v2", 2]], file: (l, t) => `rock-crash-${t}` },
  { name: "crash-b", kit: "drs", instr: "Crash_right_shank", close: [], room: true,
    desc: "Crash, Paiste Formula 602 (right), hit with the shank of the stick",
    hits: [["v2", 1], ["v2", 2]], file: (l, t) => `rock-crash-b-${t}` },
  { name: "ride-bow", kit: "drs", instr: "Ride_tip", close: ["Ride"], room: false,
    desc: "Ride bow, stick tip; Paiste Formula 602 thin crash played as a ride",
    hits: [["v2", 1], ["v2", 2], ["v1", 5], ["v1", 6]], file: (l, t) => `rock-ride-bow-${l}-${t}` },
  { name: "ride-bell", kit: "drs", instr: "Ride_shank_bell", close: ["Ride"], room: false,
    desc: "Ride bell, shank of the stick",
    hits: [["v2", 1], ["v2", 2]], file: (l, t) => `rock-ride-bell-${t}` },
  { name: "hat-closed", kit: "drs", instr: "Hihat_closed_shank", close: ["Hihat"], room: false,
    desc: "Hi-hat closed, shank of the stick on the edge; Paiste Formula 602 Medium",
    hits: [["v2", 1], ["v2", 2], ["v1", 6], ["v1", 7]], file: (l, t) => `rock-hat-closed-${l}-${t}` },
  { name: "hat-closed-tip", kit: "drs", instr: "Hihat_closed", close: ["Hihat"], room: false,
    desc: "Hi-hat closed, stick tip on the top (tighter tick)",
    hits: [["v2", 1], ["v2", 2]], file: (l, t) => `rock-hat-closed-tip-${t}` },
  { name: "hat-half", kit: "drs", instr: "Hihat_semi_open", close: ["Hihat"], room: false,
    desc: "Hi-hat half open",
    hits: [["v2", 1], ["v2", 2], ["v1", 7]], file: (l, t) => `rock-hat-half-${l}-${t}` },
  { name: "hat-open", kit: "drs", instr: "Hihat_open", close: ["Hihat"], room: false,
    desc: "Hi-hat open, hit with the shank",
    hits: [["v2", 1], ["v2", 2]], file: (l, t) => `rock-hat-open-${t}` },
  { name: "hat-pedal", kit: "drs", instr: "Hihat_foot", close: ["Hihat"], room: false,
    desc: "Hi-hat foot chick (pedal)",
    hits: [["v2", 1], ["v2", 2]], file: (l, t) => `rock-hat-pedal-${t}` },
];

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const dB = (x) => (x > 0 ? 20 * Math.log10(x) : -Infinity);

// ---- remote zip ---------------------------------------------------------
async function range(url, a, b) {
  let err;
  for (let n = 0; n < 5; n++) {
    try {
      const res = await fetch(url, { headers: { "User-Agent": UA, Range: `bytes=${a}-${b}` } });
      if (res.status !== 206) throw new Error(`http ${res.status}`);
      const buf = Buffer.from(await res.arrayBuffer());
      if (buf.length !== b - a + 1) throw new Error(`short read ${buf.length}/${b - a + 1}`);
      return buf;
    } catch (e) { err = e; await sleep(1500 * (n + 1)); }
  }
  throw err;
}
const dirs = {};
async function zipDir(url) {
  if (dirs[url]) return dirs[url];
  const head = await fetch(url, { method: "HEAD", headers: { "User-Agent": UA } });
  const size = +head.headers.get("content-length");
  const tail = await range(url, size - 65558, size - 1);
  const e = tail.lastIndexOf(Buffer.from([0x50, 0x4b, 0x05, 0x06]));
  let n = tail.readUInt16LE(e + 10), cdSize = tail.readUInt32LE(e + 12), cdOff = tail.readUInt32LE(e + 16);
  if (n === 0xffff || cdSize === 0xffffffff || cdOff === 0xffffffff) {   // zip64
    const L = tail.lastIndexOf(Buffer.from([0x50, 0x4b, 0x06, 0x07]), e);
    const zo = Number(tail.readBigUInt64LE(L + 8));
    const z = await range(url, zo, zo + 55);
    n = Number(z.readBigUInt64LE(32)); cdSize = Number(z.readBigUInt64LE(40)); cdOff = Number(z.readBigUInt64LE(48));
  }
  const cd = await range(url, cdOff, cdOff + cdSize - 1);
  const ents = {};
  for (let i = 0, p = 0; i < n; i++) {
    const method = cd.readUInt16LE(p + 10);
    let csize = cd.readUInt32LE(p + 20), usize = cd.readUInt32LE(p + 24), off = cd.readUInt32LE(p + 42);
    const fl = cd.readUInt16LE(p + 28), el = cd.readUInt16LE(p + 30), cl = cd.readUInt16LE(p + 32);
    const name = cd.toString("utf8", p + 46, p + 46 + fl);
    for (let q = p + 46 + fl; q < p + 46 + fl + el;) {
      const id = cd.readUInt16LE(q), sz = cd.readUInt16LE(q + 2);
      if (id === 1) {
        let r = q + 4;
        if (usize === 0xffffffff) { usize = Number(cd.readBigUInt64LE(r)); r += 8; }
        if (csize === 0xffffffff) { csize = Number(cd.readBigUInt64LE(r)); r += 8; }
        if (off === 0xffffffff) off = Number(cd.readBigUInt64LE(r));
      }
      q += 4 + sz;
    }
    ents[name] = { method, csize, usize, off };
    p += 46 + fl + el + cl;
  }
  return (dirs[url] = ents);
}
async function member(url, name) {
  const e = (await zipDir(url))[name];
  if (!e) throw new Error(`${name} not in archive`);
  if (e.usize > MAX_MEMBER) throw new Error(`${name} is ${e.usize} bytes, over the cap`);
  const lh = await range(url, e.off, e.off + 29);
  const start = e.off + 30 + lh.readUInt16LE(26) + lh.readUInt16LE(28);
  const data = await range(url, start, start + e.csize - 1);
  const out = e.method === 8 ? zlib.inflateRawSync(data) : e.method === 0 ? data : null;
  if (!out || out.length !== e.usize) throw new Error(`${name}: cannot unpack (method ${e.method})`);
  return out;
}

// ---- WAV ----------------------------------------------------------------
function readWav(buf) {
  let p = 12, fmt, data;
  while (p + 8 <= buf.length) {
    const id = buf.toString("ascii", p, p + 4), sz = buf.readUInt32LE(p + 4);
    if (id === "fmt ") fmt = { tag: buf.readUInt16LE(p + 8), ch: buf.readUInt16LE(p + 10), sr: buf.readUInt32LE(p + 12), bits: buf.readUInt16LE(p + 22) };
    if (id === "data") { data = buf.subarray(p + 8, p + 8 + Math.min(sz, buf.length - p - 8)); break; }
    p += 8 + sz + (sz & 1);
  }
  const bps = fmt.bits / 8, frames = Math.floor(data.length / bps / fmt.ch);
  const ch = Array.from({ length: fmt.ch }, () => new Float32Array(frames));
  for (let i = 0; i < frames; i++) for (let c = 0; c < fmt.ch; c++) {
    const o = (i * fmt.ch + c) * bps;
    ch[c][i] = fmt.bits === 32 && fmt.tag !== 1 ? data.readFloatLE(o)
      : fmt.bits === 24 ? data.readIntLE(o, 3) / 8388608 : data.readInt16LE(o) / 32768;
  }
  return { sr: fmt.sr, ch };
}
function wav24(L, R, sr) {
  const n = L.length, out = Buffer.alloc(44 + n * 6);
  out.write("RIFF", 0); out.writeUInt32LE(36 + n * 6, 4); out.write("WAVEfmt ", 8);
  out.writeUInt32LE(16, 16); out.writeUInt16LE(1, 20); out.writeUInt16LE(2, 22);
  out.writeUInt32LE(sr, 24); out.writeUInt32LE(sr * 6, 28); out.writeUInt16LE(6, 32); out.writeUInt16LE(24, 34);
  out.write("data", 36); out.writeUInt32LE(n * 6, 40);
  const q = (x) => Math.max(-8388608, Math.min(8388607, Math.round(x * 8388607)));
  for (let i = 0; i < n; i++) { out.writeIntLE(q(L[i]), 44 + i * 6, 3); out.writeIntLE(q(R[i]), 47 + i * 6, 3); }
  return out;
}

// ---- mixing -------------------------------------------------------------
function mixdown(ch, mics) {
  const n = ch[0].length, L = new Float32Array(n), R = new Float32Array(n);
  for (const [i, g, pan] of mics) {
    const a = 10 ** (g / 20), th = (pan + 1) * Math.PI / 4;
    const gl = a * Math.cos(th) * Math.SQRT2, gr = a * Math.sin(th) * Math.SQRT2; // centre = unity each side
    const src = ch[i];
    for (let k = 0; k < n; k++) { L[k] += src[k] * gl; R[k] += src[k] * gr; }
  }
  return [L, R];
}
const peakOf = (L, R) => { let p = 0; for (let k = 0; k < L.length; k++) p = Math.max(p, Math.abs(L[k]), Math.abs(R[k])); return p; };
function bounds(L, R, sr) {
  const pk = peakOf(L, R);
  let on = 0; while (on < L.length && Math.max(Math.abs(L[on]), Math.abs(R[on])) < pk * 10 ** (-30 / 20)) on++;
  const start = Math.max(0, on - Math.round(0.003 * sr));
  const floor = pk * 10 ** (-66 / 20), win = Math.round(0.01 * sr);
  let end = L.length;
  for (let k = L.length - win; k > start; k -= win) {   // last 10 ms window above the floor
    let m = 0; for (let j = k; j < k + win; j++) m = Math.max(m, Math.abs(L[j]), Math.abs(R[j]));
    if (m > floor) { end = Math.min(L.length, k + 2 * win); break; }
  }
  return { start, end, rawOnsetMs: +(on / sr * 1000).toFixed(1) };
}
function cut(L, R, { start, end }, sr, gain) {
  const n = end - start, fade = Math.min(n, Math.round(0.01 * sr));
  const a = new Float32Array(n), b = new Float32Array(n);
  for (let k = 0; k < n; k++) {
    const f = k >= n - fade ? (n - k) / fade : 1;
    a[k] = L[start + k] * gain * f; b[k] = R[start + k] * gain * f;
  }
  return [a, b];
}
// Strongest partial between 40 and 400 Hz, 30–330 ms after the attack, on the tom's close mic.
function fundamental(x, sr, at) {
  const a = at + Math.round(0.03 * sr), n = Math.min(Math.round(0.3 * sr), x.length - a);
  let best = 0, bestF = 0;
  for (let f = 40; f <= 400; f += 0.5) {
    const w = 2 * Math.PI * f / sr; let re = 0, im = 0;
    for (let k = 0; k < n; k++) { const h = 0.5 - 0.5 * Math.cos(2 * Math.PI * k / n); re += x[a + k] * h * Math.cos(w * k); im += x[a + k] * h * Math.sin(w * k); }
    const m = re * re + im * im; if (m > best) { best = m; bestF = f; }
  }
  return bestF;
}
const NOTE = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const noteOf = (f) => { const m = 69 + 12 * Math.log2(f / 440), r = Math.round(m); return `${NOTE[r % 12]}${Math.floor(r / 12) - 1}${m - r >= 0 ? "+" : ""}${Math.round((m - r) * 100)}c`; };

function probe(path) {
  try {
    const j = JSON.parse(execFileSync("ffprobe", ["-v", "error", "-show_entries",
      "format=duration:stream=sample_rate,channels,codec_name", "-of", "json", path], { encoding: "utf8" }));
    const s = j.streams[0];
    return { seconds: +(+j.format.duration).toFixed(3), sampleRate: +s.sample_rate, channels: s.channels, codec: s.codec_name };
  } catch { return null; }
}
function write(path, buf) { writeFileSync(path + ".part", buf); renameSync(path + ".part", path); }

// ---- run ----------------------------------------------------------------
mkdirSync(OUT, { recursive: true });
const old = existsSync(resolve(OUT, "SOURCES.json")) ? JSON.parse(readFileSync(resolve(OUT, "SOURCES.json"), "utf8")).sounds : [];
const manifest = [];
let total = 0, failed = 0;

for (const fam of FAMILIES) {
  const kit = KITS[fam.kit];
  const takes = {};
  const items = fam.hits.map(([layer, rank]) => {
    takes[layer] = (takes[layer] || 0) + 1;
    const base = fam.file(layer, String(takes[layer]).padStart(2, "0"));
    return { layer, rank, take: takes[layer], mix: `${base}.wav`, room: fam.room && layer !== "ghost" ? `${base}-room.wav` : null };
  });
  const files = items.flatMap((it) => [it.mix, it.room].filter(Boolean));
  const have = files.every((f) => existsSync(resolve(OUT, f)));
  let entries;
  if (have && files.every((f) => old.some((o) => o.file === f))) {
    entries = old.filter((o) => files.includes(o.file));
    console.log(`· ${fam.name}: on disk`);
  } else {
    try {
      const xml = (await member(kit.zip, `${kit.root}/${fam.instr}/${fam.instr}.xml`)).toString("utf8");
      const samples = [...xml.matchAll(/<sample name="([^"]+)" power="([^"]+)">([\s\S]*?)<\/sample>/g)].map((m) => ({
        name: m[1], power: +m[2],
        file: m[3].match(/file="([^"]+)"/)[1],
        chan: Object.fromEntries([...m[3].matchAll(/channel="([^"]+)" file="[^"]+" filechannel="(\d+)"/g)].map((c) => [c[1], +c[2] - 1])),
      })).sort((a, b) => b.power - a.power);
      const hits = [];
      for (const it of items) {
        const s = samples[it.rank - 1];
        const memberName = `${kit.root}/${fam.instr}/${s.file}`;
        const { sr, ch } = readWav(await member(kit.zip, memberName));
        const own = [...kit.room, ...kit.oh, ...fam.close];
        const mics = own.map((m) => [s.chan[m], ...MIC[m]]);
        // Snare bottom faces the other way: flip it if it fights the top.
        for (const [top, bot] of [["Snare_top", "Snare_bottom"], ["SnareTop", "SnareBottom"]]) {
          if (!fam.close.includes(bot)) continue;
          const t = ch[s.chan[top]], b = ch[s.chan[bot]]; let c = 0;
          for (let k = 0; k < Math.min(t.length, Math.round(0.05 * sr)); k++) c += t[k] * b[k];
          if (c < 0) { const i = mics.findIndex((m) => m[0] === s.chan[bot]); mics[i] = [mics[i][0], mics[i][1], mics[i][2], true]; }
        }
        const flipped = mics.filter((m) => m[3]).map((m) => m[0]);
        const chs = ch.map((x, i) => (flipped.includes(i) ? x.map((v) => -v) : x));
        const [L, R] = mixdown(chs, mics);
        const [rl, rr] = mixdown(chs, kit.room.map((m) => [s.chan[m], 0, MIC[m][1]]));
        const closeIdx = fam.close.length ? s.chan[fam.close[0]] : null;
        hits.push({ it, s, memberName, sr, L, R, rl, rr, closeIdx, ch: chs, flipped: flipped.length > 0 });
      }
      const g = 10 ** (-1 / 20) / Math.max(...hits.map((h) => peakOf(h.L, h.R)));
      const gr = 10 ** (-1 / 20) / Math.max(...hits.filter((h) => h.it.room).map((h) => peakOf(h.rl, h.rr)), 1e-9);
      entries = [];
      for (const h of hits) {
        const b = bounds(h.L, h.R, h.sr);
        const common = {
          source: kit.page, download: `${kit.zip}#${h.memberName}`, archive: kit.zip, archiveMember: h.memberName,
          author: kit.author, licence: LICENCE, licenceUrl: LICENCE_URL, attribution: kit.attribution,
          drum: fam.name, description: fam.desc, layer: h.it.layer, take: h.it.take,
          kitSample: { instrument: fam.instr, name: h.s.name, power: h.s.power, rankFromLoudest: h.it.rank, of: samples.length },
          rawOnsetMs: b.rawOnsetMs, snareBottomFlipped: h.flipped || undefined,
        };
        if (h.closeIdx !== null && /^tom/.test(fam.name)) {
          const f = fundamental(h.ch[h.closeIdx], h.sr, b.start + Math.round(0.003 * h.sr));
          common.pitchHz = f; common.pitchNote = noteOf(f);
        }
        const [ml, mr] = cut(h.L, h.R, b, h.sr, g);
        const mixBuf = wav24(ml, mr, h.sr);
        if (total + mixBuf.length > MAX_BYTES) throw new Error("over the size budget");
        write(resolve(OUT, h.it.mix), mixBuf);
        entries.push({ file: h.it.mix, mic: fam.close.length ? "mix (close + overheads + room)" : "mix (overheads + room)",
          mix: [...kit.room, ...kit.oh, ...fam.close].map((m) => `${m} ${MIC[m][0] >= 0 ? "+" : ""}${MIC[m][0]} dB pan ${MIC[m][1]}`).join(", "),
          ...common });
        if (h.it.room) {
          const [a, c] = cut(h.rl, h.rr, b, h.sr, gr);
          const roomBuf = wav24(a, c, h.sr);
          write(resolve(OUT, h.it.room), roomBuf);
          entries.push({ file: h.it.room, mic: "room", mix: `${kit.room.join(" + ")} hard left/right, cut at the same sample as ${h.it.mix}`, ...common });
        }
      }
    } catch (e) { failed++; console.error(`✗ ${fam.name}: ${e.message}`); continue; }
  }
  for (const en of entries) {
    const dest = resolve(OUT, en.file);
    const audio = probe(dest);
    if (!audio) { failed++; console.error(`✗ ${en.file}: does not decode`); continue; }
    const bytes = statSync(dest).size;
    const { ch } = readWav(readFileSync(dest));
    const peakDb = +dB(peakOf(ch[0], ch[1])).toFixed(1);
    total += bytes;
    manifest.push({ ...en, bytes, ...audio, peakDb });
    console.log(`✓ ${en.file}  ${audio.seconds}s  ${peakDb} dBFS${en.pitchHz && en.mic !== "room" ? `  ${en.pitchHz} Hz ${en.pitchNote}` : ""}`);
  }
}

writeFileSync(resolve(OUT, "SOURCES.json"), JSON.stringify({
  note: "Real acoustic drum kits recorded by the DrumGizmo team, CC BY 4.0: commercial use allowed, with credit. Each file is a stereo mix or room-pair render of one multichannel hit, trimmed; the mix recipe is on each entry.",
  licence: LICENCE, licenceUrl: LICENCE_URL, licenceTerms: LICENCE_NOTE,
  credit: "Drum samples: DRSKit and CrocellKit by the DrumGizmo team (drumgizmo.org), CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); mixed to stereo and trimmed.",
  sounds: manifest,
}, null, 2) + "\n");
console.log(`${manifest.length} files, ${(total / 1e6).toFixed(1)} MB in ${OUT}`);
if (failed) process.exitCode = 1;
