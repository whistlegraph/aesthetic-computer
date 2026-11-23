#!/usr/bin/env node
// fetch-brushes.mjs — brushy, sweeping sounds into assets/brushes/, named by
// what they are: broom sweeps, drum-brush swishes and taps, whooshes, hair,
// paintbrush and other soft strokes. A fixed list (no search, no credentials),
// so a re-run gets the same files and skips the ones already there.
// Writes assets/brushes/SOURCES.json, with a rough analysis of each file
// (spectral centroid, onset count) for picking.
//
// Sources, all real recordings, all usable in commercial music:
//   BSB — BigSoundBank (Joseph Sardin): originals, CC0 1.0.
//   FS  — Freesound public preview MP3s (the original needs an OAuth login),
//         CC0 1.0 or CC BY 4.0 as the item's API record says. BY 4.0 items
//         carry their credit in `attribution`.
//   DRS — DrumGizmo DRSKit 2.1, CC BY 4.0: the brush ("whisker") snare. The
//         2.8 GB zip is never downloaded: its central directory is read with
//         HTTP range requests and only the chosen multichannel WAVs are
//         fetched, then mixed to stereo (as fetch-rockkit.mjs does).
// Nothing here is taken from a film, TV show, game or video site.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import zlib from "node:zlib";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../assets/brushes");
const UA = "sound-fetch/1.0";
const MAX_BYTES = 50e6;        // everything kept
const MAX_FILE = 30e6;         // one download
const MAX_MEMBER = 40e6;       // one multichannel DRSKit hit
const CC0 = "CC0 1.0", BY = "CC BY 4.0";
const LICENCE_URL = { [CC0]: "https://creativecommons.org/publicdomain/zero/1.0/", [BY]: "https://creativecommons.org/licenses/by/4.0/" };

// BigSoundBank: [file, category, id, slug, description, format]
const bsb = (file, category, id, slug, description, fmt = "flac") => ({
  file, category, description, author: "Joseph Sardin (BigSoundBank)", licence: CC0,
  source: `https://bigsoundbank.com/${slug}-s${id}.html`,
  download: fmt === "flac" ? `https://bigsoundbank.com/UPLOAD/flac/${id}.flac` : `https://bigsoundbank.com/UPLOAD/bwf-en/${id}.wav`,
});
// Freesound: [file, category, id, user id (in the preview URL), username, name, licence, description]
const fs = (file, category, id, uid, author, name, licence, description) => {
  const source = `https://freesound.org/people/${encodeURIComponent(author)}/sounds/${id}/`;
  return {
    file, category, description, author, licence, source,
    download: `https://cdn.freesound.org/previews/${Math.floor(id / 1000)}/${id}_${uid}-hq.mp3`,
    attribution: licence === BY ? `"${name}" by ${author} (${source}), CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); changes, if any, must be indicated.` : undefined,
  };
};

const SOUNDS = [
  // broom: single strokes
  fs("broom-sweep-concrete-01.mp3", "broom", 552052, 7805242, "F.M.Audio", "Sweeping Old Broom Against Concrete Floor 1.wav", BY, "One stroke of an old wooden broom on a concrete floor"),
  fs("broom-sweep-concrete-02.mp3", "broom", 552050, 7805242, "F.M.Audio", "Sweeping Old Broom Against Concrete Floor 3.wav", BY, "One stroke of an old wooden broom on a concrete floor"),
  fs("broom-sweep-concrete-03.mp3", "broom", 552049, 7805242, "F.M.Audio", "Sweeping Old Broom Against Concrete Floor 4.wav", BY, "One stroke of an old wooden broom on a concrete floor"),
  fs("broom-sweep-wood-01.mp3", "broom", 521142, 1652791, "maaaks", "Sweeping.wav", CC0, "Broom sweeping a wooden floor, close (Tascam DR-40)"),
  fs("broom-sweep-wood-02.mp3", "broom", 241351, 4250920, "lyndseymel", "Brush 01.wav", CC0, "Soft-bristled broom sweeping a hardwood kitchen floor, no background"),
  fs("broom-sweep-tile-01.mp3", "broom", 515146, 7195972, "blwuens", "Broom Sweep Tile Floor.wav", CC0, "Broom sweeping a tiled bathroom floor"),
  fs("broom-sweep-tile-02.mp3", "broom", 828252, 17820271, "teffita", "broom sweeps tile floor angry fast clean", CC0, "Broom on a tile floor, soft at first, then fast continuous sweeps"),
  // broom: rhythmic passes
  bsb("broom-sweep-rhythm-01.wav", "broom", "0024", "broom", "Six sweeps of a broom on a concrete floor", "wav"),
  fs("broom-sweep-rhythm-02.mp3", "broom", 496723, 10783345, "16GPanskaKomarkova_Marina", "01-1 Sweeping, fast", CC0, "Fast sweeping on outdoor concrete pavement (Rode NTG-2)"),
  fs("broom-sweep-rhythm-03.mp3", "broom", 734668, 13973196, "Vrymaa", "Broom - Sweep, outdoor", CC0, "Rigid straw outdoor broom on concrete; close, dry, no background"),
  fs("broom-sweep-rhythm-04.mp3", "broom", 118976, 1990695, "esperri", "Straw Broom Sweeping Hard.wav", CC0, "Classic straw broom sweeping hard"),
  // drum brushes: DRSKit entries are built below; this is a played groove
  fs("brush-snare-groove-01.mp3", "drum-brush", 31518, 129090, "HerbertBoland", "BrushedSnare2.wav", BY, "Snare played with brushes by Mark Hage, unprocessed, light room (98 bpm)"),
  // whooshes
  fs("whoosh-fast-01.mp3", "whoosh", 422506, 3302499, "Nightflame", "Swinging staff whoosh (strong) 01.wav", CC0, "Bamboo staff swung hard through the air"),
  bsb("whoosh-fast-02.flac", "whoosh", 1797, "whoosh-5", "Long wooden stick swung past the mic"),
  bsb("whoosh-fast-03.flac", "whoosh", 1798, "whoosh-10", "Large bamboo stick swung past the mic"),
  fs("whoosh-fast-04.mp3", "whoosh", 60019, 71257, "qubodup", "Quadruple Whoosh!!!", CC0, "Four bamboo-stick swishes in a row"),
  fs("whoosh-slow-01.mp3", "whoosh", 855719, 5287430, "Sadiquecat", "Whoosh - Long Bamboo stick - Slow low - OS ST 2", CC0, "Long bamboo stick, slow low swing"),
  bsb("whoosh-slow-02.flac", "whoosh", 1796, "whoosh-4", "Long rope swung past the mic"),
  fs("whoosh-slow-03.mp3", "whoosh", 391447, 7280678, "saturdaysoundguy", "Shirt Whoosh 2.wav", CC0, "A shirt swung through the air (cape/sail flap), Rode NTG-3"),
  fs("whoosh-slow-04.mp3", "whoosh", 400351, 4061049, "harrietniamh", "Fabric Swish", BY, "Fabric swished through the air (Zoom H4N)"),
  // hair. No clean hair flip passed: the two candidates (ldf99 586462 "Hair
  // moving", veronicalyn 488975 "HAIR SWEEP") sit under 8 dB above their own
  // noise floor. A hand swept through thick hair is the nearest real thing.
  fs("hair-rustle-01.mp3", "hair", 138271, 2531318, "Hupguy", "Hair Rustle 1-01.R.wav", CC0, "A hand run through thick hair"),
  fs("hairbrush-01.mp3", "hair", 708322, 14714481, "Mikail_Olivier", "SFXhairbrush-Int _OWI brushing hair_Mikail_OwSfx", CC0, "Hair being brushed, edited between strokes"),
  fs("hairbrush-02.mp3", "hair", 443037, 8282799, "AmberdeMeillon", "Brushing_Knotty_Hair.WAV", CC0, "Plastic brush through knotty, thick hair"),
  bsb("hairbrush-03.flac", "hair", 1856, "horsehair-brush", "A brush through a horse's mane"),
  // other brushy textures
  fs("paintbrush-01.mp3", "texture", 86113, 1275447, "acrober", "normal paint strokes_1-2.aif", CC0, "Paintbrush strokes, normal speed"),
  fs("paintbrush-02.mp3", "texture", 86114, 1275447, "acrober", "slowpaintstrokes_1-2.aif", CC0, "Paintbrush strokes, slow"),
  fs("brush-fabric-01.mp3", "texture", 832225, 15607250, "franckwalden", "4 strokes on a fabric desk mat", CC0, "Four soft strokes across a woven fabric mat, close"),
  bsb("brush-wood-01.flac", "texture", 1106, "brush-on-wood", "A brush rubbed on wood"),
  fs("sandpaper-soft-01.mp3", "texture", 469477, 9934093, "aabbccddee123", "058_smokedeslizandoseporlatierra-004.wav", CC0, "Soft scratch on sandpaper"),
];

// ---- DRSKit brush snare ---------------------------------------------------
const DRS = {
  zip: "https://drumgizmo.org/kits/DRSKit/DRSKit2_1.zip",
  root: "DRSKit",
  page: "https://drumgizmo.org/wiki/doku.php?id=kits:drskit",
  author: "DrumGizmo team (sampled by Deva & Lars Muldjord; kit by Jes Eiler, DRSDrums)",
  attribution: "DRSKit by the DrumGizmo team (drumgizmo.org), CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); mixed to stereo and trimmed.",
};
// dB gain and pan per mic. Close mics carry the bristle detail; the room is
// kept lower than in the rock kit so the swish stays dry.
const MIC = {
  AmbL: [0, -1], AmbR: [0, 1], OHL: [0, -0.7], OHR: [0, 0.7],
  Snare_top: [0, 0], Snare_bottom: [-10, 0],
};
// `ranks` count from the loudest sample (1 = loudest).
const DRS_FAMILIES = [
  { instr: "Snare_circle_whisker", ranks: [1, 2, 3], file: (n) => `brush-snare-swish-${n}.wav`,
    desc: "Wire brush stirred in a circle on the snare head (sustained swish); DRSKit 'Snare_circle_whisker'" },
  { instr: "Snare_whisker", ranks: [1, 4, 8, 12], file: (n) => `brush-snare-tap-${n}.wav`,
    desc: "Wire brush hit on the snare (tap/slap); DRSKit 'Snare_whisker'" },
];

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

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
let dir = null;
async function zipDir(url) {
  if (dir) return dir;
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
  return (dir = ents);
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
function mixdown(ch, mics) {
  const n = ch[0].length, L = new Float32Array(n), R = new Float32Array(n);
  for (const [i, g, pan, flip] of mics) {
    const a = 10 ** (g / 20) * (flip ? -1 : 1), th = (pan + 1) * Math.PI / 4;
    const gl = a * Math.cos(th) * Math.SQRT2, gr = a * Math.sin(th) * Math.SQRT2;
    const src = ch[i];
    for (let k = 0; k < n; k++) { L[k] += src[k] * gl; R[k] += src[k] * gr; }
  }
  return [L, R];
}
const peakOf = (L, R) => { let p = 0; for (let k = 0; k < L.length; k++) p = Math.max(p, Math.abs(L[k]), Math.abs(R[k])); return p; };
// Leading silence trimmed to 3 ms before the first sound (-40 dB under the
// peak: a brush starts soft); the tail ends 60 dB under the peak, 10 ms fade.
function trim(L, R, sr, gain) {
  const pk = peakOf(L, R);
  let on = 0; while (on < L.length && Math.max(Math.abs(L[on]), Math.abs(R[on])) < pk * 0.01) on++;
  const start = Math.max(0, on - Math.round(0.003 * sr));
  const floor = pk * 0.001, win = Math.round(0.01 * sr);
  let end = L.length;
  for (let k = L.length - win; k > start; k -= win) {
    let m = 0; for (let j = k; j < k + win; j++) m = Math.max(m, Math.abs(L[j]), Math.abs(R[j]));
    if (m > floor) { end = Math.min(L.length, k + 2 * win); break; }
  }
  const n = end - start, fade = Math.min(n, win);
  const a = new Float32Array(n), b = new Float32Array(n);
  for (let k = 0; k < n; k++) {
    const f = k >= n - fade ? (n - k) / fade : 1;
    a[k] = L[start + k] * gain * f; b[k] = R[start + k] * gain * f;
  }
  return [a, b];
}
async function buildDrs(fam) {
  const xml = (await member(DRS.zip, `${DRS.root}/${fam.instr}/${fam.instr}.xml`)).toString("utf8");
  const samples = [...xml.matchAll(/<sample name="([^"]+)" power="([^"]+)">([\s\S]*?)<\/sample>/g)].map((m) => ({
    name: m[1], power: +m[2],
    file: m[3].match(/file="([^"]+)"/)[1],
    chan: Object.fromEntries([...m[3].matchAll(/channel="([^"]+)" file="[^"]+" filechannel="(\d+)"/g)].map((c) => [c[1], +c[2] - 1])),
  })).sort((a, b) => b.power - a.power);
  const hits = [];
  for (const rank of fam.ranks) {
    const s = samples[rank - 1];
    const memberName = `${DRS.root}/${fam.instr}/${s.file}`;
    const { sr, ch } = readWav(await member(DRS.zip, memberName));
    // Snare bottom faces the other way: flip it if it fights the top.
    const t = ch[s.chan.Snare_top], b = ch[s.chan.Snare_bottom]; let c = 0;
    for (let k = 0; k < Math.min(t.length, Math.round(0.05 * sr)); k++) c += t[k] * b[k];
    const mics = Object.entries(MIC).map(([m, [g, pan]]) => [s.chan[m], g, pan, m === "Snare_bottom" && c < 0]);
    const [L, R] = mixdown(ch, mics);
    hits.push({ rank, s, memberName, sr, L, R });
  }
  // One gain per family: the loudest peaks at -1 dBFS, softer ones keep their level.
  const g = 10 ** (-1 / 20) / Math.max(...hits.map((h) => peakOf(h.L, h.R)));
  return hits.map((h, i) => {
    const [a, b] = trim(h.L, h.R, h.sr, g);
    return {
      file: fam.file(String(i + 1).padStart(2, "0")), buf: wav24(a, b, h.sr),
      category: "drum-brush", description: fam.desc,
      author: DRS.author, licence: BY, source: DRS.page, download: `${DRS.zip}#${h.memberName}`,
      attribution: DRS.attribution,
      kitSample: { instrument: fam.instr, name: h.s.name, power: h.s.power, rankFromLoudest: h.rank, of: samples.length },
      mix: Object.entries(MIC).map(([m, [gg, pan]]) => `${m} ${gg >= 0 ? "+" : ""}${gg} dB pan ${pan}`).join(", "),
    };
  });
}

// ---- download, probe, analyse --------------------------------------------
async function download(url) {
  let err;
  for (let n = 0; n < 4; n++) {
    try {
      const res = await fetch(url, { headers: { "User-Agent": UA } });
      if (!res.ok) throw new Error(`http ${res.status}`);
      if (+res.headers.get("content-length") > MAX_FILE) throw new Error("over the file cap");
      const buf = Buffer.from(await res.arrayBuffer());
      if (buf.length > MAX_FILE) throw new Error("over the file cap");
      return buf;
    } catch (e) { err = e; await sleep(1500 * (n + 1)); }
  }
  throw err;
}
function probe(path) {
  try {
    const j = JSON.parse(execFileSync("ffprobe", ["-v", "error", "-show_entries",
      "format=duration:stream=sample_rate,channels,codec_name", "-of", "json", path], { encoding: "utf8" }));
    const s = j.streams[0];
    return { seconds: +(+j.format.duration).toFixed(3), sampleRate: +s.sample_rate, channels: s.channels, codec: s.codec_name };
  } catch { return null; }
}
function fft(re, im) {
  const n = re.length;
  for (let i = 1, j = 0; i < n; i++) {
    let bit = n >> 1; for (; j & bit; bit >>= 1) j ^= bit; j ^= bit;
    if (i < j) { [re[i], re[j]] = [re[j], re[i]]; [im[i], im[j]] = [im[j], im[i]]; }
  }
  for (let len = 2; len <= n; len <<= 1) {
    const a = -2 * Math.PI / len, wr = Math.cos(a), wi = Math.sin(a);
    for (let i = 0; i < n; i += len) {
      let cr = 1, ci = 0;
      for (let j = 0; j < len / 2; j++) {
        const ur = re[i + j], ui = im[i + j], k = i + j + len / 2;
        const vr = re[k] * cr - im[k] * ci, vi = re[k] * ci + im[k] * cr;
        re[i + j] = ur + vr; im[i + j] = ui + vi; re[k] = ur - vr; im[k] = ui - vi;
        [cr, ci] = [cr * wr - ci * wi, cr * wi + ci * wr];
      }
    }
  }
}
// Energy-weighted spectral centroid, and a rough count of separate events:
// a 10 ms RMS envelope (smoothed over 30 ms) that rises 9 dB above its
// minimum over the last 150 ms, within 30 dB of the peak, 120 ms apart.
function analyse(path) {
  const SR = 44100;
  const raw = execFileSync("ffmpeg", ["-v", "error", "-i", path, "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 200e6 });
  const x = new Float32Array(raw.buffer, raw.byteOffset, raw.length >> 2);
  const N = 2048; let num = 0, den = 0;
  for (let s = 0; s + N <= x.length; s += N / 2) {
    const re = new Float64Array(N), im = new Float64Array(N);
    for (let k = 0; k < N; k++) re[k] = x[s + k] * (0.5 - 0.5 * Math.cos(2 * Math.PI * k / N));
    fft(re, im);
    for (let b = 1; b < N / 2; b++) { const p = re[b] * re[b] + im[b] * im[b]; num += p * b * SR / N; den += p; }
  }
  const hop = Math.round(0.01 * SR), env = [];
  for (let s = 0; s + hop <= x.length; s += hop) { let e = 0; for (let k = s; k < s + hop; k++) e += x[k] * x[k]; env.push(e / hop); }
  const sm = env.map((_, i) => (env[i] + (env[i - 1] ?? env[i]) + (env[i + 1] ?? env[i])) / 3);
  const db = sm.map((e) => 10 * Math.log10(e + 1e-12)), top = Math.max(...db);
  let onsets = 0, last = -1e9;
  for (let i = 1; i < db.length; i++) {
    if (db[i] < top - 30 || i - last < 12) continue;
    const lo = Math.min(...db.slice(Math.max(0, i - 15), i));
    if (db[i] - lo >= 9 && db[i] >= db[i - 1]) {
      let j = i; while (j + 1 < db.length && db[j + 1] >= db[j]) j++;   // ride up to the local peak
      onsets++; last = j; i = j;
    }
  }
  onsets = Math.max(onsets, 1);
  return { centroidHz: Math.round(num / (den || 1)), onsets, events: onsets === 1 ? "single" : "multi" };
}

// ---- run -------------------------------------------------------------------
mkdirSync(OUT, { recursive: true });
const manifest = [];
let total = 0, failed = 0;

function record(s) {
  const dest = resolve(OUT, s.file);
  const audio = probe(dest);
  if (!audio) { failed++; console.error(`✗ ${s.file}: does not decode`); return; }
  const bytes = statSync(dest).size;
  total += bytes;
  const { buf, ...meta } = s;
  manifest.push({ ...meta, licenceUrl: LICENCE_URL[s.licence], bytes, ...audio, ...analyse(dest) });
  const m = manifest.at(-1);
  console.log(`✓ ${s.file}  ${audio.seconds}s  ${audio.sampleRate} Hz  ${m.events}×${m.onsets}  ${m.centroidHz} Hz`);
}

for (const s of SOUNDS) {
  const dest = resolve(OUT, s.file);
  if (!existsSync(dest)) {
    try {
      const buf = await download(s.download);
      if (total + buf.length > MAX_BYTES) throw new Error("over the size budget");
      writeFileSync(dest + ".part", buf); renameSync(dest + ".part", dest);
    } catch (e) { failed++; console.error(`✗ ${s.file}: ${e.message}`); continue; }
  }
  record(s);
}

const old = existsSync(resolve(OUT, "SOURCES.json")) ? JSON.parse(readFileSync(resolve(OUT, "SOURCES.json"), "utf8")).sounds : [];
for (const fam of DRS_FAMILIES) {
  const files = fam.ranks.map((_, i) => fam.file(String(i + 1).padStart(2, "0")));
  let entries;
  if (files.every((f) => existsSync(resolve(OUT, f)) && old.some((o) => o.file === f))) {
    entries = old.filter((o) => files.includes(o.file)).map(({ file, category, description, author, licence, source, download, attribution, kitSample, mix }) =>
      ({ file, category, description, author, licence, source, download, attribution, kitSample, mix }));
  } else {
    try {
      entries = await buildDrs(fam);
      for (const en of entries) {
        if (total + en.buf.length > MAX_BYTES) throw new Error("over the size budget");
        const dest = resolve(OUT, en.file);
        writeFileSync(dest + ".part", en.buf); renameSync(dest + ".part", dest);
      }
    } catch (e) { failed++; console.error(`✗ ${fam.instr}: ${e.message}`); continue; }
  }
  for (const en of entries) record(en);
}

writeFileSync(resolve(OUT, "SOURCES.json"), JSON.stringify({
  note: "Real recordings only. CC0 1.0 files need no credit. CC BY 4.0 files (licence field) need the credit in their `attribution` wherever the music's credits are shown. Freesound files are preview MP3s of the originals; BigSoundBank files are the originals; DRSKit files are stereo mixes of one multichannel hit. centroidHz is the energy-weighted spectral centroid; onsets is a rough count of separate events.",
  credits: [...new Set(manifest.filter((m) => m.licence === BY).map((m) => m.attribution))],
  sounds: manifest,
}, null, 2) + "\n");
console.log(`${manifest.length} files, ${(total / 1e6).toFixed(1)} MB in ${OUT}`);
if (failed) process.exitCode = 1;
