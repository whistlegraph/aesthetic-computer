#!/usr/bin/env node
// splice.mjs — the ARRANGEMENT of her take: the record is a sequence of
// SEGMENTS of the regularized stems, in any order, joined with short
// equal-power seams (12 ms on her voice, 80 ms on the sustained stems so a
// strum or a held vowel never clicks). The music around her is scored by the
// engine on the re-sequenced chart, so it is continuous by construction —
// only her own stems carry seams, and every seam sits on a beat.
//
// v13 form: verse 1 · verse 2 · chorus 1 · chorus 2 · break · bridge · outro
//   A  open → bar 27 downbeat            (intro + verse 1)
//   B  bar 43 beat 3 → chorus 2's "Oh"   (verse 2, on its "And" pickup)
//   C  bar 27 → bar 43 beat 3            (chorus 1, on its "Oh, won't you")
//   D  chorus 2's "Oh" → end             (chorus 2 · break · bridge · outro)
//   B opens with a 2-beat pickup bar and closes on one; C's closing half bar
//   ("so long") and D's opening half bar ("Oh, won't you") make one full bar.
//
// Anchors: { bar, beat } (chart bar number, beat 1-based) or
//          { word, next, afterSec, beforeSec, lead } (a sung onset from
//          src/words-aligned.json, take time, mapped through lock + reg).
//
// Reads  src/vox/reg/*.wav, measures.reg.json, src/words-aligned.json, the maps
// Writes src/vox/cut/*.wav, src/vox/cut/segmap.txt ("from to offset" in reg s),
//        measures.cut.json (bars in record order, numbers kept)
//
//   node pop/sailor-song/bin/splice.mjs

import { readFileSync, writeFileSync, mkdirSync, readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const VOX = resolve(LANE, "src/vox");
const REG = resolve(VOX, "reg"), OUT = resolve(VOX, "cut");
mkdirSync(OUT, { recursive: true });
const SR = 48000;

const M = JSON.parse(readFileSync(resolve(LANE, "measures.reg.json"), "utf8"));
const W = JSON.parse(readFileSync(resolve(LANE, "src/words-aligned.json"), "utf8"));
const through = (path) => { const m = readFileSync(path, "utf8").trim().split("\n").map((l) => l.split(" ").map((x) => Number(x) / SR));
  return (t) => { let k = m.findIndex(([s]) => s > t); if (k <= 0) k = 1; const [s0, d0] = m[k - 1], [s1, d1] = m[k]; return d0 + ((t - s0) / (s1 - s0)) * (d1 - d0); }; };
const lock = through(resolve(VOX, "locked/timemap.txt")), reg = through(resolve(REG, "timemap.txt"));
const regOf = (takeSec) => reg(lock(takeSec));
const norm = (s) => s.toLowerCase().replace(/[^a-z']/g, "");
const END = M.bars.at(-1).t + M.bars.at(-1).dur + 0.5;

// v17: "me?" runs straight into "Oh" with no breath — the split is the tuned-note onset of
// "Oh" (her pitch change), found from vox-notes.json in the 0.3 s around the word's start
// v18: her "me?" is a three-note question (G#4 F#4 G#4); "Oh" is the D#4 that follows — cut on that pitch
const OH1 = { noteNear: { word: "Oh", next: "won't", afterSec: 55, beforeSec: 62 }, window: 0.5, pitch: "D#4", lead: 0.012 };
const AND = { word: "And", next: "lately", afterSec: 88, beforeSec: 96, lead: 0.02, voiceOnset: true };   // v19d: placed on her voice, not the word stamp
const OH2 = { word: "Oh", next: "won't", afterSec: 105, beforeSec: 112, lead: 0.05 };
// v14: A runs to just before chorus 1's "Oh" so "me?" finishes; B keeps two beats of
// guitar before "And" but her VOICE is held until "And" (voiceFrom), so verse 1's last
// word and verse 2's first never overlap; C runs through all of "so long".
// v16: a proper bar between "me?" and "And lately" — one voice-free bar of her guitar
// from the intro (bar 10, G#m), carried as bar 26 so the beds hold verse 1's state —
// then verse 2 on its one-beat "And" pickup.
// v19: no inserted guitar bar. By ear her "me?" keeps going through the held D#4 that
// whisper labels "Oh" — v18 cut at that D#4 and chorus 1 (which opens on the bar-27
// downbeat, inside "me?") replayed the rest of it after verse 2. Now verse 1 runs to
// the next pitch change (the C#4 into "won't"), finishing "me" with a short release, and
// chorus 1's voice is held until that same point.
// v19c: MMS forced alignment (bin/forced-align.py) against her pitch notes settles it: "to" is
// the G#4 at take 58.37, "me?" runs F#4 → A4 → the D#4 at 59.27–59.56, and "Oh" is the NEXT
// D#4 attack at 59.77 (whisper-1 agrees, 59.72). The voice never breaks between them; the
// quietest 10 ms is take 59.66 = reg 59.182. Verse 1 ends there with a short release on
// "me?", and chorus 1's voice opens from the same point. Reg seconds (bar 27 is unlifted).
const ME_END = 59.182, OH_START = 59.19;
// v20: "ohh… ohhhhh" between the verses. By ear (SyllaWizard, hand bounds) chorus 1's "Oh"
// is take 59.52–59.94 — it had already begun inside verse 1's last segment and was cut off
// at ME_END as a 130 ms sliver, so the join to "And lately" felt disconnected. Now verse 1
// runs through the whole "Oh" to the onset of "won't" and holds the vowel with grains
// (a short "ohh"); the screw is a full bar again, and over it chorus 2's "Oh" (a second
// performance of the same D#4) re-attacks and is held long ("ohhhhh") into verse 2's pickup.
const WONT1 = { word: "won't", next: "you", afterSec: 55, beforeSec: 62 };
const WONT2 = { word: "won't", next: "you", afterSec: 105, beforeSec: 112 };
const SEGMENTS = [
  { name: "A intro+verse1", from: 0, to: WONT1, tail: { grab: 0.22, before: 0.02, len: 0.55, curve: 1.2 } },
  // v19: a breath before verse 2 kicks off — verse 1's last full bar of her guitar,
  // slowed a fifth (screwed, 7 st down — in key; 0.78 sat between keys and read "werd") and stuttered on 8ths and 16ths into the pickup;
  // charted as bar 26 so the beds hold verse 1's state and the kit keeps going
  { name: "S screw", screw: { bar: 26, rate: 2 ** (-7 / 12), lenOfBar: 44, beats: 4 }, asBar: 26,
    voice: { from: OH2, to: WONT2, at: 0.5, tail: { grab: 0.22, before: 0.02, len: 1.15, curve: 1.0 } } },   // v20: the long "ohhhhh"
  { name: "B verse2", from: { bar: 43, beat: 4 }, to: OH2, voiceFrom: AND },
  { name: "C chorus1", from: { bar: 27, beat: 1 }, to: AND, voiceFrom: OH_START },
  { name: "D chorus2..end", from: OH2, to: END },
];
const NOTES = JSON.parse(readFileSync(resolve(LANE, "vox-notes.json"), "utf8")).notes.map((n) => ({ t: regOf(n.t), note: n.note })).sort((x, y) => x.t - y.t);
// the first 10 ms, from 0.35 s before the stamp to 0.5 s after, where her voice rises out of
// a gap into singing (the regularized lead stem): quiet below 12% of the window's peak, then
// two frames above 30% of it
let _lead = null;
function voiceOnset(t) {
  _lead ??= readWav(resolve(REG, ["vocals-natural.wav", "vocals-aesthetivox.wav"].find((f) => readdirSync(REG).includes(f))))[0];
  const F = Math.round(0.01 * SR), a = Math.round((t - 0.35) * SR), n = Math.round(0.85 * SR / F);
  const rms = Array.from({ length: n }, (_, k) => { let e = 0; for (let i = 0; i < F; i++) { const v = _lead[a + k * F + i] || 0; e += v * v; } return Math.sqrt(e / F); });
  const peak = Math.max(...rms);
  for (let k = 1; k < n - 1; k++) if (rms[k - 1] < 0.12 * peak && rms[k] >= 0.3 * peak && rms[k + 1] >= 0.3 * peak) return (a + k * F) / SR;
  return t;
}
const anchor = (a) => {
  if (typeof a === "number") return a;
  if (a === 1e9) return 1e9;
  if (a.noteNear) { const w = anchor({ ...a.noteNear, lead: 0 }); const c = NOTES.filter((n) => Math.abs(n.t - w) <= (a.window || 0.3) && (!a.pitch || n.note === a.pitch)).map((n) => n.t);
    if (!c.length) throw new Error("no note onset near " + JSON.stringify(a.noteNear)); return c.reduce((p, t) => Math.abs(t - w) < Math.abs(p - w) ? t : p) - (a.lead || 0); }
  if (a.bar) { const b = M.bars.find((x) => x.n === a.bar); if (!b) throw new Error(`no bar ${a.bar}`); return b.beats[a.beat - 1]; }
  const wi = W.findIndex((w, i) => norm(w.text) === norm(a.word) && W[i + 1] && norm(W[i + 1].text) === norm(a.next) && w.fromMs / 1000 > a.afterSec && w.fromMs / 1000 < a.beforeSec);
  if (wi < 0) throw new Error(`word not found: ${JSON.stringify(a)}`);
  const t = regOf(W[wi].fromMs / 1000);
  return (a.voiceOnset ? voiceOnset(t) : t) - (a.lead || 0);
};
let offset = 0;
const barBy = (n) => { const b = M.bars.find((x) => x.n === n); if (!b) throw new Error(`no bar ${n}`); return b; };
const segs = SEGMENTS.map((s) => { if (s.screw) { const src = barBy(s.screw.bar), len = barBy(s.screw.lenOfBar).dur * (s.screw.beats || 4) / 4;   // bar 44 is already on the lifted clock
    const seg = { ...s, from: src.t, to: src.t + len, offset, voiceFrom: null, chord: src.chord,
      voice: s.voice ? { ...s.voice, from: anchor(s.voice.from), to: anchor(s.voice.to) } : null }; offset += len; return seg; }
  const from = anchor(s.from), to = anchor(s.to); const seg = { ...s, from, to, offset, voiceFrom: s.voiceFrom ? anchor(s.voiceFrom) : null }; offset += to - from; return seg; });
for (const s of segs) console.log(`${s.name.padEnd(16)} ${s.from.toFixed(3)} → ${s.to.toFixed(3)}  (${(s.to - s.from).toFixed(2)} s) at ${s.offset.toFixed(3)}`);
writeFileSync(resolve(OUT, "segmap.txt"), segs.map((s) => `${s.from.toFixed(4)} ${s.to.toFixed(4)} ${s.offset.toFixed(4)}`).join("\n") + "\n");
// v20: sung sound the words file cannot know about — a held tail past a seam, a fragment
// re-attacked over the screw — so the lyric video can label it (word-times.py applies these
// to words-record.json: `extend` stretches the word ending at `from`, else a new word)
const extras = [];
for (const s of segs) {
  if (s.tail && !s.screw) extras.push({ text: "Oh,", from: +(s.offset + (s.to - s.from)).toFixed(3), to: +(s.offset + (s.to - s.from) + s.tail.len).toFixed(3), extend: true });
  if (s.screw && s.voice) extras.push({ text: "Oh,", from: +(s.offset + s.voice.at).toFixed(3), to: +(s.offset + s.voice.at + (s.voice.to - s.voice.from) + (s.voice.tail?.len || 0)).toFixed(3) });
}
writeFileSync(resolve(OUT, "voice-extras.json"), JSON.stringify({ _: "cut-clock seconds; see word-times.py", extras }, null, 1));
const recOf = (t) => { for (const s of segs) if (!s.screw && t >= s.from && t < s.to) return t - s.from + s.offset; return null; };

// ── the chart: bars in record order, partial bars truncated at the seams ──
const bars = [];
for (const s of segs) {
  if (s.screw) { const d = s.to - s.from, src = barBy(s.screw.bar);
    bars.push({ ...src, n: s.asBar, t: +s.offset.toFixed(3), dur: +d.toFixed(3), beats: Array.from({ length: (s.screw.beats || 4) + 1 }, (_, k) => +(s.offset + d * k / (s.screw.beats || 4)).toFixed(3)), strum: [] }); continue; }
  for (const b of M.bars) {
    const t0 = b.t, t1 = b.t + b.dur;
    if (t1 <= s.from || t0 >= s.to) continue;
    const inside = b.beats.filter((t) => t >= s.from - 1e-6 && t < s.to + 1e-6);
    const beats = inside.map((t) => +(t - s.from + s.offset).toFixed(3));
    if (beats.length && beats[0] > +(Math.max(t0, s.from) - s.from + s.offset).toFixed(3) + 1e-6) beats.unshift(+(Math.max(t0, s.from) - s.from + s.offset).toFixed(3));
    const endT = +(Math.min(t1, s.to) - s.from + s.offset).toFixed(3);
    if (!beats.length || beats.at(-1) < endT - 1e-6) beats.push(endT);
    if (beats.length < 2) continue;                               // v19d: a one-beat pickup is a bar too (its kick keeps the pulse)
    const cutL = t0 < s.from, cutR = t1 > s.to;
    bars.push({ ...b, n: s.asBar ?? b.n, t: beats[0], dur: +(endT - beats[0]).toFixed(3), beats, strum: cutL ? b.strum.slice(-(beats.length - 1) * 2) : cutR ? b.strum.slice(0, (beats.length - 1) * 2) : b.strum });
  }
}
const cut = { ...M, cut: { segments: segs.map((s) => ({ name: s.name, from: +s.from.toFixed(3), to: +s.to.toFixed(3), offset: +s.offset.toFixed(3) })), totalSec: +offset.toFixed(3) },
  beats: { ...M.beats, snapped: M.beats.snapped.map(recOf).filter((t) => t !== null).sort((a, b) => a - b) },
  bars, strumList: M.strumList.map((x) => ({ ...x, t: recOf(x.t) })).filter((x) => x.t !== null).sort((a, b) => a.t - b.t).map((x) => ({ ...x, t: +x.t.toFixed(3) })) };
writeFileSync(resolve(LANE, "measures.cut.json"), JSON.stringify(cut, null, 1));
console.log(`chart: ${M.bars.length} → ${bars.length} bars in record order (${bars.filter((b) => b.beats.length < 5).map((b) => `${b.n}:${b.beats.length - 1}`).join(" ")} short), ${offset.toFixed(1)} s`);

// ── the stems ─────────────────────────────────────────────────────────
function readWav(p) {
  const buf = readFileSync(p); let i = 12, fmt = 1, ch = 1, sr = SR, bits = 16, data = null;
  while (i < buf.length - 8) { const id = buf.toString("ascii", i, i + 4), size = buf.readUInt32LE(i + 4), body = i + 8;
    if (id === "fmt ") { fmt = buf.readUInt16LE(body); ch = buf.readUInt16LE(body + 2); sr = buf.readUInt32LE(body + 4); bits = buf.readUInt16LE(body + 14); }
    else if (id === "data") { data = { body, size }; break; }
    i = body + size + (size & 1); }
  if (!data || sr !== SR) throw new Error(`bad wav ${p}`);
  const bps = bits / 8, frames = Math.floor(data.size / (bps * ch)); const out = Array.from({ length: ch }, () => new Float32Array(frames));
  for (let f = 0; f < frames; f++) for (let c = 0; c < ch; c++) { const o = data.body + (f * ch + c) * bps;
    out[c][f] = fmt === 3 ? buf.readFloatLE(o) : bits === 24 ? ((buf.readUInt8(o) | (buf.readUInt8(o + 1) << 8) | (buf.readInt8(o + 2) << 16)) / 8388608) : buf.readInt16LE(o) / 32768; }
  return out;
}
function writeWav(p, chans) {
  const ch = chans.length, n = chans[0].length, buf = Buffer.alloc(44 + n * ch * 4);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * ch * 4, 4); buf.write("WAVE", 8); buf.write("fmt ", 12); buf.writeUInt32LE(16, 16);
  buf.writeUInt16LE(3, 20); buf.writeUInt16LE(ch, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * ch * 4, 28); buf.writeUInt16LE(ch * 4, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36); buf.writeUInt32LE(n * ch * 4, 40);
  for (let f = 0; f < n; f++) for (let c = 0; c < ch; c++) buf.writeFloatLE(chans[c][f], 44 + (f * ch + c) * 4);
  writeFileSync(p, buf);
}
// the seam: each segment fades in over xf and the previous fades out over the same span (equal power), overlapping
function assemble(x, xfSec, voice) {
  const total = Math.round(offset * SR), out = new Float32Array(total), XF = Math.round(xfSec * SR);
  for (const s of segs) {
    const a = Math.round(s.from * SR), b = Math.round(s.to * SR), o = Math.round(s.offset * SR);
    if (s.screw) {
      if (!voice) { screw(x, out, s); continue; }
      if (s.voice) {                                   // v20: a sung fragment re-attacked over the screw, then held
        const a = Math.round(s.voice.from * SR), b = Math.round(s.voice.to * SR), o = Math.round((s.offset + s.voice.at) * SR), VF = Math.round(0.012 * SR);
        for (let i = 0; i < b - a && o + i < total; i++) out[o + i] += (x[a + i] || 0) * (i < VF ? 0.5 - 0.5 * Math.cos(Math.PI * i / VF) : 1);
        if (s.voice.tail) freezeTail(x, out, b, o + (b - a), s.voice.tail);
      }
      continue;
    }
    for (let i = 0; i < b - a && o + i < total; i++) {
      const src = a + i < x.length ? x[a + i] : 0;
      let gin = s.offset > 0 && i < XF ? Math.sqrt(0.5 - 0.5 * Math.cos(Math.PI * i / XF)) : 1;
      if (voice && s.voiceFrom !== null) { const v0 = Math.round((s.voiceFrom - s.from) * SR), VF = Math.round(0.012 * SR);   // her voice held until voiceFrom
        gin *= i < v0 - VF ? 0 : i < v0 ? 0.5 - 0.5 * Math.cos(Math.PI * (i - (v0 - VF)) / VF) : 1; }
      out[o + i] += src * gin;
    }
    if (voice && s.tail) freezeTail(x, out, b, o + (b - a), s.tail);
    // the previous segment's tail rides under this one's head (voice: a 40 ms release, not a hard 12)
    const XO = voice ? Math.round(0.04 * SR) : XF;
    const prevTail = segs.find((p) => (p.tail || p.screw) && Math.abs(p.offset + (p.to - p.from) - s.offset) < 1e-6);
    if (s.offset > 0 && !(voice && prevTail) && !(prevTail && prevTail.screw)) for (let i = 0; i < XO && o + i < total && a - XO + i >= 0; i++) {
      const prev = segs.find((p) => Math.abs(p.offset + (p.to - p.from) - s.offset) < 1e-6);
      if (!prev) break;
      const pb = Math.round(prev.to * SR); const src = pb + i < x.length ? x[pb + i] : 0;
      out[o + i] += src * Math.sqrt(0.5 + 0.5 * Math.cos(Math.PI * i / XO));
    }
  }
  return out;
}
// a held release for a note that never ends in the take: overlapping Hann grains read from
// the last `grab` seconds before the cut (so pitch and timbre stay hers), crossfaded in
// from the real signal and faded out over `len` with a gentle curve.
// chopped and screwed: the source bar read at `rate` (slower and lower, linear interp),
// the first five eighths played through, then the sixth eighth stuttered twice and the
// last quarter as four 16th repeats of one slice, each chunk with 6 ms fades
function screw(x, out, s) {
  const o = Math.round(s.offset * SR), n = Math.round((s.to - s.from) * SR), a = Math.round(s.from * SR), r = s.screw.rate;
  const slow = new Float32Array(n);
  for (let i = 0; i < n; i++) { const p = a + i * r, k = Math.floor(p), f = p - k; slow[i] = (x[k] || 0) * (1 - f) + (x[k + 1] || 0) * f; }
  const e8 = Math.floor(n / 8), F = Math.round(0.006 * SR);
  const chunks = [[0, 5 * e8], [5 * e8, e8], [5 * e8, e8], [6 * e8, e8 / 2], [6 * e8, e8 / 2], [6 * e8, e8 / 2], [6 * e8, e8 / 2]];
  let at = 0;
  chunks.forEach(([from, len], c) => { len = Math.round(len); const g = c < 3 ? 1 : 0.9 - 0.12 * (c - 3);
    for (let i = 0; i < len && at + i < n; i++) { const fade = Math.min(1, i / F, (len - i) / F); out[o + at + i] += slow[from + i] * fade * g; }
    at += len; });
}
function freezeTail(x, out, cutAt, at, { grab, len, before = 0, curve = 1.6 }) {
  const G = Math.round(0.09 * SR), HOP = Math.round(0.0225 * SR), N = Math.round(len * SR), B = Math.round(grab * SR);
  const base = cutAt - Math.round(before * SR) - B, XIN = Math.round(0.03 * SR);   // `before`: grab clear of the move into the next word
  let seed = 7; const rand = () => (seed = (seed * 16807) % 2147483647) / 2147483647;
  const tail = new Float32Array(N + G);
  for (let g = 0; g * HOP < N; g++) {
    const src = base + Math.floor(rand() * Math.max(1, B - G));
    for (let i = 0; i < G; i++) tail[g * HOP + i] += (x[src + i] || 0) * (0.5 - 0.5 * Math.cos(2 * Math.PI * i / G)) * 0.5;
  }
  for (let i = 0; i < N && at + i < out.length; i++) {
    const fade = Math.pow(1 - i / N, curve), inn = i < XIN ? i / XIN : 1;
    out[at + i] += tail[i] * fade * inn;   // the take past the cut is "Oh": never used
  }
  // the real vowel fades under the grains' entrance so there is no step at the cut
  for (let i = 0; i < XIN && at - XIN + i >= 0; i++) out[at - XIN + i] *= 1 - 0.35 * (i / XIN);
}
for (const f of readdirSync(REG).filter((f) => f.endsWith(".wav"))) {
  const sustained = /guitar|replay|hum|choir|jeffrey/.test(f);
  const chans = readWav(resolve(REG, f)).map((x) => assemble(x, sustained ? 0.08 : 0.012, !sustained));
  writeWav(resolve(OUT, f), chans);
  console.log(`  ${sustained ? "80ms" : "12ms"} ${f}`);
}
console.log(`✓ ${OUT}`);
