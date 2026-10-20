// voiced.mjs — tighten word timings to where the voice really is.
//   node sync/voiced.mjs sung   (vox-grid.wav + words-grid.json → words-voiced.json)
//   node sync/voiced.mjs spoken (spoken16k.wav + spoken.json → spoken-voiced.json)
// words-grid gives a word its whole slot, rest included ("Jeffrey," reads as
// 1.25 s; the voice is gone by 1.3), and whisper's word edges smear into the
// pauses. A word's end becomes the last 10 ms window, before the next word
// starts, within 24 dB of the word's own peak; a spoken word's start snaps to
// the first such window up to 120 ms either side of whisper's.
import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
const side = process.argv[2];
const [wav, src, dst] = side === "spoken" ? ["spoken16k.wav", "spoken.json", "spoken-voiced.json"] : ["vox-grid.wav", "words-grid.json", "words-voiced.json"];
const SR = 16000, HOP = 160;
const pcm = spawnSync("ffmpeg", ["-v", "error", "-i", wav, "-ac", "1", "-ar", String(SR), "-f", "s16le", "-"], { maxBuffer: 1 << 30 }).stdout;
const s = new Int16Array(pcm.buffer, pcm.byteOffset, pcm.length >> 1);
const db = []; for (let i = 0; i + HOP <= s.length; i += HOP) { let e = 0; for (let k = 0; k < HOP; k++) e += s[i + k] ** 2; db.push(10 * Math.log10(e / HOP + 1)); }
const win = (ms) => Math.max(0, Math.min(db.length, Math.round(ms / 10)));

// both sides as { text, fromMs, toMs }; whisper tokens merge into words as warp-to-sing does
let words;
if (side === "spoken") {
  words = [];
  for (const t of JSON.parse(readFileSync(src, "utf8")).transcription) {
    if (!t.text.trim()) continue;
    if (t.text.startsWith(" ") || !words.length) words.push({ text: t.text.trim(), fromMs: t.offsets.from, toMs: t.offsets.to });
    else { words.at(-1).text += t.text.trim(); words.at(-1).toMs = t.offsets.to; }
  }
} else words = JSON.parse(readFileSync(src, "utf8"));

const out = words.map((w, i) => {
  const next = words[i + 1]?.fromMs ?? w.toMs;
  const a = win(side === "spoken" ? w.fromMs - 120 : w.fromMs), b = win(side === "spoken" ? Math.max(w.toMs, next) : next);
  let peak = -1; for (let f = a; f < b; f++) peak = Math.max(peak, db[f]);
  const loud = (f) => db[f] > Math.max(peak - 24, 40);
  let end = a; for (let f = a; f < b; f++) if (loud(f)) end = f + 1;
  let fromMs = w.fromMs;
  if (side === "spoken") { let f = a; while (f < Math.min(b, win(w.fromMs + 120)) && !loud(f)) f++; fromMs = f * 10; }
  const toMs = Math.min(side === "spoken" ? next : w.toMs, Math.max(fromMs + 120, end * 10));
  return { text: w.text, fromMs, toMs, slotMs: w.toMs };
});
for (let i = 1; i < out.length; i++) if (out[i].fromMs < out[i - 1].toMs) out[i].fromMs = out[i - 1].toMs; // stay monotonic
writeFileSync(dst, JSON.stringify(side === "spoken"
  ? { transcription: out.map((w) => ({ text: " " + w.text, offsets: { from: w.fromMs, to: w.toMs } })) }
  : out));
const moved = out.filter((w, i) => Math.abs(w.toMs - words[i].toMs) > 100 || Math.abs(w.fromMs - words[i].fromMs) > 60);
console.log(`${side}: ${out.length} words · ${moved.length} retimed · e.g. ${moved.slice(0, 6).map((w) => `${w.text} ${(w.fromMs / 1e3).toFixed(2)}–${(w.toMs / 1e3).toFixed(2)}`).join(", ")}`);
