// regrid.mjs — re-time the sung take onto a fixed grid, holding the notes.
//   node regrid.mjs [bpm]   → vox-grid.wav, words-grid.json, grid2.json
// Each word starts on an 8th and is stretched to fill whole 8ths; a comma
// holds one more; a sentence end rings to the bar line and the next sentence
// waits for the next beat. rubberband R3 (formant-preserving) runs one pass
// over the whole take through the word-onset timemap, so there are no seams.
import { readFileSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";

const BPM = +(process.argv[2] || 120), E8 = 60 / BPM / 2, SR = 48000;
const words = JSON.parse(readFileSync("sung-words.json", "utf8")).map((w) => ({ text: w.text, from: w.fromMs / 1000, to: w.toMs / 1000 }));
const ends = (w) => /[.?!]$/.test(w.text), comma = (w) => /[,;:]$/.test(w.text);
let cur = 0; const out = [];
words.forEach((w, i) => {
  const d = Math.max(0.08, (words[i + 1]?.from ?? w.to) - w.from); // words tile: a word runs to the next onset
  if (i && ends(words[i - 1])) cur = Math.ceil(cur / 2) * 2; // a new sentence starts on a beat
  let slots = Math.max(1, Math.round((d * 1.35) / E8));
  slots = Math.min(slots, Math.max(1, Math.floor((d * 3.2) / E8))); // keep stretch ≤ 3.2×
  if (comma(w)) slots += 1;
  if (ends(w)) { const toBar = 8 - (((cur + slots) % 8) + 8) % 8; slots += toBar <= 4 ? toBar : 4; }
  out.push({ text: w.text, src: w.from, srcTo: words[i + 1]?.from ?? w.to, slot: cur, slots });
  cur += slots;
});
const t = (slot) => slot * E8;
const map = out.map((w) => [Math.round(w.src * SR), Math.round(t(w.slot) * SR)]);
const last = out.at(-1);
map.push([Math.round(last.srcTo * SR), Math.round(t(last.slot + last.slots) * SR)]);
const mono = map.filter((p, i) => i === 0 || (p[0] > map[i - 1][0] && p[1] > map[i - 1][1]));
console.log(`timemap ${mono.length}/${map.length} points (dropped zero-length words)`);
writeFileSync("timemap-grid.txt", mono.map((p) => p.join(" ")).join("\n") + "\n");
const total = t(cur) + 0.5;
spawnSync("ffmpeg", ["-v", "error", "-y", "-i", "mix/vox-gain.wav", "-ac", "1", "-ar", String(SR), "-c:a", "pcm_s16le", "vox-src-mono.wav"]);
const r = spawnSync("rubberband", ["-3", "-F", "-D", total.toFixed(4), "-M", "timemap-grid.txt", "vox-src-mono.wav", "vox-grid.wav"], { encoding: "utf8" });
if (r.status !== 0) throw new Error(r.stderr);
const ratios = out.filter((w) => w.srcTo - w.src > 0.02).map((w) => (w.slots * E8) / (w.srcTo - w.src));
writeFileSync("words-grid.json", JSON.stringify(out.map((w) => ({ text: w.text, fromMs: Math.round(t(w.slot) * 1000), toMs: Math.round(t(w.slot + w.slots) * 1000) }))));
writeFileSync("grid2.json", JSON.stringify({ bpm: BPM, e8: E8, slots: cur, seconds: t(cur), words: out }, null, 1));
ratios.sort((a, b) => a - b);
console.log(`${out.length} words → ${cur} 8ths = ${(cur / 8).toFixed(1)} bars = ${t(cur).toFixed(1)}s at ${BPM} BPM`);
console.log(`stretch median ${ratios[ratios.length >> 1].toFixed(2)}× · p90 ${ratios[Math.floor(ratios.length * 0.9)].toFixed(2)}× · max ${ratios.at(-1).toFixed(2)}×`);
