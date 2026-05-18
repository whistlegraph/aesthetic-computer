#!/usr/bin/env node
// karaoke-words.mjs — lock Fía's KNOWN lyrics to her actual vocals,
// for a TWO-VOICE duet.
//
//   Voice A (lead)  = verse 1 + verse 2  (Circuito 2 + 3), concatenated
//   Voice B (duet)  = verse 3            (Circuito 4), a different voice
//
// We have the exact lyrics, so per voice: Whisper transcribes that
// voice's cleaned take for word timestamps, alignWords() maps OUR
// canonical words onto those times. Output one combined list, each word
// tagged {voice:"A"|"B", line, start, end} in that voice's CLEAN
// timebase — preview-spin multiplies by the matching vocal-map voice.
//
// Usage: node pop/jungle/bin/karaoke-words.mjs --slug solafiya

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { alignWords } from "../../bin/align-words.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i]; if (!a.startsWith("--")) continue;
  const n = process.argv[i + 1];
  if (n === undefined || n.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = n; i++; }
}
const SLUG = flags.slug || "solafiya";
const G = "/Users/jas/Documents/Working Desktop/gens";

// ── the exact lyrics, per voice, line by line ────────────────────────
// Voice A (lead) — the "Me levanto" set.
const LINES_A = [
  "Me levanto en la mañana y digo meow meow",
  "Me tomo un café y digo meow meow",
  "salgo a caminar y a brincar digo meow meow",
  "me gusta bailar meow meow meow meow meow meourrrr",
];
// Voice B (duet, verse 3) — the "Soy un gatito" set.
const LINES_B = [
  "Soy un gatito y me gusta pintar",
  "Tomo mis pinceles a todo color",
  "Mis pinceles de colores mis pinturas de colores crayolas confetti todo tipo de arte",
  "Yo soy un gatito meow meow",
  "Soy un gatito meow meow",
  "Tomo kombucha y café por la mañana",
  "Me estiro en la noche y hago algo de yoga",
  "Me gusta leer libros también meow meow",
];

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const v = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(v, "utf8").split("\n"))
    if (line.startsWith("OPENAI_API_KEY="))
      return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("no OPENAI_API_KEY");
}
const apiKey = loadOpenAIKey();
const CHAIN = "highpass=f=110,afftdn=nr=20,acompressor=threshold=-22dB:ratio=4:makeup=6,alimiter=limit=0.96";

// must mirror render.mjs: N verses, 0.40s breath gap between each.
function cleanConcat(inputs, out) {
  const ins = inputs.flatMap((p) => ["-i", p]);
  let fc;
  if (inputs.length > 1) {
    let pre = ""; const labs = [];
    for (let i = 0; i < inputs.length; i++) {
      if (i < inputs.length - 1) { pre += `[${i}:a]apad=pad_dur=0.40[a${i}];`; labs.push(`[a${i}]`); }
      else labs.push(`[${i}:a]`);
    }
    fc = `${pre}${labs.join("")}concat=n=${inputs.length}:v=0:a=1,${CHAIN}[o]`;
  } else fc = `[0:a]${CHAIN}[o]`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
    ...ins, "-filter_complex", fc, "-map", "[o]", "-ar", "16000", "-ac", "1", out]);
  if (r.status !== 0) throw new Error(`clean failed: ${out}`);
  return out;
}
function probe(p) {
  const r = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "default=nw=1:nk=1", p], { encoding: "utf8" });
  return parseFloat(r.stdout) || 30;
}
async function whisper(audio, promptText) {
  const fd = new FormData();
  fd.append("file", new Blob([readFileSync(audio)], { type: "audio/wav" }), "v.wav");
  fd.append("model", "whisper-1");
  fd.append("language", "es");
  fd.append("response_format", "verbose_json");
  fd.append("timestamp_granularities[]", "word");
  fd.append("prompt", promptText);
  const res = await fetch("https://api.openai.com/v1/audio/transcriptions", {
    method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
  });
  if (!res.ok) throw new Error(`whisper ${res.status}: ${(await res.text()).slice(0, 200)}`);
  const j = await res.json();
  return (j.words || []).map((w) => ({
    text: w.word, fromMs: Math.round((w.start || 0) * 1000), toMs: Math.round((w.end || 0) * 1000),
  }));
}

// align one voice: canonical lyric LINES → words tagged voice+line.
async function alignVoice(voice, lineSet, inputs, lineBase) {
  const canon = [], lineOf = [];
  lineSet.forEach((ln, li) => ln.trim().split(/\s+/).forEach((w) => {
    canon.push(w); lineOf.push(lineBase + li);
  }));
  const clean = `/tmp/_solafiya_kw_${voice}.wav`;
  cleanConcat(inputs, clean);
  const dur = probe(clean);
  const ww = await whisper(clean, lineSet.join(". "));
  console.log(`  voice ${voice}: whisper ${ww.length} words → aligning ${canon.length} canonical (clean ${dur.toFixed(1)}s)`);
  const win = alignWords(canon, ww);
  const out = [];
  let prev = 0;
  for (let i = 0; i < canon.length; i++) {
    let s = (win[i]?.fromMs ?? prev * 1000) / 1000;
    let e = (win[i]?.toMs ?? (s + 0.3) * 1000) / 1000;
    if (!(e > s)) e = s + 0.28;
    s = Math.max(prev, Math.min(s, dur));
    e = Math.max(s + 0.12, Math.min(e, dur));
    out.push({ word: canon[i], start: s, end: e, line: lineOf[i], voice });
    prev = s;
  }
  return out;
}

// ONE voice, all verses SEQUENTIAL: align the full lyric set against
// the combined verse1+verse2+verse3 clean (same concat as render.mjs).
const v1 = `${G}/Circuito Coto Amate 2.m4a`;
const v2 = `${G}/Circuito Coto Amate 3.m4a`;
const v3 = `${G}/Circuito Coto Amate 4.m4a`;
const verses = [v1, v2, v3].filter(existsSync);
const ALL = [...LINES_A, ...LINES_B];
const all = await alignVoice("A", ALL, verses, 0);
const dst = `${LANE}/out/${SLUG}-fia-words.json`;
writeFileSync(dst, JSON.stringify(all, null, 1));
console.log(`✓ ${all.length} karaoke words (${ALL.length} lines, ${verses.length} verses sequential) → ${dst.replace(REPO + "/", "")}`);
