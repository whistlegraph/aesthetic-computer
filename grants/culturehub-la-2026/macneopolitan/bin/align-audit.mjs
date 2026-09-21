#!/usr/bin/env node
// align-audit.mjs — are the synthesizer's word onsets telling the truth?
//
// Every sung line starts from spoken TTS plus one onset per word, reported by
// AVSpeechSynthesizer's willSpeakRange callback. The core trusts those onsets
// completely: they decide which slice of audio becomes which note. If they
// drift, the wrong consonant lands on the wrong beat and the word is lost —
// which would explain why compact Samantha (7.8% WER) sings far clearer than
// Noelle/Allison Enhanced (18.5%), with the same notes and the same core.
//
// This reads the speech cache (SINGER_SPEECH_CACHE: pcm + fs + spans per
// voice·rate·text), writes each entry's audio to a wav, asks whisper for real
// per-word times, and reports the onset error per voice.
//
//   node bin/align-audit.mjs [--limit N] [--out align-audit.json]
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync, rmSync } from "node:fs";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..", "..");
const MODEL = process.env.WHISPER_MODEL
  || [resolve(REPO, "recap/models/ggml-small.en.bin"), resolve(REPO, "recap/models/ggml-base.en.bin")].find(existsSync);
const CACHE = process.env.SINGER_SPEECH_CACHE || resolve(tmpdir(), "mnp-hear", "speech-cache");
const argv = process.argv.slice(2);
const flag = (n, d) => { const i = argv.indexOf(n); return i >= 0 ? argv[i + 1] : d; };
const limit = Number(flag("--limit", 0)) || Infinity;
const outPath = resolve(LANE, "hear", flag("--out", "align-audit.json"));
const work = resolve(tmpdir(), "mnp-hear", "align-audit");
mkdirSync(work, { recursive: true });
mkdirSync(resolve(LANE, "hear"), { recursive: true });

const norm = (s) => String(s).toLowerCase().replace(/[^a-z0-9']/g, "");

// A cache entry does not record its voice — the key is a hash. Recover the
// voice by matching each entry's word count + duration against the scores'
// lyrics, per member. Cheaper and exact: the SPEECH is what we measure, and
// the entry's own sample rate + span count identify its shape; we group by
// (fs, nspans) and by which member's lyric it matches.
const lyrics = [];              // {member, voice, text, words}
for (const f of readdirSync(resolve(LANE, "scores")).filter((f) => f.endsWith(".mbscore"))) {
  const s = JSON.parse(readFileSync(resolve(LANE, "scores", f), "utf8"));
  for (const v of s.voices || []) {
    if (!v.lyrics) continue;
    const member = String(v.name).split(/\s|·/)[0];
    for (const line of v.lyrics.trim().split("/").map((x) => x.trim()).filter(Boolean)) {
      const words = line.split(/\s+/).map((t) => t.split("-").join(""));
      lyrics.push({ member, voice: v.singVoice || "?", text: words.join(" "), words });
    }
  }
}

function transcribeWords(wav) {
  const w16 = wav.replace(/\.wav$/, ".16k.wav");
  spawnSync("ffmpeg", ["-y", "-loglevel", "error", "-i", wav, "-ar", "16000", "-ac", "1", w16], { stdio: "ignore" });
  const r = spawnSync("whisper-cli", ["-m", MODEL, "-f", w16, "-l", "en", "-ml", "1", "-np", "-t", "4"], { encoding: "utf8" });
  rmSync(w16, { force: true });
  const out = [];
  for (const line of (r.stdout || "").split("\n")) {
    const m = line.match(/^\[(\d+):(\d+):([\d.]+) --> (\d+):(\d+):([\d.]+)\]\s*(.*)$/);
    if (!m) continue;
    const from = Number(m[1]) * 3600 + Number(m[2]) * 60 + Number(m[3]);
    const to = Number(m[4]) * 3600 + Number(m[5]) * 60 + Number(m[6]);
    const text = norm(m[7]);
    if (text) out.push({ from, to, text });
  }
  return out;
}

function writeWav(pcm, fs, path) {
  const n = pcm.length;
  const buf = Buffer.alloc(44 + n * 2);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 2, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20); buf.writeUInt16LE(1, 22);
  buf.writeUInt32LE(fs, 24); buf.writeUInt32LE(fs * 2, 28); buf.writeUInt16LE(2, 32); buf.writeUInt16LE(16, 34);
  buf.write("data", 36); buf.writeUInt32LE(n * 2, 40);
  for (let i = 0; i < n; i++) buf.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(pcm[i] * 32767))), 44 + i * 2);
  writeFileSync(path, buf);
}

const files = readdirSync(CACHE).filter((f) => f.endsWith(".json")).slice(0, limit === Infinity ? undefined : limit);
const rows = [];
let i = 0;
for (const f of files) {
  i++;
  let j;
  try { j = JSON.parse(readFileSync(resolve(CACHE, f), "utf8")); } catch { continue; }
  if (!j.pcm || !j.spans || !j.fs) continue;
  const dur = j.pcm.length / j.fs;
  const cand = lyrics.filter((l) => l.words.length === j.spans.length);
  const wav = resolve(work, f.replace(".json", ".wav"));
  writeWav(j.pcm, j.fs, wav);
  const heard = transcribeWords(wav);
  rmSync(wav, { force: true });
  // which lyric is this? the one whose words match the transcript best
  let best = null;
  for (const l of cand) {
    const want = l.words.map(norm);
    let hit = 0;
    for (const w of want) if (heard.some((h) => h.text === w)) hit++;
    if (!best || hit / want.length > best.score) best = { l, score: hit / want.length };
  }
  if (!best || best.score < 0.6) { rows.push({ file: f, dur, nwords: j.spans.length, matched: false }); continue; }
  // onset error: AVSpeech's span start vs whisper's first time for that word
  const want = best.l.words.map(norm);
  const errs = [];
  let cursor = 0;
  for (let k = 0; k < want.length; k++) {
    const hi = heard.findIndex((h, idx) => idx >= cursor && h.text === want[k]);
    if (hi < 0) continue;
    cursor = hi + 1;
    errs.push((j.spans[k][0] / j.fs) - heard[hi].from);
  }
  rows.push({ file: f, dur, nwords: want.length, matched: true, member: best.l.member, text: best.l.text,
    score: best.score, onsetErrMs: errs.map((e) => Math.round(e * 1000)),
    meanAbsMs: errs.length ? Math.round(errs.reduce((a, e) => a + Math.abs(e), 0) / errs.length * 1000) : null,
    biasMs: errs.length ? Math.round(errs.reduce((a, e) => a + e, 0) / errs.length * 1000) : null });
  if (i % 20 === 0) console.log(`  ${i}/${files.length}…`);
}
writeFileSync(outPath, JSON.stringify({ at: new Date().toISOString(), cache: CACHE, model: basename(MODEL), rows }, null, 2) + "\n");
const ok = rows.filter((r) => r.matched && r.meanAbsMs != null);
const by = {};
for (const r of ok) { (by[r.member] ??= []).push(r); }
console.log(`\n${ok.length}/${rows.length} cache entries aligned (model ${basename(MODEL)})`);
for (const [m, rs] of Object.entries(by)) {
  const mean = rs.reduce((a, r) => a + r.meanAbsMs, 0) / rs.length;
  const bias = rs.reduce((a, r) => a + r.biasMs, 0) / rs.length;
  const worst = rs.slice().sort((a, b) => b.meanAbsMs - a.meanAbsMs)[0];
  console.log(`  ${m.padEnd(10)} n=${String(rs.length).padStart(3)}  onset |err| ${mean.toFixed(0).padStart(4)} ms  bias ${bias > 0 ? "+" : ""}${bias.toFixed(0)} ms  worst ${worst.meanAbsMs} ms "${worst.text}"`);
}
console.log(`  → hear/${basename(outPath)}`);
