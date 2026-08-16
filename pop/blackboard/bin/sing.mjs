#!/usr/bin/env node
// sing.mjs — jeffrey SINGS Tim Ingold's blackboard passage (pop/blackboard).
//
// The spinging/aesthetivox chain, exactly as its first caller
// (pop/menuband/bin/sing-jingle.mjs, round 6.5) runs it — explicit mode:
//
//   1 · TTS through /api/say (provider "jeffrey", stability 0.55 ≥ 0.5 —
//       identity; per-line mp3 cached by text hash, never re-spent)
//   2 · whisper-cli -ml 1 word boundaries, reconciled by
//       spinging/lib/align-words.mjs (+ presplit / rescale / repair guards)
//   3 · CHORAL NOTATION (spinging/lib/notation.mjs): syllable underlay +
//       {onset, nucleus, coda} phonemes from curated GenAm IPA per note
//   4 · spinging/lib/sing_line_world.py — line-continuous WORLD synthesis,
//       harmony lock 0.875, per-line octave fit + register ladder, legato
//       bridging, consonant stretching, self-choir gated to vowels;
//       percentile-conformance + click-scan QA gate per render, whisper
//       round-trip WER logged per line (tracked metric, not a blocker here —
//       fallback registers still trade against it)
//   5 · lines placed at absolute time → vocal stem; reverb halo
//       (spinging/lib/vocal_bus.py) on the vocal bus
//   6 · mix over out/blackboard-bed.wav (consonant-span duck + level
//       sidechain), click scan, then 2-pass measured loudnorm to
//       -14 LUFS / -1 dBTP (spinging SING_TARGET) → out/blackboard.{wav,mp3}
//
// Per-line renders are cached in out/sung/blackboard/words/ with a QA
// sidecar keyed by a plan hash — re-running reuses finished lines unless
// their spec changed or --fresh is passed. --only v1,hook-2 re-renders
// just those ids.
//
// Run:  node pop/blackboard/bin/sing.mjs [--fresh] [--only id,id] [--harmony 0.875]

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, mkdirSync, copyFileSync, readdirSync }
  from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import { alignWords } from "../../../spinging/lib/align-words.mjs";
import { buildLineScore, writeLineScore } from "../../../spinging/lib/notation.mjs";
import { sourceCounts } from "../../../spinging/lib/pronounce.mjs";
import { decodeAudioMono } from "../../lib/preview-shared.mjs";
import { LINES, DURATION_S, SR } from "./score.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const STEMS = `${LANE}/stems`;
const POP = resolve(LANE, "..");
const REPO = resolve(POP, "..");
const SPINGING = `${REPO}/spinging`;
const VENV_PY = `${POP}/.venv/bin/python`;
const WORLD_HELPER = `${SPINGING}/lib/sing_line_world.py`;
const VOCAL_BUS = `${SPINGING}/lib/vocal_bus.py`;
const GOALPOSTS = `${SPINGING}/cache/goalposts.json`;
const WHISPER_MODEL = `${process.env.HOME}/.whisper-models/ggml-base.en.bin`;
const SAY_URL = "https://aesthetic.computer/api/say";
const STABILITY = 0.55;       // ≥ 0.5 keeps jeffrey's identity
const SLUG = "blackboard";

const argv = process.argv.slice(2);
const hIdx = argv.indexOf("--harmony");
const HARMONY = hIdx >= 0 ? parseFloat(argv[hIdx + 1]) : 0.875;
const FRESH = argv.includes("--fresh");
const oIdx = argv.indexOf("--only");
const ONLY = oIdx >= 0 ? new Set(argv[oIdx + 1].split(",")) : null;
// --prep N: render at most N fresh lines this invocation, then exit before
// assembly/mix (chunked execution on the 8 GB box — every finished line is
// cached, so repeated runs walk the whole score and the last run assembles).
const pIdx = argv.indexOf("--prep");
const PREP_N = pIdx >= 0 ? parseInt(argv[pIdx + 1], 10) : Infinity;
let freshRendered = 0;
let prepStopped = false;

const QA_PASSES = 3;          // re-render budget per line (percentile gate)
const CLARITY_PASSES = 2;     // extra re-renders if the whisper WER gate fails
const BRIDGE_MAX_S = 0.45;    // keep in sync with sing_line_world.BRIDGE_MAX_S
const WER_GATE = 0.25;        // per-line whisper round-trip ceiling (logged)
const CONTINUITY_GATE = 0.95; // intra-phrase voicing continuity floor

// ── helpers (adopted from sing-jingle.mjs round 6.5) ───────────────────────
function adjustTweaks(tweaks, conf) {
  if (!conf) return;
  const miss = (k) => (conf[k] && conf[k].pass === false ? conf[k] : null);
  let m;
  if ((m = miss("plateau_drift_cents"))) {
    if (m.value > m.hi) { tweaks.drift_scale *= 0.55; tweaks.beta_scale *= 0.75; }
    else tweaks.drift_scale *= 1.6;
  }
  if ((m = miss("onset_glide_ms")) || (m = miss("onset_glide_cents"))) {
    tweaks.glide_scale *= m.value > m.hi ? 0.7 : 1.35;
  }
  if ((m = miss("release_cents"))) {
    if (m.value > m.hi) tweaks.beta_scale *= 0.8;
  }
  if ((m = miss("vib_depth_cents"))) {
    tweaks.vib_depth_scale *= m.value > m.hi ? 0.6 : 1.5;
  }
  if ((m = miss("hf_ratio"))) {
    tweaks.air_scale *= m.value > m.hi ? 0.6 : 1.5;
  }
}

function measureLoudnorm(file) {
  const r = sh("ffmpeg", ["-y", "-i", file, "-af",
    "loudnorm=I=-14:TP=-1.5:LRA=11:print_format=json", "-f", "null", "-"]);
  const m = r.stderr.toString().match(/\{[^{}]*"input_i"[\s\S]*?\}/);
  if (!m) throw new Error(`loudnorm measure pass printed no JSON for ${file}`);
  return JSON.parse(m[0]);
}

const sh = (cmd, args, opts = {}) => {
  const r = spawnSync(cmd, args, { encoding: "utf8", maxBuffer: 256 * 1024 * 1024, ...opts });
  if (r.status !== 0) throw new Error(`${cmd} ${args[0]}: ${r.stderr?.toString().slice(0, 400)}`);
  return r;
};

function writeWavF32(path, samples, sr = SR) {
  const n = samples.length;
  const buf = Buffer.alloc(44 + n * 4);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 4, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(sr, 24); buf.writeUInt32LE(sr * 4, 28);
  buf.writeUInt16LE(4, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36); buf.writeUInt32LE(n * 4, 40);
  for (let i = 0; i < n; i++) buf.writeFloatLE(samples[i], 44 + i * 4);
  writeFileSync(path, buf);
}

async function ttsLine(text, outFile, attempt = 0) {
  if (existsSync(outFile)) return;
  try {
    const res = await fetch(SAY_URL, {
      method: "POST",
      headers: { "Content-Type": "application/json", Origin: "https://aesthetic.computer" },
      body: JSON.stringify({ from: text, provider: "jeffrey", stability: STABILITY }),
      redirect: "follow",
    });
    if (!res.ok) throw new Error(`/api/say ${res.status}: ${(await res.text()).slice(0, 300)}`);
    const ctype = res.headers.get("content-type") || "";
    let buf;
    if (ctype.includes("application/json")) {
      const json = await res.json();
      if (json.audio) buf = Buffer.from(json.audio, "base64");
      else if (json.url) buf = Buffer.from(await (await fetch(json.url)).arrayBuffer());
      else throw new Error(`/api/say JSON without audio`);
    } else buf = Buffer.from(await res.arrayBuffer());
    if (!buf || buf.length < 256) throw new Error(`/api/say tiny body (${buf?.length})`);
    writeFileSync(outFile, buf);
  } catch (e) {
    if (attempt >= 2) throw e;
    console.log(`  … /api/say hiccup (${String(e.message).slice(0, 60)}), retrying`);
    await new Promise((r) => setTimeout(r, 2500 * (attempt + 1)));
    await ttsLine(text, outFile, attempt + 1);
  }
}

// whisper.cpp -ml 1 tokens → words (punctuation merges TEXT, never TIME)
const punctOnly = (s) => /^[^\w'"“”‘’(]+$/.test(s);
function wordsFromWhisper(jsonPath) {
  const raw = JSON.parse(readFileSync(jsonPath, "utf8"));
  const words = [];
  for (const seg of raw.transcription) {
    const piece = seg.text;
    if (!piece || !piece.trim()) continue;
    const text = piece.trim();
    if (piece.startsWith(" ") || words.length === 0) {
      words.push({ text, fromMs: seg.offsets.from, toMs: seg.offsets.to });
    } else {
      const prev = words[words.length - 1];
      prev.text += text;
      if (!punctOnly(text)) prev.toMs = seg.offsets.to;
    }
  }
  const merged = [];
  for (const w of words) {
    if (punctOnly(w.text) && merged.length > 0) merged[merged.length - 1].text += w.text;
    else merged.push(w);
  }
  return merged;
}

const norm = (s) => (s || "").toLowerCase().replace(/[^a-z']/g, "");

function editDist(a, b) {
  const m = a.length, n = b.length;
  let prev = Array.from({ length: n + 1 }, (_, j) => j);
  for (let i = 1; i <= m; i++) {
    const curr = [i];
    for (let j = 1; j <= n; j++) {
      curr[j] = a[i - 1] === b[j - 1] ? prev[j - 1] : 1 + Math.min(prev[j - 1], prev[j], curr[j - 1]);
    }
    prev = curr;
  }
  return prev[n];
}

// ── whisper round-trip WER (round 4 machinery) ─────────────────────────────
const STOPWORDS = new Set(["a", "an", "the", "in", "on", "of", "to", "for",
  "your", "its", "is", "and", "at", "up", "out", "that"]);
const HOMOPHONES = {
  to: "two", too: "two", for: "four", fore: "four",
  won: "one", knew: "new", oar: "or", ore: "or", write: "right", rite: "right",
  seen: "screen",   // whisper drops initial s-clusters on sung lines sometimes
};
const CONTRACTIONS = { wanna: ["want", "to"], gonna: ["going", "to"], "i'm": ["im"] };
function normTokens(text) {
  const rough = String(text).toLowerCase()
    .replace(/[’‘]/g, "'")
    .replace(/[.…]+/g, " ")
    .replace(/[^a-z0-9' ]+/g, " ")
    .split(/\s+/).filter(Boolean);
  const out = [];
  for (const w of rough) {
    if (CONTRACTIONS[w]) out.push(...CONTRACTIONS[w]);
    else out.push(w.replace(/'/g, ""));
  }
  return out.map((w) => HOMOPHONES[w] || w);
}
const tokEq = (a, b) =>
  a === b || (a.length >= 3 && b.length >= 3 && editDist(a, b) <= 1);
function rewedge(refToks, hypToks) {
  const out = [];
  for (let i = 0; i < hypToks.length; i++) {
    let merged = false;
    for (let k = 4; k >= 2 && !merged; k--) {
      if (i + k > hypToks.length) continue;
      const cat = hypToks.slice(i, i + k).join("");
      if (refToks.some((r) => tokEq(r, cat))) { out.push(cat); i += k - 1; merged = true; }
    }
    if (!merged) out.push(hypToks[i]);
  }
  return out;
}
function dewedge(refToks, hypToks) {
  const out = [];
  let i = 0;
  for (const h of hypToks) {
    let done = false;
    for (let s = i; s < refToks.length && !done; s++) {
      if (tokEq(refToks[s], h)) { out.push(refToks[s]); i = s + 1; done = true; break; }
      for (let k = 2; k <= 4 && s + k <= refToks.length; k++) {
        const cat = refToks.slice(s, s + k).join("");
        if (editDist(cat, h) <= Math.max(1, Math.ceil(cat.length / 5))) {
          out.push(...refToks.slice(s, s + k));
          i = s + k; done = true; break;
        }
      }
    }
    if (!done) out.push(h);
  }
  return out;
}
function werScore(refToks, hypToks) {
  const m = refToks.length, n = hypToks.length;
  let prev = Array.from({ length: n + 1 }, (_, j) => j);
  for (let i = 1; i <= m; i++) {
    const curr = [i];
    for (let j = 1; j <= n; j++) {
      const sub = (tokEq(refToks[i - 1], hypToks[j - 1]) ? 0 : 1) + prev[j - 1];
      curr[j] = Math.min(sub, 1 + prev[j], 1 + curr[j - 1]);
    }
    prev = curr;
  }
  return m ? prev[n] / m : 0;
}
function evalWER(refText, hypText) {
  const ref = normTokens(refText);
  const hyp = dewedge(ref, rewedge(ref, normTokens(hypText)));
  const wer = +werScore(ref, hyp).toFixed(3);
  const missing = ref.filter((w) =>
    w.length >= 3 && !STOPWORDS.has(w) && !hyp.some((h) => tokEq(w, h)));
  return { wer, missing, pass: wer <= WER_GATE && missing.length === 0 };
}

function whisperTranscribe(audioPath) {
  const wav16 = audioPath.replace(/\.(wav|mp3)$/, "") + "-tx16k.wav";
  sh("ffmpeg", ["-y", "-v", "error", "-i", audioPath, "-ac", "1", "-ar", "16000", wav16]);
  const oPath = wav16.replace(/\.wav$/, "");
  sh("whisper-cli", ["-m", WHISPER_MODEL, "-f", wav16, "-oj", "-of", oPath],
    { stdio: ["ignore", "ignore", "pipe"] });
  const j = JSON.parse(readFileSync(`${oPath}.json`, "utf8"));
  return j.transcription.map((s) => s.text).join(" ").replace(/\s+/g, " ").trim();
}
function whisperTranscribeSpan(audioPath, t0, t1, outBase) {
  const seg = `${outBase}.wav`;
  sh("ffmpeg", ["-y", "-v", "error", "-i", audioPath,
    "-ss", Math.max(0, t0).toFixed(3), "-to", t1.toFixed(3),
    "-ac", "1", "-ar", "16000", seg]);
  sh("whisper-cli", ["-m", WHISPER_MODEL, "-f", seg, "-oj", "-of", outBase],
    { stdio: ["ignore", "ignore", "pipe"] });
  const j = JSON.parse(readFileSync(`${outBase}.json`, "utf8"));
  return j.transcription.map((s) => s.text).join(" ").replace(/\s+/g, " ").trim();
}

// pre-split heard words that weld consecutive score words ("powerpoint")
function presplitHeard(scoreWords, heard) {
  const out = [];
  let i = 0;
  for (const h of heard) {
    const hn = norm(h.text);
    const singleDist = i < scoreWords.length ? editDist(norm(scoreWords[i]), hn) : Infinity;
    let best = 0, bestDist = singleDist;
    for (let k = 2; k <= 4 && i + k <= scoreWords.length; k++) {
      const cat = scoreWords.slice(i, i + k).map(norm).join("");
      const d = editDist(cat, hn);
      if (d <= Math.max(1, Math.ceil(cat.length / 4)) && d <= bestDist && d < singleDist) {
        best = k; bestDist = d;
      }
    }
    if (best >= 2) {
      const parts = scoreWords.slice(i, i + best).map(norm);
      const chars = parts.reduce((s, p) => s + Math.max(1, p.length), 0);
      let t0 = h.fromMs;
      const span = h.toMs - h.fromMs;
      for (const p of parts) {
        const w = (span * Math.max(1, p.length)) / chars;
        out.push({ text: p, fromMs: Math.round(t0), toMs: Math.round(t0 + w) });
        t0 += w;
      }
      i += best;
    } else {
      out.push(h);
      if (i < scoreWords.length) {
        const sn = norm(scoreWords[i]);
        if (sn === hn || editDist(sn, hn) <= 2 || (sn.length >= 3 && hn.startsWith(sn))) i++;
      }
    }
  }
  return out;
}

function energySplit(audio, aMs, bMs, n) {
  const hop = Math.floor(SR / 100);
  const a = Math.max(0, Math.floor((aMs / 1000) * SR));
  const b = Math.min(audio.length, Math.floor((bMs / 1000) * SR));
  let env = [];
  for (let s = a; s + hop <= b; s += hop) {
    let e = 0;
    for (let k = s; k < s + hop; k++) e += audio[k] * audio[k];
    env.push(Math.sqrt(e / hop));
  }
  const thr = Math.max(0.008, 0.15 * Math.max(...env, 0));
  let f = env.findIndex((e) => e >= thr);
  let l = env.length - 1;
  while (l > 0 && env[l] < thr) l--;
  if (f > 0 || l < env.length - 1) {
    f = Math.max(0, f - 2); l = Math.min(env.length - 1, l + 4);
    aMs = aMs + f * 10; bMs = aMs + (l - f + 1) * 10;
    env = env.slice(f, l + 1);
  }
  const bounds = [Math.round(aMs)];
  if (n > 1) {
    const lo = Math.floor(env.length * 0.12), hi = Math.ceil(env.length * 0.94);
    const minSep = Math.max(3, Math.floor(env.length / (n * 2)));
    const cand = [];
    for (let k = lo; k < hi; k++) cand.push([env[k], k]);
    cand.sort((x, y) => x[0] - y[0]);
    const picks = [];
    for (const [, k] of cand) {
      if (picks.length >= n - 1) break;
      if (picks.every((p) => Math.abs(p - k) >= minSep)) picks.push(k);
    }
    while (picks.length < n - 1) picks.push(Math.floor(((picks.length + 1) / n) * env.length));
    picks.sort((x, y) => x - y);
    for (const p of picks) bounds.push(Math.round(aMs + p * 10));
  }
  bounds.push(Math.round(bMs));
  return bounds;
}

function rescaleHeard(heard, audio, lineLenMs) {
  if (heard.length < 2) return heard;
  const hop = Math.floor(SR / 100);
  const env = [];
  for (let s = 0; s + hop <= audio.length; s += hop) {
    let e = 0;
    for (let k = s; k < s + hop; k++) e += audio[k] * audio[k];
    env.push(Math.sqrt(e / hop));
  }
  const thr = Math.max(0.008, 0.15 * Math.max(...env, 0));
  let f = env.findIndex((e) => e >= thr);
  let l = env.length - 1;
  while (l > 0 && env[l] < thr) l--;
  const speechStart = Math.max(0, f * 10 - 20);
  const speechEnd = Math.min(lineLenMs, (l + 1) * 10 + 40);
  if (heard[heard.length - 2].toMs <= speechEnd + 120) return heard;
  const wStart = heard[0].fromMs;
  const wEnd = Math.max(...heard.map((h) => h.toMs));
  const scale = (speechEnd - speechStart) / Math.max(1, wEnd - wStart);
  for (const h of heard) {
    h.fromMs = Math.round(speechStart + (h.fromMs - wStart) * scale);
    h.toMs = Math.round(speechStart + (h.toMs - wStart) * scale);
  }
  return heard;
}

function repairWindows(windows, lineLenMs, audio) {
  for (const w of windows) {
    w.toMs = Math.min(w.toMs, lineLenMs);
    w.fromMs = Math.min(w.fromMs, Math.max(0, w.toMs - 40));
  }
  let i = 0;
  while (i < windows.length) {
    if (windows[i].toMs - windows[i].fromMs >= 60) { i++; continue; }
    let j = i;
    while (j < windows.length && windows[j].toMs - windows[j].fromMs < 60) j++;
    const host = i > 0 ? windows[i - 1] : null;
    const hostStart = host ? host.fromMs : 0;
    if (j >= windows.length && host) {
      const tail = energySplit(audio, host.toMs, lineLenMs, j - i);
      const speechMs = tail[tail.length - 1] - tail[0];
      if (speechMs >= 120 * (j - i)) {
        for (let w = i, k = 0; w < j; w++, k++) {
          windows[w].fromMs = tail[k]; windows[w].toMs = tail[k + 1];
        }
        i = j; continue;
      }
    }
    const hostEnd = j >= windows.length ? lineLenMs
      : Math.min(lineLenMs, Math.max(windows[j - 1].toMs, host ? host.toMs : 0));
    const parts = (host ? 1 : 0) + (j - i);
    const bounds = energySplit(audio, hostStart, hostEnd, parts);
    let k = 0;
    if (host) { host.fromMs = bounds[0]; host.toMs = bounds[1]; k = 1; }
    for (let w = i; w < j; w++, k++) { windows[w].fromMs = bounds[k]; windows[w].toMs = bounds[k + 1]; }
    i = j;
  }
  for (let k = 1; k < windows.length; k++) {
    const a = windows[k - 1], b = windows[k];
    if (b.fromMs < a.toMs) {
      const mid = Math.round((Math.max(b.fromMs, a.fromMs) + a.toMs) / 2);
      a.toMs = Math.max(a.fromMs + 40, mid);
      b.fromMs = Math.min(a.toMs, Math.max(b.toMs - 40, mid));
    }
  }
  return windows;
}

// ── main ───────────────────────────────────────────────────────────────────
console.log(`▸ ${SLUG} — jeffrey sings Ingold (harmony lock ${HARMONY}, β = ${(1 - HARMONY).toFixed(3)})`);
const dir = `${OUT}/sung/${SLUG}`;
mkdirSync(`${dir}/words`, { recursive: true });
mkdirSync(STEMS, { recursive: true });
if (!existsSync(WHISPER_MODEL)) throw new Error(`whisper model missing: ${WHISPER_MODEL}`);
if (!existsSync(VENV_PY)) throw new Error(`pop venv missing: ${VENV_PY}`);
if (!existsSync(GOALPOSTS)) throw new Error(`goalposts missing: ${GOALPOSTS}`);
const BED = `${OUT}/${SLUG}-bed.wav`;
if (!existsSync(BED)) throw new Error(`bed missing: ${BED} — run render-bed.mjs first`);

const masterFull = new Float32Array(Math.ceil(DURATION_S * SR));
const masterLead = new Float32Array(Math.ceil(DURATION_S * SR));
const sungWords = [];
const qaLines = [];
const lineSpans = [];
const consSpans = [];
const regFallbacks = [];

// flatten words across lines for next-word lookahead
const flat = [];
LINES.forEach((line, li) => line.words.forEach(([w, slots]) => flat.push({ w, slots, li })));

for (let li = 0; li < LINES.length; li++) {
  const line = LINES[li];
  const regLadder = [line.register, ...(line.fallbacks ?? []).filter((r) => r < line.register)];
  const planHash = createHash("sha1")
    .update(JSON.stringify({ tts: line.tts, words: line.words, reg: regLadder, h: HARMONY }))
    .digest("hex").slice(0, 10);
  const hash = createHash("sha1").update(line.tts).digest("hex").slice(0, 8);
  const mp3 = `${dir}/line-${li}-${hash}.mp3`;
  const outWav = `${dir}/words/line-${li}-sung.wav`;
  const leadWav = `${dir}/words/line-${li}-lead.wav`;
  const qaPath = `${dir}/words/line-${li}-qa.json`;

  // ── reuse a finished line unless asked otherwise ─────────────────────────
  const wantFresh = FRESH || (ONLY && ONLY.has(line.id));
  if (!wantFresh && existsSync(outWav) && existsSync(leadWav) && existsSync(qaPath)) {
    const qa = JSON.parse(readFileSync(qaPath, "utf8"));
    if (qa.planHash === planHash) {
      console.log(`  line ${li} [${line.id}]: reusing cached render (WER ${qa.whisper.wer})`);
      qaLines.push(qa.qaLine);
      lineSpans.push(qa.lineSpan);
      consSpans.push(...qa.consSpans);
      sungWords.push(...qa.sungWords);
      if (qa.regFallback) regFallbacks.push(qa.regFallback);
      const { audio: sung } = decodeAudioMono(outWav, SR);
      const { audio: lead } = decodeAudioMono(leadWav, SR);
      const at = Math.floor(qa.lineT0 * SR);
      for (let i = 0; i < sung.length && at + i < masterFull.length; i++) masterFull[at + i] += sung[i];
      for (let i = 0; i < lead.length && at + i < masterLead.length; i++) masterLead[at + i] += lead[i];
      continue;
    }
    console.log(`  line ${li} [${line.id}]: spec changed (plan hash) — re-rendering`);
  }
  if (freshRendered >= PREP_N) { prepStopped = true; continue; }
  freshRendered++;

  // TTS — adopt any cached take of the same text (the three hooks share one)
  if (!existsSync(mp3)) {
    const prior = readdirSync(dir).find((f) => new RegExp(`^line-\\d+-${hash}\\.mp3$`).test(f));
    if (prior) {
      console.log(`  line ${li} [${line.id}]: adopting cached take ${prior}`);
      copyFileSync(`${dir}/${prior}`, mp3);
    }
  }
  await ttsLine(line.tts, mp3);

  const w16 = mp3.replace(/\.mp3$/, "-16k.wav");
  if (!existsSync(w16)) sh("ffmpeg", ["-y", "-v", "error", "-i", mp3, "-ac", "1", "-ar", "16000", w16]);
  const w48 = mp3.replace(/\.mp3$/, "-48k.wav");
  if (!existsSync(w48)) sh("ffmpeg", ["-y", "-v", "error", "-i", mp3, "-ac", "1", "-ar", String(SR), w48]);
  const wj = mp3.replace(/\.mp3$/, "-words");
  if (!existsSync(`${wj}.json`)) {
    sh("whisper-cli", ["-m", WHISPER_MODEL, "-f", w16, "-ml", "1", "-oj", "-ojf", "-of", wj],
      { stdio: ["ignore", "ignore", "pipe"] });
  }
  const { audio: lineAudio } = decodeAudioMono(mp3, SR);
  const lineLen = lineAudio.length / SR;
  const mapWords = line.words.map(([w]) => w);
  const heard = presplitHeard(mapWords,
    rescaleHeard(wordsFromWhisper(`${wj}.json`), lineAudio, lineLen * 1000));
  const windows = repairWindows(alignWords(mapWords, heard), lineLen * 1000, lineAudio);
  console.log(`  line ${li} [${line.id}]: "${line.tts}" · whisper heard "${heard.map((h) => h.text).join(" ")}"`);

  // choral notation sidecar — phrase grouping + curated GenAm IPA phonemes
  const score = await buildLineScore({
    text: line.tts,
    words: line.words.map(([w, slots]) => ({ w, slots })),
  });
  const scorePath = `${dir}/words/line-${li}-score.json`;
  writeLineScore(scorePath, score);
  const phraseStartOf = new Array(line.words.length).fill(false);
  for (const n of score.notes) {
    if (n.syllableIndex === 0 && n.articulation === "phraseStart") phraseStartOf[n.wordIndex] = true;
  }

  // plan words with padded source windows
  const planWords = [];
  const lineSungWords = [];
  for (let wi = 0; wi < line.words.length; wi++) {
    const [w, slots] = line.words[wi];
    const win = windows[wi];
    const tStart = slots[0].t;
    const last = slots[slots.length - 1];
    const globalIdx = LINES.slice(0, li).reduce((s, l) => s + l.words.length, 0) + wi;
    const next = flat[globalIdx + 1];
    let tEnd = last.t + Math.min(last.dur, 1.8);
    if (next) tEnd = Math.min(tEnd, next.slots[0].t - (next.li === li ? 0.01 : 0.12));
    if (tEnd <= tStart + 0.1) tEnd = tStart + 0.1;
    const prevWin = wi > 0 ? windows[wi - 1] : null;
    const nextWin = wi + 1 < windows.length ? windows[wi + 1] : null;
    let s0 = win.fromMs - 60;
    let s1 = win.toMs + 100;
    if (prevWin) s0 = Math.max(s0, (prevWin.toMs + win.fromMs) / 2);
    if (nextWin) s1 = Math.min(s1, (win.toMs + nextWin.fromMs) / 2 + 20);
    s0 = Math.max(0, s0); s1 = Math.min(lineLen * 1000, s1);
    planWords.push({
      w, wordIndex: wi, srcFromMs: Math.round(s0), srcToMs: Math.round(s1),
      slots, hardEnd: +tEnd.toFixed(4), phraseStart: phraseStartOf[wi],
    });
    lineSungWords.push({
      text: w, fromMs: Math.round(tStart * 1000), toMs: Math.round(tEnd * 1000), line: li,
    });
  }
  // karaoke windows ride the legato bridges
  for (let wi = 0; wi + 1 < line.words.length; wi++) {
    if (phraseStartOf[wi + 1]) continue;
    const cur = lineSungWords[wi], nxt = lineSungWords[wi + 1];
    const gap = nxt.fromMs - cur.toMs;
    if (gap > 0 && gap <= BRIDGE_MAX_S * 1000) cur.toMs = nxt.fromMs;
  }
  sungWords.push(...lineSungWords);

  const lineT0 = Math.max(0, planWords[0].slots[0].t - 0.35);
  const lineT1 = Math.min(DURATION_S, planWords[planWords.length - 1].hardEnd + 0.4);
  const planPath = `${dir}/words/line-${li}-plan.json`;

  const tweaks = { drift_scale: 1.6, glide_scale: 1, vib_depth_scale: 1, beta_scale: 1,
    air_scale: 1, cons_stretch_scale: 1 };
  const renderPlan = (register) => {
    const plan = {
      line_wav: w48, out_wav: outWav, lead_wav: leadWav,
      phoneme_sidecar: mp3.replace(/\.mp3$/, ".phonemes.json"),
      score: scorePath, goalposts: GOALPOSTS,
      line_t0: +lineT0.toFixed(4), line_t1: +lineT1.toFixed(4),
      harmony: HARMONY, seed: 7 + li,
      f0_floor: 60, f0_ceil: 300,
      octave_opt: true, choir: true, register, tweaks,
      words: planWords,
    };
    writeFileSync(planPath, JSON.stringify(plan, null, 1));
    const wr = sh(VENV_PY, [WORLD_HELPER, planPath]);
    let st = {};
    try { st = JSON.parse(wr.stdout.trim().split("\n").pop()); } catch {}
    return st;
  };

  // render loop: percentile gate + click scan, whisper round-trip, register
  // ladder; every render is transcribed and the best take wins (conformance
  // first, then WER)
  const bestWav = `${dir}/words/line-${li}-sung-best.wav`;
  const bestLead = `${dir}/words/line-${li}-lead-best.wav`;
  const confOf = (s) => s?.conformance?._pass !== false;
  const best = { res: null, transcript: "", stats: null, conf: false };
  let lastIsBest = false;
  const consider = (st) => {
    const tx = whisperTranscribe(outWav);
    const wr = evalWER(line.tts, tx);
    const cf = confOf(st);
    const wins = best.stats === null
      || (wr.wer < best.res.wer && (cf || !best.conf))
      || (wr.wer <= best.res.wer && cf && !best.conf);
    if (wins) {
      Object.assign(best, { res: wr, transcript: tx, stats: st, conf: cf });
      copyFileSync(outWav, bestWav);
      copyFileSync(leadWav, bestLead);
    }
    lastIsBest = wins;
    return wr;
  };
  let stats = {};
  let passes = 0, clarityPasses = 0;
  const registersTried = [];
  for (const reg of regLadder) {
    registersTried.push(reg);
    Object.assign(tweaks, { drift_scale: 1.6, glide_scale: 1, vib_depth_scale: 1,
      beta_scale: 1, air_scale: 1, cons_stretch_scale: 1 });
    for (let pass = 1; pass <= QA_PASSES; pass++) {
      passes++;
      stats = renderPlan(reg);
      if (stats.error) break;
      const clean = stats.clicks && stats.clicks.clicks === 0 && stats.clicks.flux_spikes === 0;
      if ((!stats.conformance || stats.conformance._pass) && clean) { consider(stats); break; }
      consider(stats);
      if (pass === QA_PASSES) break;
      adjustTweaks(tweaks, stats.conformance);
      console.log(`    ↻ pass ${pass} (reg +${reg}): out of band — retweak ` +
        Object.entries(tweaks).map(([k, v]) => `${k}=${v.toFixed(2)}`).join(" "));
    }
    if (stats.error) break;
    for (let cp = 1; cp <= CLARITY_PASSES && !best.res.pass; cp++) {
      clarityPasses++;
      tweaks.air_scale *= 0.6;
      tweaks.vib_depth_scale *= 0.7;
      tweaks.cons_stretch_scale = Math.min(1.3, tweaks.cons_stretch_scale * 1.15);
      console.log(`    ↻ clarity pass ${cp} (reg +${reg}): WER ${best.res.wer} ` +
        `(heard "${best.transcript}") — less air/vibrato, more stretch`);
      const st = renderPlan(reg);
      if (st.error) break;
      consider(st);
    }
    if (best.res?.pass) break;
    if (reg !== regLadder[regLadder.length - 1]) {
      console.log(`    ↧ register +${reg} missed the WER gate (${best.res?.wer}) — falling back`);
    }
  }
  if (stats.error) throw new Error(`line ${li} [${line.id}]: engine error — ${stats.error}`);
  if (!lastIsBest) {
    copyFileSync(bestWav, outWav);
    copyFileSync(bestLead, leadWav);
  }
  const werRes = best.res, transcript = best.transcript;
  stats = best.stats;
  const finalReg = best.stats?.register ?? regLadder[0];
  let regFallback = null;
  if (finalReg !== line.register) {
    regFallback = { line: li, id: line.id, text: line.tts,
      asked: line.register, used: finalReg, wer: best.res.wer };
    regFallbacks.push(regFallback);
    console.log(`    ⤵ line ${li} register fallback: +${line.register} → +${finalReg}`);
  }
  const leadTranscript = whisperTranscribe(leadWav);
  const leadWer = evalWER(line.tts, leadTranscript);
  const lineConsSpans = (stats.consonant_spans || []).map(([a, b]) => [lineT0 + a, lineT0 + b]);
  consSpans.push(...lineConsSpans);
  const cont = stats.voicing_continuity || {};
  const contPass = cont.min == null || cont.min >= CONTINUITY_GATE;
  if (!contPass) console.log(`  ⚠ line ${li}: voicing continuity ${cont.min} < ${CONTINUITY_GATE}`);
  if (!werRes.pass) console.log(`  ⚠ line ${li}: WER ${werRes.wer} above gate — logged, best take kept`);
  console.log(`    conf ${stats.conformance?._pass ? "PASS" : "miss"} · clicks ` +
    `${stats.clicks?.clicks}/${stats.clicks?.flux_spikes} · WER ${werRes.wer} · ` +
    `heard "${transcript}" · lead-WER ${leadWer.wer}`);
  const qaLine = {
    line: li, id: line.id, text: line.tts, passes, clarityPasses, tweaks,
    register: finalReg, registersTried, registerFallback: finalReg !== line.register,
    lineTranspose: stats.line_transpose, beta: stats.beta, harmony: HARMONY,
    consStretchScale: stats.cons_stretch_scale,
    f0JumpMaxCents: stats.f0_jump_max_cents, f0JumpP95Cents: stats.f0_jump_p95_cents,
    voicingContinuity: cont, voicingContinuityPass: contPass,
    voicedOnsetJumpMaxCents: stats.voiced_onset_jump_max_cents,
    whisper: { transcript, wer: werRes.wer, missing: werRes.missing, pass: werRes.pass },
    whisperLead: { transcript: leadTranscript, wer: leadWer.wer,
      missing: leadWer.missing, pass: leadWer.pass },
    conformance: stats.conformance, clicks: stats.clicks,
  };
  qaLines.push(qaLine);
  const lineSpan = {
    li, id: line.id, text: line.tts,
    t0: Math.max(0, planWords[0].slots[0].t - 0.15),
    t1: Math.min(DURATION_S, planWords[planWords.length - 1].hardEnd + 0.3),
  };
  lineSpans.push(lineSpan);

  // per-line QA cache — the run is resumable
  writeFileSync(qaPath, JSON.stringify({
    planHash, lineT0, qaLine, lineSpan, consSpans: lineConsSpans,
    sungWords: lineSungWords, regFallback,
    whisper: { wer: werRes.wer },
  }, null, 1));

  const { audio: sung } = decodeAudioMono(outWav, SR);
  const { audio: lead } = decodeAudioMono(leadWav, SR);
  const at = Math.floor(lineT0 * SR);
  for (let i = 0; i < sung.length && at + i < masterFull.length; i++) masterFull[at + i] += sung[i];
  for (let i = 0; i < lead.length && at + i < masterLead.length; i++) masterLead[at + i] += lead[i];
}

if (prepStopped) {
  console.log(`▸ --prep ${PREP_N}: rendered ${freshRendered} fresh line(s), more remain — run again`);
  process.exit(0);
}

// ── stems: full vocal, lead, choir (= full − lead), normalized together ────
let peak = 0;
for (let i = 0; i < masterFull.length; i++) peak = Math.max(peak, Math.abs(masterFull[i]));
const g = peak > 0 ? 0.85 / peak : 1;
const choir = new Float32Array(masterFull.length);
for (let i = 0; i < masterFull.length; i++) {
  masterFull[i] *= g;
  masterLead[i] *= g;
  choir[i] = masterFull[i] - masterLead[i];
}
const vocalWav = `${OUT}/${SLUG}-vocal.wav`;
writeWavF32(vocalWav, masterFull);
writeWavF32(`${STEMS}/vocal.wav`, masterFull);
writeWavF32(`${STEMS}/vocal-lead.wav`, masterLead);
writeWavF32(`${STEMS}/vocal-choir.wav`, choir);
copyFileSync(BED, `${STEMS}/instruments.wav`);

// reverb halo on the vocal bus (quiet wet, short decay)
const vocalWet = `${OUT}/${SLUG}-vocal-wet.wav`;
sh(VENV_PY, [VOCAL_BUS, "reverb", vocalWav, vocalWet, "-16", "1.1"]);

// consonant-span duck control track (the bed releases before each burst)
const bedDur = parseFloat(sh("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "csv=p=0", BED]).stdout.trim());
const duckWav = `${OUT}/${SLUG}-consduck.wav`;
{
  const ctrl = new Float32Array(masterFull.length);
  const ramp = Math.floor(0.006 * SR);
  for (const [a, b] of consSpans) {
    const s0 = Math.max(0, Math.floor((a - 0.025) * SR));
    const s1 = Math.min(ctrl.length, Math.ceil((b + 0.015) * SR));
    for (let i = s0; i < s1; i++) {
      let gg = 1;
      if (i - s0 < ramp) gg = (i - s0) / ramp;
      if (s1 - i < ramp) gg = Math.min(gg, (s1 - i) / ramp);
      ctrl[i] = Math.max(ctrl[i], 0.35 * gg);
    }
  }
  for (let i = 0; i < ctrl.length; i++) ctrl[i] *= Math.sin((2 * Math.PI * 400 * i) / SR);
  writeWavF32(duckWav, ctrl);
}

// ── mix: vocal bus over the bed (sing-jingle filtergraph) ──────────────────
const premaster = `${OUT}/${SLUG}-premaster.wav`;
sh("ffmpeg", ["-y", "-v", "error", "-i", BED, "-i", vocalWet, "-i", duckWav, "-filter_complex",
  "[1:a]aformat=sample_rates=48000:channel_layouts=stereo,highpass=f=70," +
  "acompressor=threshold=0.125:ratio=3:attack=12:release=250:makeup=2," +
  "deesser=i=0.15,asplit=2[sc][v];" +
  "[2:a]aformat=sample_rates=48000:channel_layouts=stereo[cd];" +
  "[0:a]aformat=sample_rates=48000:channel_layouts=stereo[b0];" +
  "[b0][cd]sidechaincompress=threshold=0.08:ratio=2:attack=5:release=90[b];" +
  "[b][sc]sidechaincompress=threshold=0.05:ratio=5:attack=12:release=220[duck];" +
  "[duck][v]amix=inputs=2:duration=first:normalize=0:weights=1 1.25[m];" +
  "[m]highpass=f=30,alimiter=limit=0.89:level=false,apad=pad_dur=2," +
  `atrim=0:${bedDur.toFixed(4)},asetpts=PTS-STARTPTS[out]`,
  "-map", "[out]", premaster]);

// hard glitch gate on the premaster
const mixScan = JSON.parse(
  sh(VENV_PY, [VOCAL_BUS, "scan", premaster]).stdout.trim().split("\n").pop());
if (mixScan.clicks > 0) {
  console.log(`  ⚠ click scan flagged ${mixScan.clicks} clicks at ${mixScan.positions_s}`);
}

// ── MASTER: 2-pass measured loudnorm (linear) to -14 LUFS / -1 dBTP ────────
const masterWavPath = `${OUT}/${SLUG}.wav`;
const masterMp3Path = `${OUT}/${SLUG}.mp3`;
const mjson = measureLoudnorm(premaster);
const lnFilter =
  `loudnorm=I=-14:TP=-1.5:LRA=11:measured_I=${mjson.input_i}:measured_TP=${mjson.input_tp}` +
  `:measured_LRA=${mjson.input_lra}:measured_thresh=${mjson.input_thresh}` +
  `:offset=${mjson.target_offset}:linear=true`;
sh("ffmpeg", ["-y", "-v", "error", "-i", premaster, "-af", lnFilter,
  "-ar", "48000", "-c:a", "pcm_s24le", masterWavPath]);
sh("ffmpeg", ["-y", "-v", "error", "-i", masterWavPath,
  "-c:a", "libmp3lame", "-q:a", "2", masterMp3Path]);
const verifyWav = measureLoudnorm(masterWavPath);
const verifyMp3 = measureLoudnorm(masterMp3Path);
console.log(`  mastered: ${mjson.input_i} LUFS / ${mjson.input_tp} dBTP → ` +
  `wav ${verifyWav.input_i} LUFS / ${verifyWav.input_tp} dBTP · ` +
  `mp3 ${verifyMp3.input_i} LUFS / ${verifyMp3.input_tp} dBTP`);

// ── whisper end-to-end: per-line WER on the vocal stem + one mix smoke ─────
console.log(`  whisper end-to-end on the vocal stem …`);
const stemLines = lineSpans.map((s) => {
  const tx = whisperTranscribeSpan(vocalWav, s.t0, s.t1, `${dir}/words/stem-line-${s.li}`);
  const w = evalWER(s.text, tx);
  console.log(`    ${w.pass ? "✓" : "✗"} L${s.li} [${s.id}] WER ${w.wer} → heard "${tx}"`);
  return { line: s.li, id: s.id, text: s.text, transcript: tx, wer: w.wer,
    missing: w.missing, pass: w.pass };
});
const mixTranscript = whisperTranscribe(masterMp3Path);
console.log(`  mix smoke transcript: "${mixTranscript}"`);

writeFileSync(`${OUT}/${SLUG}.words.sung.json`, JSON.stringify(sungWords, null, 2));
writeFileSync(`${OUT}/${SLUG}-sung-qa.json`, JSON.stringify({
  slug: SLUG, harmony: HARMONY, engine: "spinging/lib/sing_line_world.py (round 6.5)",
  goalposts: GOALPOSTS,
  gates: { werMax: WER_GATE, voicingContinuityMin: CONTINUITY_GATE,
    note: "whisper-WER is a tracked metric on this lane, not a blocker" },
  registerFallbacks: regFallbacks,
  consDuckSpans: consSpans.length,
  pronunciationSources: { ...sourceCounts },
  lines: qaLines,
  stemWhisper: stemLines,
  mixWhisperSmoke: mixTranscript,
  mixClickScan: mixScan,
  mastered: {
    wav: { lufs: parseFloat(verifyWav.input_i), truePeakDb: parseFloat(verifyWav.input_tp) },
    mp3: { lufs: parseFloat(verifyMp3.input_i), truePeakDb: parseFloat(verifyMp3.input_tp) },
  },
}, null, 1));
console.log(`✓ ${masterWavPath}`);
console.log(`✓ ${masterMp3Path}`);
console.log(`✓ ${OUT}/${SLUG}-sung-qa.json · pronunciations ${JSON.stringify(sourceCounts)}`);
