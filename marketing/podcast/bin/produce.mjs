#!/usr/bin/env node
// produce.mjs — essay → jeffrey-voiced reading (podcast episode).
//
//   [intro jingle] → "A reading of the essay: <Title>, by jeffrey.
//   Approximately <N> minutes." → <the essay, paragraph by paragraph, with
//   breath between> → "Here ends the reading." → [outro jingle]
//
// Voice: ElevenLabs jeffrey-pvc via /api/say (provider=jeffrey, voice=neutral:0),
// content-hash cached in out/cache/. The announced length is measured from the
// real synthesized body (ffprobe), not estimated.
//
// Usage:
//   node bin/produce.mjs ../../papers/essay-named-markets/named-markets.tex
//   node bin/produce.mjs ../../opinion/lotus-notes.md --open
//   flags: --open --force --stability 0.5 --similarity 0.8 --speed 1.0

import { writeFileSync, readFileSync, mkdirSync, existsSync, rmSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import { execFileSync, spawnSync } from "node:child_process";
import { essayToScript } from "./essay-to-script.mjs";
import { renderJingles, renderBed, BED_BPM } from "./jingle.mjs";
import { renderCover } from "./cover.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");

// ── args ────────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const nx = argv[i + 1];
    if (nx !== undefined && !nx.startsWith("--")) { flags[k] = nx; i++; } else flags[k] = true;
  } else positional.push(a);
}
if (!positional[0]) {
  console.error("usage: node bin/produce.mjs <essay.tex|.md> [--open] [--force]");
  process.exit(1);
}

const essayPath = resolve(process.cwd(), positional[0]);
if (!existsSync(essayPath)) { console.error(`✗ not found: ${essayPath}`); process.exit(1); }

const VOICE = {
  provider: flags.provider || "jeffrey",
  voice: flags.voice || "neutral:0",
  stability: flags.stability !== undefined ? Number(flags.stability) : 0.5,
  similarity: flags.similarity !== undefined ? Number(flags.similarity) : 0.8,
  speed: flags.speed !== undefined ? Number(flags.speed) : 1.0,
};
const FORCE = flags.force === true;
const SAY_URL = process.env.SAY_ENDPOINT || "https://aesthetic.computer/api/say";

const CACHE = resolve(ROOT, "out", "cache");
mkdirSync(CACHE, { recursive: true });

// ── /api/say with content-hash cache ─────────────────────────────────────
async function say(text, label) {
  const body = {
    from: text, provider: VOICE.provider, voice: VOICE.voice,
    stability: VOICE.stability, similarity: VOICE.similarity,
  };
  if (VOICE.speed !== 1.0) body.speed = Math.max(0.7, Math.min(1.2, VOICE.speed));
  const hash = createHash("sha256").update(JSON.stringify(body)).digest("hex").slice(0, 16);
  const out = resolve(CACHE, `${hash}.mp3`);
  if (!FORCE && existsSync(out) && readFileSync(out).length > 0) {
    process.stdout.write(`  · ${label} cached\n`);
    return out;
  }
  process.stdout.write(`  → ${label} (${text.length} chars) …`);
  // Retry transient network / 5xx failures (the essay is many small calls;
  // one dropped connection shouldn't sink the whole run).
  const ATTEMPTS = 4;
  let lastErr;
  for (let attempt = 1; attempt <= ATTEMPTS; attempt++) {
    try {
      const res = await fetch(SAY_URL, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body),
        redirect: "follow",
      });
      if (!res.ok) {
        if (res.status >= 500 && attempt < ATTEMPTS) throw new Error(`HTTP ${res.status}`);
        process.stdout.write(" ✗\n");
        throw new Error(`/api/say ${res.status}: ${(await res.text()).slice(0, 200)}`);
      }
      const ct = res.headers.get("content-type") || "";
      if (!ct.includes("audio") && !ct.includes("octet-stream")) {
        const j = await res.json().catch(() => null);
        if (j && j.audio) writeFileSync(out, Buffer.from(j.audio, "base64"));
        else throw new Error(`unexpected /api/say content-type: ${ct}`);
      } else {
        writeFileSync(out, Buffer.from(await res.arrayBuffer()));
      }
      lastErr = null;
      break;
    } catch (e) {
      lastErr = e;
      if (attempt < ATTEMPTS) {
        process.stdout.write(` retry${attempt}`);
        await new Promise((r) => setTimeout(r, 800 * attempt));
      }
    }
  }
  if (lastErr) { process.stdout.write(" ✗\n"); throw lastErr; }
  process.stdout.write(" ✓\n");
  return out;
}

const dur = (f) =>
  Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", f]).toString().trim()) || 0;

function fmtLength(sec) {
  if (sec < 75) return "about a minute";
  const m = Math.round(sec / 60);
  return `${m} minute${m === 1 ? "" : "s"}`;
}

// Convert any audio file to a uniform 44.1k/stereo/16-bit wav in the build dir.
function toWav(src, dst) {
  execFileSync("ffmpeg", ["-y", "-i", src, "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", dst], { stdio: "ignore" });
  return dst;
}
function silence(dst, seconds) {
  execFileSync("ffmpeg", ["-y", "-f", "lavfi", "-i", "anullsrc=r=44100:cl=stereo", "-t", String(seconds), "-c:a", "pcm_s16le", dst], { stdio: "ignore" });
  return dst;
}

// ── main ──────────────────────────────────────────────────────────────────
const script = essayToScript(essayPath);
const speaker = script.author.replace(/^@/, "");
console.log(`\n▸ ${script.title} — ${script.paragraphs.length} paragraphs · ${script.wordCount} words · voice ${VOICE.provider}/${VOICE.voice}\n`);

// ── beat grid (shared with the bed) ────────────────────────────────────
const BEAT = 60 / BED_BPM;      // seconds per beat
const BAR = BEAT * 4;
const BEAT_ALIGN = !flags.nobeatalign && !flags.nobed;
// Snap a time up to the next grid line, after a minimum advance.
const snapUp = (t, grid, minAdvance) => Math.ceil((t + minAdvance) / grid - 1e-6) * grid;

// Split a paragraph into sentences (the utterances we place on the grid).
function sentences(p) {
  const parts = p.split(/(?<=[.!?])\s+/).map((s) => s.trim()).filter(Boolean);
  const out = [];
  for (const s of parts) {
    if (out.length && s.length < 22) out[out.length - 1] += " " + s; // glue tiny fragments
    else out.push(s);
  }
  return out;
}

// 1. Narrate the body, one sentence at a time (each an utterance to place).
console.log("Narrating body…");
const units = []; // { text, mp3, dur, paraEnd }
for (let pi = 0; pi < script.paragraphs.length; pi++) {
  const ss = sentences(script.paragraphs[pi]);
  for (let si = 0; si < ss.length; si++) {
    const mp3 = await say(ss[si], `¶${pi + 1}.${si + 1}`);
    units.push({ text: ss[si], mp3, dur: dur(mp3), paraEnd: si === ss.length - 1 });
  }
}

// 2. Arrange the body on the beat grid: each sentence onset snaps to a beat
// (paragraph breaks snap to the bar for a longer rest). Gaps flex; words don't.
// bodyStart is treated as an on-grid origin (0) here; the assembler places the
// body on a real bar boundary so absolute onsets land on beats.
const gapsBefore = [0]; // silence before unit i (index 0 = none)
{
  let t = units[0].dur;
  for (let i = 1; i < units.length; i++) {
    const prev = units[i - 1];
    let g;
    if (BEAT_ALIGN) {
      const grid = prev.paraEnd ? BAR : BEAT;
      const minBreath = prev.paraEnd ? 0.5 : 0.26;
      g = snapUp(t, grid, minBreath) - t;
    } else {
      g = prev.paraEnd ? 0.7 : 0.4;
    }
    gapsBefore.push(g);
    t += g + units[i].dur;
  }
}
const bodySec = units.reduce((s, u) => s + u.dur, 0) + gapsBefore.reduce((a, b) => a + b, 0);
const lengthText = fmtLength(bodySec);
console.log(`\n${units.length} utterances · body ${bodySec.toFixed(1)}s${BEAT_ALIGN ? ` · beat-aligned @ ${BED_BPM}bpm` : ""} → announcing "${lengthText}".\n`);

// 3. Intro + outro voice-over (liturgical framing), with the measured length.
const introText = `A reading of the essay: ${script.title}, by ${speaker}. Approximately ${lengthText}.`;
const outroText = `Here ends the reading.`;
console.log("Narrating frame…");
const introVo = await say(introText, "intro");
const outroVo = await say(outroText, "outro");

// 4. Jingles.
const { intro: introJingle, outro: outroJingle } = renderJingles(resolve(ROOT, "assets"));

// 5. Assemble.
console.log("\nAssembling…");
const build = resolve(ROOT, "out", "build", script.slug);
rmSync(build, { recursive: true, force: true });
mkdirSync(build, { recursive: true });

const seq = [];
let n = 0;
let clock = 0; // absolute seconds, so we can land the body on the bed's grid
const add = (src) => { const d = resolve(build, `seg_${String(n++).padStart(3, "0")}.wav`); toWav(src, d); seq.push(d); clock += dur(d); };
const gap = (s) => { if (s <= 0) return; const d = resolve(build, `seg_${String(n++).padStart(3, "0")}.wav`); silence(d, s); seq.push(d); clock += s; };

add(introJingle);
gap(0.35);
add(introVo);
// Push the body to the next bar boundary so every sentence onset lands on a beat.
gap((BEAT_ALIGN ? snapUp(clock, BAR, 0.6) : clock + 0.8) - clock);
for (let i = 0; i < units.length; i++) {
  gap(gapsBefore[i]); // gapsBefore[0] = 0
  add(units[i].mp3);
}
gap((BEAT_ALIGN ? snapUp(clock, BAR, 0.5) : clock + 0.8) - clock);
add(outroVo);
gap(0.3);
add(outroJingle);

const listFile = resolve(build, "concat.txt");
writeFileSync(listFile, seq.map((f) => `file '${f.replace(/'/g, "'\\''")}'`).join("\n") + "\n");

const outMp3 = resolve(ROOT, "out", `${script.slug}.mp3`);
mkdirSync(dirname(outMp3), { recursive: true });

// 5a. Concat the voice track (no normalization yet).
const voiceWav = resolve(build, "voice.wav");
execFileSync("ffmpeg", [
  "-y", "-f", "concat", "-safe", "0", "-i", listFile,
  "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", voiceWav,
], { stdio: "ignore" });

// 5b. Score a lo-fi bed (melody + rhythm) under the whole reading,
// sidechain-ducked by the voice so speech stays clear. --nobed to skip;
// --bedgain N to tune (default 0.35).
const audioTmp = resolve(build, "audio.mp3");
if (flags.nobed) {
  execFileSync("ffmpeg", [
    "-y", "-i", voiceWav, "-af", "loudnorm=I=-16:TP=-1.5:LRA=11",
    "-c:a", "libmp3lame", "-b:a", "256k", audioTmp,
  ], { stdio: "ignore" });
} else {
  console.log("Scoring bed…");
  const bedGain = flags.bedgain !== undefined ? Number(flags.bedgain) : 0.22;
  const bedWav = resolve(build, "bed.wav");
  renderBed(dur(voiceWav) + 1.0, bedWav);
  // Voice chain (sharper + upfront): high-pass rumble, presence + air EQ,
  // gentle compression for a consistent forward level.
  const vox = "highpass=f=85," +
    "equalizer=f=3200:t=q:w=1.4:g=3.5," +
    "equalizer=f=7000:t=q:w=1.6:g=2.5," +
    "acompressor=threshold=-18dB:ratio=3:attack=6:release=180:makeup=2";
  // Bed chain: quiet, with a slow phaser sweep for movement, then ducked hard
  // under the voice so speech always sits on top.
  const bedfx = `volume=${bedGain},aphaser=type=t:speed=0.25:decay=0.4`;
  execFileSync("ffmpeg", [
    "-y", "-i", voiceWav, "-i", bedWav,
    "-filter_complex",
      `[0:a]${vox}[vox];` +
      `[1:a]${bedfx}[bedfx];` +
      `[bedfx][vox]sidechaincompress=threshold=0.015:ratio=8:attack=5:release=340[bd];` +
      `[vox][bd]amix=inputs=2:duration=first:normalize=0[m];` +
      `[m]loudnorm=I=-16:TP=-1.5:LRA=11[out]`,
    "-map", "[out]",
    "-c:a", "libmp3lame", "-b:a", "256k", audioTmp,
  ], { stdio: "ignore" });
}

// 5b. Cover art (square, on-brand) → embed as ID3 album art + keep the file.
console.log("Rendering cover…");
const { full: coverFull, embed: coverEmbed } = renderCover(script, resolve(ROOT, "out"));

execFileSync("ffmpeg", [
  "-y", "-i", audioTmp, "-i", coverEmbed,
  "-map", "0:a", "-map", "1:v",
  "-c", "copy", "-id3v2_version", "3",
  "-metadata:s:v", "title=Album cover", "-metadata:s:v", "comment=Cover (front)",
  "-disposition:v", "attached_pic",
  "-metadata", `title=${script.title}`,
  "-metadata", `artist=${speaker}`,
  "-metadata", `album=Aesthetic Computer — Readings`,
  "-metadata", `genre=Spoken Word`,
  "-metadata", `date=${script.date}`,
  "-metadata", `comment=A reading of the essay "${script.title}". ${lengthText}.`,
  outMp3,
], { stdio: "ignore" });

rmSync(build, { recursive: true, force: true });
console.log(`  cover: ${coverFull}`);
const total = dur(outMp3);

// Episode metadata sidecar → drives the RSS feed / index.json. Preserve the
// original pubDate across re-runs so the feed order stays stable.
const sidecarPath = resolve(ROOT, "out", `${script.slug}.json`);
let pubDate = new Date().toUTCString();
if (existsSync(sidecarPath)) {
  try { const prev = JSON.parse(readFileSync(sidecarPath, "utf8")); if (prev.pubDate) pubDate = prev.pubDate; } catch { /* ignore */ }
}
writeFileSync(sidecarPath, JSON.stringify({
  slug: script.slug, title: script.title, author: speaker, date: script.date,
  description: script.paragraphs[0], lengthText,
  durationSec: Math.round(total), bytes: readFileSync(outMp3).length,
  wordCount: script.wordCount,
  audio: `${script.slug}.mp3`, cover: `${script.slug}-cover.png`,
  source: positional[0], pubDate,
}, null, 2) + "\n");
console.log(`  meta:  ${sidecarPath}`);

console.log(`\n✓ ${outMp3}`);
console.log(`  ${Math.floor(total / 60)}m ${String(Math.round(total % 60)).padStart(2, "0")}s · ${(readFileSync(outMp3).length / 1024 / 1024).toFixed(1)} MB\n`);

if (flags.open) {
  const r = spawnSync("slab-afplay", [outMp3], { stdio: "inherit" });
  if (r.error) spawnSync("afplay", [outMp3], { stdio: "inherit" });
}
