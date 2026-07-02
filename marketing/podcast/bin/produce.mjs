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
import { renderJingles } from "./jingle.mjs";
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
  const res = await fetch(SAY_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
    redirect: "follow",
  });
  if (!res.ok) {
    process.stdout.write(" ✗\n");
    throw new Error(`/api/say ${res.status}: ${(await res.text()).slice(0, 200)}`);
  }
  const ct = res.headers.get("content-type") || "";
  if (!ct.includes("audio") && !ct.includes("octet-stream")) {
    // Some deployments return JSON {audio: base64}; handle both.
    const j = await res.json().catch(() => null);
    if (j && j.audio) { writeFileSync(out, Buffer.from(j.audio, "base64")); }
    else throw new Error(`unexpected /api/say content-type: ${ct}`);
  } else {
    writeFileSync(out, Buffer.from(await res.arrayBuffer()));
  }
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

// 1. Narrate the body, paragraph by paragraph.
console.log("Narrating body…");
const paraFiles = [];
for (let i = 0; i < script.paragraphs.length; i++) {
  paraFiles.push(await say(script.paragraphs[i], `¶${i + 1}/${script.paragraphs.length}`));
}

// 2. Measure the real reading length (body VO + inter-paragraph breaths).
const PARA_GAP = 0.55;
const bodySec = paraFiles.reduce((s, f) => s + dur(f), 0) + PARA_GAP * (paraFiles.length - 1);
const lengthText = fmtLength(bodySec);
console.log(`\nBody runs ${bodySec.toFixed(1)}s → announcing "${lengthText}".\n`);

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
const add = (src) => { const d = resolve(build, `seg_${String(n++).padStart(3, "0")}.wav`); toWav(src, d); seq.push(d); };
const gap = (s) => { const d = resolve(build, `seg_${String(n++).padStart(3, "0")}.wav`); silence(d, s); seq.push(d); };

add(introJingle);
gap(0.35);
add(introVo);
gap(0.8);
for (let i = 0; i < paraFiles.length; i++) {
  add(paraFiles[i]);
  if (i < paraFiles.length - 1) gap(PARA_GAP);
}
gap(0.8);
add(outroVo);
gap(0.3);
add(outroJingle);

const listFile = resolve(build, "concat.txt");
writeFileSync(listFile, seq.map((f) => `file '${f.replace(/'/g, "'\\''")}'`).join("\n") + "\n");

const outMp3 = resolve(ROOT, "out", `${script.slug}.mp3`);
mkdirSync(dirname(outMp3), { recursive: true });

// 5a. Mix down the audio (loudnorm → mp3) to a temp file.
const audioTmp = resolve(build, "audio.mp3");
execFileSync("ffmpeg", [
  "-y", "-f", "concat", "-safe", "0", "-i", listFile,
  "-af", "loudnorm=I=-16:TP=-1.5:LRA=11",
  "-c:a", "libmp3lame", "-b:a", "256k",
  audioTmp,
], { stdio: "ignore" });

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
console.log(`\n✓ ${outMp3}`);
console.log(`  ${Math.floor(total / 60)}m ${String(Math.round(total % 60)).padStart(2, "0")}s · ${(readFileSync(outMp3).length / 1024 / 1024).toFixed(1)} MB\n`);

if (flags.open) {
  const r = spawnSync("slab-afplay", [outMp3], { stdio: "inherit" });
  if (r.error) spawnSync("afplay", [outMp3], { stdio: "inherit" });
}
