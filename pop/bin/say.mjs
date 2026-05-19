#!/usr/bin/env node
// say.mjs — POST a lyric file to /api/say, cache the vocal stem.
//
// Mirrors `recap/bin/tts.mjs` but reads a plain lyric file (the
// pop/big-pictures/<slug>.txt format) instead of an audience config:
//   - strips section headers (hook / verse 1 / verse 2 / outro)
//   - expands "hook" repeats inline (when a header appears alone it
//     repeats the named section's body)
//   - drops blank lines
//   - replaces em dashes with commas for cleaner TTS prosody
//
// Voice: jeffrey-pvc — same config as the 24h recap pipeline
// (provider="jeffrey", voice="neutral:0"). Endpoint is the production
// /api/say proxy, which costs real money via ElevenLabs — caches by
// content-hash. Pass --force to bypass.
//
// Usage:
//   node bin/say.mjs ../big-pictures/plork.txt
//   node bin/say.mjs ../big-pictures/plork.txt --section hook
//   node bin/say.mjs ../big-pictures/plork.txt --out ../big-pictures/out/plork-vocal.mp3
//   node bin/say.mjs ../big-pictures/plork.txt --provider jeffrey --voice neutral:0

import { writeFileSync, readFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  } else positional.push(a);
}

if (!positional[0]) {
  console.error("usage: node bin/say.mjs <lyric-file.txt> [--section hook] [--out path.mp3] [--force]");
  process.exit(1);
}

function expandHome(p) {
  if (!p || typeof p !== "string") return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const lyricPath = resolve(process.cwd(), positional[0]);
if (!existsSync(lyricPath)) {
  console.error(`✗ lyric file not found: ${lyricPath}`);
  process.exit(1);
}

const slug = basename(lyricPath).replace(/\.[^.]+$/, "");
const SECTION = flags.section || null; // null = full track
const PROVIDER = flags.provider || "jeffrey";
const VOICE_ID = flags.voice || "neutral:0";
const SPEED = Number(flags.speed ?? 1.0); // 0.7-1.2 for ElevenLabs jeffrey provider
const STYLE = flags.style !== undefined ? Number(flags.style) : null; // 0-1 style exaggeration
const STABILITY = flags.stability !== undefined ? Number(flags.stability) : null; // 0-1
const SIMILARITY = flags.similarity !== undefined ? Number(flags.similarity) : null; // 0-1
const FORCE = flags.force === true;
// `--timestamps` opts into ElevenLabs `/with-timestamps` endpoint, which
// returns per-character alignment alongside the audio. Lossless, exact,
// free — the source-of-truth replacement for whisper STT word boundaries.
// When set, writes `${OUT_PATH}.alignment.json` next to the mp3.
const TIMESTAMPS = flags.timestamps === true;
const OUT_PATH = expandHome(flags.out)
  || `${ROOT}/big-pictures/out/${slug}${SECTION ? `-${SECTION.replace(/\s+/g, "_")}` : ""}-vocal.mp3`;

// ── Parse lyric file ───────────────────────────────────────────────────
// Sections are headers (lowercase, no colon, alphabetic). Body lines
// follow until the next header or EOF. A header line with no body
// before the next header is a repeat marker.
function parseLyrics(text) {
  const HEADER_RE = /^(hook|verse \d+|outro|bridge|chorus|intro)$/i;
  const sections = {};         // name → first-seen body lines
  const order = [];            // array of section names (with repeats)
  let current = null;
  let buffer = [];

  const flush = () => {
    if (current === null) return;
    if (buffer.length === 0) {
      // Repeat marker — body comes from first occurrence.
      order.push(current);
    } else {
      if (!sections[current]) sections[current] = buffer.slice();
      order.push(current);
    }
    buffer = [];
  };

  for (const raw of text.split("\n")) {
    const line = raw.trim();
    if (!line) continue;
    if (HEADER_RE.test(line)) {
      flush();
      current = line.toLowerCase();
      continue;
    }
    if (current === null) {
      // Lyrics before any header — treat as 'intro'
      current = "intro";
    }
    buffer.push(line);
  }
  flush();

  return { sections, order };
}

const { sections, order } = parseLyrics(readFileSync(lyricPath, "utf8"));

if (SECTION && !sections[SECTION.toLowerCase()]) {
  console.error(`✗ section '${SECTION}' not found. available: ${Object.keys(sections).join(", ")}`);
  process.exit(1);
}

// ── Build narration text ───────────────────────────────────────────────
function cleanLine(line) {
  return line
    .replace(/—/g, ",")     // em dash → comma for TTS pacing
    .replace(/–/g, ",")     // en dash too
    .replace(/\s+/g, " ")
    .trim();
}

let narration;
if (SECTION) {
  const body = sections[SECTION.toLowerCase()];
  narration = body.map(cleanLine).join("\n");
} else {
  const lines = [];
  for (const name of order) {
    const body = sections[name];
    if (!body) continue;
    for (const l of body) lines.push(cleanLine(l));
    lines.push(""); // blank line between sections
  }
  narration = lines.filter((l, i, a) => !(l === "" && a[i + 1] === "")).join("\n").trim();
}

if (!narration) {
  console.error(`✗ no narration content extracted from ${lyricPath}`);
  process.exit(1);
}

// ── Build request + cache key ──────────────────────────────────────────
const body = { from: narration, provider: PROVIDER, voice: VOICE_ID };
if (SPEED !== 1.0) body.speed = Math.max(0.7, Math.min(1.2, SPEED));
if (STYLE !== null && Number.isFinite(STYLE)) body.style = Math.max(0, Math.min(1, STYLE));
if (STABILITY !== null && Number.isFinite(STABILITY)) body.stability = Math.max(0, Math.min(1, STABILITY));
if (SIMILARITY !== null && Number.isFinite(SIMILARITY)) body.similarity = Math.max(0, Math.min(1, SIMILARITY));
if (TIMESTAMPS) body.withTimestamps = true;
const inputHash = createHash("sha256")
  .update(JSON.stringify(body))
  .digest("hex").slice(0, 16);

const hashFile = `${OUT_PATH}.hash`;
const ALIGNMENT_PATH = `${OUT_PATH}.alignment.json`;
mkdirSync(dirname(OUT_PATH), { recursive: true });

if (!FORCE && existsSync(OUT_PATH) && existsSync(hashFile)) {
  const cached = readFileSync(hashFile, "utf8").trim();
  // When --timestamps was requested we also need the alignment sidecar.
  // If the mp3 is cached but the alignment isn't, force a re-fetch.
  const alignmentReady = !TIMESTAMPS || existsSync(ALIGNMENT_PATH);
  if (cached === inputHash && alignmentReady) {
    const size = (readFileSync(OUT_PATH).length / 1024).toFixed(0);
    console.log(`✓ ${OUT_PATH} cached (${size} KB · hash ${inputHash}) — skipping /api/say`);
    if (TIMESTAMPS) console.log(`  alignment: ${ALIGNMENT_PATH}`);
    process.exit(0);
  }
}

console.log(`→ POST /api/say · ${narration.length} chars · ${PROVIDER}/${VOICE_ID}` + (SPEED !== 1.0 ? ` · speed=${SPEED}` : "") + (STYLE !== null ? ` · style=${STYLE}` : "") + (SECTION ? ` · section=${SECTION}` : "") + (TIMESTAMPS ? " · with-timestamps" : ""));
console.log(`  preview: ${narration.split("\n")[0].slice(0, 80)}…`);

// SAY_ENDPOINT lets the pipeline target a locally-run say endpoint
// (e.g. pop/chillwave/bin/say-local.mjs) when the production host is
// unreachable. Defaults to production.
const SAY_URL = process.env.SAY_ENDPOINT || "https://aesthetic.computer/api/say";
const res = await fetch(SAY_URL, {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify(body),
  redirect: "follow",
});

if (!res.ok) {
  console.error(`✗ /api/say returned ${res.status}: ${await res.text()}`);
  process.exit(1);
}

if (TIMESTAMPS) {
  // Server returned JSON `{audio, alignment, normalizedAlignment}`.
  // Write mp3 to OUT_PATH (same as the non-timestamp path) and the
  // alignment sidecar to OUT_PATH.alignment.json.
  const ct = res.headers.get("content-type") || "";
  if (!ct.includes("application/json")) {
    console.error(`✗ --timestamps was set but server returned ${ct}; was the server patched?`);
    process.exit(1);
  }
  const json = await res.json();
  const buf = Buffer.from(json.audio, "base64");
  writeFileSync(OUT_PATH, buf);
  writeFileSync(hashFile, inputHash + "\n");

  // Build word-level boundaries from the per-character alignment.
  // Words are runs of consecutive non-space characters; word time is
  // [first-char start, last-char end].
  const a = json.alignment || {};
  const chars = a.characters || [];
  const starts = a.character_start_times_seconds || [];
  const ends = a.character_end_times_seconds || [];
  const words = [];
  let i = 0;
  while (i < chars.length) {
    if (/\s/.test(chars[i])) { i++; continue; }
    const wStart = starts[i];
    let txt = "";
    let lastEnd = ends[i];
    while (i < chars.length && !/\s/.test(chars[i])) {
      txt += chars[i];
      lastEnd = ends[i];
      i++;
    }
    words.push({
      text: txt,
      fromMs: Math.round(wStart * 1000),
      toMs: Math.round(lastEnd * 1000),
    });
  }

  const alignmentDoc = {
    source: "elevenlabs/with-timestamps",
    text: narration,
    voice: json.voice || PROVIDER,
    characters: chars,
    char_starts_s: starts,
    char_ends_s: ends,
    words,
  };
  writeFileSync(ALIGNMENT_PATH, JSON.stringify(alignmentDoc, null, 2));
  console.log(`✓ ${OUT_PATH} (${(buf.length / 1024).toFixed(0)} KB · hash ${inputHash})`);
  console.log(`✓ ${ALIGNMENT_PATH} (${chars.length} chars · ${words.length} words)`);
} else {
  const buf = Buffer.from(await res.arrayBuffer());
  writeFileSync(OUT_PATH, buf);
  writeFileSync(hashFile, inputHash + "\n");
  console.log(`✓ ${OUT_PATH} (${(buf.length / 1024).toFixed(0)} KB · hash ${inputHash})`);
}
