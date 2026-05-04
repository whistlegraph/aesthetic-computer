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
const inputHash = createHash("sha256")
  .update(JSON.stringify(body))
  .digest("hex").slice(0, 16);

const hashFile = `${OUT_PATH}.hash`;
mkdirSync(dirname(OUT_PATH), { recursive: true });

if (!FORCE && existsSync(OUT_PATH) && existsSync(hashFile)) {
  const cached = readFileSync(hashFile, "utf8").trim();
  if (cached === inputHash) {
    const size = (readFileSync(OUT_PATH).length / 1024).toFixed(0);
    console.log(`✓ ${OUT_PATH} cached (${size} KB · hash ${inputHash}) — skipping /api/say`);
    process.exit(0);
  }
}

console.log(`→ POST /api/say · ${narration.length} chars · ${PROVIDER}/${VOICE_ID}` + (SPEED !== 1.0 ? ` · speed=${SPEED}` : "") + (STYLE !== null ? ` · style=${STYLE}` : "") + (SECTION ? ` · section=${SECTION}` : ""));
console.log(`  preview: ${narration.split("\n")[0].slice(0, 80)}…`);

const res = await fetch("https://aesthetic.computer/api/say", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify(body),
  redirect: "follow",
});

if (!res.ok) {
  console.error(`✗ /api/say returned ${res.status}: ${await res.text()}`);
  process.exit(1);
}

const buf = Buffer.from(await res.arrayBuffer());
writeFileSync(OUT_PATH, buf);
writeFileSync(hashFile, inputHash + "\n");
console.log(`✓ ${OUT_PATH} (${(buf.length / 1024).toFixed(0)} KB · hash ${inputHash})`);
