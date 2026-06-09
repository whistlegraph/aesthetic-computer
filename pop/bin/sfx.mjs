#!/usr/bin/env node
// sfx.mjs — POST a sound-effect description to /api/sfx, cache the stem.
//
// Sibling of `say.mjs` (which renders vocals). Where `say.mjs` reads a
// lyric file and hits ElevenLabs text-to-speech, this hits the
// text-to-sound-effects model (eleven_text_to_sound_v2) via the
// production /api/sfx proxy. Same content-hash local cache discipline —
// the endpoint costs real money per generation, so reruns are free.
//
// Two input modes:
//   1. inline prompt:   node bin/sfx.mjs --text "distant thunder" --out out/thunder.mp3
//   2. descriptions file (one cue per line, "# slug : prompt"):
//        node bin/sfx.mjs ../big-pictures/plork.sfx.txt
//      → renders each line to out/sfx/<slug>.mp3
//
// Flags:
//   --text "..."        inline single prompt (skip the file)
//   --out path.mp3      output path (single mode; default out/sfx/<slug>.mp3)
//   --duration N        clip length in seconds (0.5–30); omit = auto
//   --influence N       prompt_influence 0–1 (default server-side 0.3)
//   --loop              request a seamless loop (v2 only)
//   --force             bypass the local cache
//
// Descriptions-file line format (blank lines + #-only comment lines skip):
//   slug : a short natural-language description of the sound
//   thunder : distant thunder rolling over a wet city at night
//   coin : bright 8-bit coin pickup, short and snappy
// A line with no "slug :" prefix uses a zero-padded index as the slug.

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

function expandHome(p) {
  if (!p || typeof p !== "string") return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const DURATION = flags.duration !== undefined ? Number(flags.duration) : null; // 0.5–30 s
const INFLUENCE = flags.influence !== undefined ? Number(flags.influence) : null; // 0–1
const LOOP = flags.loop === true;
const FORCE = flags.force === true;

const SFX_URL = process.env.SFX_ENDPOINT || "https://aesthetic.computer/api/sfx";

// Render one cue → mp3 at outPath. Returns true if it hit the network.
async function renderCue(text, outPath) {
  const body = { text };
  if (DURATION !== null && Number.isFinite(DURATION)) body.duration_seconds = Math.max(0.5, Math.min(30, DURATION));
  if (INFLUENCE !== null && Number.isFinite(INFLUENCE)) body.prompt_influence = Math.max(0, Math.min(1, INFLUENCE));
  if (LOOP) body.loop = true;

  const inputHash = createHash("sha256").update(JSON.stringify(body)).digest("hex").slice(0, 16);
  const hashFile = `${outPath}.hash`;
  mkdirSync(dirname(outPath), { recursive: true });

  if (!FORCE && existsSync(outPath) && existsSync(hashFile)) {
    const cached = readFileSync(hashFile, "utf8").trim();
    if (cached === inputHash) {
      const size = (readFileSync(outPath).length / 1024).toFixed(0);
      console.log(`✓ ${outPath} cached (${size} KB · hash ${inputHash}) — skipping /api/sfx`);
      return false;
    }
  }

  console.log(`→ POST /api/sfx · "${text.slice(0, 64)}${text.length > 64 ? "…" : ""}"` +
    (DURATION !== null ? ` · ${DURATION}s` : "") +
    (INFLUENCE !== null ? ` · influence=${INFLUENCE}` : "") +
    (LOOP ? " · loop" : ""));

  const res = await fetch(SFX_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
    redirect: "follow",
  });

  if (!res.ok) {
    console.error(`✗ /api/sfx returned ${res.status}: ${await res.text()}`);
    process.exit(1);
  }

  const buf = Buffer.from(await res.arrayBuffer());
  writeFileSync(outPath, buf);
  writeFileSync(hashFile, inputHash + "\n");
  console.log(`✓ ${outPath} (${(buf.length / 1024).toFixed(0)} KB · hash ${inputHash})`);
  return true;
}

// ── Single inline prompt ───────────────────────────────────────────────
if (flags.text) {
  const text = String(flags.text).trim();
  if (!text) { console.error("✗ --text was empty"); process.exit(1); }
  const slug = text.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 40) || "sfx";
  const outPath = expandHome(flags.out) || `${ROOT}/big-pictures/out/sfx/${slug}.mp3`;
  await renderCue(text, outPath);
  process.exit(0);
}

// ── Descriptions file (batch) ──────────────────────────────────────────
if (!positional[0]) {
  console.error("usage: node bin/sfx.mjs <descriptions.txt> | --text \"a sound\" [--out path.mp3] [--duration N] [--influence N] [--loop] [--force]");
  process.exit(1);
}

const cuesPath = resolve(process.cwd(), positional[0]);
if (!existsSync(cuesPath)) {
  console.error(`✗ descriptions file not found: ${cuesPath}`);
  process.exit(1);
}

const stem = basename(cuesPath).replace(/\.[^.]+$/, "").replace(/\.sfx$/, "");
const OUT_DIR = expandHome(flags.out) || `${ROOT}/big-pictures/out/sfx/${stem}`;

const lines = readFileSync(cuesPath, "utf8").split("\n");
const cues = [];
let idx = 0;
for (const raw of lines) {
  const line = raw.trim();
  if (!line || line.startsWith("#")) continue;
  const m = line.match(/^([a-zA-Z0-9_-]+)\s*:\s*(.+)$/);
  if (m) cues.push({ slug: m[1], text: m[2].trim() });
  else cues.push({ slug: String(idx).padStart(3, "0"), text: line });
  idx++;
}

if (cues.length === 0) {
  console.error(`✗ no cues found in ${cuesPath}`);
  process.exit(1);
}

console.log(`🔊 ${cues.length} cue(s) → ${OUT_DIR}`);
let rendered = 0;
for (const cue of cues) {
  const outPath = `${OUT_DIR}/${cue.slug}.mp3`;
  if (await renderCue(cue.text, outPath)) rendered++;
}
console.log(`✓ done — ${rendered} generated, ${cues.length - rendered} cached`);
