#!/usr/bin/env node
// record-corpus.mjs — generate the jeffrey harness phoneme corpus by
// hitting /api/say with jeffrey-pvc, three carrier contexts per phoneme.
//
// Mirrors the cache pattern of pop/bin/say.mjs: every request is hashed
// against `{ text, voice, settings }` and skipped if the cached WAV +
// alignment match. Reruns cost $0 unless phonemes.json changes.
//
// Usage:
//   node bin/record-corpus.mjs                           # full corpus
//   node bin/record-corpus.mjs --only iy,ah,schwa        # subset
//   node bin/record-corpus.mjs --context iso             # just iso recordings
//   node bin/record-corpus.mjs --dry-run                 # plan only, no API
//   node bin/record-corpus.mjs --force                   # bypass cache

import {
  writeFileSync, readFileSync, mkdirSync, existsSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const CORPUS = resolve(ROOT, "corpus");
const PHONEMES = JSON.parse(readFileSync(resolve(CORPUS, "phonemes.json"), "utf8"));

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}

const ONLY = flags.only ? new Set(String(flags.only).split(",")) : null;
const CONTEXTS = flags.context
  ? [String(flags.context)]
  : ["iso", "digit", "cvc"];
const DRY = flags["dry-run"] === true;
const FORCE = flags.force === true;

const SETTINGS = PHONEMES.settings;
const ENDPOINT = "https://aesthetic.computer/api/say";

const manifest = {
  version: 1,
  generated: new Date().toISOString(),
  endpoint: ENDPOINT,
  settings: SETTINGS,
  items: {},
};

function hashOf(body) {
  return createHash("sha256").update(JSON.stringify(body)).digest("hex").slice(0, 16);
}

function buildBody(text, isIso, phoneme) {
  const speed = isIso
    ? (phoneme.iso_speed ?? SETTINGS.iso_default_speed)
    : SETTINGS.default_speed;
  return {
    from: text,
    provider: SETTINGS.voice.provider,
    voice: SETTINGS.voice.voice,
    stability: SETTINGS.stability,
    similarity: SETTINGS.similarity,
    style: SETTINGS.style,
    speed,
    withTimestamps: true,
  };
}

async function fetchOne(text, body, outMp3, outAlign) {
  const res = await fetch(ENDPOINT, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
    redirect: "follow",
  });
  if (!res.ok) throw new Error(`/api/say ${res.status}: ${await res.text()}`);
  const ct = res.headers.get("content-type") || "";
  if (ct.includes("application/json")) {
    // Server returned JSON-with-timestamps. Save audio + alignment.
    const json = await res.json();
    const buf = Buffer.from(json.audio, "base64");
    writeFileSync(outMp3, buf);
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
        txt += chars[i]; lastEnd = ends[i]; i++;
      }
      words.push({ text: txt, fromMs: Math.round(wStart * 1000), toMs: Math.round(lastEnd * 1000) });
    }
    writeFileSync(outAlign, JSON.stringify({
      source: "elevenlabs/with-timestamps",
      text, voice: `${body.provider}/${body.voice}`,
      characters: chars, char_starts_s: starts, char_ends_s: ends, words,
    }, null, 2));
    return { bytes: buf.length, words: words.length, hasAlignment: true };
  }
  // Fallback: server returned raw mp3 (jeffrey-pvc currently doesn't
  // support /with-timestamps). Save the audio; alignment can be re-derived
  // later via WhisperX if needed.
  const buf = Buffer.from(await res.arrayBuffer());
  writeFileSync(outMp3, buf);
  writeFileSync(outAlign, JSON.stringify({
    source: "elevenlabs/no-timestamps",
    text, voice: `${body.provider}/${body.voice}`,
    note: "server returned raw mp3; no character-level alignment available",
  }, null, 2));
  return { bytes: buf.length, words: 0, hasAlignment: false };
}

let plannedRequests = 0;
let executedRequests = 0;
let cachedHits = 0;
const errors = [];

for (const phoneme of PHONEMES.phonemes) {
  if (ONLY && !ONLY.has(phoneme.id)) continue;
  manifest.items[phoneme.id] = {
    ipa: phoneme.ipa,
    class: phoneme.class,
    contexts: {},
  };

  for (const ctx of CONTEXTS) {
    const text = phoneme[ctx];
    if (!text) continue;
    plannedRequests++;
    const body = buildBody(text, ctx === "iso", phoneme);
    const inputHash = hashOf(body);

    const dir = resolve(CORPUS, "raw", ctx);
    mkdirSync(dir, { recursive: true });
    const stem = resolve(dir, phoneme.id);
    const outMp3 = `${stem}.mp3`;
    const outAlign = `${stem}.json`;
    const outHash = `${stem}.hash`;

    const cached =
      !FORCE &&
      existsSync(outMp3) &&
      existsSync(outAlign) &&
      existsSync(outHash) &&
      readFileSync(outHash, "utf8").trim() === inputHash;

    manifest.items[phoneme.id].contexts[ctx] = {
      text,
      hash: inputHash,
      mp3: `corpus/raw/${ctx}/${phoneme.id}.mp3`,
      alignment: `corpus/raw/${ctx}/${phoneme.id}.json`,
      cached,
    };

    if (cached) {
      cachedHits++;
      continue;
    }
    if (DRY) {
      console.log(`  [plan] ${phoneme.id}/${ctx}: ${JSON.stringify(text)}`);
      continue;
    }
    try {
      console.log(`→ ${phoneme.id}/${ctx} (${text.length} chars)`);
      const r = await fetchOne(text, body, outMp3, outAlign);
      writeFileSync(outHash, inputHash + "\n");
      executedRequests++;
      console.log(`  ✓ ${(r.bytes / 1024).toFixed(0)} KB · ${r.words} words · hash ${inputHash}`);
    } catch (err) {
      errors.push({ phoneme: phoneme.id, ctx, error: err.message });
      console.error(`  ✗ ${err.message}`);
    }
  }
}

if (!DRY) {
  writeFileSync(resolve(CORPUS, "manifest.json"), JSON.stringify(manifest, null, 2));
}

console.log("");
console.log(`planned:  ${plannedRequests}`);
console.log(`executed: ${executedRequests}`);
console.log(`cached:   ${cachedHits}`);
if (errors.length) {
  console.log(`errors:   ${errors.length}`);
  for (const e of errors) console.log(`  - ${e.phoneme}/${e.ctx}: ${e.error}`);
  process.exit(1);
}
