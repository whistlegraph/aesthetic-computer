#!/usr/bin/env node
// tts.mjs — POST audience narration to /api/say, save MP3 to out/recap.mp3.
//
// Caching: keyed on a content hash of (narration text + voice provider +
// voice id). If `out/recap.mp3` exists AND `out/recap.mp3.hash` matches
// the current input hash, skip the /api/say call entirely (which costs
// real money via ElevenLabs proxying). Pass `--force` to bypass.
//
// Usage:
//   node bin/tts.mjs [audience-name]            (default: fia)
//   node bin/tts.mjs jeffrey-73h-2026-05-02 --force

import { writeFileSync, mkdirSync, existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const argv = process.argv.slice(2);
const force = argv.includes("--force");
const audienceName = argv.find((a) => !a.startsWith("--")) || "fia";

const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);

const body = {
  from: audience.narration,
  provider: audience.voice.provider,
  voice: audience.voice.voice,
};

const inputHash = createHash("sha256")
  .update(JSON.stringify(body))
  .digest("hex")
  .slice(0, 16);

const out = `${ROOT}/out/recap.mp3`;
const hashFile = `${out}.hash`;
mkdirSync(`${ROOT}/out`, { recursive: true });

// Skip if cached output matches the current input hash. ElevenLabs is
// the only real-money step here, so skipping saves ~$ on every rerun.
if (!force && existsSync(out) && existsSync(hashFile)) {
  const cached = readFileSync(hashFile, "utf8").trim();
  if (cached === inputHash) {
    const size = (readFileSync(out).length / 1024).toFixed(0);
    console.log(`✓ ${out} cached (${size} KB · hash ${inputHash}) — skipping /api/say`);
    process.exit(0);
  }
}

console.log(`→ POST /api/say · ${audience.narration.length} chars · ${audience.voice.provider}`);
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
writeFileSync(out, buf);
writeFileSync(hashFile, inputHash + "\n");
console.log(`✓ ${out} (${(buf.length / 1024).toFixed(0)} KB · hash ${inputHash})`);
