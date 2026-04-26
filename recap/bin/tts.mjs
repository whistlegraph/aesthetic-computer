#!/usr/bin/env node
// tts.mjs — POST audience narration to /api/say, save MP3 to out/recap.mp3.
// Usage: node bin/tts.mjs [audience-name]   (default: fia)

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const audienceName = process.argv[2] || "fia";

const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);

const body = {
  from: audience.narration,
  provider: audience.voice.provider,
  voice: audience.voice.voice,
};

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
const out = `${ROOT}/out/recap.mp3`;
writeFileSync(out, buf);
console.log(`✓ ${out} (${(buf.length / 1024).toFixed(0)} KB)`);
