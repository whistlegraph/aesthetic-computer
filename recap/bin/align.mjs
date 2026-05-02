#!/usr/bin/env node
// align.mjs — match audience.segments[].marker against out/words.json word
// timestamps and produce out/segments.json:
//   [{name, startSec, endSec, durationSec}, ...]
// Each marker is normalized (lowercase, punctuation stripped) and matched as
// a contiguous run of N words. Unmatched markers fail loud.
//
// Caching: keyed on a hash of words.json + audience.segments. If the inputs
// are unchanged AND segments.json exists, skip alignment. Pass --force to
// bypass.
//
// Usage:
//   node bin/align.mjs [audience-name]
//   node bin/align.mjs jeffrey-73h-2026-05-02 --force

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const argv = process.argv.slice(2);
const force = argv.includes("--force");
const audienceName = argv.find((a) => !a.startsWith("--")) || "fia";
const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);

const words = JSON.parse(readFileSync(`${ROOT}/out/words.json`, "utf8"));
const segmentsPath = `${ROOT}/out/segments.json`;
const hashFile = `${segmentsPath}.hash`;

// Hash on words.json content + the segments[] markers/names. Trailing
// silence is included since it affects endMs computation.
const inputHash = createHash("sha256")
  .update(JSON.stringify(words))
  .update(JSON.stringify(audience.segments.map((s) => ({ n: s.name, m: s.marker, t: s.trailingSilenceSec || 0 }))))
  .digest("hex")
  .slice(0, 16);

if (!force && existsSync(segmentsPath) && existsSync(hashFile)) {
  const cached = readFileSync(hashFile, "utf8").trim();
  if (cached === inputHash) {
    const segments = JSON.parse(readFileSync(segmentsPath, "utf8"));
    console.log(`✓ ${segmentsPath} cached · ${segments.length} segments · hash ${inputHash} — skipping align`);
    process.exit(0);
  }
}

const norm = (s) => s.toLowerCase().replace(/[^a-z0-9]+/g, " ").trim();
const wordTokens = words.map((w) => norm(w.text));
const audioEndMs = words[words.length - 1].toMs;

function findMarker(marker, fromIdx) {
  if (marker === "__END__") return -1;
  const tokens = norm(marker).split(/\s+/);
  for (let i = fromIdx; i <= wordTokens.length - tokens.length; i++) {
    let ok = true;
    for (let j = 0; j < tokens.length; j++) {
      if (wordTokens[i + j] !== tokens[j]) { ok = false; break; }
    }
    if (ok) return i;
  }
  return -2; // not found
}

const starts = [];
let cursor = 0;
for (const seg of audience.segments) {
  if (seg.marker === "__END__") {
    starts.push({ ...seg, idx: -1, startMs: audioEndMs });
    continue;
  }
  const idx = findMarker(seg.marker, cursor);
  if (idx === -2) {
    console.error(`✗ marker not found in transcript: "${seg.marker}" (segment ${seg.name})`);
    console.error(`  cursor at word ${cursor}/${wordTokens.length}: "${wordTokens.slice(cursor, cursor + 8).join(" ")}"`);
    process.exit(1);
  }
  starts.push({ ...seg, idx, startMs: words[idx].fromMs });
  cursor = idx + 1;
}

const trailing = (audience.segments[audience.segments.length - 1].trailingSilenceSec || 0) * 1000;
const endMs = audioEndMs + trailing;

const segments = starts.map((s, i) => {
  const next = i + 1 < starts.length ? starts[i + 1].startMs : endMs;
  return {
    name: s.name,
    startSec: +(s.startMs / 1000).toFixed(3),
    endSec: +(next / 1000).toFixed(3),
    durationSec: +((next - s.startMs) / 1000).toFixed(3),
    marker: s.marker,
  };
});

writeFileSync(segmentsPath, JSON.stringify(segments, null, 2));
writeFileSync(hashFile, inputHash + "\n");
console.log(`✓ ${segmentsPath} · hash ${inputHash}`);
for (const s of segments) {
  console.log(`  ${s.name.padEnd(18)} ${String(s.startSec).padStart(6)}s → ${String(s.endSec).padStart(6)}s  (${s.durationSec.toFixed(2)}s)  "${s.marker}"`);
}
console.log(`  audio ends at ${(audioEndMs / 1000).toFixed(2)}s · video ends at ${(endMs / 1000).toFixed(2)}s`);
