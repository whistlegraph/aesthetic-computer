#!/usr/bin/env node
// fetch-pvc-samples.mjs — pull the original training samples for an
// ElevenLabs Professional Voice Clone, profile each for dryness, and
// stage them for listening. Use this to audit a PVC's training corpus
// when the local source clips have been lost.
//
// For jeffrey-pvc (voice_id ZXoQQp5X0PKHGwyZpVIT) the 5 sources are
// (per grants/lacma-2026/SESSION-LOG.md):
//   - mediation lecture
//   - India HCI
//   - Korea HCI
//   - 35c3 RDP
//   - one more (unnamed in log)
//
// Authentication: pass --api-key XYZ, or set ELEVENLABS_API_KEY in env.
// The /api/say production proxy holds the key server-side; we don't
// touch that here.
//
// Usage:
//   ELEVENLABS_API_KEY=sk_... node bin/fetch-pvc-samples.mjs
//   node bin/fetch-pvc-samples.mjs --api-key sk_... --voice-id ZXoQQp5X0PKHGwyZpVIT
//   node bin/fetch-pvc-samples.mjs --out ~/Desktop/jeffrey-pvc-samples
//
// Output:
//   <OUT_DIR>/01-<filename>.mp3          (original sample audio)
//   <OUT_DIR>/01-<filename>.profile.json (dryness profile)
//   <OUT_DIR>/INDEX.md                   (ranked summary)

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const PROFILE = resolve(HERE, "profile-vocal.mjs");

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
    else flags[k] = true;
  }
}

const API_KEY = flags["api-key"] || process.env.ELEVENLABS_API_KEY;
const VOICE_ID = flags["voice-id"] || "ZXoQQp5X0PKHGwyZpVIT";
const OUT_DIR = flags.out
  ? resolve(process.cwd(), flags.out.replace(/^~\//, `${homedir()}/`))
  : `${homedir()}/Desktop/jeffrey-pvc-samples`;

if (!API_KEY) {
  console.error(`
✗ No ElevenLabs API key provided.

   Set ELEVENLABS_API_KEY in env or pass --api-key.

   The key for the AC account is stored server-side in lith /
   Netlify env (used by system/netlify/functions/say.js). Easiest
   paths to retrieve it:

   • paste from your password manager into this terminal:
       export ELEVENLABS_API_KEY=sk_...
   • or pass inline:
       node bin/fetch-pvc-samples.mjs --api-key sk_...

   For jeffrey-pvc specifically, voice_id = ZXoQQp5X0PKHGwyZpVIT.
`);
  process.exit(2);
}

mkdirSync(OUT_DIR, { recursive: true });

// ── Fetch voice metadata + sample list ────────────────────────────────
async function api(path) {
  const r = await fetch(`https://api.elevenlabs.io/v1${path}`, {
    headers: { "xi-api-key": API_KEY, "Accept": "application/json" },
  });
  if (!r.ok) {
    const txt = await r.text();
    throw new Error(`GET ${path} → ${r.status}: ${txt.slice(0, 200)}`);
  }
  return r.json();
}

console.log(`→ voice ${VOICE_ID}`);
const voice = await api(`/voices/${VOICE_ID}`);
console.log(`  name: ${voice.name}`);
console.log(`  category: ${voice.category}`);
const samples = voice.samples || [];
console.log(`  samples: ${samples.length}`);
if (samples.length === 0) {
  console.error("✗ no samples on this voice. Are you authenticated with the right account?");
  process.exit(1);
}

// ── Download each sample audio ────────────────────────────────────────
function safe(s) {
  return (s || "sample").replace(/[^a-zA-Z0-9._-]+/g, "_").slice(0, 60);
}

const results = [];
for (let i = 0; i < samples.length; i++) {
  const s = samples[i];
  const idx = String(i + 1).padStart(2, "0");
  const stem = `${idx}-${safe(s.file_name || s.name || s.sample_id)}`;
  const mp3 = `${OUT_DIR}/${stem}.mp3`;
  console.log(`\n[${idx}] ${s.file_name || s.name || s.sample_id}`);
  console.log(`    duration: ${s.duration_secs ? `${s.duration_secs.toFixed(1)}s` : "?"}  size: ${s.size_bytes ? `${(s.size_bytes/1024/1024).toFixed(2)} MB` : "?"}`);
  if (existsSync(mp3) && !flags.force) {
    console.log(`    cached: ${mp3}`);
  } else {
    const r = await fetch(
      `https://api.elevenlabs.io/v1/voices/${VOICE_ID}/samples/${s.sample_id}/audio`,
      { headers: { "xi-api-key": API_KEY } },
    );
    if (!r.ok) {
      console.warn(`    ✗ download failed: ${r.status}`);
      continue;
    }
    const buf = Buffer.from(await r.arrayBuffer());
    writeFileSync(mp3, buf);
    console.log(`    ✓ ${mp3} (${(buf.length / 1024 / 1024).toFixed(2)} MB)`);
  }

  // Profile dryness
  const p = spawnSync("node", [PROFILE, mp3, "--json"], { encoding: "utf8" });
  let profile = null;
  try { profile = JSON.parse(p.stdout); } catch {}
  if (profile) {
    console.log(`    profile: ratio ${profile.ratio.toFixed(4)}  tail50 ${profile.tail50.toFixed(4)}  →  ${profile.verdict}`);
    writeFileSync(`${OUT_DIR}/${stem}.profile.json`, JSON.stringify(profile, null, 2));
  }
  results.push({
    idx, sampleId: s.sample_id, name: s.file_name || s.name,
    durationSec: s.duration_secs, mp3, profile,
  });
}

// ── Write ranked index ────────────────────────────────────────────────
results.sort((a, b) => (a.profile?.ratio ?? 99) - (b.profile?.ratio ?? 99));
const md = [
  `# jeffrey-pvc training samples — dryness audit`,
  ``,
  `Voice: **${voice.name}** (\`${VOICE_ID}\`)`,
  `Samples: ${samples.length}  ·  Audit date: ${new Date().toISOString().slice(0, 10)}`,
  ``,
  `Ranked by floor/active ratio (lower = drier).`,
  ``,
  `| Rank | Sample | Duration | Ratio | tail50 | Verdict |`,
  `|------|--------|----------|-------|--------|---------|`,
  ...results.map((r, i) => `| ${i + 1} | \`${r.name}\` | ${r.durationSec?.toFixed(1)}s | ${r.profile?.ratio.toFixed(4) ?? "?"} | ${r.profile?.tail50.toFixed(4) ?? "?"} | **${r.profile?.verdict ?? "?"}** |`),
  ``,
  `## Recovery plan`,
  ``,
  `1. Listen to the top-ranked (driest) samples first.`,
  `2. Keep any sample with **ratio < 0.04** — those match the dry-roll regime in production.`,
  `3. For a retrain: build a corpus from the dry subset only, OR record fresh dry close-mic material to extend it.`,
];
writeFileSync(`${OUT_DIR}/INDEX.md`, md.join("\n") + "\n");
console.log(`\n✓ wrote ${OUT_DIR}/INDEX.md`);
console.log(`\nRanked driest → wettest:`);
for (let i = 0; i < results.length; i++) {
  const r = results[i];
  console.log(`  ${i + 1}. ratio ${r.profile?.ratio.toFixed(4) ?? "?"}  ${r.name}`);
}
