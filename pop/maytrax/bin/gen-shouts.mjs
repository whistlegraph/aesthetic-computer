#!/usr/bin/env node
// gen-shouts.mjs — render the maytrax drop SHOUTS in jeffrey-pvc (ElevenLabs)
// as short percussive clips, one per phrase, cached under out/shouts/.
//
// The phrases come from maytrax.np ("wake up / it's real / hold on / let go",
// "follow the white rabbit / now"). Each is rendered with a punchy, slightly
// driven jeffrey-pvc setting (style up for shout energy, stability at 0.5 so
// the voice identity holds — see feedback_jeffrey_pvc_settings). maytrax.mjs
// loads these and slams them on the drop downbeats.
//
//   node pop/maytrax/bin/gen-shouts.mjs            # cached; --force to redo
//
// Output: pop/maytrax/out/shouts/<slug>.mp3 (+ kit-shouts.json index)

import { writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const POP = resolve(LANE, "..");
const SAY = resolve(POP, "bin", "say.mjs");
const OUT_DIR = resolve(LANE, "out", "shouts");
const FORCE = process.argv.includes("--force");
mkdirSync(OUT_DIR, { recursive: true });

const PHRASES = ["wake up", "it's real", "hold on", "let go", "follow the white rabbit", "now"];
const slug = (s) => s.replace(/[^a-z0-9]+/gi, "_").toLowerCase();

// Apple `say` synth voice layered alongside jeffrey (man + machine chorus).
const SAY_VOICE = process.env.MAYTRAX_SAY_VOICE || "Daniel";

const index = {};
for (const phrase of PHRASES) {
  const out = resolve(OUT_DIR, `${slug(phrase)}.mp3`);
  const wav = out.replace(/\.mp3$/, ".wav");
  const sayWav = resolve(OUT_DIR, `${slug(phrase)}-say.wav`);
  index[slug(phrase)] = { phrase, path: wav, sayPath: sayWav };
  // Apple `say` variant (cheap/offline) — always (re)render unless cached.
  if (!existsSync(sayWav) || FORCE) {
    const aiff = resolve(tmpdir(), `maytrax-say-${slug(phrase)}.aiff`);
    if (spawnSync("say", ["-v", SAY_VOICE, "-o", aiff, phrase], { stdio: "ignore" }).status === 0)
      spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", aiff, "-ar", "48000", "-ac", "1", sayWav], { stdio: "ignore" });
  }
  if (existsSync(wav) && !FORCE) { console.log(`✓ cached ${slug(phrase)} (+say)`); continue; }
  const tmp = resolve(tmpdir(), `maytrax-shout-${slug(phrase)}.txt`);
  writeFileSync(tmp, phrase + "\n");
  console.log(`• say "${phrase}" → ${slug(phrase)}.wav`);
  const r = spawnSync("node", [SAY, tmp,
    "--provider", "jeffrey", "--voice", "neutral:0",
    "--style", "0.7", "--stability", "0.5", "--similarity", "0.9", "--speed", "1.05",
    "--out", out], { stdio: ["ignore", "inherit", "inherit"] });
  if (r.status !== 0) { console.error(`✗ failed: ${phrase}`); process.exit(1); }
  // maytrax.mjs reads WAV one-shots — transcode to 48k mono float wav.
  const cv = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", out, "-ar", "48000", "-ac", "1", wav], { stdio: "inherit" });
  if (cv.status !== 0) { console.error(`✗ wav convert failed: ${phrase}`); process.exit(1); }
}

writeFileSync(resolve(LANE, "shouts.json"), JSON.stringify(index, null, 2) + "\n");
console.log(`✓ wrote ${LANE}/shouts.json (${PHRASES.length} shouts)`);
