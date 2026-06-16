#!/usr/bin/env node
// generate "computer" utterances via macOS `say` across a curated voice set.
// outputs mono 48k wav to utterances/computer/<voice>-<rate>.wav

import { execSync, spawnSync } from "node:child_process";
import { mkdirSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

const here = dirname(fileURLToPath(import.meta.url));
const root = dirname(here);
const outDir = join(root, "utterances", "computer");
mkdirSync(outDir, { recursive: true });

// curated voice list. classic + novelty mac voices. rate variations get
// us natural / slow / fast versions for each — useful for fitting bpm.
const VOICES = [
  // classic synth-voice canon
  "Fred", "Ralph", "Albert", "Junior", "Kathy",
  // natural-ish
  "Samantha", "Alex", "Daniel", "Karen", "Moira",
  // novelty/musical
  "Bahh", "Bells", "Boing", "Bubbles", "Cellos",
  "Wobble", "Jester", "Organ", "Superstar", "Trinoids",
  "Whisper", "Zarvox",
  // "Pipe Organ" sometimes registers as "Organ"; "Bad News" / "Good News"
  // also work when present on the host
  "Bad News", "Good News", "Pipe Organ", "Princess",
  // modern conversational voices (Siri-generation; actual Siri voices are
  // not exposed to `say`) — the smooth end of the spectrum
  "Flo (English (US))", "Sandy (English (US))", "Shelley (English (US))",
  "Reed (English (US))", "Eddy (English (US))",
];

const RATES = [
  { label: "slow", wpm: 110 },
  { label: "mid",  wpm: 175 },
  { label: "fast", wpm: 240 },
];

const WORD = "computer";

// `say -v "?"` lists installed voices; first whitespace-delimited token is the
// short name. multi-word names like "Bad News" / "Pipe Organ" appear as such.
let _voices = null;
function listInstalledVoices() {
  if (_voices) return _voices;
  const out = execSync("say -v '?'", { encoding: "utf8" });
  const set = new Set();
  for (const line of out.split("\n")) {
    // line format: "<name>  <locale>  # comment" — parenthesized names
    // ("Eddy (English (US))") sit only ONE space from the locale, so match
    // lazily up to any whitespace + locale.
    const m = line.match(/^(.+?)\s+[a-z]{2}_[A-Z]{2}\b/);
    if (m) set.add(m[1].trim());
  }
  _voices = set;
  return set;
}
function voiceExists(name) { return listInstalledVoices().has(name); }

function slug(name) {
  return name.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}

function tellSay(voice, rateWpm, outAiff) {
  // vanilla AIFF — let ffmpeg downstream pick the format. `--data-format`
  // gets rejected with "fmt?" on some macOS builds for novelty voices.
  const args = ["-v", voice, "-r", String(rateWpm), "-o", outAiff, WORD];
  const res = spawnSync("say", args, { stdio: ["ignore", "inherit", "inherit"] });
  return res.status === 0;
}

function aiffToWav(aiff, wav) {
  // mono, 48k, normalized to -1 dB peak, ~25 ms trim of leading/trailing silence
  const res = spawnSync("ffmpeg", [
    "-y", "-i", aiff,
    "-af", "silenceremove=start_periods=1:start_silence=0.01:start_threshold=-50dB:detection=peak,areverse,silenceremove=start_periods=1:start_silence=0.01:start_threshold=-50dB:detection=peak,areverse,loudnorm=I=-14:TP=-1:LRA=11",
    "-ac", "1", "-ar", "48000",
    wav,
  ], { stdio: ["ignore", "ignore", "ignore"] });
  return res.status === 0;
}

function main() {
  let made = 0, skipped = 0, failed = 0;
  const log = [];
  for (const v of VOICES) {
    if (!voiceExists(v)) { log.push(`  · ${v}  (not installed)`); continue; }
    for (const r of RATES) {
      const name = `${slug(v)}-${r.label}`;
      const wav = join(outDir, `${name}.wav`);
      if (existsSync(wav)) { skipped++; continue; }
      const aiff = join(tmpdir(), `${name}-${Date.now()}.aiff`);
      const okSay = tellSay(v, r.wpm, aiff);
      if (!okSay) { failed++; log.push(`  ✗ ${name}  (say failed)`); continue; }
      const okFf = aiffToWav(aiff, wav);
      spawnSync("rm", ["-f", aiff]);
      if (!okFf) { failed++; log.push(`  ✗ ${name}  (ffmpeg failed)`); continue; }
      made++;
      log.push(`  ✓ ${name}`);
    }
  }
  console.log(`# computer TTS: ${made} new, ${skipped} skipped, ${failed} failed`);
  console.log(log.join("\n"));
}

main();
