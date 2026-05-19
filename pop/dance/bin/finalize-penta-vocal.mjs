#!/usr/bin/env node
// finalize-penta-vocal.mjs — trancepenta stage-3 finalize WITH the
// jeffrey SUNG LEAD-DOUBLE bus woven in.
//
// This is the EXACT trancepenta.md stage-3 chain (acompressor → EQ →
// highshelf → loudnorm I=-14 → alimiter → duration-aware 18s fade)
// with ONE addition: the placed sung-double bus is a 2nd ffmpeg input,
// gently band-shaped + level-trimmed so it sits as a PRESENT CO-LEAD
// (NOT tucked under like the old hum counterpoint), then amix'd into
// the master BEFORE loudnorm so the whole thing is mastered together
// to Spotify-clean ≈ -14 LUFS / TP ≤ -1.
//
// jeffrey now sings ALONG with the lead, so this is a co-lead level
// (~+2…+4 dB above the old hum's -7 dB): default -3 dB, --vox-db tunes
// it by ear/RMS so it's clearly present but never slams the bed.
//
// Vocal bus shaping (clarity without masking the synth lead):
//   highpass 130  — keep it out of the kick/sub but a bit more body
//   gentle dip 2.6kHz (-2)  — small pocket so the synth lead still cuts
//   air   +1.5 @ 9k — keeps jeffrey's breath/timbre alive on long holds
//   volume VOX_DB   — co-lead trim (default -3 dB)
//
// Usage:
//   node pop/dance/bin/finalize-penta-vocal.mjs \
//        --scr "$O/.tp-scr.wav" --vox "$O/.tp-vox-bus.wav" \
//        --vox-db -3 --out "$O/trancepenta-MASTER.wav"

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { resolve } from "node:path";
import { homedir } from "node:os";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else flags[a.slice(2)] = next;
}
const expandHome = (p) => !p ? p : p === "~" ? homedir()
  : p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p;

const SCR = resolve(process.cwd(), expandHome(flags.scr));
const VOX = resolve(process.cwd(), expandHome(flags.vox));
const OUT = resolve(process.cwd(), expandHome(flags.out));
const VOX_DB = Number(flags["vox-db"] ?? -3); // sung co-lead trim
for (const [n, p] of [["scr", SCR], ["vox", VOX]]) {
  if (!p || !existsSync(p)) { console.error(`✗ ${n} missing: ${p}`); process.exit(1); }
}

// Duration-aware 18s fade (matches trancepenta.md): fade starts at D-18.
const probe = spawnSync("ffprobe", ["-v","error","-show_entries",
  "format=duration","-of","csv=p=0", SCR], { encoding: "utf8" });
const D = parseFloat(probe.stdout.trim());
const fadeSt = (D - 18).toFixed(3);

// Vocal bus: gentle shaping for a PRESENT co-lead (not tucked under).
const voxChain =
  `highpass=f=130,` +
  `equalizer=f=2600:t=q:w=1.2:g=-2,` +
  `equalizer=f=9000:t=q:w=1.0:g=1.5,` +
  `volume=${VOX_DB}dB`;

// Master chain — IDENTICAL to trancepenta.md stage 3, applied to the
// bed+vocal sum. amix normalize=0 so the bed keeps its own level and
// the (already-attenuated) vocal just adds a quiet layer; loudnorm then
// brings the SUM to -14 LUFS so the published track is still
// Spotify-clean with the counterpoint baked in.
const filter =
  `[1:a]${voxChain}[vox];` +
  `[0:a][vox]amix=inputs=2:duration=first:dropout_transition=0:normalize=0[mix];` +
  `[mix]acompressor=threshold=-19dB:ratio=2.4:attack=15:release=240:makeup=1:knee=6,` +
  `equalizer=f=3000:t=q:w=1.2:g=2,highshelf=f=10000:g=1.5,` +
  `loudnorm=I=-14:TP=-1.5:LRA=11,` +
  `alimiter=limit=0.94:attack=8:release=120:level=disabled,` +
  `afade=t=out:st=${fadeSt}:d=18[out]`;

const r = spawnSync("ffmpeg", [
  "-hide_banner","-y","-loglevel","error",
  "-i", SCR, "-i", VOX,
  "-filter_complex", filter,
  "-map","[out]",
  "-ar","44100","-sample_fmt","s16",
  OUT,
], { stdio: ["ignore","inherit","inherit"] });
if (r.status !== 0) { console.error("✗ finalize failed"); process.exit(1); }

const p2 = spawnSync("ffprobe", ["-v","error","-show_entries",
  "format=duration","-of","csv=p=0", OUT], { encoding: "utf8" });
console.log(`✓ ${OUT} (${parseFloat(p2.stdout.trim()).toFixed(2)}s · vocal bus ${VOX_DB}dB · fade @ ${fadeSt}s)`);
