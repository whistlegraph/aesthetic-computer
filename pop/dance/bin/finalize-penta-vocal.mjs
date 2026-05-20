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

// Master chain — RADIO-LEANING mix bus, applied to the bed+vocal sum.
//   · highpass at 28 Hz — strips sub-rumble that FM/small speakers
//     can't reproduce and that just wastes headroom.
//   · acompressor (-16 / 3.0:1 / 8 dB knee) — heavier glue compression
//     than the album chain (was -19 / 2.4:1) so all the multi-track
//     elements sit in one cohesive level instead of each peaking past
//     each other. "Brings everything into view."
//   · 4-band corrective EQ — tame low-mud at 120 Hz, scoop 250 Hz for
//     clarity, bump 3.5 kHz for radio presence, lift 11 kHz for air.
//   · loudnorm I=-14 LRA=6 — Spotify-compatible LUFS but a much tighter
//     dynamic range than the album chain (was LRA=11) so the track
//     stays loud + present through the whole arrangement.
//   · alimiter 0.94 / fast (5 ms attack / 80 ms release) — broadcast-
//     style ceiling that catches any remaining transients tightly.
// Reverb-duck envelope around the audio stamp (~93-101 s). Drops the
// wet-reverb path from 1.0 down to 0.02 (≈ −34 dB) during the stamp
// window so the room "sucks dry" before the stamp lands, then ramps
// back to full so the post-stamp build returns into space. Ramps:
// 91 → 92 s  hard suck (1.0 → 0.02), 101 → 103 s smooth recover.
const stampDuck =
  `'if(lt(t,91), 1, if(lt(t,92), 1-0.98*(t-91), if(lt(t,101), 0.02, if(lt(t,103), 0.02+0.49*(t-101), 1))))'`;

const filter =
  `[1:a]${voxChain}[vox];` +
  `[0:a][vox]amix=inputs=2:duration=first:dropout_transition=0:normalize=0[mix];` +
  // TRANCE-SPACE master chain — dry path + reverb-tail path summed,
  // where the reverb tail is GATED by the stamp-duck envelope.
  `[mix]highpass=f=28,asplit=2[dry][toverb];` +
  `[toverb]aecho=0.85:0.45:80|170|340|620:0.18|0.13|0.08|0.05,` +
    `volume=${stampDuck}:eval=frame[wetGated];` +
  `[dry][wetGated]amix=inputs=2:duration=first:dropout_transition=0:normalize=0[spaced];` +
  `[spaced]acompressor=threshold=-16dB:ratio=3.0:attack=10:release=150:makeup=1.5:knee=8,` +
  `equalizer=f=120:t=q:w=1.2:g=-1,` +
  `equalizer=f=250:t=q:w=1.6:g=-1.2,` +
  `equalizer=f=3500:t=q:w=1.4:g=2.5,` +
  `highshelf=f=11000:g=1.8,` +
  `loudnorm=I=-14:TP=-1.5:LRA=6,` +
  `alimiter=limit=0.94:attack=5:release=80:level=disabled,` +
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
