#!/usr/bin/env node
// place-penta-vocal.mjs — lay the 25.2s jeffrey counterpoint phrase
// (pop/dance/out/trancepenta-vocal.mp3) into a FULL-LENGTH bus matched
// to the trancepenta master, as spaced call-and-response entries that
// answer in the lead's gaps. Output: a stereo wav the length of the
// master, silent everywhere except the chosen entry windows.
//
// Placement (trancepenta is ~190.7s; struct sections from struct.json):
//   · skip intro+gallop  (0–~33s)        — keep the opening beatless/clean
//   · Entry A  @ 34.0s                    — break1 (lead airy → answer)
//   · gap over build1 + drop1 onset       — drop hits clean, no vocal
//   · Entry B  @ 104.0s                   — drop1 tail, AFTER the ~95.3s
//                                           "aesthetic dot computer" stamp
//                                           + restamp have settled
//   · Entry C  @ 134.0s                   — break2→build2→drop2 weave
//   · hard stop by ~159s                  — leaves the gnarly last ~30s
//                                           (≈160–190.7s) untouched
// Each entry: 1.5s fade-in / 2.5s fade-out so it breathes in/out of the
// texture rather than punching in.
//
// Usage:
//   node pop/dance/bin/place-penta-vocal.mjs \
//        --phrase pop/dance/out/trancepenta-vocal.mp3 \
//        --dur 190.693 \
//        --out "$O/.tp-vox-bus.wav"

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

const PHRASE = resolve(process.cwd(), expandHome(flags.phrase) ||
  "pop/dance/out/trancepenta-vocal.mp3");
const TOTAL = Number(flags.dur ?? 190.693);
const OUT = resolve(process.cwd(), expandHome(flags.out) || "/tmp/.tp-vox-bus.wav");
if (!existsSync(PHRASE)) { console.error(`✗ phrase missing: ${PHRASE}`); process.exit(1); }

// Measure the phrase length so the last entry can be tail-trimmed if it
// would otherwise spill into the protected final 30s.
const probe = spawnSync("ffprobe", ["-v","error","-show_entries",
  "format=duration","-of","default=noprint_wrappers=1:nokey=1", PHRASE],
  { encoding: "utf8" });
const PH = parseFloat(probe.stdout.trim()) || 25.2;

// Entry start times (s). Each entry plays the whole phrase unless capped
// by `maxEnd` (keeps the gnarly last ~30s vocal-free).
const FADE_IN = 1.5, FADE_OUT = 2.5;
const GNARLY_START = 159.0; // no vocal energy past here
const entries = [
  { name: "A-break1",       start: 34.0  },
  { name: "B-drop1tail",    start: 104.0 },
  { name: "C-break2-drop2", start: 134.0 },
];

const inputs = [];
const parts = [];
entries.forEach((e, i) => {
  inputs.push("-i", PHRASE);
  // Cap this entry so it never sounds past GNARLY_START.
  const room = Math.max(0, GNARLY_START - e.start);
  const playLen = Math.min(PH, room);
  if (playLen < 3) return; // too short to be musical — skip
  const fo = Math.min(FADE_OUT, playLen / 2);
  const fi = Math.min(FADE_IN, playLen / 3);
  const delayMs = Math.round(e.start * 1000);
  // trim → stereo → fades → schedule at start
  parts.push(
    `[${i}:a]atrim=0:${playLen.toFixed(3)},asetpts=N/SR/TB,` +
    `pan=stereo|c0=c0|c1=c1,` +
    `afade=t=in:st=0:d=${fi.toFixed(3)},` +
    `afade=t=out:st=${(playLen - fo).toFixed(3)}:d=${fo.toFixed(3)},` +
    `adelay=${delayMs}|${delayMs}[e${i}]`
  );
});
const lbls = parts.map((_, i) => `[e${i}]`).join("");
// Sum the entries, then pad/trim the bus to exactly the master length.
const filter =
  parts.join(";") +
  `;${lbls}amix=inputs=${parts.length}:duration=longest:dropout_transition=0:normalize=0[mix];` +
  `[mix]apad=whole_dur=${TOTAL.toFixed(3)},atrim=0:${TOTAL.toFixed(3)},asetpts=N/SR/TB[out]`;

const r = spawnSync("ffmpeg", [
  "-hide_banner","-y","-loglevel","error",
  ...inputs,
  "-filter_complex", filter,
  "-map","[out]",
  "-ar","44100","-ac","2","-c:a","pcm_s16le",
  OUT,
], { stdio: ["ignore","inherit","inherit"] });
if (r.status !== 0) { console.error("✗ bus render failed"); process.exit(1); }

const p2 = spawnSync("ffprobe", ["-v","error","-show_entries",
  "format=duration","-of","default=noprint_wrappers=1:nokey=1", OUT],
  { encoding: "utf8" });
console.log(`✓ ${OUT} (${parseFloat(p2.stdout.trim()).toFixed(2)}s · phrase ${PH.toFixed(1)}s × ${parts.length} entries @ ${entries.map((e)=>e.start+"s").join(", ")})`);
