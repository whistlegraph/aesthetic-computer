#!/usr/bin/env node
// place-penta-vocal.mjs — lay the jeffrey SUNG LEAD-DOUBLE phrase
// (pop/dance/out/trancepenta-vocal.mp3) into a FULL-LENGTH bus matched
// to the trancepenta master. jeffrey now SINGS ALONG WITH THE LEAD —
// continuous-but-gapped through the POST-DROP body only. Output: a
// stereo wav the length of the master, silent everywhere except the
// chosen sung windows.
//
// Placement (trancepenta is ~190.7s; struct sections from struct.json):
//   · 0 – ~58s   = the keyed-up beatless/dampened INTRO → NOTHING here
//                   (the drop + grinding hellsine beat land ~0:58-1:00)
//   · 0:58 – ~76s = build1 into the drop1 onset → still clean (let the
//                   drop hit bare)
//   · Entry A  @ 78.0s  (play 12.0s)  — drop1 body lead-in: the opening
//                   rise + first sustains co-sing the lead, then breathe
//   · GAP ~90 – ~104s   — the centred "aesthetic dot computer" stamp +
//                   arpeggiated neigh + grenade live at track-centre
//                   (~93-101s); leave it totally clean
//   · Entry B  @ 104.0s (play FULL)   — the big drippin body: the whole
//                   sung lead-double draws out across drop1-tail →
//                   break2 → build2 onset
//   · Entry C  @ 145.0s (play 12.5s)  — the recap resolve: descent +
//                   the final long "hum" hold, lands home
//   · hard stop by ~158s — leaves the gnarly/tamed-wavy last ~30s
//                   (≈160-190.7s) and the outro vocal-free
// Each entry: gentle fade-in / fade-out so the sung double breathes in
// and out of the bed rather than punching in.
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

// Measure the phrase length so entries can be tail-trimmed to their
// windows (the drippin phrase is long — the full body entry plays it
// all, the lead-in / resolve entries play a trimmed slice).
const probe = spawnSync("ffprobe", ["-v","error","-show_entries",
  "format=duration","-of","default=noprint_wrappers=1:nokey=1", PHRASE],
  { encoding: "utf8" });
const PH = parseFloat(probe.stdout.trim()) || 31.0;

// Protected regions:
//   · STAMP centre — the clean "aesthetic dot computer" + arpeggiated
//     neigh + grenade are mixed dead-centre (~track/2 ≈ 95.3s); the
//     full sequence spans ~93-101s. Keep a clean ~90-104s gap.
//   · GNARLY tail  — scratch-mix tames/waves the last ~30s; no sung
//     energy past ~158s (leaves the outro + wall-of-noise clean).
const GNARLY_START = 158.0;

// Sung-along entries. `play` caps how much of the (long, drippin)
// phrase this entry sounds — undefined ⇒ the whole phrase.
const entries = [
  { name: "A-drop1-leadin",  start: 78.0,  play: 12.0, fi: 2.0, fo: 3.0 },
  { name: "B-drippin-body",  start: 104.0, play: PH,   fi: 2.5, fo: 5.0 },
  { name: "C-recap-resolve", start: 145.0, play: 12.5, fi: 2.5, fo: 5.0 },
];

const inputs = [];
const parts = [];
let placed = 0;
entries.forEach((e) => {
  // Cap so the entry never sounds into a protected region.
  const room = Math.max(0, GNARLY_START - e.start);
  const playLen = Math.min(PH, e.play, room);
  if (playLen < 3) return; // too short to be musical — skip
  const fo = Math.min(e.fo, playLen / 2);
  const fi = Math.min(e.fi, playLen / 3);
  const delayMs = Math.round(e.start * 1000);
  const idx = placed;
  inputs.push("-i", PHRASE);
  // trim → stereo → fades → schedule at start
  parts.push(
    `[${idx}:a]atrim=0:${playLen.toFixed(3)},asetpts=N/SR/TB,` +
    `pan=stereo|c0=c0|c1=c1,` +
    `afade=t=in:st=0:d=${fi.toFixed(3)},` +
    `afade=t=out:st=${(playLen - fo).toFixed(3)}:d=${fo.toFixed(3)},` +
    `adelay=${delayMs}|${delayMs}[e${idx}]`
  );
  placed++;
});
if (placed === 0) { console.error("✗ no entries placed"); process.exit(1); }

const lbls = parts.map((_, i) => `[e${i}]`).join("");
// Sum the entries, then pad/trim the bus to exactly the master length.
const filter =
  parts.join(";") +
  `;${lbls}amix=inputs=${placed}:duration=longest:dropout_transition=0:normalize=0[mix];` +
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
console.log(`✓ ${OUT} (${parseFloat(p2.stdout.trim()).toFixed(2)}s · phrase ${PH.toFixed(1)}s · ${placed} sung entries @ ${entries.map((e)=>e.start+"s").join(", ")})`);
