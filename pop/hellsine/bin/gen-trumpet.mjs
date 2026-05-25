#!/usr/bin/env node
// gen-trumpet.mjs — synth a smooth jazz trumpet melody via MIDI →
// fluidsynth → 48k mono WAV. Output: pop/hellsine/samples/trumpet-solo.wav.
//
// The line lands at t=128 s in hellsine (mid-climax). Climax is the D-minor
// theme transposed +14 semitones → E minor. The chord progression at
// bars 13-22 of climax (relative): G  D  D  Em  Em  G  G  D  D  Em.
// Each bar at 182 BPM = 1.3187 s.
//
// Two voices: LEAD (D5–G6, comfortable trumpet register) + HARMONY (a
// sixth/third below). Lyrical phrasing — long sustained notes that breathe,
// chord-tone arpeggios, no altissimo screams or chromatic bursts. The
// earlier outlandish solo read as gross through VintageDreamsWaves; this
// version stays smooth in-range and gives the soundfont fewer chances to
// glitch. Velocities held at 80-100 (never 127) so the bake limiter does
// not gnash them.
//
// Run once after edits: `node pop/hellsine/bin/gen-trumpet.mjs`.

import { writeFileSync, mkdtempSync, existsSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT_WAV = resolve(HERE, "../samples/trumpet-solo.wav");
const SF2 = "/opt/homebrew/share/fluid-synth/sf2/VintageDreamsWaves-v2.sf2";

const BPM = 182;
const TPQ = 480;                          // ticks per quarter
const SPB = 60 / BPM;
const usPerQ = Math.round(60_000_000 / BPM);

// MIDI variable-length quantity
function vlq(n) {
  const bytes = [n & 0x7f];
  n >>>= 7;
  while (n > 0) { bytes.unshift((n & 0x7f) | 0x80); n >>>= 7; }
  return bytes;
}
function u16(n) { return [(n >> 8) & 0xff, n & 0xff]; }
function u32(n) { return [(n >>> 24) & 0xff, (n >>> 16) & 0xff, (n >>> 8) & 0xff, n & 0xff]; }

// Notes: [beat_offset_from_t0, midi, duration_beats, velocity, channel]
// Channel 0 = lead trumpet, channel 1 = harmony trumpet
// Bars at 1.3187s each. 10 bars × 4 beats = 40 beats, ~13.2s.
// E minor key. Chord progression by bar:
//   0:G  1:D  2:D  3:Em  4:Em  5:G  6:G  7:D  8:D  9:Em
const notes = [];
const L = (b, m, d, v) => notes.push([b, m, d, v, 0]);   // LEAD
const H = (b, m, d, v) => notes.push([b, m, d, v, 1]);   // HARMONY

// Bar 0 — G major. Lyrical opening.
L(0.00, 79, 2.00, 90);   // G5 long
L(2.00, 83, 1.00, 92);   // B5
L(3.00, 86, 1.00, 95);   // D6
H(0.00, 71, 2.00, 78);   // B4 (third below G5)
H(2.00, 74, 2.00, 80);   // D5

// Bar 1 — D major. Settle into root area.
L(4.00, 81, 1.00, 92);   // A5
L(5.00, 78, 1.00, 90);   // F#5
L(6.00, 74, 2.00, 90);   // D5 long
H(4.00, 74, 1.00, 80);   // D5
H(5.00, 71, 1.00, 78);   // B4
H(6.00, 66, 2.00, 80);   // F#4

// Bar 2 — D major. Rising arc.
L(8.00, 78, 1.00, 92);   // F#5
L(9.00, 81, 1.00, 95);   // A5
L(10.00, 86, 2.00, 98);  // D6 long
H(8.00, 66, 1.00, 78);   // F#4
H(9.00, 69, 1.00, 80);   // A4
H(10.00, 74, 2.00, 82);  // D5

// Bar 3 — E minor. Outline.
L(12.00, 76, 2.00, 90);  // E5 long
L(14.00, 79, 1.00, 92);  // G5
L(15.00, 83, 1.00, 95);  // B5
H(12.00, 64, 2.00, 78);  // E4
H(14.00, 67, 1.00, 80);  // G4
H(15.00, 71, 1.00, 82);  // B4

// Bar 4 — E minor. Sustained peak.
L(16.00, 88, 4.00, 100); // E6 long peak — the breath note
H(16.00, 76, 4.00, 88);  // E5 octave double

// Bar 5 — G major. Higher arc.
L(20.00, 91, 2.00, 100); // G6 high G
L(22.00, 86, 1.00, 95);  // D6
L(23.00, 83, 1.00, 92);  // B5
H(20.00, 79, 2.00, 88);  // G5
H(22.00, 74, 1.00, 82);  // D5
H(23.00, 71, 1.00, 80);  // B4

// Bar 6 — G major. Stepwise descent.
L(24.00, 79, 2.00, 92);  // G5 long
L(26.00, 83, 1.00, 90);  // B5
L(27.00, 86, 1.00, 92);  // D6
H(24.00, 67, 2.00, 80);  // G4
H(26.00, 71, 1.00, 78);  // B4
H(27.00, 74, 1.00, 82);  // D5

// Bar 7 — D major. Hold + release.
L(28.00, 81, 2.00, 92);  // A5 long
L(30.00, 78, 2.00, 90);  // F#5 long
H(28.00, 69, 2.00, 80);  // A4
H(30.00, 66, 2.00, 78);  // F#4

// Bar 8 — D major. Quiet build.
L(32.00, 74, 2.00, 88);  // D5 long
L(34.00, 81, 2.00, 92);  // A5 long
H(32.00, 66, 2.00, 78);
H(34.00, 69, 2.00, 80);

// Bar 9 — E minor. Final cadence.
L(36.00, 88, 4.00, 102); // E6 long final note
H(36.00, 76, 4.00, 90);  // E5 octave double

// Sort by start time then by channel (lead before harmony) for cleaner deltas.
notes.sort((a, b) => a[0] - b[0] || a[4] - b[4]);

// Build a single Type-0 MIDI file.
const events = [];

// Meta: tempo
events.push({ tick: 0, bytes: [0xff, 0x51, 0x03, (usPerQ >> 16) & 0xff, (usPerQ >> 8) & 0xff, usPerQ & 0xff] });
// Meta: time signature 4/4
events.push({ tick: 0, bytes: [0xff, 0x58, 0x04, 4, 2, 24, 8] });
// Program change → trumpet (GM #57, 0-indexed = 56) on ch0 + ch1.
// Bank select MSB=0, LSB=0 for both channels (forces standard GM bank in
// case the soundfont remaps).
events.push({ tick: 0, bytes: [0xb0, 0x00, 0x00] });
events.push({ tick: 0, bytes: [0xb0, 0x20, 0x00] });
events.push({ tick: 0, bytes: [0xc0, 56] });    // ch0 trumpet
events.push({ tick: 0, bytes: [0xb1, 0x00, 0x00] });
events.push({ tick: 0, bytes: [0xb1, 0x20, 0x00] });
events.push({ tick: 0, bytes: [0xc1, 56] });    // ch1 trumpet
// Channel volume + expression — push them louder than default 100.
events.push({ tick: 0, bytes: [0xb0, 7, 120] });
events.push({ tick: 0, bytes: [0xb1, 7, 100] });
events.push({ tick: 0, bytes: [0xb0, 11, 127] });
events.push({ tick: 0, bytes: [0xb1, 11, 110] });

// Note events
for (const [bt, midi, durB, vel, ch] of notes) {
  const onTick  = Math.round(bt * TPQ);
  const offTick = Math.round((bt + durB) * TPQ) - 1;
  events.push({ tick: onTick,  bytes: [0x90 | ch, midi, vel] });
  events.push({ tick: offTick, bytes: [0x80 | ch, midi, 0] });
}
// End of track 2 beats after last note
const lastTick = Math.max(...events.map((e) => e.tick));
events.push({ tick: lastTick + 2 * TPQ, bytes: [0xff, 0x2f, 0x00] });

// Stable sort by tick (preserve order for ties)
events.sort((a, b) => a.tick - b.tick);

// Encode track with delta-time
const track = [];
let prev = 0;
for (const e of events) {
  const delta = e.tick - prev;
  prev = e.tick;
  for (const b of vlq(delta)) track.push(b);
  for (const b of e.bytes) track.push(b);
}

const header = [
  ...[0x4d, 0x54, 0x68, 0x64], // "MThd"
  ...u32(6),
  ...u16(0),                   // format 0
  ...u16(1),                   // 1 track
  ...u16(TPQ),
];
const trackHeader = [
  ...[0x4d, 0x54, 0x72, 0x6b], // "MTrk"
  ...u32(track.length),
];
const midiBytes = Buffer.from([...header, ...trackHeader, ...track]);

const tmpDir = mkdtempSync(`${tmpdir()}/trumpet-`);
const midiPath = `${tmpDir}/solo.mid`;
const rawWav = `${tmpDir}/raw.wav`;
writeFileSync(midiPath, midiBytes);
console.log(`[trumpet] wrote MIDI · ${midiBytes.length}B · ${notes.length} notes → ${midiPath}`);

// Render via fluidsynth
if (!existsSync(SF2)) { console.error(`[trumpet] soundfont missing at ${SF2}`); process.exit(1); }
const r = spawnSync("fluidsynth", [
  "-ni",
  "-F", rawWav,
  "-r", "48000",
  "-g", "1.2",
  SF2, midiPath,
], { stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("[trumpet] fluidsynth failed"); process.exit(1); }

// Convert to 48k mono, light HP filter to remove DC + sub rumble, normalize.
// Trim leading silence so the solo starts at t=0 in the wav.
const samplesDir = dirname(OUT_WAV);
if (!existsSync(samplesDir)) mkdirSync(samplesDir, { recursive: true });
const ff = spawnSync("ffmpeg", [
  "-y", "-i", rawWav,
  "-af", "highpass=f=150,silenceremove=start_periods=1:start_threshold=-50dB:start_silence=0.05,loudnorm=I=-14:TP=-1.5:LRA=9",
  "-ar", "48000", "-ac", "1", "-c:a", "pcm_s16le", OUT_WAV,
], { stdio: ["ignore", "inherit", "inherit"] });
if (ff.status !== 0) { console.error("[trumpet] ffmpeg conversion failed"); process.exit(1); }
console.log(`[trumpet] wrote ${OUT_WAV}`);
