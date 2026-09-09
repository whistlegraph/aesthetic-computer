#!/usr/bin/env node
// npscore-render.mjs — render a .npscore (fedac/native/tools/npscore-gen.mjs
// format: seconds-based voices of GM program notes + a percussion voice) to
// a wav through FluidSynth, so a native-machine score can score a podcast
// bed offline. The score is the same file the native player and menuband
// play live; only the synth differs (GeneralUser GS here, gm_synth there).
//
//   node bin/npscore-render.mjs score.npscore out.wav [--sf2 path] [--gain 0.5]
//
// Soundfont: --sf2, else $AC_SF2, else ~/.cache/ac/soundfonts/GeneralUserGS.sf2.

import { readFileSync, writeFileSync, existsSync, mkdtempSync } from "node:fs";
import { resolve, join } from "node:path";
import { tmpdir, homedir } from "node:os";
import { execFileSync } from "node:child_process";

const argv = process.argv.slice(2);
const flags = {}, pos = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) { flags[a.slice(2)] = argv[i + 1]; i++; } else pos.push(a);
}
if (pos.length < 2) { console.error("usage: npscore-render.mjs score.npscore out.wav [--sf2 path] [--gain 0.5]"); process.exit(1); }
const [scorePath, outWav] = pos.map((p) => resolve(process.cwd(), p));
const sf2 = flags.sf2 || process.env.AC_SF2 || join(homedir(), ".cache/ac/soundfonts/GeneralUserGS.sf2");
if (!existsSync(sf2)) { console.error(`✗ soundfont not found: ${sf2}`); process.exit(1); }
const gain = flags.gain !== undefined ? Number(flags.gain) : 0.5;

const score = JSON.parse(readFileSync(scorePath, "utf8"));
const bpm = score.bpm || 120;
const PPQ = 480;
const ticks = (sec) => Math.round(sec * (bpm / 60) * PPQ);

// GM percussion (channel 10) note numbers for the score's drum names.
const DRUM = {
  kick: 36, snare: 38, clap: 39, snap: 37, rim: 37, "hat-c": 42, "hat-o": 46,
  hat: 42, crash: 49, ride: 51, tom: 47, block: 76, tambo: 54, shaker: 82, cowbell: 56,
};

// ── SMF encoding ─────────────────────────────────────────────────────────
const vlq = (n) => {
  const out = [n & 0x7f];
  while ((n >>= 7) > 0) out.unshift((n & 0x7f) | 0x80);
  return out;
};
const be32 = (n) => [(n >>> 24) & 255, (n >>> 16) & 255, (n >>> 8) & 255, n & 255];
const be16 = (n) => [(n >>> 8) & 255, n & 255];
const chunk = (tag, bytes) => Buffer.from([...tag.split("").map((c) => c.charCodeAt(0)), ...be32(bytes.length), ...bytes]);
const track = (events, endTick) => {
  events.sort((a, b) => a.tick - b.tick || a.order - b.order);
  const bytes = [];
  let last = 0;
  for (const e of events) { bytes.push(...vlq(e.tick - last), ...e.data); last = e.tick; }
  bytes.push(...vlq(Math.max(0, endTick - last)), 0xff, 0x2f, 0x00);
  return chunk("MTrk", bytes);
};

const clampVel = (v) => Math.max(1, Math.min(127, Math.round(v)));
let lastSec = 0;
const tracks = [];
// Tempo track.
const usPerQn = Math.round(60e6 / bpm);
tracks.push(track([{ tick: 0, order: 0, data: [0xff, 0x51, 0x03, (usPerQn >> 16) & 255, (usPerQn >> 8) & 255, usPerQn & 255] }], 0));

let melodicCh = 0;
const nextCh = () => { if (melodicCh === 9) melodicCh++; return melodicCh++; };
for (const v of score.voices || []) {
  const perc = v.kind === "percussion";
  const ch = perc ? 9 : nextCh();
  const ev = [];
  if (!perc) ev.push({ tick: 0, order: 0, data: [0xc0 | ch, (v.program || 0) & 127] });
  ev.push({ tick: 0, order: 0, data: [0xb0 | ch, 7, 100] }); // channel volume; notes carry dynamics
  for (const n of v.notes || []) {
    const start = n.start || 0;
    const dur = perc ? 0.25 : (n.dur || 0.25);
    const midi = perc ? (DRUM[n.drum] ?? 76) : (n.midi ?? 60);
    const vel = clampVel(n.velocity ?? v.velocity ?? 90);
    ev.push({ tick: ticks(start), order: 1, data: [0x90 | ch, midi & 127, vel] });
    ev.push({ tick: ticks(start + dur), order: 0, data: [0x80 | ch, midi & 127, 0] });
    lastSec = Math.max(lastSec, start + dur);
  }
  tracks.push(track(ev, ticks(lastSec + (score.tailSeconds || 2))));
}
const endTick = ticks(lastSec + (score.tailSeconds || 2));
tracks[0] = track([{ tick: 0, order: 0, data: [0xff, 0x51, 0x03, (usPerQn >> 16) & 255, (usPerQn >> 8) & 255, usPerQn & 255] }], endTick);

const smf = Buffer.concat([chunk("MThd", [...be16(1), ...be16(tracks.length), ...be16(PPQ)]), ...tracks]);
const tmp = mkdtempSync(join(tmpdir(), "npscore-"));
const midPath = join(tmp, "score.mid"), rawWav = join(tmp, "raw.wav");
writeFileSync(midPath, smf);

// ── FluidSynth → wav ─────────────────────────────────────────────────────
execFileSync("fluidsynth", ["-ni", "-q", "-g", String(gain), "-r", "44100",
  "-o", "synth.reverb.room-size=0.55", "-o", "synth.reverb.level=0.35",
  "-F", rawWav, sf2, midPath], { stdio: "ignore" });
// Trim FluidSynth's run-out to the score's own length + tail; land on stereo 44.1k.
const total = lastSec + (score.tailSeconds || 2);
execFileSync("ffmpeg", ["-y", "-i", rawWav, "-t", total.toFixed(3),
  "-af", `afade=t=out:st=${Math.max(0, total - 1.5).toFixed(3)}:d=1.5`,
  "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", outWav], { stdio: "ignore" });

const n = (score.voices || []).reduce((s, v) => s + (v.notes || []).length, 0);
console.log(`${outWav} — "${score.name || "score"}" ${bpm} bpm · ${n} events · ${total.toFixed(1)}s · sf2 ${sf2.split("/").pop()}`);
