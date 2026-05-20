#!/usr/bin/env node
// cover-video.mjs — per-frame canvas-rendered "score-train" music
// video. Draws each frame programmatically via node-canvas and pipes
// raw RGBA into ffmpeg for encoding. This replaces the pre-rendered
// PNG approach so we get pixel-perfect alignment AND can animate
// events (note flashes on trigger, etc.) without resampling.
//
// Layout (top → bottom):
//   • illustration (ken-burns drift) — top 1000 px
//   • char-by-char bouncing title typography (floats over the scene)
//   • score panel (1000 → 1500 px): multi-lane piano-roll-style
//     timeline that scrolls horizontally past a fixed center playhead.
//     Notes flash brighter at their trigger time, fading back to the
//     base color over ~120 ms so the rhythm reads visually.
//   • duration label at bottom-left
//
// Inputs:
//   --illustration   path to raw illustration PNG (no typography)
//   --track          path to the rendered MP3
//   --struct         path to .struct.json (defaults to <track>.assets/struct.json)
//   --title          title string
//   --bpm            beats per minute
//   --out            output mp4 path

import { existsSync, readFileSync, mkdirSync, writeFileSync } from "node:fs";
import { spawn, spawnSync } from "node:child_process";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { createCanvas, loadImage } from "canvas";
import { makeVerletString, hexToRgb as ncHexToRgb } from "../../lib/cover-engine.mjs";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}

function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const ILLUSTRATION = expandHome(flags.illustration);
// Optional per-section illustrations — comma-separated `name=path`
// pairs. The visualizer crossfades between them as the song's
// musical section shifts. Sections not mapped fall back to the
// default --illustration.
const SECTION_ILLUSTRATIONS = (() => {
  const m = new Map();
  const raw = flags["illustrations"];
  if (!raw) return m;
  for (const pair of String(raw).split(",")) {
    const [k, v] = pair.split("=");
    if (k && v) m.set(k.trim(), expandHome(v.trim()));
  }
  return m;
})();
const TRACK = expandHome(flags.track);
function locateStruct(track) {
  if (!track) return null;
  const stem = track.replace(/\.mp3$/, "");
  const sub = `${stem}.assets/struct.json`;
  const legacy = `${stem}.struct.json`;
  if (existsSync(sub)) return sub;
  if (existsSync(legacy)) return legacy;
  return sub;
}
const STRUCT = expandHome(flags.struct) || locateStruct(TRACK);
const OUT = expandHome(flags.out) || (TRACK ? TRACK.replace(/\.mp3$/, "-cover.mp4") : null);
const TITLE = (flags.title || "track").toString();
// --platform insta|tiktok (default insta = the Instagram-Story cut).
//   insta : title snug at top (y=155), section progress bar, timecode,
//           side pals.
//   tiktok: NO progress bar, NO timecode, NO side pals (karaoke KEPT);
//           the title is typed into the CENTRE of screen, then on the
//           boot melody it jumps in, the chars bounce + section-tint,
//           and after ~4s they FALL away and are GONE for good.
// Both cuts share the SAME audio/struct — only the visuals branch.
const PLATFORM = String(flags.platform || (flags.tiktok ? "tiktok" : "insta")).toLowerCase();
const TIKTOK   = PLATFORM === "tiktok";
const BPM = Number(flags.bpm ?? 137.143);
const FPS = Number(flags.fps ?? 30);
// Comma-separated lane keys to omit from the timeline (e.g. piano
// for tracks where it only fires in a small slice of the song).
const HIDE_LANES = new Set(
  (flags["hide-lanes"] || "").toString().split(",").map((s) => s.trim()).filter(Boolean)
);
// --liquid → trancenwaltzi feel: smooth WATER-RIPPLE displacement +
// rippled section dissolve, instead of the slit-scan row-tear /
// column-shear melt used for trancenwaltz proper.
const LIQUID = Boolean(flags.liquid);

if (!ILLUSTRATION || !existsSync(ILLUSTRATION)) {
  console.error(`✗ --illustration is required and must exist (got: ${ILLUSTRATION})`);
  process.exit(1);
}
if (!TRACK || !existsSync(TRACK)) {
  console.error(`✗ --track is required and must exist`);
  process.exit(1);
}
if (!STRUCT || !existsSync(STRUCT)) {
  console.error(`✗ --struct is required and must exist (default: <track>.assets/struct.json)`);
  process.exit(1);
}

// Probe track duration.
const probe = spawnSync("ffprobe", [
  "-v", "error",
  "-show_entries", "format=duration",
  "-of", "csv=p=0",
  TRACK,
], { encoding: "utf8" });
const duration = parseFloat(probe.stdout.trim()) || 60;
const struct = JSON.parse(readFileSync(STRUCT, "utf8"));

// ── pre-decode raw audio + amplitude envelope ────────────────────────
// Decode the audio at AUDIO_SR Hz mono. We keep BOTH:
//   • raw audio samples — used to draw actual waveform shapes inside
//     each SFX event rectangle so the timeline reads as a mini-DAW.
//   • per-60Hz normalized RMS — drives the title bounce + scene wobble.
const AUDIO_SR = 4000; // 4 kHz mono — gives ~125 µs resolution for the SFX waveform display
console.log(`▸ extracting audio @ ${AUDIO_SR} Hz`);
const decRaw = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error",
  "-i", TRACK,
  "-f", "f32le", "-ar", String(AUDIO_SR), "-ac", "1",
  "-",
], { encoding: "buffer", maxBuffer: 1024 * 1024 * 256 });
let envelope = new Float32Array(Math.ceil(duration * 60) + 4);
let audioRaw = new Float32Array(0);
let audioPeak = 1;
if (decRaw.status === 0 && decRaw.stdout) {
  audioRaw = new Float32Array(decRaw.stdout.buffer, decRaw.stdout.byteOffset, decRaw.stdout.byteLength / 4);
  // Track peak so the waveform display can normalize amplitudes.
  for (let i = 0; i < audioRaw.length; i++) {
    const a = Math.abs(audioRaw[i]);
    if (a > audioPeak) audioPeak = a;
  }
  // Compute per-60-Hz RMS over a ~30 ms window centered on each frame.
  const ENV_OUT_SR = 60;
  const winSamples = Math.floor(0.030 * AUDIO_SR);
  const stepSamples = AUDIO_SR / ENV_OUT_SR;
  envelope = new Float32Array(Math.ceil(duration * ENV_OUT_SR) + 4);
  for (let i = 0; i < envelope.length; i++) {
    const center = Math.floor(i * stepSamples);
    let sum = 0, n = 0;
    for (let j = -winSamples; j <= winSamples; j++) {
      const idx = center + j;
      if (idx < 0 || idx >= audioRaw.length) continue;
      sum += audioRaw[idx] * audioRaw[idx];
      n++;
    }
    envelope[i] = Math.sqrt(sum / Math.max(1, n));
  }
  // Normalize envelope so peak = 1.0 (relative loudness across track).
  let envMax = 0;
  for (let i = 0; i < envelope.length; i++) if (envelope[i] > envMax) envMax = envelope[i];
  if (envMax > 0) for (let i = 0; i < envelope.length; i++) envelope[i] /= envMax;
}
function envAt(t) {
  const idx = Math.floor(t * 60);
  if (idx < 0 || idx >= envelope.length) return 0;
  return envelope[idx];
}

// YWFT-Processing's TTF only resolves correctly under ImageMagick's
// freetype path — node-canvas (cairo + fontconfig) renders it as
// missing-glyph tofu boxes. So we pre-rasterize all YWFT text via
// magick to transparent PNGs at startup and composite the resulting
// images per-frame instead of using canvas text APIs. The rest of
// the labels (section names, lane gutters, duration) use the
// default system sans which node-canvas DOES render fine.
const YWFT_PATH = `${homedir()}/Library/Fonts/ywft-processing-bold.ttf`;
// Auto-detect face rect for the illustration if a sidecar exists.
// Run detect-face.py once via:
//   recap/.venv/bin/python3 pop/dance/bin/detect-face.py <illustration>
let faceRect = null;
if (ILLUSTRATION) {
  const facePath = `${ILLUSTRATION}.face.json`;
  if (existsSync(facePath)) {
    try {
      const f = JSON.parse(readFileSync(facePath, "utf8"));
      if (f.detected !== false && f.w && f.h) {
        faceRect = f;
      }
    } catch {}
  }
}

// Per-section face + synthetic laptop bboxes, in IMAGE coords. Used
// by the lane-driven backlight (kicks light up the face region, hats
// light up the laptop region). Loaded from <illustration>.face.json
// sidecars. Laptop bbox is SYNTHESIZED below the face — color-based
// detection on the painterly green laptop is too noisy across mixed
// scenes; "below the face" is reliable for all our compositions.
function loadRectsForIllustration(path) {
  if (!path || !existsSync(path)) return null;
  const facePath = `${path}.face.json`;
  if (!existsSync(facePath)) return null;
  try {
    const f = JSON.parse(readFileSync(facePath, "utf8"));
    if (f.detected === false || !f.w || !f.h) return { face: null, laptop: null, imgW: f.imgW, imgH: f.imgH };
    const face = { x: f.x, y: f.y, w: f.w, h: f.h };
    // Synthetic laptop just below the face.
    const lx = Math.max(0, f.x - f.w * 0.30);
    const ly = Math.min(f.imgH - 1, f.y + f.h * 1.25);
    const lw = Math.min(f.imgW - lx, f.w * 1.60);
    const lh = Math.min(f.imgH - ly, f.h * 0.85);
    const laptop = { x: lx, y: ly, w: lw, h: lh };
    return { face, laptop, imgW: f.imgW, imgH: f.imgH };
  } catch { return null; }
}

// ── canvas layout ────────────────────────────────────────────────────
// The illustration fills the entire 1500×1500 canvas (NO banding).
// Score panel + title + duration all render ON TOP of it. The score
// occupies a translucent strip across the bottom so the illustration
// shows through behind the lanes.
// Canvas size — default 1500x1500 square. Pass --size 1080x1920 for
// a vertical 9:16 render (the same v8 illustrations cover-fit at this
// aspect with a small horizontal crop, no new gens needed).
function parseSize(s) {
  const m = /^(\d+)x(\d+)$/.exec(String(s).trim());
  if (!m) return [1500, 1500];
  return [Number(m[1]), Number(m[2])];
}
const [W, H] = parseSize(flags.size || "1500x1500");
// Score panel spans the FULL canvas height — events overlay the
// illustration directly with NO dark backing strip. Each event gets
// a strong outline so it reads against varied illustration colors.
// Title/progress/timecode chrome floats at the very top and bottom
// edges.
const SCORE_TOP = 40;
const SCORE_BOTTOM = H - 40;
const SCORE_H = SCORE_BOTTOM - SCORE_TOP;

// Lanes — one strip per instrument category. Every fired sound in
// the rendered track is represented in some lane; macro-textures
// (pad) are conveyed via the per-section background tints instead.
const LANES = [
  { key: "sfx",      label: "SFX",   color: "#ff9a40", lo: 0,  hi: 0  }, // boot/chime/sniper/impact/zoo/guns/drones/whistle
  { key: "vox",      label: "VOX",   color: "#f599c6", lo: 0,  hi: 0  }, // greeting/booty/screams/bye/vocal stem
  { key: "sub",      label: "SUB",   color: "#a855f7", lo: 30, hi: 60 },
  { key: "kick",     label: "KICK",  color: "#ff5a8a", lo: 30, hi: 50 },
  { key: "snare",    label: "SNARE", color: "#e7c4ff", lo: 0,  hi: 0  }, // snare-roll subdivisions (build crescendo)
  { key: "hat",      label: "HAT",   color: "#7ad9f5", lo: 60, hi: 78 },
  { key: "bells",    label: "BELLS", color: "#4ad1bf", lo: 30, hi: 90 },
  { key: "lead",     label: "LEAD",  color: "#aef240", lo: 40, hi: 84 },
  { key: "piano",    label: "PIANO", color: "#f5d240", lo: 50, hi: 90 },
  { key: "supersaw", label: "SAW",   color: "#9aff4d", lo: 40, hi: 84 },
  { key: "arp",      label: "ARP",   color: "#8a7cff", lo: 40, hi: 88 }, // intro/break1 square arpeggio
];
// laneH is computed lazily inside drawFrame() so empty-lane filtering
// (which happens later, once event arrays are loaded) sizes lanes to
// the populated count, not the declared-lane count.
let laneH;
// Per-section timeline zoom — drop sections STRETCH out so each
// climactic beat gets visual space; intros/outros SQUASH so you
// see further ahead/behind. Transitioned smoothly at section
// boundaries (1.5 s ease). The base PX_PER_SEC is used as a
// fallback for sections not listed here.
const PX_PER_SEC = 820;
const SECTION_PX_PER_SEC = {
  intro:  600,   // squashed — see the whole opening at once
  break1: 760,
  build1: 1000,  // stretched — anticipation builds
  drop1:  1180,  // peak stretch — drop detail
  break2: 760,
  build2: 1000,
  drop2:  1300,  // most stretched — final climax
  outro:  650,   // squashed — retrospective wind-down
};
// Audio-sync compensation — the AAC encoder inserts ~21 ms priming
// at the start of the audio stream, so audio playback lags the
// visual playhead by that amount. Shifting the visual readout
// LATER by this delay realigns the visualization with what the
// listener actually hears.
const AUDIO_DELAY_SEC = Number(flags["audio-delay"] ?? 0.045);
const playheadX = Math.round(W / 2);

// Section tints — DESATURATED palette for the progress bar / dividers.
// Each section gets a muted version of its color family so the bar
// reads as a soft pastel painting of the song structure rather than
// loud neon. Alpha here is the baseline; rendering further modulates
// per-section play state.
const SECTION_TINTS = {
  intro:  "rgba(46,210,118,0.20)",    // vivid emerald
  break1: "rgba(54,150,240,0.20)",    // saturated azure
  build1: "rgba(245,168,40,0.20)",    // bright amber
  drop1:  "rgba(240,64,156,0.20)",    // hot magenta-pink
  break2: "rgba(54,150,240,0.20)",
  build2: "rgba(245,168,40,0.20)",
  drop2:  "rgba(240,64,156,0.20)",
  outro:  "rgba(150,120,205,0.20)",   // dusk violet
};
// --section-tints '{"intro":"rgba(...)",...}' — per-track palette
// override (build.mjs passes the trancepenta DISMAL palette here so
// the section dividers / typography / melt tints run far greyer and
// lower-chroma than the bright trancenwaltzi defaults above). Any
// unspecified section keeps its default.
if (flags["section-tints"]) {
  try {
    const ov = JSON.parse(String(flags["section-tints"]));
    for (const k of Object.keys(ov)) SECTION_TINTS[k] = ov[k];
    console.log(`▸ section-tints override · ${Object.keys(ov).join(",")}`);
  } catch (e) {
    console.error(`⚠ --section-tints parse failed: ${e.message}`);
  }
}

// ── pre-sort events by lane so per-frame culling is fast ─────────────
// Each lane's events also get a `stackRow` index — events that overlap
// in time within the same lane get assigned to different vertical
// rows so doubled hits don't visually merge into a single block.
const laneEvents = {};
for (const lane of LANES) {
  const evs = (struct.events?.[lane.key] || []).slice();
  // Sort by displayedT/t so the tape-stop remap is respected.
  evs.sort((a, b) => (a.displayedT ?? a.t) - (b.displayedT ?? b.t));
  // Greedy interval-graph coloring — pick the lowest row whose
  // previous event has already ended (with a small gap window).
  // Events that overlap go on different rows so the timeline shows
  // doubled hits as visually separated marks.
  const rowEndT = [];
  const STACK_GAP = 0.02; // 20 ms cluster threshold
  for (const ev of evs) {
    const startT = ev.displayedT ?? ev.t;
    const endT = startT + Math.max(0.02, ev.dur || 0.06);
    let row = 0;
    while (row < rowEndT.length && rowEndT[row] > startT - STACK_GAP) row++;
    if (row === rowEndT.length) rowEndT.push(endT);
    else rowEndT[row] = endT;
    ev.stackRow = row;
  }
  lane.maxStackRows = Math.max(1, rowEndT.length);
  laneEvents[lane.key] = evs;
}

// ── lane-population validation ───────────────────────────────────────
// Confirm every declared lane has the data we expect. Prints a table
// to stderr so missing/empty lanes are obvious BEFORE we spend 6 min
// rendering. Pitched lanes also show their MIDI range so it's clear
// whether the events will land inside or get clamped outside the lane.
// Empty lanes are DROPPED from the active LANES array so the
// visualization only shows lanes with actual data (so chill mode's
// empty SNARE/VOX slots don't sit blank).
const allLanes = LANES.slice();
console.log(`▸ lane populations:`);
for (const lane of allLanes) {
  const evs = laneEvents[lane.key];
  const note = evs.length === 0 ? "·· EMPTY ··"
             : ev => ev.midi !== undefined
                ? `midi[${Math.min(...evs.map(e => e.midi))}-${Math.max(...evs.map(e => e.midi))}]`
                : "";
  const pitchInfo = (lane.hi - lane.lo) > 0 && evs.length > 0
    ? ` pitch range ${Math.min(...evs.map(e => e.midi ?? 0))}-${Math.max(...evs.map(e => e.midi ?? 0))} (lane ${lane.lo}-${lane.hi})`
    : "";
  console.log(`  · ${lane.label.padEnd(6)} (${lane.key.padEnd(8)}) → ${String(evs.length).padStart(4)} events, stack=${lane.maxStackRows}${evs.length === 0 ? "  ·· EMPTY ·· (dropped)" : pitchInfo}`);
  void note;
}
// Filter LANES down to populated lanes (and respect --hide-lanes).
// The visualization no longer reserves space for empty slots or for
// lanes the user explicitly hid.
{
  const populated = allLanes.filter((l) => (laneEvents[l.key] || []).length > 0 && !HIDE_LANES.has(l.key));
  LANES.length = 0;
  for (const l of populated) LANES.push(l);
}
console.log(`  → rendering ${LANES.length} populated lane${LANES.length === 1 ? "" : "s"}${HIDE_LANES.size ? ` (hidden: ${[...HIDE_LANES].join(",")})` : ""}`);

// Now that LANES is finalized, compute per-lane height.
laneH = Math.floor(SCORE_H / Math.max(1, LANES.length));
const dropImpacts = (struct.events?.dropImpact || []).slice().sort((a, b) => a.t - b.t);

// ── verlet string + rotating disc + string→illustration warp
//    now live in pop/lib/cover-engine.mjs (shared with undabeach).
//    The engine is constructed after `ctx` exists; see below.

// ── singalong karaoke — ONE word at centre, decode-on-arrival ───────
// Exactly one word is readable at a time: the word being sung NOW,
// pinned at the horizontal centre. Neighbouring words slide through
// as scrambled symbol-strings that DECODE into real letters as they
// approach centre (and re-scramble as they leave). Fixed HUD, lower
// third, dark offset for legibility over the rotating tracks.
const KARA_SYM = "!<>-_/\\[]{}=+*#%&@$?:;~^|".split("");
// Mirrors recap/bin/trance.mjs VOCAL_SECTION_GAIN — the sung stem is
// SILENT in builds + outro, quiet in intro/breaks, full in drops. The
// hook karaoke is gated to match what's actually audible; crossing into
// a silent (build / outro) section the on-screen word DROPS off screen
// like the ending title, then stays hidden until the vocal returns.
const VOCAL_SECTION_GAIN = {
  intro: 0.75, break1: 0.85, build1: 0.0, drop1: 1.0,
  break2: 0.85, build2: 0.0, drop2: 1.0, outro: 0.0,
};
const KARA_FALL_WIN = 0.85;  // s the drop-off animation runs (quick clear)
const KARA_FALL_LEAD = 0.30; // start the fall this long BEFORE the vocal fades
// The continuous sung stem only stays in sync through the OPENING
// audible stretch (intro + break1). At the first silent section (the
// yellow/orange build) the hook captions drop off ONCE and stay gone
// for the rest of the track — they never re-sync in the later drops.
// (greeting + "bye jeffrey" are keep:true and exempt.)
const FIRST_SILENT_START = (() => {
  for (const s of struct.sections)
    if ((VOCAL_SECTION_GAIN[s.name] ?? 1) <= 0.001) return s.startSec;
  return Infinity;
})();
function sectionAt(audioT) {
  const S = struct.sections;
  for (let i = 0; i < S.length; i++) {
    if (audioT >= S[i].startSec && audioT < S[i].endSec) return S[i];
  }
  return audioT >= S[S.length - 1].endSec ? S[S.length - 1] : S[0];
}
function drawKaraoke(audioT) {
  if (lyricWords.length === 0) return;
  const first = lyricWords[0], last = lyricWords[lyricWords.length - 1];
  if (audioT < first.t0 - 1.0 || audioT > last.t1 + 1.0) return;
  let idx = -1;
  for (let i = 0; i < lyricWords.length; i++) {
    if (audioT >= lyricWords[i].t0) idx = i; else break;
  }
  if (idx < 0) idx = 0;
  const act = lyricWords[idx];
  // ── SECTION GATE — hook words only show where the vocal is audible.
  //    greeting / bye words (keep:true) ignore the gate (captions come
  //    back just for "bye jeffrey" even though the outro is silent).
  if (!act.keep) {
    // TERMINAL CUTOFF — hook captions live only up to the first build
    // (the yellow section). They tumble off ONCE there and stay gone
    // permanently until the very end; they do NOT come back in the
    // later drops (the continuous stem no longer lines up).
    {
      const fe = audioT - (FIRST_SILENT_START - KARA_FALL_LEAD);
      if (fe > KARA_FALL_WIN) return;        // cut off — gone for good
      if (fe >= 0) {
        // Tumble the word that's on screen RIGHT NOW off (gravity +
        // spin + fade — same feel as the ending title).
        const word = lyricWords[idx].text;
        const fs = Math.round(titleFontSize * 0.74);
        const [sr, sg, sb] = sectionTcRgb(audioT);
        ctx.save();
        ctx.font = `bold ${fs}px "DejaVu Sans Mono", "Menlo", monospace`;
        ctx.textBaseline = "middle";
        ctx.textAlign = "left";
        const baseY = Math.round(H * 0.82);
        const tw = ctx.measureText(word).width;
        let dx = Math.round(W / 2 - tw / 2);
        for (let c = 0; c < word.length; c++) {
          const g = word[c];
          const cw = ctx.measureText(g).width;
          const cfe = Math.max(0, fe - c * 0.045);       // staggered
          const fdy = 0.5 * 2650 * cfe * cfe;            // gravity
          const spin = (((c * 53) % 11) - 5) * 0.55;
          const frot = spin * cfe + 0.16 * Math.sin(cfe * 9 + c);
          const fa = Math.max(0, 1 - fe / KARA_FALL_WIN);
          if (fa > 0.001) {
            ctx.save();
            ctx.translate(dx + cw / 2, baseY + fdy);
            ctx.rotate(frot);
            ctx.globalAlpha = 0.9 * fa;
            ctx.fillStyle = "rgba(0,0,0,0.9)";
            ctx.fillText(g, -cw / 2 + 3, 4);
            ctx.globalAlpha = fa;
            ctx.fillStyle = `rgb(${sr},${sg},${sb})`;
            ctx.fillText(g, -cw / 2, 0);
            ctx.restore();
          }
          dx += cw;
        }
        ctx.restore();
        return;
      }
      // fe < 0 → still audible; fall hasn't begun → normal render below.
    }
  }
  // Decode lagged a touch — it was running "a bit eager" (letters
  // resolving just before the voice). KARA_LAG holds each letter
  // encoded slightly longer so it snaps as the syllable lands.
  const KARA_LAG = 0.14;
  const prog = Math.max(0, Math.min(1,
    (audioT - act.t0 - KARA_LAG) / Math.max(0.08, act.t1 - act.t0)));
  // The note SLOT is long (held/sustained), but the word is actually
  // PRONOUNCED quickly near the onset then held. So front-load the
  // decode: all letters resolve within the first ~42% of the slot
  // (matches real pronunciation), then stay decoded through the hold.
  const decodeProg = Math.min(1, prog / 0.42);
  const secRgb = sectionTcRgb(audioT); // typography tints to the section
  const fs = Math.round(titleFontSize * 0.74);
  const baseY = Math.round(H * 0.82);
  ctx.save();
  ctx.font = `bold ${fs}px "DejaVu Sans Mono", "Menlo", monospace`;
  ctx.textBaseline = "middle";
  ctx.textAlign = "left";
  const GAP = Math.round(fs * 1.1);
  const wOf = (s) => ctx.measureText(s).width;
  // words in play: idx-1 … idx+2
  const js = [];
  for (let d = -1; d <= 2; d++) { const j = idx + d; if (j >= 0 && j < lyricWords.length) js.push(j); }
  // row-local layout + the focus point that must sit at W/2 (slides
  // from centre-of-active → centre-of-next across prog → continuous,
  // one word centred at a time).
  let lx = 0; const cx = {};
  for (const j of js) { const ww = wOf(lyricWords[j].text); cx[j] = lx + ww / 2; lx += ww + GAP; }
  // TRAIN-AT-STATION: the active word DWELLS dead-centre the whole
  // time it's being sung, then in the last slice of its duration the
  // "train departs" — a smooth eased slide that brings the NEXT word
  // to centre exactly as it becomes active. No mid-word drift.
  const DWELL = 0.74; // hold centred for the first 74% of the word
  let slide = prog <= DWELL ? 0 : (prog - DWELL) / (1 - DWELL);
  slide = slide * slide * (3 - 2 * slide); // smoothstep depart
  const focusLocal = cx[idx] + ((cx[idx + 1] ?? cx[idx]) - cx[idx]) * slide;
  const rowX = W / 2 - focusLocal;
  const tBucket = Math.floor(audioT * 14); // re-scramble cadence
  for (const j of js) {
    const word = lyricWords[j].text;
    const isActive = j === idx;
    if (!isActive) {
      const departed = j < idx;
      let glyphs = "";
      let alpha;
      let extraLeft = 0;
      if (departed) {
        // DEPARTED — only ever the word that JUST left centre; the
        // moment it's no longer current its characters GARBLE straight
        // away (full scramble, instantly — never readable parked text)
        // and it keeps sliding LEFT while fading, fully gone within the
        // first third of the next word. It never stays put.
        if (j !== idx - 1) continue;             // older words: gone
        const deadProg = Math.min(1, prog / 0.35);
        if (deadProg >= 1) continue;             // fully dead
        for (let c = 0; c < word.length; c++) {
          let s = ((j * 131 + c * 17 + tBucket * 7) >>> 0);
          s = (s ^ (s >>> 13)) >>> 0;
          glyphs += KARA_SYM[(s >>> 3) % KARA_SYM.length];
        }
        alpha = (1 - deadProg) * 0.42;
        extraLeft = deadProg * fs * 2.6;         // keeps moving off
      } else {
        // UPCOMING — arrives ENCODED; pure scramble until it's current.
        for (let c = 0; c < word.length; c++) {
          let s = ((j * 131 + c * 17 + tBucket * 7) >>> 0);
          s = (s ^ (s >>> 13)) >>> 0;
          glyphs += KARA_SYM[(s >>> 3) % KARA_SYM.length];
        }
        alpha = 0.34;
      }
      const tw = wOf(glyphs);
      const dx = Math.round(rowX + cx[j] - tw / 2 - extraLeft);
      ctx.globalAlpha = alpha * 0.7;
      ctx.fillStyle = "rgba(0,0,0,0.85)";
      ctx.fillText(glyphs, dx + 3, baseY + 4);
      ctx.globalAlpha = alpha;
      ctx.fillStyle = departed
        ? `rgba(${secRgb[0]},${secRgb[1]},${secRgb[2]},1)`
        : "rgba(225,222,212,1)";
      ctx.fillText(glyphs, dx, baseY);
      continue;
    }
    // CURRENT word — arrives ENCODED and the VOICE decodes it: each
    // letter is a scrambled symbol until the singing edge reaches it,
    // then it snaps to the real letter (and bounces). The decode IS
    // the highlight — way cooler than a plain colour sweep.
    const tw = wOf(word);
    let dx = Math.round(rowX + cx[j] - tw / 2);
    const sungEdge = decodeProg * word.length; // front-loaded decode
    // The single character the voice is ON right now — gets a distinct
    // hot highlight (bigger, glowing, near-white) as it's sung, so the
    // current letter pops, not just the whole decoded run.
    const decodeActive = sungEdge < word.length - 0.001;
    const singC = Math.max(0, Math.min(word.length - 1, Math.floor(sungEdge)));
    for (let c = 0; c < word.length; c++) {
      const decoded = c < sungEdge;       // the voice has reached it
      const isSinging = decodeActive && c === singC && decoded;
      let glyph;
      if (decoded) {
        glyph = word[c];
      } else {
        let s = ((j * 131 + c * 17 + tBucket * 7) >>> 0);
        s = (s ^ (s >>> 13)) >>> 0;
        glyph = KARA_SYM[(s >>> 3) % KARA_SYM.length];
      }
      const cw = wOf(glyph);
      // bump travels with the decode edge — the letter decoding right
      // now lifts most; tiny idle shimmer on the rest.
      const near = Math.max(0, 1 - Math.abs((c + 0.5) - sungEdge) / 1.6);
      const cy = baseY - (near * near) * (fs * 0.32)
               - Math.sin(audioT * 9 + c) * 1.4
               - (isSinging ? fs * 0.16 : 0);   // sung char lifts extra
      ctx.globalAlpha = 0.92;
      ctx.fillStyle = "rgba(0,0,0,0.9)";
      ctx.fillText(glyph, dx + 3, cy + 4);
      ctx.globalAlpha = 1.0;
      // section-tinted: decoded = bright section colour, edge = same
      // pushed toward white, still-encoded = a dim section-tint.
      const [sr, sg, sb] = secRgb;
      ctx.fillStyle = decoded
        ? `rgb(${sr},${sg},${sb})`
        : near > 0.15
          ? `rgb(${Math.round(sr + (255 - sr) * 0.55)},${Math.round(sg + (255 - sg) * 0.55)},${Math.round(sb + (255 - sb) * 0.55)})`
          : `rgba(${Math.round(sr * 0.55 + 110)},${Math.round(sg * 0.55 + 110)},${Math.round(sb * 0.55 + 110)},0.9)`;
      ctx.fillText(glyph, dx, cy);
      // CURRENT-CHAR HIGHLIGHT — overdraw the sung letter bigger, hot
      // (section colour blasted toward white) with a soft glow.
      if (isSinging) {
        ctx.save();
        const xc = dx + cw / 2;
        ctx.textAlign = "center";
        ctx.font = `bold ${Math.round(fs * 1.34)}px "DejaVu Sans Mono", "Menlo", monospace`;
        ctx.shadowColor = `rgba(${sr},${sg},${sb},0.95)`;
        ctx.shadowBlur = fs * 0.6;
        ctx.fillStyle = `rgb(${Math.round(sr + (255 - sr) * 0.8)},${Math.round(sg + (255 - sg) * 0.8)},${Math.round(sb + (255 - sb) * 0.8)})`;
        ctx.fillText(glyph, xc, cy);
        ctx.restore();
        ctx.textAlign = "left";
      }
      dx += cw;
    }
  }
  ctx.restore();
}

// ── title setup ──────────────────────────────────────────────────────
const TITLE_PALETTE = [
  "#aef240", "#f54aa6", "#ffffff", "#4ad1bf",
  "#f599c6", "#9aff4d", "#a44af5", "#7fe05a",
];
const titleFontSize = 95; // smaller — chrome lives in a single flush-bottom strip
const titleChars = TITLE.toLowerCase().split("");
const beatHz = BPM / 60;

// Measure each prefix width via magick (the only renderer that
// understands this YWFT TTF correctly).
function magickMeasureWidth(text) {
  const r = spawnSync("magick", [
    "-font", YWFT_PATH,
    "-pointsize", String(titleFontSize),
    `label:${text}`,
    "-format", "%w",
    "info:",
  ], { encoding: "utf8" });
  if (r.status !== 0) return 0;
  return parseInt(r.stdout.trim(), 10) || 0;
}
const prefixWidths = [0];
let cum = "";
for (const ch of titleChars) {
  cum += ch;
  prefixWidths.push(magickMeasureWidth(cum) || prefixWidths[prefixWidths.length - 1] + titleFontSize * 0.45);
}
// Title sits bottom-left, just inside a small inset.
const titleBaseX = 30;

// ── pre-render YWFT title chars via magick → load as Images ─────────
const charBoxW = Math.ceil(titleFontSize * 1.4);
const charBoxH = Math.ceil(titleFontSize * 1.7);
const charBaselineY = Math.ceil(titleFontSize * 1.25);
const charImgs = new Array(titleChars.length).fill(null);
{
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  mkdirSync(assetsDir, { recursive: true });
  for (let i = 0; i < titleChars.length; i++) {
    const ch = titleChars[i];
    if (ch.trim().length === 0) continue;
    const out = `${assetsDir}/titlechar.${i}.png`;
    // CLEAN single glyph only (solid white, transparent bg, NO baked
    // offset shadow / colour) — the renderer recolours it per section
    // via source-in and draws its own dark drop. Baking a shadow here
    // made source-in tint glyph+shadow → "weird colour doubling".
    const r = spawnSync("magick", [
      "-size", `${charBoxW}x${charBoxH}`,
      "xc:none",
      "-font", YWFT_PATH,
      "-pointsize", String(titleFontSize),
      "-fill", "white",
      "-annotate", `+3+${charBaselineY}`, ch,
      out,
    ]);
    if (r.status !== 0) {
      console.error(`✗ magick failed to render title char '${ch}'`);
      process.exit(1);
    }
    charImgs[i] = await loadImage(out);
  }
}

// ── pre-render section labels + lane labels via magick ───────────────
const sectionLabelImgs = new Map();
const sectionFontSize = 32;
for (const sec of struct.sections) {
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  const out = `${assetsDir}/section.${sec.name}.png`;
  const r = spawnSync("magick", [
    "-background", "none",
    "-fill", "rgba(255,255,255,0.85)",
    "-font", YWFT_PATH,
    "-pointsize", String(sectionFontSize),
    `label:${sec.name}`,
    out,
  ]);
  if (r.status === 0) {
    sectionLabelImgs.set(sec.name, await loadImage(out));
  }
}
const laneLabelImgs = new Map();
const laneFontSize = 22;
for (const lane of LANES) {
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  const out = `${assetsDir}/lane.${lane.key}.png`;
  const r = spawnSync("magick", [
    "-background", "rgba(0,0,0,0.75)",
    "-fill", lane.color,
    "-font", YWFT_PATH,
    "-pointsize", String(laneFontSize),
    `label:${lane.label}`,
    "-bordercolor", "rgba(0,0,0,0.75)",
    "-border", "8x4",
    out,
  ]);
  if (r.status === 0) {
    laneLabelImgs.set(lane.key, await loadImage(out));
  }
}

// ── pre-render the timecode like the wanderbeach video ─────────────
// The whole "mm:ss / mm:ss" string is rasterised ONCE per second as a
// near-WHITE YWFT plate plus a matching solid-BLACK plate (the drop
// shadow). At draw time the white plate is recoloured to the current
// section tint (brightened by section progress) and the black plate
// is stamped twice behind it — identical compositing to wanderbeach.
// Timecode read BIGGER than the title at equal point size (different
// glyph metrics) — pulled to ~0.82× so it visually matches the title.
const tcFontSize = Math.round(titleFontSize * 0.82);
const tcCache = new Map(); // "mm:ss / mm:ss" → { img, shadow }
{
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  mkdirSync(assetsDir, { recursive: true });
  const renderPlate = async (text, fill, outPath) => {
    const r = spawnSync("magick", [
      "-background", "none",
      "-fill", fill,
      "-font", YWFT_PATH,
      "-pointsize", String(tcFontSize),
      `label:${text}`,
      outPath,
    ]);
    if (r.status !== 0) { console.error(`✗ magick failed on timecode '${text}'`); process.exit(1); }
    return await loadImage(outPath);
  };
  const totMm = Math.floor(duration / 60);
  const totSs = Math.floor(duration - totMm * 60).toString().padStart(2, "0");
  for (let s = 0; s <= Math.ceil(duration); s++) {
    const mm = Math.floor(s / 60);
    const ss = (s - mm * 60).toString().padStart(2, "0");
    const text = `${mm}:${ss} / ${totMm}:${totSs}`;
    if (tcCache.has(text)) continue;
    const safe = text.replace(/[^0-9]/g, "_");
    const img = await renderPlate(text, "rgba(255,253,242,0.97)", `${assetsDir}/tc.${safe}.png`);
    const shadow = await renderPlate(text, "rgba(0,0,0,1)", `${assetsDir}/tc.${safe}.shadow.png`);
    tcCache.set(text, { img, shadow });
  }
}
// ── SINGALONG KARAOKE — the sung-vocal words, timed to the stem ────
// trance.mjs places the jeffrey-pvc vocal stem once at the `vocal`
// vox event; trance-hook-vocal-words.json holds per-word {fromMs,
// toMs} relative to the stem. Absolute word time = vocalStart +
// from/1000. Each unique word is pre-rasterised in YWFT (white plate
// + black shadow plate) like the timecode.
// Default to the STRETCHED word timeline (matches the slow sung
// stem — score-stretch.mjs --words-only). The pre-stretch
// trance-hook-vocal-words.json runs ~4× too fast.
const LYRICS_PATH = flags.lyrics
  ? (flags.lyrics.startsWith("~/") ? resolve(homedir(), flags.lyrics.slice(2)) : flags.lyrics)
  : resolve(dirname(fileURLToPath(import.meta.url)), "../out/trance-hook-stretched-words.json");
const lyricWords = []; // { text, t0, t1, img, shadow }
const lyricPlates = new Map(); // text → { img, shadow }
if (existsSync(LYRICS_PATH)) {
  const vEv = (struct.events?.vox || []).find((e) => e.name === "vocal");
  const vocalStart = vEv ? (vEv.displayedT ?? vEv.t) : 2.7;
  let raw = [];
  try { raw = JSON.parse(readFileSync(LYRICS_PATH, "utf8")); } catch {}
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  mkdirSync(assetsDir, { recursive: true });
  const lyrFont = Math.round(titleFontSize * 0.78);
  for (const wd of raw) {
    const text = String(wd.text ?? "").trim();
    if (!text) continue;
    const t0 = vocalStart + (wd.fromMs ?? 0) / 1000;
    const t1 = vocalStart + (wd.toMs ?? (wd.fromMs ?? 0) + 400) / 1000;
    if (!lyricPlates.has(text)) {
      const safe = text.replace(/[^A-Za-z0-9]/g, "_").slice(0, 20) || "w";
      const wp = `${assetsDir}/lyr.${safe}.png`;
      const sp = `${assetsDir}/lyr.${safe}.sh.png`;
      const mk = (fill, out) => {
        const r = spawnSync("magick", [
          "-background", "none", "-fill", fill, "-font", YWFT_PATH,
          "-pointsize", String(lyrFont), `label:${text}`, out,
        ]);
        if (r.status !== 0) { console.error(`✗ magick lyric '${text}'`); process.exit(1); }
        return out;
      };
      mk("rgba(255,253,242,0.98)", wp);
      mk("rgba(0,0,0,1)", sp);
      lyricPlates.set(text, { img: await loadImage(wp), shadow: await loadImage(sp) });
    }
    const pl = lyricPlates.get(text);
    // gate:true → hook words are section-gated (hidden + dropped off
    // screen when the vocal goes silent in builds/outro).
    lyricWords.push({ text, t0, t1, img: pl.img, shadow: pl.shadow, gate: true });
  }
  // ── opening greeting: "good evening jeffrey enjoy los angeles" ──
  // Rides the AC `greeting` vox event so the cold-open speaks it as
  // part of the same lyric train (outside-in → inside-out).
  {
    const gEv = (struct.events?.vox || []).find((e) => e.name === "greeting");
    const gT = gEv ? (gEv.displayedT ?? gEv.t ?? 0.05) : 0.05;
    const gDur = gEv ? (gEv.dur ?? 3.7) : 3.7;
    // Prefer the FORCED-ALIGNED greeting timeline (fromMs/toMs relative
    // to the greeting audio t=0, which the `greeting` vox event places
    // at gT). Falls back to even spacing if the alignment file is
    // missing. This fixes "good evening jeffrey…" being mis-timed.
    const gAlignPath = resolve(
      dirname(fileURLToPath(import.meta.url)), "../out/trance-greeting-words.json");
    let gSeq;
    if (existsSync(gAlignPath)) {
      try { gSeq = JSON.parse(readFileSync(gAlignPath, "utf8")); } catch { gSeq = null; }
    }
    if (!Array.isArray(gSeq) || gSeq.length === 0) {
      const gWords = "good evening jeffrey enjoy los angeles".split(" ");
      const slot = gDur / gWords.length;
      gSeq = gWords.map((text, gi) => ({
        text, fromMs: gi * slot * 1000, toMs: (gi + 0.92) * slot * 1000,
      }));
    }
    for (let gi = 0; gi < gSeq.length; gi++) {
      const text = String(gSeq[gi].text ?? "").trim();
      if (!text) continue;
      if (!lyricPlates.has(text)) {
        const safe = text.replace(/[^A-Za-z0-9]/g, "_").slice(0, 20) || "w";
        const wp = `${assetsDir}/lyr.${safe}.png`;
        const sp = `${assetsDir}/lyr.${safe}.sh.png`;
        const mk = (fill, out) => {
          const r = spawnSync("magick", [
            "-background", "none", "-fill", fill, "-font", YWFT_PATH,
            "-pointsize", String(lyrFont), `label:${text}`, out,
          ]);
          if (r.status !== 0) { console.error(`✗ magick greeting '${text}'`); process.exit(1); }
          return out;
        };
        mk("rgba(255,253,242,0.98)", wp);
        mk("rgba(0,0,0,1)", sp);
        lyricPlates.set(text, { img: await loadImage(wp), shadow: await loadImage(sp) });
      }
      const pl = lyricPlates.get(text);
      lyricWords.push({
        text,
        t0: gT + (gSeq[gi].fromMs ?? 0) / 1000,
        t1: gT + (gSeq[gi].toMs ?? (gSeq[gi].fromMs ?? 0) + 400) / 1000,
        img: pl.img, shadow: pl.shadow, keep: true, // always shown
      });
    }
  }
  // ── closing line: "bye jeffrey" (shutdown) — captions come BACK
  //    just for this even though the outro is otherwise vocal-silent.
  {
    const bEv = (struct.events?.vox || []).find((e) => e.name === "bye-jeffrey");
    if (bEv) {
      // Anchor to .t — that's where trance.mjs actually mixes the bye
      // audio (BYE_SEC). displayedT is a slowdown-perceptual hint that
      // ran the caption ~0.6s late.
      const bT = bEv.t ?? bEv.displayedT;
      const byePath = resolve(
        dirname(fileURLToPath(import.meta.url)), "../out/trance-bye-words.json");
      let bSeq = [{ text: "bye", fromMs: 0, toMs: 500 },
                  { text: "jeffrey", fromMs: 520, toMs: 1100 }];
      if (existsSync(byePath)) {
        try { const j = JSON.parse(readFileSync(byePath, "utf8")); if (Array.isArray(j) && j.length) bSeq = j; } catch {}
      }
      for (const bw of bSeq) {
        const text = String(bw.text ?? "").trim();
        if (!text) continue;
        if (!lyricPlates.has(text)) {
          const safe = text.replace(/[^A-Za-z0-9]/g, "_").slice(0, 20) || "w";
          const wp = `${assetsDir}/lyr.${safe}.png`;
          const sp = `${assetsDir}/lyr.${safe}.sh.png`;
          const mk = (fill, out) => {
            const r = spawnSync("magick", [
              "-background", "none", "-fill", fill, "-font", YWFT_PATH,
              "-pointsize", String(lyrFont), `label:${text}`, out,
            ]);
            if (r.status !== 0) { console.error(`✗ magick bye '${text}'`); process.exit(1); }
            return out;
          };
          mk("rgba(255,253,242,0.98)", wp);
          mk("rgba(0,0,0,1)", sp);
          lyricPlates.set(text, { img: await loadImage(wp), shadow: await loadImage(sp) });
        }
        const pl = lyricPlates.get(text);
        lyricWords.push({
          text,
          t0: bT + (bw.fromMs ?? 0) / 1000,
          t1: bT + (bw.toMs ?? (bw.fromMs ?? 0) + 400) / 1000,
          img: pl.img, shadow: pl.shadow, keep: true,
        });
      }
    }
  }
  lyricWords.sort((a, b) => a.t0 - b.t0);
  console.log(`  → karaoke: ${lyricWords.length} sung words ${lyricWords.length ? `(${lyricWords[0].t0.toFixed(1)}s–${lyricWords[lyricWords.length-1].t1.toFixed(1)}s)` : ""}`);
}
// Offscreen used to recolour white plates (timecode + karaoke) to a
// target colour via source-in (matches wanderbeach).
const lyrTint = createCanvas(8, 8);
const lyrTintCtx = lyrTint.getContext("2d");
// Offscreen used to recolour the white timecode plate to the section
// tint via source-in (matches wanderbeach).
const tcTint = createCanvas(8, 8);
const tcTintCtx = tcTint.getContext("2d");
function tintRgb(s) {
  const m = String(s).match(/rgba?\((\d+),\s*(\d+),\s*(\d+)/);
  return m ? [+m[1], +m[2], +m[3]] : [200, 200, 200];
}
// Section tint brightened toward white by how far through the section
// we are — the timecode reports WHICH section and HOW FAR.
function sectionTcRgb(audioT) {
  let s = struct.sections[struct.sections.length - 1];
  for (const sec of struct.sections) {
    if (audioT >= sec.startSec && audioT < sec.endSec) { s = sec; break; }
  }
  let [r, g, b] = tintRgb(SECTION_TINTS[s.name] || "rgba(255,253,242,1)");
  const span = Math.max(0.001, s.endSec - s.startSec);
  const lp = Math.max(0, Math.min(1, (audioT - s.startSec) / span));
  const k = 0.30 * lp;
  r = Math.round(r + (255 - r) * k);
  g = Math.round(g + (255 - g) * k);
  b = Math.round(b + (255 - b) * k);
  return [r, g, b];
}

// ── two-pals side watermarks (ported from undabeach) ────────────────
// The canonical AC purple-pals.svg, rasterised once, stamped sideways
// hugging the LEFT + RIGHT edges, section-coloured, music-wiggled,
// "seeped" in via multiply/burn/overlay so it stains the picture.
const PALS_WM_SIZE = 212;
let palsImg = null, palsBlur = null;
const wmCanvas = createCanvas(8, 8);
const wmCtx = wmCanvas.getContext("2d");
function hslToRgb(h, s, l) {
  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = l - c / 2;
  let r = 0, g = 0, b = 0;
  if (h < 60) { r = c; g = x; }
  else if (h < 120) { r = x; g = c; }
  else if (h < 180) { g = c; b = x; }
  else if (h < 240) { g = x; b = c; }
  else if (h < 300) { r = x; b = c; }
  else { r = c; b = x; }
  return [Math.round((r + m) * 255), Math.round((g + m) * 255), Math.round((b + m) * 255)];
}
function palsTinted(rgb, src) {
  const ww = src.width, wh = src.height;
  wmCanvas.width = ww; wmCanvas.height = wh;
  wmCtx.clearRect(0, 0, ww, wh);
  wmCtx.globalCompositeOperation = "source-over";
  wmCtx.drawImage(src, 0, 0);
  wmCtx.globalCompositeOperation = "source-in";
  wmCtx.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
  wmCtx.fillRect(0, 0, ww, wh);
  wmCtx.globalCompositeOperation = "source-over";
  return wmCanvas;
}
{
  const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
  const aDir = TRACK.replace(/\.mp3$/, ".assets");
  mkdirSync(aDir, { recursive: true });
  const palsSvg = `${REPO}/system/public/purple-pals.svg`;
  const palsPng = `${aDir}/pals-watermark.png`;
  const palsBlurPng = `${aDir}/pals-watermark-blur.png`;
  if (existsSync(palsSvg)) {
    const r = spawnSync("rsvg-convert",
      ["-w", String(PALS_WM_SIZE * 2), "-h", String(PALS_WM_SIZE * 2), "-o", palsPng, palsSvg]);
    if (r.status === 0 && existsSync(palsPng)) {
      palsImg = await loadImage(palsPng);
      const rb = spawnSync("magick", [palsPng, "-channel", "A", "-blur", "0x1.5", "+channel", palsBlurPng]);
      palsBlur = (rb.status === 0 && existsSync(palsBlurPng)) ? await loadImage(palsBlurPng) : palsImg;
    }
  }
  console.log(`  pals watermark: ${palsImg ? "loaded" : "MISSING"}`);
}
function drawWatermark(audioT) {
  if (!palsImg) return;
  const s = 190;
  const env = envAt(audioT);
  const hue = ((audioT * 14) % 360 + 360) % 360;
  const hRgb = hslToRgb(hue, 0.9, 0.62);
  const [sr, sg, sb] = sectionTcRgb(audioT);
  const col = [
    Math.round(sr * 0.6 + hRgb[0] * 0.4),
    Math.round(sg * 0.6 + hRgb[1] * 0.4),
    Math.round(sb * 0.6 + hRgb[2] * 0.4),
  ];
  const wig = (6 + 14 * env) * Math.sin(audioT * 2.1) + 4 * Math.sin(audioT * 0.7);
  const swiv = 0.05 * Math.sin(audioT * 1.3) + 0.06 * env * Math.sin(audioT * 5.0);
  const inset = s * 0.34;
  const spots = [
    { cx: inset - wig,     cy: H * 0.84, rot:  Math.PI / 2 + swiv },
    { cx: W - inset + wig, cy: H * 0.16, rot: -Math.PI / 2 - swiv },
  ];
  const passes = [["multiply", 0.78], ["color-burn", 0.42], ["overlay", 0.58], ["source-over", 0.06]];
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy);
    ctx.rotate(sp.rot);
    palsTinted([0, 0, 0], palsImg);
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.26;
    ctx.drawImage(wmCanvas, -s / 2 + 3, -s / 2 + 4, s, s);
    palsTinted(col, palsBlur);
    for (const [op, a] of passes) {
      ctx.globalCompositeOperation = op;
      ctx.globalAlpha = a;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.30;
    ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
}

// ── main canvas + load illustrations ─────────────────────────────────
// We load one image per section if mapped, else the default applies
// to every section. Crossfade between adjacent illustrations over a
// 1.5-second window at each section boundary.
console.log(`▸ loading illustration: ${ILLUSTRATION}`);
const defaultIllus = await loadImage(ILLUSTRATION);
const sectionIllusImgs = new Map(); // section.name → Image
const sectionRects    = new Map(); // section.name → { face, laptop, imgW, imgH }
const defaultRects = loadRectsForIllustration(ILLUSTRATION);
for (const [secName, path] of SECTION_ILLUSTRATIONS) {
  if (path && existsSync(path)) {
    sectionIllusImgs.set(secName, await loadImage(path));
    const r = loadRectsForIllustration(path);
    if (r) sectionRects.set(secName, r);
    console.log(`  · section "${secName}" illustration: ${path}${r?.face ? " (face+laptop bboxes loaded)" : ""}`);
  }
}
function illusForSection(secName) {
  return sectionIllusImgs.get(secName) || defaultIllus;
}
function rectsForSection(secName) {
  return sectionRects.get(secName) || defaultRects;
}
// Optional INTRO PRELUDE — a pre-music "everyone is quiet, laptops
// closed, jeffrey head-down" illustration shown ONLY during the
// opening voice prefix. Swaps to the regular intro at the SNIPER hit
// (the "first gunshot"). Wired via the --prelude flag.
const PRELUDE_PATH = expandHome(flags.prelude);
const preludeImg = PRELUDE_PATH && existsSync(PRELUDE_PATH)
  ? await loadImage(PRELUDE_PATH) : null;
// Sniper event marks the gunshot moment where laptops fly open.
const sniperEv = (struct.events?.sfx || []).find((e) => e.name === "sniper");
const PRELUDE_END_SEC = sniperEv ? sniperEv.t : 2.625;

// ── Doom-style strip melt scaffolding ────────────────────────────────
// Each vertical column gets a random "delay" (0..1). During a
// transition, the OLD illustration's column slides DOWNWARD off the
// canvas, revealing the NEW illustration underneath. Columns with
// smaller delays start moving sooner, larger delays last. Like the
// classic Doom screen-melt.
const MELT_COL_W = 25;            // 25-px wide columns → 60 columns on 1500-wide canvas
const MELT_COLS = Math.ceil(W / MELT_COL_W);
const meltDelays = new Float32Array(MELT_COLS);
{
  let s = 999331;
  const rng = () => { s = (s * 1103515245 + 12345) & 0x7fffffff; return s / 0x7fffffff; };
  for (let i = 0; i < MELT_COLS; i++) meltDelays[i] = rng() * 0.55; // up to 0.55 of melt window delay
}
// Coarse columns for the ALWAYS-ON ambient slitscan (cheaper than the
// fine melt columns — the ambient displacement is only a few px so it
// doesn't need fine resolution). Each gets a random phase so the slit
// shimmers organically rather than as one rolling wave.
const SLIT_COL_W = Math.max(40, Math.round(W / 18));
const SLIT_COLS = Math.ceil(W / SLIT_COL_W);
const slitPhase = new Float32Array(SLIT_COLS);
const slitFreq = new Float32Array(SLIT_COLS);
{
  let s = 20240517;
  const rng = () => { s = (s * 1103515245 + 12345) & 0x7fffffff; return s / 0x7fffffff; };
  for (let i = 0; i < SLIT_COLS; i++) {
    slitPhase[i] = rng() * Math.PI * 2;
    slitFreq[i] = 1.1 + rng() * 1.8;
  }
}

// HSL → "r,g,b" string (0..360 / 0..1 / 0..1). Used so the backlight
// hue can drift liberally over time instead of being locked to the
// section colour.
function hslStr(h, s, l) {
  h = ((h % 360) + 360) % 360;
  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = l - c / 2;
  let r = 0, g = 0, b = 0;
  if (h < 60) { r = c; g = x; }
  else if (h < 120) { r = x; g = c; }
  else if (h < 180) { g = c; b = x; }
  else if (h < 240) { g = x; b = c; }
  else if (h < 300) { r = x; b = c; }
  else { r = c; b = x; }
  return `${Math.round((r + m) * 255)},${Math.round((g + m) * 255)},${Math.round((b + m) * 255)}`;
}

// Single reusable offscreen canvas for the OLD image when melting.
const meltCanvas = createCanvas(W, H);
const meltCtx = meltCanvas.getContext("2d");
// A second offscreen for the parallax BACK layer / ambient-slit source.
const backCanvas = createCanvas(W, H);
const backCtx = backCanvas.getContext("2d");

// Reusable offscreen for the shutdown CRT-squeeze pass.
const shutdownCanvas = createCanvas(W, H);
const shutdownCtx = shutdownCanvas.getContext("2d");

// ── STAINED-GLASS BACKLIGHT plumbing ────────────────────────────────
// The lane backlight is rendered as light coming from BEHIND the
// illustration, not a glow stamped on top. To do that we treat each
// illustration as a stained-glass panel: a per-image TRANSMISSION
// MASK says how much light passes through each pixel. Bright / lightly
// pencilled regions transmit (glow with the light colour); dark
// linework + deep shadows act as the leaded came — they block the
// light and stay punchy, so darks keep (even gain) contrast.
//
// The mask is computed ONCE per illustration (8 of them, not per
// frame): luminance → a steep contrast curve crushed hard in the
// shadows. Stored as an opaque-white canvas whose ALPHA is the
// transmission amount, so it can gate the glow via `destination-in`.
const glowCanvas = createCanvas(W, H);
const glowCtx = glowCanvas.getContext("2d");
// Offscreen for crossfading the transmission mask OLD→NEW during a
// section melt so the backlight/contrast layer transitions with the
// picture instead of staying stuck on the old illustration.
const maskBlend = createCanvas(W, H);
const maskBlendCtx = maskBlend.getContext("2d");
const _transmissionMasks = new WeakMap(); // Image → mask Canvas
function getTransmissionMask(img) {
  let m = _transmissionMasks.get(img);
  if (m) return m;
  const iw = img.width, ih = img.height;
  const tmp = createCanvas(iw, ih);
  const tctx = tmp.getContext("2d");
  tctx.drawImage(img, 0, 0, iw, ih);
  const id = tctx.getImageData(0, 0, iw, ih);
  const px = id.data;
  // Transmission curve: smoothstep across [LO, HI] in luminance, then
  // raised to GAMMA so the dark half is crushed toward zero (the lead
  // lines fully block light → high contrast on darks). A small FLOOR
  // lets a whisper of colour bleed through near-blacks so the panel
  // doesn't read as dead matte.
  const LO = 0.30, HI = 0.92, GAMMA = 1.7, FLOOR = 0.04;
  for (let i = 0; i < px.length; i += 4) {
    const L = (0.2126 * px[i] + 0.7152 * px[i + 1] + 0.0722 * px[i + 2]) / 255;
    let s = (L - LO) / (HI - LO);
    s = s < 0 ? 0 : s > 1 ? 1 : s;
    s = s * s * (3 - 2 * s);          // smoothstep
    const tA = FLOOR + (1 - FLOOR) * Math.pow(s, GAMMA);
    px[i] = 255; px[i + 1] = 255; px[i + 2] = 255;
    px[i + 3] = Math.round(tA * 255);
  }
  tctx.putImageData(id, 0, 0);
  _transmissionMasks.set(img, tmp);
  return tmp;
}

// Detect whether this track has the tape-stop ending — drives the
// video shutdown fade in drawFrame. Only normal-mode tracks have it.
const hasTapeStop = (struct.events?.sfx || []).some((e) => e.name === "tape-stop");

// Boot + shutdown beep events — rendered as FULLSCREEN dinks (radial
// flash + thin ring) on the canvas regardless of whether they're
// in the regular per-lane event loop or skipped (shutdown beeps live
// inside the tape-stop region but their audio still plays at original
// wall-clock time).
const bootBeeps = (struct.events?.sfx || [])
  .filter((e) => /^boot-beep-\d+$/.test(e.name))
  .map((e) => ({ t: e.t, kind: "boot" }));
// Keyclick events — "trancenwaltz" typed into the AC prompt before
// the boot melody. Sorted; their times drive the per-char reveal.
const keyClicks = (struct.events?.sfx || [])
  .filter((e) => /^keyclick-\d+$/.test(e.name))
  .map((e) => e.t)
  .sort((a, b) => a - b);
const shutdownBeeps = (struct.events?.sfx || [])
  .filter((e) => /^shutdown-beep-\d+$/.test(e.name))
  .map((e) => ({ t: e.t, kind: "shutdown" }));
// (shutdownBeeps are visualized by the CRT-collapse shutdown
// sequence at the end of the video, not by a separate ring dink.)
void shutdownBeeps;

// ── kick-driven zoom ────────────────────────────────────────────────
// Each kick pulses a soft zoom-in. The camera AIM no longer snaps
// per-kick (that read as a 4 Hz positional saw at 120 BPM) — it
// continuously orbits the face center on sub-Hz incommensurate sines
// (see the orbit math in the per-frame draw block below).
const kicksSorted = (struct.events?.kick || []).slice().sort((a, b) => (a.displayedT ?? a.t) - (b.displayedT ?? b.t));

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

// ── shared cover engine: verlet string + rotating disc + the
//    string→illustration WARP (pop/lib/cover-engine.mjs). Bound to
//    the legacy local names so every existing call site is unchanged.
const _vs = makeVerletString(ctx, { W, H, playheadX, duration });
const trackTheta = _vs.trackTheta;
const withRotation = _vs.withRotation;
const pluckNeedle = _vs.pluck;
const stepNeedle = _vs.step;
const needleXAt = _vs.needleXAt;
const drawNeedleString = _vs.draw;
const warpIllustration = _vs.warp;
const ROT_DIAG = _vs.ROT_DIAG;
// ── rotating-record groove (ported from chillwave/undabeach) ──────────
// Each waveform block additionally arcs about the disc centre by its
// time-offset from the needle — straight AT the playhead, curving more
// the further out, like a groove on a spinning record.
const ROT_CX = Math.round(W / 2);   // engine disc centre
const ROT_CY = Math.round(H / 2);
const GROOVE_R = 950;               // groove radius — larger = gentler arc

// ── string→illustration warp, COUNTER-ROTATED (ported from undabeach)
// The engine's warp() samples an axis-aligned strip, so run plain it
// does NOT turn with the disc. Instead: snapshot the upright frame,
// rotate it −θ into a buffer, then redraw the strip under +θ. The two
// rotations cancel for the CONTENT (illustration stays upright, never
// spins) while the strip + its per-row string-bend live in the
// string's rotated frame — so ONLY the distortion field under the
// string turns WITH the line. Feathered bands → no seam.
// Many THIN bands (was 7 → visibly stripey: each band shifted rigidly
// so the seams showed). 32 thin slices make the horizontal shear
// near-continuous; a small per-band overlap fuses any remaining seam.
const WU_HALF = 240, WU_W = WU_HALF * 2, WU_STEP = 3, WU_BANDS = 56;
const WU_STR = 0.20, WU_BW = WU_W / WU_BANDS, WU_OVER = 2;
const wuWin = new Float64Array(WU_BANDS);
for (let b = 0; b < WU_BANDS; b++) {
  wuWin[b] = Math.sin(Math.PI * ((b + 0.5) / WU_BANDS)) ** 2;
}
const WU_DSZ = Math.ceil(ROT_DIAG) + 4;
const wuSnap = createCanvas(W, H);
const wuSnapC = wuSnap.getContext("2d");
const wuCR = createCanvas(WU_DSZ, WU_DSZ);
const wuCRC = wuCR.getContext("2d");
function warpUnderString(theta, tSec = 0) {
  const { devPeak } = _vs.deflection();
  if (Math.abs(devPeak) < 2) return;          // string ~straight → skip
  // 1) snapshot the upright frame (scene + backlight + lanes)
  wuSnapC.clearRect(0, 0, W, H);
  wuSnapC.drawImage(canvas, 0, 0);
  // 2) counter-rotate it by −θ, centred in the big square
  wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  wuCRC.clearRect(0, 0, WU_DSZ, WU_DSZ);
  wuCRC.translate(WU_DSZ / 2, WU_DSZ / 2);
  wuCRC.rotate(-theta);
  wuCRC.translate(-W / 2, -H / 2);
  wuCRC.drawImage(wuSnap, 0, 0);
  wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  // 3) redraw only the strip under +θ, each band shifted by the
  //    string's local deflection (content upright, strip rotated)
  const sox = (WU_DSZ - W) / 2, soy = (WU_DSZ - H) / 2;
  const x0 = Math.round(playheadX - WU_HALF);
  ctx.save();
  ctx.translate(ROT_CX, ROT_CY);
  ctx.rotate(theta);
  ctx.translate(-ROT_CX, -ROT_CY);
  // SLIT-SCAN + JPEG/DATAMOSH GNARL — no x-band mosaic (that grid read
  // as cliché). Each scan-row shifts the WHOLE strip by the string's
  // local deflection (true slit-scan, zero vertical seams). The shift
  // is then SNAPPED to 8 px macroblocks + hash-jittered so it reads as
  // compression artefacting / datamosh; on hard bends a sub-block is
  // re-stamped offset (a smeared P-frame "drag"). Gnarl scales with the
  // string deflection so it only chews up the picture when it's tugged.
  // Subtle now — the mechanism was reading too clearly. The slit shift
  // is SMOOTH (no hard macroblock stair-step), gentler (WU_STR low),
  // and TAPERS to zero across a feather margin at the strip's left &
  // right edges so the displaced strip dissolves into the un-warped
  // picture with no visible seam. Only the centre band gets full shift
  // (one continuous blit → no mosaic); the thin edge margins ramp the
  // shift 0→full via a handful of slices. Datamosh drag is rare + only
  // on hard bends, so it reads as occasional corruption, not a grid.
  // LIQUID (trancenwaltzi) wants a MUCH gentler string distortion —
  // 0.4× strength + jitter. trancenwaltz proper keeps full strength.
  const wuMul = LIQUID ? 0.4 : 1;
  const gnarlAmt = Math.min(1, Math.abs(devPeak) / 44) * wuMul;
  const FEATHER = 80;                       // px each edge that ramps in
  const MS = 9;                             // feather sub-slices per edge
  const msW = FEATHER / MS;
  const smooth = (u) => u * u * (3 - 2 * u);
  // The jitter PHASE drifts slowly + continuously with time (a creeping
  // band, not a fixed diagonal locked to row index) and the slit also
  // gets a gentle slow sinusoidal sway. Magnitude is small so it reads
  // as a living analog wobble, not hard glitch.
  const tPhase = tSec * 1.7;                       // slow creep (rows/s-ish)
  const sway = Math.sin(tSec * 0.6) * 1.6 * gnarlAmt; // whole-strip drift
  for (let y = 0; y < H; y += WU_STEP) {
    const yi = (y / WU_STEP) | 0;
    let hsh = ((yi + Math.floor(tPhase)) * 2654435761) >>> 0;
    hsh = ((hsh ^ (hsh >>> 15)) * 2246822519) >>> 0;
    // smooth time-interpolated jitter so the band moves continuously
    const fr = tPhase - Math.floor(tPhase);
    let h2 = ((yi + Math.floor(tPhase) + 1) * 2654435761) >>> 0;
    h2 = ((h2 ^ (h2 >>> 15)) * 2246822519) >>> 0;
    const j0 = (hsh & 255) / 255 - 0.5, j1 = (h2 & 255) / 255 - 0.5;
    const jit = ((j0 * (1 - fr) + j1 * fr)) * 1.1 * gnarlAmt + sway; // soft
    const dx = (needleXAt(y) - playheadX) * WU_STR * wuMul + jit; // smooth slit
    // left feather margin — shift ramps 0 → dx
    for (let m = 0; m < MS; m++) {
      const sxl = x0 + m * msW;
      const s = dx * smooth((m + 0.5) / MS);
      ctx.drawImage(wuCR, sox + sxl, soy + y, msW + 1, WU_STEP,
        sxl + s, y, msW + 1, WU_STEP);
    }
    // centre band — one continuous full-shift blit (no internal seam)
    const cL = x0 + FEATHER, cW = WU_W - 2 * FEATHER;
    ctx.drawImage(wuCR, sox + cL, soy + y, cW, WU_STEP,
      cL + dx, y, cW, WU_STEP);
    // right feather margin — shift ramps dx → 0
    for (let m = 0; m < MS; m++) {
      const sxr = x0 + WU_W - FEATHER + m * msW;
      const s = dx * smooth(1 - (m + 0.5) / MS);
      ctx.drawImage(wuCR, sox + sxr, soy + y, msW + 1, WU_STEP,
        sxr + s, y, msW + 1, WU_STEP);
    }
    // datamosh DRAG — VERY rare, only on a strong bend; gentle smear
    // (was reading "a bit glitchy" — pulled way back).
    if ((hsh & 63) === 0 && gnarlAmt > 0.72) {
      const bw = 40 + (hsh % 24);
      const bx = x0 + FEATHER + ((hsh >>> 5) % Math.max(1, cW - bw));
      ctx.drawImage(wuCR, sox + bx, soy + y, bw, WU_STEP,
        bx + dx + 5, y, bw, WU_STEP);
    }
  }
  ctx.restore();
}
// Lane-centre Y stays here (lane geometry, not string state).
const laneCenterY = {};
for (let li = 0; li < LANES.length; li++) {
  laneCenterY[LANES[li].key] = SCORE_TOP + li * laneH + laneH / 2;
}

// ── one-frame renderer ───────────────────────────────────────────────
// `t` is the visual-frame time. `audioT` is the audio time currently
// hitting the listener's ears (shifted later by AAC encoder delay).
// All event positioning + envelope readouts use audioT so the
// timeline animation lines up with what's playing.
// Hoisted bottom-chrome constants. Layout (top-to-bottom):
//   …illustration + score-train events fill from y=0 down to chromeTop
//   chromeTop = H - chromeH        ← bottom strip begins
//     title bouncing (left)        ← within strip
//     timecode (right)             ← within strip
//   progressY = H - progressH      ← progress bar flush to the very bottom
const progressH = 22; // matches the wanderbeach progress bar height
const progressY = H - progressH;
// Bottom-chrome strip — holds the bouncing title + the timecode just
// above the progress bar.
const chromeH = 130;            // enough room for ~95-px title bouncing
const chromeTop = H - progressH - chromeH;
// Title glyph-top anchor — matches the WORKING chillwave insta-story
// model (preview-score.mjs TITLE_TOP_Y = 155, measured against the IG
// Story chrome / "Your story" username row). Moved up from 250 to sit
// snug at the top like chillwave. The typing-intro prompt shares this
// anchor (gy = titleY - charBaselineY) so they never jump apart.
const IG_TOP_SAFE = 155;
// SQUARE cut (1500x1500 trancenwaltzi): title sits up in the very
// TOP-LEFT corner. Vertical (insta-story) keeps the 155 anchor.
// Title hugs the very TOP — minimal air above it (square + vertical
// both sit tight under the top edge / IG-Story UI). Was 44 / 155.
// Vertical/IG-story: 48 hugged the top too tight (barely crossing the
// IG UI). Nudged down ~half the distance we'd raised it (155→48), so
// it clears the chrome. Square cut (not an IG story) stays at 36.
const titleTopInset = (W === H) ? 36 : 100;
const titleY = titleTopInset + charBaselineY;
function drawFrame(t) {
  const audioT = t - AUDIO_DELAY_SEC;
  const env = envAt(audioT);

  // ── TYPING INTRO — before the boot melody the whole screen is the
  //    AC prompt: a pink command block + "trancenwaltz" typed in
  //    char-by-char synced to the keyclick events, with a blinking
  //    pink caret. At the first boot beep the normal frame (with the
  //    pixelate-startup) takes over.
  if (keyClicks.length > 0 && bootBeeps.length > 0 && audioT < bootBeeps[0].t) {
    // The LAPTOPS-CLOSED prelude frame is behind the boot prompt (gets
    // more screen time) — drawn cover-fit, then a translucent AC-purple
    // wash on top so it reads as the prompt.mjs theme without hiding the
    // art. The prelude override then HOLDS this same frame after typing
    // until the snap (PRELUDE_END_SEC) so there's no scene pop.
    {
      const bi = preludeImg || defaultIllus;
      const sc = Math.max(W / bi.width, H / bi.height);
      const dw = bi.width * sc, dh = bi.height * sc;
      ctx.drawImage(bi, (W - dw) / 2, (H - dh) / 2, dw, dh);
      ctx.fillStyle = "rgba(27,0,51,0.62)"; // #1b0033 wash — art shows through
      ctx.fillRect(0, 0, W, H);
    }
    const PINK = "#e0468c"; // AC prompt pink
    let typed = 0;
    for (const kt of keyClicks) { if (audioT >= kt) typed++; else break; }
    typed = Math.min(typed, titleChars.length);
    const blockW = Math.round(charBoxW * 0.30);
    const blockH = Math.round(charBaselineY * 0.96);
    // INSTA: type from the left at the top safe anchor. TIKTOK: type
    // into the CENTRE of screen (matches the centred track title so the
    // hand-off doesn't move).
    const gy = TIKTOK
      ? Math.round((H - charBoxH) / 2)
      : titleY - charBaselineY;                 // glyph-box top (no bounce)
    const startX = TIKTOK
      ? Math.round((W - prefixWidths[titleChars.length]) / 2)
      : titleBaseX;
    const promptY = gy + Math.round((charBoxH - blockH) / 2);
    // Typed chars render through the EXACT same stack as the in-video
    // title (soft shade + hard dark drop + section/palette tint) so the
    // letters are pixel-identical in size/weight to when they "show up"
    // in the song — no jump in scale at boot.
    const [tSr, tSg, tSb] = sectionTcRgb(audioT);
    const tintLocal = (im, r, g, b) => {
      lyrTint.width = im.width; lyrTint.height = im.height;
      lyrTintCtx.clearRect(0, 0, im.width, im.height);
      lyrTintCtx.globalCompositeOperation = "source-over";
      lyrTintCtx.drawImage(im, 0, 0);
      lyrTintCtx.globalCompositeOperation = "source-in";
      lyrTintCtx.fillStyle = `rgb(${r},${g},${b})`;
      lyrTintCtx.fillRect(0, 0, im.width, im.height);
      return lyrTint;
    };
    for (let i = 0; i < typed; i++) {
      const img = charImgs[i];
      if (!img) continue;
      const cx = startX + prefixWidths[i] - 3;
      const blk = tintLocal(img, 0, 0, 0);
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      for (const [ox, oy, a] of [[-2,-1,0.18],[2,3,0.20],[5,7,0.20],[0,3,0.22]]) {
        ctx.globalAlpha = a; ctx.drawImage(blk, cx + ox, gy + oy);
      }
      for (const [ox, oy, a] of [[3,4,0.95],[4,5,0.85],[3,5,0.70]]) {
        ctx.globalAlpha = a; ctx.drawImage(blk, cx + ox, gy + oy);
      }
      ctx.restore();
      const pal = ncHexToRgb(TITLE_PALETTE[i % TITLE_PALETTE.length]);
      ctx.drawImage(tintLocal(img,
        Math.round(tSr * 0.55 + pal[0] * 0.45),
        Math.round(tSg * 0.55 + pal[1] * 0.45),
        Math.round(tSb * 0.55 + pal[2] * 0.45)), cx, gy);
    }
    const caretX = startX + prefixWidths[typed] + 4; // RIGHT of the typed text
    // The pink block IS the cursor — it rides at the right of the
    // text, advancing as each letter lands. Blinks ~3 Hz, so before
    // the first keystroke it blinks a couple of times in place.
    if (Math.floor(audioT * 6) % 2 === 0) {
      ctx.fillStyle = PINK;
      ctx.fillRect(Math.round(caretX), promptY, blockW, blockH);
    }
    // Pals watermark + greeting captions belong on the boot screen too
    // (pals present the WHOLE video; the "good evening jeffrey…" line is
    // captioned in real time here so it never plays catch-up later).
    // TIKTOK has NO side pals (karaoke is kept).
    if (!TIKTOK) drawWatermark(audioT);
    drawKaraoke(audioT);
    return; // this IS the whole frame during the typing phase
  }

  // ── compute per-section PX_PER_SEC with smooth crossfade ──────────
  // Lerp from current section's px/sec to the next at section
  // boundaries over a 1.5 s window. Outside the boundary, the value
  // stays at the current section's level.
  let pps = PX_PER_SEC;
  {
    let cur = null, nxt = null;
    for (let i = 0; i < struct.sections.length; i++) {
      if (audioT >= struct.sections[i].startSec && audioT < struct.sections[i].endSec) {
        cur = struct.sections[i];
        nxt = struct.sections[i + 1] || null;
        break;
      }
    }
    if (cur) {
      const curPPS = SECTION_PX_PER_SEC[cur.name] ?? PX_PER_SEC;
      const nxtPPS = nxt ? (SECTION_PX_PER_SEC[nxt.name] ?? PX_PER_SEC) : curPPS;
      const tillNext = nxt ? (cur.endSec - audioT) : 1e9;
      const xfWindow = 1.5;
      if (tillNext < xfWindow) {
        const k = 1 - tillNext / xfWindow; // 0 → 1
        // Smoothstep ease for a gentle squash/stretch transition.
        const e = k * k * (3 - 2 * k);
        pps = curPPS + (nxtPPS - curPPS) * e;
      } else {
        pps = curPPS;
      }
    }
  }

  // ── FULL illustration as background ── face-focused ken-burns ────
  // Per-section illustrations crossfade at boundaries (1.5 s window
  // before each section change). When --illustrations isn't passed,
  // every section uses the same default illustration so no crossfade
  // is visible.
  // Musically-timed Doom-melt transition — centered ON each section
  // boundary, runs for exactly 1 bar (half before / half after).
  // crossfadeT = 0 at start of melt → 1 at end.
  // We HOLD currentSec at the OLD section throughout the melt so the
  // OLD illustration is what falls away. As soon as the melt ends,
  // currentSec advances to the new one normally.
  const beatSecLocal = 60 / BPM;
  const barSecLocal = beatSecLocal * (struct.meter || 3);
  const meltWindow = barSecLocal;
  // The melt RESOLVES exactly on the musical landing — the section
  // boundary, or a dropImpact within ~0.45 bar of it (snap to the
  // drop for sample-tight sync). The whole fall happens across the
  // bar BEFORE the landing, so the NEW illustration "lands" on the
  // drop instead of the old symmetric ±half-bar straddle.
  const DROP_SNAP = barSecLocal * 0.45;
  const anchorFor = (i) => {
    const bnd = struct.sections[i].endSec;
    let a = bnd, best = DROP_SNAP;
    for (const di of dropImpacts) {
      const d = Math.abs(di.t - bnd);
      if (d < best) { best = d; a = di.t; }
    }
    return a;
  };
  let currentSec = struct.sections[0];
  let nextSec = struct.sections[1] || null;
  let crossfadeT = 0;
  let meltAnchor = null; // resolved landing time of the NEXT transition
  // Default section lookup.
  if (audioT >= struct.sections[struct.sections.length - 1].endSec) {
    currentSec = struct.sections[struct.sections.length - 1];
    nextSec = null;
  } else {
    for (let i = 0; i < struct.sections.length; i++) {
      if (audioT >= struct.sections[i].startSec && audioT < struct.sections[i].endSec) {
        currentSec = struct.sections[i];
        nextSec = struct.sections[i + 1] || null;
        break;
      }
    }
  }
  // Soonest upcoming landing → drives the pre-melt slit ramp; and if
  // we're inside its 1-bar melt window, drive the crossfade + hold OLD.
  for (let i = 0; i < struct.sections.length - 1; i++) {
    const a = anchorFor(i);
    if (a <= audioT) continue; // already past this landing
    meltAnchor = a;            // the nearest future landing
    if (audioT >= a - meltWindow) {
      currentSec = struct.sections[i];
      nextSec = struct.sections[i + 1];
      crossfadeT = Math.max(0, Math.min(1, (audioT - (a - meltWindow)) / meltWindow));
    }
    break;
  }
  let illusImg = illusForSection(currentSec.name);
  let nextIllus = nextSec ? illusForSection(nextSec.name) : null;
  // PRELUDE OVERRIDE — show the "head-down, all-laptops-closed" frame
  // from t=0 up to the sniper hit (the "first gunshot"). Then DOOM-melt
  // into the regular intro illustration over 0.6 s, peaking with a
  // white-flash punch right on the sniper.
  let preludeFlash = 0;
  if (preludeImg) {
    const meltDur = 0.6;
    if (audioT < PRELUDE_END_SEC - meltDur * 0.5) {
      illusImg = preludeImg;
      nextIllus = null;
      crossfadeT = 0;
    } else if (audioT < PRELUDE_END_SEC + meltDur * 0.5) {
      // Force a melt with prelude → real intro section.
      illusImg = preludeImg;
      nextIllus = sectionIllusImgs.get("intro") || defaultIllus;
      const localT = (audioT - (PRELUDE_END_SEC - meltDur * 0.5)) / meltDur;
      crossfadeT = Math.max(0, Math.min(1, localT));
      // White-flash punch peaks AT the sniper instant and decays.
      const dt = audioT - PRELUDE_END_SEC;
      if (dt >= -0.05 && dt < 0.35) {
        preludeFlash = Math.max(0, 1 - Math.abs(dt) / 0.35);
      }
    }
  }

  const aspect = illusImg.width / illusImg.height;
  // ── smooth slow camera ──────────────────────────────────────────
  // Cover-fit so portrait or square illustrations always fully cover
  // the canvas. baseScale is the LARGER of width-fit / height-fit, so
  // the illustration overhangs at least one axis. Then a 20 % zoom
  // factor on top gives ken-burns slack.
  // COVER-fit (fills the frame — NO letterbox, no added top/bottom
  // pixels / repeated band). baseScale 1.0 (below) sits exactly at
  // cover so it crops far LESS than the old 1.12 overscan while the
  // ken-burns still breathes in/out.
  const coverScale = Math.max(W / illusImg.width, H / illusImg.height);
  // Pulled WAY back — sit just above exact-cover so the whole scene
  // (pixies + setting), not just jeffrey's face, reads. Then BOUNCE:
  // a slow breath + a punchy envelope-driven zoom so the picture
  // pumps with the music instead of staying clamped tight.
  // Enough overscan that the slow ken-burns + soft kick zoom never
  // expose an edge (1.005 left ~0 room → any motion slammed off-frame
  // = the "really bad / jerky" pan). Smooth, never jerky.
  const baseScale = 1.0; // rest exactly at full-width edges (was 1.12 overscan)
  // ORGANIC motion: a slow ken-burns breath (~16 s) + a SOFT kick
  // zoom — each kick eases the picture in a touch then releases. Soft
  // attack (~70 ms) → gentle exp release; max() over recent kicks so
  // dense kicks read as one continuous swell, never a per-frame snap.
  const slowBreath = 0.028 * (0.5 - 0.5 * Math.cos(audioT * (2 * Math.PI / 16)));
  let kickZoom = 0;
  // Softer kick zoom — 140 ms attack (was 70 ms / ~2 frames at 30 fps,
  // which read as a hard snap) into a gentle ~0.34 s exponential
  // release. max() over recent kicks → dense kicks read as one swell.
  for (let i = kicksSorted.length - 1; i >= 0; i--) {
    const kt = kicksSorted[i].displayedT ?? kicksSorted[i].t;
    const since = audioT - kt;
    if (since < 0) continue;
    if (since > 1.0) break;
    const e01 = since < 0.14 ? since / 0.14 : Math.exp(-(since - 0.14) / 0.34);
    if (e01 > kickZoom) kickZoom = e01;
  }
  // Slower kick-activity envelope — drives the face MAGNET on the aim.
  // 200 ms attack + ~0.6 s release, so the camera glides toward the
  // face center when kicks are active and drifts back to the slow orbit
  // when they stop. No snap at either edge.
  let kickAct = 0;
  for (let i = kicksSorted.length - 1; i >= 0; i--) {
    const kt = kicksSorted[i].displayedT ?? kicksSorted[i].t;
    const since = audioT - kt;
    if (since < 0) continue;
    if (since > 1.5) break;
    const e01 = since < 0.20 ? since / 0.20 : Math.exp(-(since - 0.20) / 0.60);
    if (e01 > kickAct) kickAct = e01;
  }
  // BEAT-REACTIVE camera: a slow ken-burns breath + a smoothed kick
  // zoom PLUS the verlet string's signed-spring scene bump (cover-
  // engine sceneOffset() — already low-passed so it follows the bend
  // without per-frame jerk). kickZoom amp bumped back up to 0.10 so the
  // in-pump on the face reads, but the 140 ms attack keeps it from
  // snapping. stringPan dampened (* 0.45) so dense-kick sections
  // (break1, drops) don't shake the scene — the bend is felt through
  // the zoomMul instead.
  const _so = _vs.sceneOffset();
  const zoom = (baseScale + slowBreath + kickZoom * 0.10) * _so.zoomMul;
  const stringPanX = _so.dx * 0.25;
  const stringPanY = _so.dy * 0.25;
  void aspect;
  const baseW = illusImg.width * coverScale * zoom;
  const baseH = illusImg.height * coverScale * zoom;
  // Wobble — frame-by-frame motion at multiple cross-frequencies so
  // the painting reads as alive rather than dead-still. AMPLITUDES
  // pulled to ~40% of the prior values: a faint living drift only,
  // not the wandering pan it was reading as.
  const wobbleX = 3.6 * Math.sin(t * 0.55) + 1.6 * Math.cos(t * 0.9);
  const wobbleY = 2.8 * Math.cos(t * 0.5) + 1.2 * Math.sin(t * 0.8);
  // Continuous slow orbit around the face center, with a smooth FACE
  // MAGNET driven by kickAct: when kicks are firing the aim glides
  // toward the face center (camera tightens on the face); when they
  // stop, it drifts back to the orbit. Both signals are continuous —
  // no kick-coupled snap, no per-beat velocity reversals.
  let centerX = (W - baseW) / 2;
  let centerY = (H - baseH) / 2;
  const imgW = faceRect ? faceRect.imgW : illusImg.width;
  const imgH = faceRect ? faceRect.imgH : illusImg.height;
  const focusFx = faceRect ? faceRect.x + faceRect.w * 0.5 : imgW / 2;
  const focusFy = faceRect ? faceRect.y + faceRect.h * 0.5 : imgH / 2;
  const orbitR = Math.min(imgW, imgH) * 0.020;
  const orbitFx = focusFx + Math.cos(audioT * 0.18) * orbitR;
  const orbitFy = focusFy + Math.sin(audioT * 0.12) * orbitR * 0.7;
  const magnet = kickAct * 0.65;
  const aimFx = orbitFx + (focusFx - orbitFx) * magnet;
  const aimFy = orbitFy + (focusFy - orbitFy) * magnet;
  // Translate aim point to a draw offset that puts the aim under
  // the canvas center.
  const scaledFx = aimFx * (baseW / imgW);
  const scaledFy = aimFy * (baseH / imgH);
  centerX = W / 2 - scaledFx;
  centerY = H / 2 - scaledFy;
  // + the verlet string's signed-spring pan so the picture visibly
  // lurches WITH the string swing. It's the low-passed sceneOffset()
  // from cover-engine (not a raw deflection) so it eases, never jerks;
  // the cover-clamp just below guarantees it can never expose an edge.
  let drawX = centerX + wobbleX + stringPanX;
  let drawY = centerY + wobbleY + stringPanY;
  // Clamp so the illustration always covers the canvas — no black
  // bars on the sides. For a baseW × baseH image to cover W × H, the
  // draw position must satisfy: W - baseW ≤ drawX ≤ 0 (and same for Y).
  // Cover-clamp — image always covers the canvas, no black/bled edge.
  drawX = Math.min(0, Math.max(W - baseW, drawX));
  drawY = Math.min(0, Math.max(H - baseH, drawY));
  // ── parallax / depth / slit drivers ────────────────────────────
  // Layer separation gives depth + a tight shake. The slitscan is a
  // MUSIC-DRIVEN horizontal-row tear: near-silent when quiet, growing
  // with amplitude, and exploding into a FINE per-pixel-row scan in
  // the bar before each drop (preMelt).
  const tillAnchor = meltAnchor !== null ? meltAnchor - audioT : Infinity;
  const preMelt = tillAnchor > 0 && tillAnchor < barSecLocal * 1.6
    ? 1 - tillAnchor / (barSecLocal * 1.6) : 0;
  // PreMelt-only swell — env-driven constant slit removed (it was
  // adding a faint per-row ripple every frame that read as a shaky
  // illustration). The pre-drop splash is preserved.
  const slitAmp = preMelt * preMelt * 72;
  const parScale = 1.055 + env * 0.03;
  const pX = 9 * Math.sin(t * 0.55) + env * 16 * Math.sin(t * 2.6);
  const pY = 7 * Math.cos(t * 0.50) + env * 13 * Math.cos(t * 2.2);
  // Horizontal-row slitscan. Row height shrinks toward 2 px as the
  // drop approaches → "much finer grain individual pixel rows pre
  // transition". Each row's shift rides the BEAT (musical) plus a
  // per-row chaotic comb that tears it apart right before the landing.
  const beatPhase = audioT * beatHz * 2 * Math.PI;
  const paintSlit = (img, dx, dy, dw, dh, amp) => {
    if (LIQUID) {
      // WATER RIPPLE — fine rows displaced by smooth traveling sines
      // (no tear / no chaos): the picture reads as seen through gently
      // rippling water, swelling a touch with the music + pre-drop.
      backCtx.clearRect(0, 0, W, H);
      backCtx.drawImage(img, dx, dy, dw, dh);
      const rh = 3;
      const a = 5 + env * 9 + preMelt * 26;          // gentle → swelling
      for (let y = 0; y < H; y += rh) {
        const off = a * (Math.sin(y * 0.017 + audioT * 1.7) * 0.6
                       + Math.sin(y * 0.046 - audioT * 2.3) * 0.4);
        ctx.drawImage(backCanvas, 0, y, W, rh, off, y, W, rh);
      }
      return;
    }
    if (amp < 3) { ctx.drawImage(img, dx, dy, dw, dh); return; }
    backCtx.clearRect(0, 0, W, H);
    backCtx.drawImage(img, dx, dy, dw, dh);
    const grain = Math.min(1, preMelt + env * 0.3);
    const rowH = Math.max(2, Math.round(16 - 14 * grain));
    for (let y = 0; y < H; y += rowH) {
      const n = Math.sin(y * 12.9898 + 7.233) * 43758.5453;
      const rnd = n - Math.floor(n); // stable per-row 0..1
      const wave = Math.sin(beatPhase * (0.5 + rnd) + rnd * 6.283);
      const chaos = preMelt * (rnd - 0.5) * 2; // torn rows near the drop
      const off = amp * (wave * 0.6 + chaos);
      ctx.drawImage(backCanvas, 0, y, W, rowH, off, y, W, rowH);
    }
  };
  // Dim parallax BACK plate behind the crisp front — the differential
  // motion (front fixed, back swimming on pX/pY) reads as depth.
  const drawDepth = (img, fx, fy, fw, fh, parMul) => {
    ctx.fillStyle = "#000";
    ctx.fillRect(0, 0, W, H);
    ctx.drawImage(img,
      fx - pX * parMul, fy - pY * parMul, fw * parScale, fh * parScale);
    ctx.fillStyle = "rgba(0,0,0,0.18)"; // lighter — illos not so darkened
    ctx.fillRect(0, 0, W, H);
  };

  // String-bend pan/zoom/bump is already folded into drawX/drawY/zoom
  // above (no ctx transform → backlight mask + lanes stay registered).
  if (nextIllus && crossfadeT > 0 && nextIllus !== illusImg) {
    // ── DOOM melt WITH two-layer depth ─────────────────────────────
    // NEW is the incoming plane: its own parallax back-plate + a
    // slit-displaced front (so the two layers + slit carry through
    // the whole transition). OLD then melts down in fine columns on
    // top, each column also carrying the ambient slit so the melt
    // grows organically out of it.
    const nextCoverScale = Math.max(W / nextIllus.width, H / nextIllus.height);
    const nextBaseW2 = nextIllus.width * nextCoverScale * zoom;
    const nextBaseH2 = nextIllus.height * nextCoverScale * zoom;
    let nextX = (W - nextBaseW2) / 2 + wobbleX;
    let nextY = (H - nextBaseH2) / 2 + wobbleY;
    nextX = Math.min(0, Math.max(W - nextBaseW2, nextX));
    nextY = Math.min(0, Math.max(H - nextBaseH2, nextY));
    drawDepth(nextIllus, nextX, nextY, nextBaseW2, nextBaseH2, 0.55);
    paintSlit(nextIllus, nextX, nextY, nextBaseW2, nextBaseH2,
              Math.max(slitAmp, 4));
    if (LIQUID) {
      // LIQUID DISSOLVE — the OLD plate ripples + fades into the NEW
      // through water (no column shear). A smooth opacity crossfade on
      // the rippled old illo.
      const ease = crossfadeT * crossfadeT * (3 - 2 * crossfadeT);
      ctx.save();
      ctx.globalAlpha = Math.max(0, 1 - ease);
      paintSlit(illusImg, drawX, drawY, baseW, baseH, slitAmp);
      ctx.restore();
    } else {
      // OLD column-melt on top.
      meltCtx.clearRect(0, 0, W, H);
      meltCtx.drawImage(illusImg, drawX, drawY, baseW, baseH);
      for (let c = 0; c < MELT_COLS; c++) {
        const colX = c * MELT_COL_W;
        const localT = (crossfadeT - meltDelays[c]) / Math.max(0.01, 1 - meltDelays[c]);
        if (localT >= 1) continue;
        const sc = Math.floor(colX / SLIT_COL_W) % SLIT_COLS;
        const jitter = slitAmp * 0.6 * Math.sin(t * slitFreq[sc] * 1.6 + slitPhase[sc]);
        const fall = Math.max(0, localT) * H * 1.05 + jitter;
        ctx.drawImage(meltCanvas,
          colX, 0, MELT_COL_W, H,
          colX, fall, MELT_COL_W, H);
      }
    }
  } else {
    // No transition — depth back-plate + slit-displaced front.
    drawDepth(illusImg, drawX, drawY, baseW, baseH, 1.0);
    paintSlit(illusImg, drawX, drawY, baseW, baseH, slitAmp);
  }

  // PRELUDE → INTRO punch — a screen-blended white flash that peaks
  // right on the sniper hit, mirrors the "laptops fly open / we here"
  // moment in the audio.
  if (preludeFlash > 0.01) {
    ctx.save();
    ctx.globalCompositeOperation = "screen";
    ctx.globalAlpha = preludeFlash * 0.85;
    ctx.fillStyle = "rgba(255,255,240,1)";
    ctx.fillRect(0, 0, W, H);
    ctx.restore();
  }

  // ── LANE BACKLIGHT — kicks light up the face region; hats light up
  //                    the laptop region. Both render as soft radial
  //                    glows in lane colors, anchored to the current
  //                    section's face/laptop bboxes (transformed from
  //                    image space to live canvas space using the
  //                    ken-burns scale + offset). Each event fades over
  //                    BACKLIGHT_DECAY seconds.
  {
    const rects = rectsForSection(currentSec.name);
    if (rects && (rects.face || rects.laptop)) {
      const sx = baseW / illusImg.width;
      const sy = baseH / illusImg.height;
      // Image-rect → live canvas rect.
      const imgToCanvas = (r) => r ? {
        cx: drawX + (r.x + r.w / 2) * sx,
        cy: drawY + (r.y + r.h / 2) * sy,
        rw: r.w * sx,
        rh: r.h * sy,
      } : null;
      const faceCanvas   = imgToCanvas(rects.face);
      const laptopCanvas = imgToCanvas(rects.laptop);
      // FAST + TIGHT: a very short decay so each hit snaps on and
      // clears before the next, locked hard to event timing.
      const BACKLIGHT_DECAY = 0.42; // s — long enough to read as a BLAST
      // Most-recent kick / hat intensity inside the decay window.
      const recentMax = (lane) => {
        let m = 0;
        for (const ev of (struct.events?.[lane] || [])) {
          const evT = ev.displayedT ?? ev.t;
          const since = audioT - evT;
          if (since >= 0 && since < BACKLIGHT_DECAY) {
            const k = 1 - since / BACKLIGHT_DECAY;
            if (k > m) m = k;
          }
        }
        return m;
      };
      // Amplitude-reactive + sensitive: the live envelope gates the
      // flash so loud passages flare and quiet ones barely register,
      // plus a whisper of always-on env shimmer so the panel breathes.
      // EXTREME / VIBEIN — push the transmitted illumination hard.
      const ampGate = 0.30 + 1.05 * env; // pulled back — was blowing out
      const ambient = 0.20 * env;
      // BEAT-REACTIVE BACKLIT BUMP — an explicit kick-synced punch on
      // TOP of the envelope gate so the light visibly THUMPS behind
      // the faces with the kick (sharp ~30ms attack, ~260ms exp tail),
      // independent of the slower RMS envelope. This is the "backlit
      // bump that pulses with the kick/amplitude".
      let kickPunch = 0;
      for (let ki = kicksSorted.length - 1; ki >= 0; ki--) {
        const kt = kicksSorted[ki].displayedT ?? kicksSorted[ki].t;
        const dts = audioT - kt;
        if (dts < 0) continue;
        if (dts > 0.6) break;
        const p = dts < 0.03 ? dts / 0.03 : Math.exp(-(dts - 0.03) / 0.26);
        if (p > kickPunch) kickPunch = p;
      }
      const bump = 1 + 0.85 * kickPunch;            // up to ~1.85× on the beat
      const maxK = faceCanvas   ? Math.max(recentMax("kick") * ampGate * bump, ambient + 0.55 * kickPunch) : 0;
      const maxH = laptopCanvas ? Math.max(recentMax("hat")  * ampGate * bump, ambient) : 0;
      // Hue drifts LIBERALLY over time + amplitude — not locked to the
      // section colour. Kick + hat ride different points of the wheel.
      const kickHue = audioT * 23 + env * 90;
      const hatHue  = audioT * 17 + 140 + env * 70;
      const kHi = hslStr(kickHue, 0.95, 0.60), kLo = hslStr(kickHue, 0.95, 0.16);
      const hHi = hslStr(hatHue,  0.95, 0.58), hLo = hslStr(hatHue,  0.95, 0.15);

      if (maxK > 0.008 || maxH > 0.008) {
        // Paint a hot radial into the offscreen glow buffer. Additive
        // (`lighter`) with a near-white core so it reads as an actual
        // emitter, not a tint — overlapping face+laptop glows sum.
        const stamp = (region, rgb, k0) => {
          if (!region || k0 <= 0.008) return;
          // Punchy attack, very tight tail.
          const k = Math.pow(Math.min(1, k0), 0.40);
          const radius = Math.max(region.rw, region.rh) * 1.7; // bigger blast
          const g = glowCtx.createRadialGradient(
            region.cx, region.cy, 0, region.cx, region.cy, radius);
          // BLASTING through: hot white core → saturated body → edge.
          g.addColorStop(0.0, `rgba(255,255,255,${0.98 * k})`);
          g.addColorStop(0.14, `rgba(255,255,255,${0.72 * k})`);
          g.addColorStop(0.30, `rgba(${rgb},${1.0 * k})`);
          g.addColorStop(0.58, `rgba(${rgb},${0.62 * k})`);
          g.addColorStop(1.0, `rgba(${rgb},0)`);
          glowCtx.globalCompositeOperation = "lighter";
          glowCtx.fillStyle = g;
          glowCtx.fillRect(
            Math.max(0, region.cx - radius),
            Math.max(0, region.cy - radius),
            Math.min(W, radius * 2),
            Math.min(H, radius * 2));
        };
        // Mask gate: during a section melt, CROSSFADE the old + new
        // transmission masks by crossfadeT so the backlight/contrast
        // layer transitions WITH the picture (was stuck on the old
        // illustration through the whole melt).
        let maskDraw;
        if (nextIllus && crossfadeT > 0 && nextIllus !== illusImg) {
          const mOld = getTransmissionMask(illusImg);
          const mNew = getTransmissionMask(nextIllus);
          maskBlendCtx.clearRect(0, 0, W, H);
          maskBlendCtx.globalAlpha = 1 - crossfadeT;
          maskBlendCtx.drawImage(mOld, drawX, drawY, baseW, baseH);
          maskBlendCtx.globalAlpha = crossfadeT;
          maskBlendCtx.drawImage(mNew, drawX, drawY, baseW, baseH);
          maskBlendCtx.globalAlpha = 1;
          maskDraw = (c) => c.drawImage(maskBlend, 0, 0);
        } else {
          const m = getTransmissionMask(illusImg);
          maskDraw = (c) => c.drawImage(m, drawX, drawY, baseW, baseH);
        }
        // ── PASS 1: transmitted light ──────────────────────────────
        // Light that passes THROUGH the panel. Gate the glow by the
        // transmission mask (keep it only where the illustration is
        // light/translucent) then add it to the scene. Dark linework
        // has ~0 transmission so it stays untouched & punchy.
        glowCtx.globalCompositeOperation = "source-over";
        glowCtx.clearRect(0, 0, W, H);
        stamp(faceCanvas,   kHi, maxK);
        stamp(laptopCanvas, hHi, maxH);
        glowCtx.globalCompositeOperation = "destination-in";
        maskDraw(glowCtx);
        // BRIGHTER WASH — the transmitted light-ups are pushed harder
        // now: a strong additive pass + a soft offset bloom so the
        // glow really washes through the panel.
        ctx.save();
        ctx.globalCompositeOperation = "lighter";
        ctx.globalAlpha = 0.58; // was 1.0 — additive glow was blowing out
        ctx.drawImage(glowCanvas, 0, 0);
        ctx.globalAlpha = 0.15; // soft bloom — was 0.32
        ctx.drawImage(glowCanvas, -4, -4, W + 8, H + 8);
        ctx.restore();
        // ── PASS 2: leaded contrast — the VIBING top layer ─────────
        // The contrast-preserving layer doesn't sit still: it pixel-
        // vibes + jitters + flickers a touch PER FRAME so the leading
        // shimmers and adds grain instead of being a static multiply.
        glowCtx.globalCompositeOperation = "source-over";
        glowCtx.clearRect(0, 0, W, H);
        stamp(faceCanvas,   kLo, maxK);
        stamp(laptopCanvas, hLo, maxH);
        glowCtx.globalCompositeOperation = "destination-out";
        maskDraw(glowCtx);
        {
          // per-frame deterministic vibe
          const fidx = Math.round(audioT * FPS);
          const rr = (n) => { let s = (fidx * 374761393 + n * 668265263) >>> 0; s = (s ^ (s >>> 13)) >>> 0; return (s & 0xffff) / 0xffff; };
          const jx = Math.round((rr(1) - 0.5) * 7);
          const jy = Math.round((rr(2) - 0.5) * 7);
          const blk = 2 + Math.floor(rr(3) * 4);          // 2..5 px pixel-vibe
          const flick = 0.85 + rr(4) * 0.30;              // alpha flicker
          // pixelate the leaded layer through backCanvas (vary per frame)
          backCtx.clearRect(0, 0, W, H);
          backCtx.imageSmoothingEnabled = false;
          const sw = Math.max(1, Math.floor(W / blk));
          const sh = Math.max(1, Math.floor(H / blk));
          backCtx.drawImage(glowCanvas, 0, 0, W, H, 0, 0, sw, sh);
          ctx.save();
          ctx.imageSmoothingEnabled = false;
          ctx.globalCompositeOperation = "multiply";
          ctx.globalAlpha = Math.min(0.34, 0.16 * Math.max(maxK, maxH) * flick); // gentler darks
          ctx.drawImage(backCanvas, 0, 0, sw, sh, jx, jy, W, H);
          ctx.restore();
          ctx.imageSmoothingEnabled = true;
        }
      }
    }
  }

  // No dark backdrop — events render directly over the illustration.

  // Section tints — visible only for sections within the viewport.
  // PER-LANE TINTING: in addition to the overall section band tint,
  // we color the BACKGROUND of each lane that is ACTIVE in this
  // section. So a section like break1 (no kick, no sub) leaves
  // those lanes uncolored, while drop1 (kick, hat, sub, bells, lead,
  // piano all active) lights up every lane background. The event
  // blocks then render on top as the foreground.
  // Section dividers — thin dashed verticals at section boundaries.
  // Section text labels REMOVED per user direction; the section
  // colors are conveyed by the per-section illustration crossfade
  // + the colored progress bar at the top of the canvas.
  for (const sec of struct.sections) {
    const x0 = playheadX + (sec.startSec - audioT) * pps;
    if (x0 < 0 || x0 > W) continue;
    ctx.strokeStyle = "rgba(255,255,255,0.35)";
    ctx.lineWidth = 1.5;
    ctx.setLineDash([6, 6]);
    ctx.beginPath();
    ctx.moveTo(x0, 0);
    ctx.lineTo(x0, H);
    ctx.stroke();
    ctx.setLineDash([]);
  }

  // Bar-grid ticks.
  const beatSec = 60 / BPM;
  const barSec = beatSec * (struct.meter || 3);
  const introStart = struct.sections[0]?.startSec ?? 2.7;
  ctx.strokeStyle = "rgba(255,255,255,0.08)";
  ctx.lineWidth = 1;
  for (let bar = 0; bar < struct.totalBars; bar++) {
    const tBar = introStart + bar * barSec;
    const x = playheadX + (tBar - audioT) * pps;
    if (x < 0 || x > W) continue;
    ctx.beginPath();
    ctx.moveTo(x, SCORE_TOP);
    ctx.lineTo(x, SCORE_BOTTOM);
    ctx.stroke();
  }

  // ── per-lane events ────────────────────────────────────────────────
  // Viewport time window: audioT ± (W/2) / pps seconds.
  // Widened to the rotation diagonal: the track group rotates as a
  // disc, so events must keep drawing well PAST the screen edges or
  // the rotated tracks stop short of the corners.
  const viewportPad = ROT_DIAG / pps;
  const tMin = audioT - viewportPad;
  const tMax = audioT + viewportPad;
  // The track group (lane waveforms + the verlet string) rotates as
  // one disc — a plain 360° about centre across the whole track.
  const trackRot = trackTheta(audioT);
  withRotation(trackRot, () => {
  for (let li = 0; li < LANES.length; li++) {
    const lane = LANES[li];
    const laneY0 = SCORE_TOP + li * laneH;
    // Lane labels removed — the timeline reads as pure colored
    // event lanes overlaid on the illustration with no extra chrome.

    const evs = laneEvents[lane.key];
    for (const ev of evs) {
      if (ev.skipped) continue;          // beyond tape-stop max coverage
      const evT = ev.displayedT ?? ev.t; // tape-stop remap if present
      if (evT > tMax) break;             // sorted → can short-circuit
      const evDur = ev.dur || 0.06;
      if (evT + evDur < tMin) continue;
      // Cap visible width so a long-sustain bell note doesn't render
      // as a 900-pixel slab. Drum hits keep their natural short
      // width; pitched lanes get capped to a tight 0.18 s marker.
      // SFX events show the actual mini-waveform of their underlying
      // audio so their visualization length matches the sample's true
      // duration, capped to keep on-screen footprint reasonable.
      // Cap visual duration so SFX/VOX pills/waveforms never exceed a
      // sane width. Long vocal stems (greeting: 3.7s, booty: 9.8s) at
      // section pps=600 produced pills 2200-5900 px wide — Cairo's
      // wide-pill draw path (gradient + shadow + screen-blend at
      // megapixel scale) corrupts the canvas backing buffer mid-render
      // and the canvas stops updating from that point onward. Capping
      // to 1.5s keeps every visualization within reach of the playhead
      // viewport without poisoning Cairo.
      const SAMPLE_MAX_VIS_DUR = 1.5;
      const visualDur = ev.midi !== undefined
        ? Math.min(evDur, 0.18)
        : Math.min(evDur, SAMPLE_MAX_VIS_DUR);
      const w = Math.max(3, visualDur * pps);
      const x = playheadX + (evT - audioT) * pps;

      // UNIFORM event height across all lanes — drums and pitched
      // events both use ~40% of the lane height, centered vertically
      // (pitched events shift up/down by midi within a narrower
      // band). Keeps the visualization visually consistent: no
      // "big drum slab" vs "tiny pitched note" mismatch.
      const baseNoteH = Math.max(8, Math.floor(laneH * 0.82));
      let y0 = laneY0 + Math.floor((laneH - baseNoteH) / 2);
      let yh = baseNoteH;
      if (ev.midi !== undefined) {
        const t01 = Math.min(1, Math.max(0, (ev.midi - lane.lo) / Math.max(1, lane.hi - lane.lo)));
        // Pitched lanes — same height as drums, but the y center
        // shifts by ±35% of lane height based on midi position so
        // pitch contour reads while heights stay uniform.
        const pitchOffset = Math.floor((t01 - 0.5) * (laneH - baseNoteH));
        y0 = laneY0 + Math.floor((laneH - baseNoteH) / 2) - pitchOffset;
      } else if (lane.maxStackRows > 1) {
        // Stacked drum hits (pre-roll vs in-bar hats etc.) — divide
        // the centered note-band into rows. Cap rendered depth at 3
        // to avoid crushing.
        const visibleRows = Math.min(lane.maxStackRows, 3);
        const rowH = baseNoteH / visibleRows;
        const rowIdx = Math.min(ev.stackRow, visibleRows - 1);
        y0 = laneY0 + Math.floor((laneH - baseNoteH) / 2) + rowIdx * rowH;
        yh = Math.max(4, rowH - 1);
      }

      // Trigger flash — when the event's start is just left of the
      // playhead, FLASH brighter and grow a brief halo. Reads as
      // a visual "the note is firing right now." Longer fade-tail
      // (180 ms) than before so the visual stays alive after the
      // hit, and the halo size scales with the flash energy.
      const sinceTrigger = audioT - evT;
      const flashWindow = 0.18;
      let flash = 0;
      if (sinceTrigger >= 0 && sinceTrigger < flashWindow) {
        flash = 1 - sinceTrigger / flashWindow;
      }
      // Future / upcoming events get dimmed; played events stay
      // saturated but fade slightly as they leave the playhead.
      const isFuture = evT > audioT + 0.02;
      const isPlayed = sinceTrigger >= flashWindow;
      let baseAlpha = 1.0;
      if (isFuture) baseAlpha = 0.55;       // dimmer upcoming, but visible
      else if (isPlayed) baseAlpha = 0.82;  // mostly-saturated played
      // ACTIVE pulse — when flash > 0 the bubble pumps to FULL opacity
      // (and a touch more saturated alpha for the spike) so the
      // playhead-aligned event reads as the loudest moment on screen.
      if (flash > 0) baseAlpha = Math.min(1.0, baseAlpha + flash * 0.4);

      // PLUCK the verlet string the single frame this event crosses
      // the playhead — a horizontal impulse at the event's lane y.
      // SKIP plucking on dense bed-style SFX (gallop, wave, amb-grain
      // fire at multi-Hz rates and would make the scene shimmy along
      // with them via sceneOffset). Dramatic one-shot SFX (sniper,
      // thunder, neigh, chill-drop, etc.) still pluck — the bed events
      // are visible on the timeline regardless.
      if (sinceTrigger >= 0 && sinceTrigger < (1 / FPS) + 1e-4) {
        const isBedSfx = lane.key === "sfx" &&
          (ev.name === "gallop" || ev.name === "wave" || ev.name === "amb-grain");
        if (!isBedSfx) {
          const yc = laneCenterY[lane.key] ?? (SCORE_TOP + SCORE_H / 2);
          const sign = ((Math.floor(evT * 97) + li) & 1) ? 1 : -1;
          // Pluck amplitude dampened further — the line + scene still
          // react to beats but very quietly, keeping the illustration
          // mostly still even in dense sections.
          const amount = 16 + 26 * Math.min(1, env + 0.2);
          pluckNeedle(yc, amount, sign, ncHexToRgb(lane.color));
        }
      }

      ctx.globalAlpha = baseAlpha;
      ctx.fillStyle = lane.color;
      // SFX / VOX lanes: draw the ACTUAL waveform of the audio at
      // this event's time region. Per visible pixel column, sample
      // the audio peaks in a tiny window and draw a vertical
      // excursion from the lane's center. Gives the timeline a
      // mini-DAW feel for any time-extended sample-driven event
      // (drop impacts, guns, drones, vocals, screams, etc).
      //
      // Events tagged `point: true` are DECORRELATED markers — they
      // don't draw a waveform (the underlying audio is already shown
      // by the span event they sit inside). They render as a small
      // outlined diamond on top so the user can spot the discrete
      // chime/event even though it lives inside a bigger span.
      const isPoint = ev.point === true;
      // BEACH TRACK MODEL — every non-point event is drawn as the
      // waveform of the underlying audio at its time region (no pills).
      const showWaveform = !isPoint && audioRaw.length > 0;
      if (isPoint) {
        const cx = x + w / 2;
        const cy = y0 + yh / 2;
        const size = Math.min(yh / 2 + 2, 14);
        ctx.globalAlpha = baseAlpha;
        ctx.strokeStyle = "#ffffff";
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(cx, cy - size);
        ctx.lineTo(cx + size, cy);
        ctx.lineTo(cx, cy + size);
        ctx.lineTo(cx - size, cy);
        ctx.closePath();
        ctx.stroke();
        ctx.fillStyle = lane.color;
        ctx.globalAlpha = baseAlpha * 0.5;
        ctx.fill();
        ctx.globalAlpha = 1.0;
      } else if (showWaveform) {
        // BEACH TRACK MODEL — chunky filled BAR BLOCKS. Per ~9px
        // column: peak amplitude → a centred vertical bar; alpha keyed
        // to the playhead (future dim / pressed bright / held decay /
        // played settled). Screen-blended so overlapping triggers
        // combine into the illustration. Bars ride the verlet string's
        // local deflection so they bend with the pluck.
        // NOT clamped to the screen — the disc rotates, so bars must
        // draw at their true position past the edges (otherwise the
        // rotated tracks get sheared off at the old frame border).
        const wfX = x;
        const wfXEnd = x + w;
        const wfW = wfXEnd - wfX;
        if (wfW > 1) {
          const midY = y0 + yh / 2;
          const wfH = yh;
          const BLOCK_PX = 9;
          const colsN = Math.max(2, Math.floor(wfW / BLOCK_PX));
          const blockW = Math.max(3, wfW / colsN - 2);
          const startSamp = Math.max(0, Math.floor(evT * AUDIO_SR));
          const endSamp = Math.min(audioRaw.length - 1, Math.floor((evT + visualDur) * AUDIO_SR));
          const spc = (endSamp - startSamp) / colsN;
          const rgb = ncHexToRgb(lane.color).join(",");
          const bend = (needleXAt(midY) - playheadX) * 0.5;
          ctx.save();
          ctx.globalCompositeOperation = "screen";
          for (let c = 0; c < colsN; c++) {
            const s0 = startSamp + Math.floor(c * spc);
            const s1 = Math.min(endSamp, startSamp + Math.floor((c + 1) * spc));
            let pk = 0;
            for (let s = s0; s < s1; s++) { const a = Math.abs(audioRaw[s]); if (a > pk) pk = a; }
            pk = Math.min(1, (pk / audioPeak) * 1.6);
            const tCol = evT + (c / colsN) * visualDur;
            const dt = audioT - tCol;
            let alpha;
            if (dt < 0) alpha = 0.16;                       // future — dim
            else if (dt < 0.05) alpha = 1.0;                // PRESSED
            else if (dt < 0.45) alpha = 1.0 - ((dt - 0.05) / 0.40) * 0.55; // HELD
            else alpha = 0.42;                              // played
            const half = Math.max(2, (pk * wfH) / 2);
            const bx = wfX + (c / colsN) * wfW + bend;
            // record-groove: curve the whole track around the disc —
            // straight at the needle, arcing more the further out.
            const aGroove = (bx - playheadX) / GROOVE_R;
            ctx.save();
            ctx.translate(ROT_CX, ROT_CY);
            ctx.rotate(aGroove);
            ctx.translate(-ROT_CX, -ROT_CY);
            ctx.fillStyle = `rgba(${rgb},${alpha.toFixed(3)})`;
            ctx.fillRect(bx, midY - half, blockW, half * 2);
            ctx.restore();
          }
          ctx.restore();
        }
      } else {
        // DIMENSIONAL PILL — the body uses a vertical color gradient
        // (lighter at top, darker at bottom) so each capsule reads
        // as a 3D candy. Plus a soft drop shadow below, and a thin
        // highlight stroke just inside the top so the specular pop
        // reads on the curved edge. Screen blend so events glow
        // against the illustration.
        const r = Math.min(yh / 2, w / 2);
        const drawPill = (px, py, pw, ph, pr) => {
          ctx.beginPath();
          ctx.moveTo(px + pr, py);
          ctx.arcTo(px + pw, py, px + pw, py + ph, pr);
          ctx.arcTo(px + pw, py + ph, px, py + ph, pr);
          ctx.arcTo(px, py + ph, px, py, pr);
          ctx.arcTo(px, py, px + pw, py, pr);
        };
        // 1) Drop shadow — soft dark band ~2 px below.
        ctx.save();
        ctx.globalAlpha = baseAlpha * 0.45;
        ctx.shadowBlur = 4;
        ctx.shadowColor = "rgba(0,0,0,0.7)";
        ctx.shadowOffsetY = 2;
        ctx.fillStyle = "rgba(0,0,0,0.0)"; // shadow is the visible part
        ctx.fillStyle = "rgba(0,0,0,0.5)";
        drawPill(x, y0, w, yh, r);
        ctx.fill();
        ctx.restore();
        // 2) Main pill body — vertical gradient, screen blend.
        ctx.save();
        ctx.globalCompositeOperation = "screen";
        const bodyGrad = ctx.createLinearGradient(x, y0, x, y0 + yh);
        // Lighter at top → core color → slightly darker at bottom.
        // Use rgba blends of the lane color with white/black.
        bodyGrad.addColorStop(0,    lane.color + "ff"); // top — full sat
        bodyGrad.addColorStop(0.5,  lane.color + "cc"); // mid — slight pull-down
        bodyGrad.addColorStop(1,    lane.color + "55"); // bottom — darker for depth
        ctx.fillStyle = bodyGrad;
        drawPill(x, y0, w, yh, r);
        ctx.fill();
        ctx.restore();
        // 3) Top inner highlight — thin white arc just inside the top.
        ctx.save();
        ctx.globalAlpha = baseAlpha * 0.55;
        ctx.strokeStyle = "rgba(255,255,255,0.85)";
        ctx.lineWidth = 1.2;
        ctx.beginPath();
        // Draw only the upper half of the pill outline (the top arc).
        const inset = 1.2;
        const ix = x + inset, iy = y0 + inset;
        const iw = w - 2 * inset, ih = yh - 2 * inset;
        const ir = Math.min(ih / 2, iw / 2);
        // Move along the top edge: top-left curve to top-right curve.
        ctx.moveTo(ix, iy + ir);
        ctx.arcTo(ix, iy, ix + ir, iy, ir);
        ctx.lineTo(ix + iw - ir, iy);
        ctx.arcTo(ix + iw, iy, ix + iw, iy + ir, ir);
        ctx.stroke();
        ctx.restore();
      }

      // (No separate active-glow band — the beach per-column alpha
      //  press/held/played IS the activation, and it stays registered
      //  to the bar centre so there is no vertical-offset highlight.)
      // ── NOTATION POP — particle burst on activation ───────────────
      // For freshly-fired events (flash > 0) spray ~8 small dots
      // outward from the pill's center, fading over a 240 ms window.
      // Deterministic angles seeded by lane index + event time so the
      // burst doesn't shimmer between frames.
      if (flash > 0.05 && !isPoint) {
        const burstAge = (audioT - evT) / 0.24;
        if (burstAge >= 0 && burstAge < 1) {
          const cx = x + w / 2;
          const cy = y0 + yh / 2;
          const NUM = 8;
          const seed = Math.floor(evT * 1000) + li * 31;
          let s = seed >>> 0;
          const rnd = () => { s = (s * 1664525 + 1013904223) >>> 0; return (s & 0xffffff) / 0xffffff; };
          ctx.save();
          ctx.globalCompositeOperation = "screen";
          for (let p = 0; p < NUM; p++) {
            const baseAng = (p / NUM) * Math.PI * 2;
            const jitter = (rnd() - 0.5) * 0.8;
            const ang = baseAng + jitter;
            const reach = 14 + rnd() * 26;     // max distance from center
            const dist = reach * burstAge;
            const px = cx + Math.cos(ang) * dist;
            const py = cy + Math.sin(ang) * dist;
            const radius = Math.max(0.5, 2.8 * (1 - burstAge));
            ctx.globalAlpha = (1 - burstAge) * 0.95;
            ctx.fillStyle = lane.color;
            ctx.beginPath();
            ctx.arc(px, py, radius, 0, Math.PI * 2);
            ctx.fill();
          }
          ctx.restore();
        }
      }
      ctx.globalAlpha = 1.0;
    }
  }
  }); // end withRotation(track group)

  // ── drop-impact markers — vertical white-on-pink bars ─────────────
  for (const imp of dropImpacts) {
    const impT = imp.displayedT ?? imp.t;
    if (impT < tMin || impT > tMax) continue;
    const x = playheadX + (impT - audioT) * pps;
    ctx.strokeStyle = "rgba(255,255,255,0.9)";
    ctx.lineWidth = 3;
    ctx.beginPath();
    ctx.moveTo(x, SCORE_TOP);
    ctx.lineTo(x, SCORE_BOTTOM);
    ctx.stroke();
    ctx.strokeStyle = "#ff5a8a";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(x, SCORE_TOP);
    ctx.lineTo(x, SCORE_BOTTOM);
    ctx.stroke();
  }

  // ── COLOR BLASTS — full-canvas flashes on dropImpact + section
  //                   boundaries. SUPPRESSED during the shutdown
  //                   window so the ending is a calm fade-to-black
  //                   without strobing.
  const inShutdown = hasTapeStop && audioT > duration - 5;
  if (!inShutdown) {
    let blastAlpha = 0;
    let blastColor = "rgba(255,255,255,0.6)";
    if (nextSec) {
      const sinceBoundary = audioT - currentSec.endSec;
      if (sinceBoundary >= -0.04 && sinceBoundary < 0.18) {
        const t01 = Math.max(0, sinceBoundary + 0.04) / 0.22;
        blastAlpha = Math.max(blastAlpha, 0.55 * (1 - t01));
        const tint = SECTION_TINTS[nextSec.name] || "rgba(255,255,255,0.5)";
        blastColor = tint.replace(/,([0-9.]+)\)/, ",1)").replace("rgba", "rgba");
      }
    }
    for (const imp of dropImpacts) {
      const impT = imp.displayedT ?? imp.t;
      const dt = audioT - impT;
      if (dt >= 0 && dt < 0.30) {
        const t01 = dt / 0.30;
        const a = 0.65 * (1 - t01);
        if (a > blastAlpha) {
          blastAlpha = a;
          blastColor = "rgba(255,90,160,1)";
        }
      }
    }
    if (blastAlpha > 0.01) {
      ctx.save();
      ctx.globalCompositeOperation = "screen";
      ctx.globalAlpha = blastAlpha;
      ctx.fillStyle = blastColor;
      ctx.fillRect(0, 0, W, H);
      ctx.restore();
    }
  }

  // ── playhead — verlet physics string (ported from wanderbeach) ───
  // Events plucked it as they crossed (above); advance the sim one
  // step then draw the oscillating/settling nylon string.
  stepNeedle();
  // GEOMETRIC DISTORTION — the undabeach counter-rotated warp: snapshot
  // the upright frame, counter-rotate it, then redraw the strip under
  // +trackRot so the distortion field rides the rotated line WITH the
  // disc (content stays upright, only the warp turns).
  warpUnderString(trackRot, audioT);
  // String rotates with the track group (same theta as the lanes).
  withRotation(trackRot, () => { drawNeedleString(); });

  // ── two-pals side watermarks (fixed HUD, seeped into the image) ──
  // TIKTOK has NO side pals.
  if (!TIKTOK) drawWatermark(audioT);

  // ── bouncing title — pre-rendered chars composited per frame ─────
  // Bounce amplitude has three components:
  //   • a small per-beat sin (always pulses with rhythm)
  //   • a multiplier tied to envelope so loud passages bounce more
  //   • per-char phase offset so letters bob in a traveling wave
  const baseBounce = 8;
  const envBounce = env * 36;
  // ALL typography tints to the current section colour (per-char
  // hue still varies — the section tint is BLENDED with each
  // character's own palette colour, not flattened).
  const [tSr, tSg, tSb] = sectionTcRgb(audioT);
  // "JUMPED IN" — right after the boot melody fires (prompt entered)
  // the title springs into place: a fast decaying per-char overshoot
  // so it reads as energetically ACTIVE, like we just loaded in.
  const bootT = bootBeeps.length ? bootBeeps[0].t : -1e9;
  const jSince = audioT - bootT;
  const jumping = jSince >= 0 && jSince < 0.8;
  // helper: recolour the glyph plate (alpha shape) to an rgb
  const tintGlyph = (img, r, g, b) => {
    lyrTint.width = img.width; lyrTint.height = img.height;
    lyrTintCtx.clearRect(0, 0, img.width, img.height);
    lyrTintCtx.globalCompositeOperation = "source-over";
    lyrTintCtx.drawImage(img, 0, 0);
    lyrTintCtx.globalCompositeOperation = "source-in";
    lyrTintCtx.fillStyle = `rgb(${r},${g},${b})`;
    lyrTintCtx.fillRect(0, 0, img.width, img.height);
    return lyrTint;
  };
  // TIKTOK: title is CENTRED on screen (matches the centred typing-
  // intro). INSTA: top-left at the safe anchor.
  const ttStartX = Math.round((W - prefixWidths[titleChars.length]) / 2);
  const ttTopY   = Math.round((H - charBoxH) / 2);
  for (let i = 0; i < titleChars.length; i++) {
    const img = charImgs[i];
    if (!img) continue;
    const x = (TIKTOK ? ttStartX : titleBaseX) + prefixWidths[i] - 3;
    const phase = (i / titleChars.length) * 2 * Math.PI;
    const beatWave = Math.sin(2 * Math.PI * beatHz * audioT + phase);
    let y = (TIKTOK ? ttTopY : titleY - charBaselineY)
      + (baseBounce + envBounce) * beatWave;
    if (jumping) {
      const e = 1 - jSince / 0.8;            // 1 → 0 over 0.8 s
      y += -e * e * 44 * Math.cos(jSince * 26 - i * 0.6); // staggered spring
    }
    if (y < 2) y = 2;
    // ── FINALE: in the last seconds the letters DROP OUT — a
    //    staggered left→right cascade, gravity + tumble + fade, so
    //    the title falls off the bottom as the track resolves.
    // Letters start dropping the moment the LAST section begins (not at
    // the very end) — staggered left→right, each finishing its fall a
    // beat or two into the final section.
    // INSTA: drop at the LAST section. TIKTOK: the title only bounces a
    // few seconds after it loads in, then falls away and is GONE for
    // good (never reappears).
    const lastSec = struct.sections[struct.sections.length - 1];
    const fallStart = TIKTOK
      ? (bootBeeps.length ? bootBeeps[0].t + 4.0 : 4.0)
      : lastSec.startSec;
    const charFallT = fallStart + i * 0.11;      // staggered per char
    let fdy = 0, frot = 0, falpha = 1;
    if (audioT > charFallT) {
      const fe = audioT - charFallT;             // seconds this char has been falling
      fdy = 0.5 * 2650 * fe * fe;                // gravity (px)
      const spin = (((i * 53) % 11) - 5) * 0.55; // deterministic ± per char
      frot = spin * fe + 0.16 * Math.sin(fe * 9 + i); // tumble + wobble
      falpha = Math.max(0, 1 - fe / 2.4);        // fade out as it leaves
    }
    if (falpha <= 0.001) continue;               // gone — skip
    const blk = tintGlyph(img, 0, 0, 0);
    const pal = ncHexToRgb(TITLE_PALETTE[i % TITLE_PALETTE.length]);
    const cr = Math.round(tSr * 0.55 + pal[0] * 0.45);
    const cg = Math.round(tSg * 0.55 + pal[1] * 0.45);
    const cb = Math.round(tSb * 0.55 + pal[2] * 0.45);
    // Draw the full glyph stack (soft shade + hard drop + colour) at a
    // local origin so the falling transform can rotate it about its
    // own centre while it tumbles away.
    const drawStack = (px, py) => {
      ctx.globalCompositeOperation = "source-over";
      for (const [ox, oy, a] of [[-2,-1,0.18],[2,3,0.20],[5,7,0.20],[0,3,0.22]]) {
        ctx.globalAlpha = a * falpha;
        ctx.drawImage(blk, px + ox, py + oy);
      }
      for (const [ox, oy, a] of [[3,4,0.95],[4,5,0.85],[3,5,0.70]]) {
        ctx.globalAlpha = a * falpha;
        ctx.drawImage(blk, px + ox, py + oy);
      }
      ctx.globalAlpha = falpha;
      ctx.drawImage(tintGlyph(img, cr, cg, cb), px, py);
    };
    ctx.save();
    if (fdy > 0 || frot !== 0) {
      const cx = x + img.width / 2;
      const cy = y + img.height / 2;
      ctx.translate(cx, cy + fdy);
      ctx.rotate(frot);
      drawStack(-img.width / 2, -img.height / 2);
    } else {
      drawStack(x, y);
    }
    ctx.restore();
  }

  // ── segmented progress bar — identical to the wanderbeach video ──
  // FULL WIDTH: each section paints a near-black tinted TRACK across
  // its width; the PLAYED portion is the bright saturated tint on top.
  // The LAST section runs all the way to the right edge (sections end
  // a couple seconds before the padded total) so the bar spans the
  // entire display with no short gap. TIKTOK: NO progress bar.
  if (!TIKTOK) {
    const playedX = (W * Math.max(0, audioT)) / duration;
    const lastI = struct.sections.length - 1;
    for (let si = 0; si < struct.sections.length; si++) {
      const sec = struct.sections[si];
      // First section pinned to the LEFT edge, last to the RIGHT edge
      // (sections start/end a little inside the padded total) so the
      // bar spans the entire display with no gaps either side.
      const segX0 = si === 0 ? 0 : (sec.startSec / duration) * W;
      const segX1 = si === lastI ? W : (sec.endSec / duration) * W;
      const [r, g, b] = tintRgb(SECTION_TINTS[sec.name] || "rgba(200,200,200,1)");
      ctx.fillStyle = `rgba(${Math.round(r * 0.16)},${Math.round(g * 0.16)},${Math.round(b * 0.16)},0.85)`;
      ctx.fillRect(segX0, progressY, segX1 - segX0, progressH);
      const filledX1 = Math.min(segX1, playedX);
      if (filledX1 > segX0) {
        ctx.fillStyle = `rgba(${r},${g},${b},0.96)`;
        ctx.fillRect(segX0, progressY, filledX1 - segX0, progressH);
      }
      ctx.fillStyle = "rgba(255,253,242,0.35)";
      ctx.fillRect(segX1 - 1, progressY, 1, progressH);
    }
  }

  // ── timecode — identical compositing to the wanderbeach video ────
  // TIKTOK: NO timecode.
  if (!TIKTOK) {
    const totMm = Math.floor(duration / 60);
    const totSs = Math.floor(duration - totMm * 60).toString().padStart(2, "0");
    const sec = Math.min(Math.max(0, Math.floor(audioT)), Math.ceil(duration));
    const mm = Math.floor(sec / 60);
    const ss = (sec - mm * 60).toString().padStart(2, "0");
    const entry = tcCache.get(`${mm}:${ss} / ${totMm}:${totSs}`);
    if (entry) {
      const { img, shadow } = entry;
      // bottom-right, above the progress bar; bounces with the audio
      // envelope plus a small breathing wobble.
      const bounce = 16 * env;
      const wobble = 2.2 * Math.sin(audioT * 5.5);
      const x = W - img.width - 32;
      const y = progressY - img.height - 14 - bounce + wobble;
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.95;
      ctx.drawImage(shadow, x + 3, y + 4);
      ctx.drawImage(shadow, x + 2, y + 3);
      ctx.globalAlpha = 1;
      const [tr, tg, tb] = sectionTcRgb(audioT);
      tcTint.width = img.width;
      tcTint.height = img.height;
      tcTintCtx.clearRect(0, 0, img.width, img.height);
      tcTintCtx.globalCompositeOperation = "source-over";
      tcTintCtx.drawImage(img, 0, 0);
      tcTintCtx.globalCompositeOperation = "source-in";
      tcTintCtx.fillStyle = `rgb(${tr},${tg},${tb})`;
      tcTintCtx.fillRect(0, 0, img.width, img.height);
      ctx.drawImage(tcTint, x, y);
      ctx.restore();
    }
  }

  // ── singalong karaoke — fixed HUD, over the rotating tracks ──────
  drawKaraoke(audioT);

  // ── STARTUP + SHUTDOWN — shared fullscreen pixelate / fuzz / blink
  //    decay engine. NO squash/stretch: the frame just pixel-decays,
  //    fuzzes with static, RGB-tears and strobes. level 0 = clean
  //    passthrough, level 1 = destroyed → black. Startup runs it in
  //    REVERSE (begins destroyed, resolves to clean) snapped to the
  //    boot beeps; shutdown runs it forward snapped to the shutdown
  //    beeps over the tape-stop tail. "beep beep boop boop".
  const applyScreenDecay = (level) => {
    if (level <= 0.002) return;
    const L = Math.min(1, level);
    const fidx = Math.round(audioT * FPS);
    const rng = (n) => {
      let s = (fidx * 2246822519 + n * 3266489917) >>> 0;
      s = (s ^ (s >>> 15)) >>> 0; s = Math.imul(s, 2246822519) >>> 0;
      return ((s ^ (s >>> 13)) >>> 0) / 4294967295;
    };
    // 1) PIXEL DECAY — uniform square mosaic, blocks grow with level.
    const blk = Math.max(1, Math.round(1 + Math.pow(L, 1.4) * 64));
    if (blk > 1) {
      const sw = Math.max(1, Math.floor(W / blk));
      const sh = Math.max(1, Math.floor(H / blk));
      backCtx.imageSmoothingEnabled = false;
      backCtx.clearRect(0, 0, W, H);
      backCtx.drawImage(canvas, 0, 0, W, H, 0, 0, sw, sh);
      ctx.save();
      ctx.imageSmoothingEnabled = false;
      ctx.drawImage(backCanvas, 0, 0, sw, sh, 0, 0, W, H);
      ctx.restore();
      ctx.imageSmoothingEnabled = true;
    }
    ctx.save();
    // 2) RGB tear — a few horizontal slabs shoved sideways, cyan/mag.
    const slabs = 2 + Math.floor(L * 4);
    ctx.globalCompositeOperation = "screen";
    for (let i = 0; i < slabs; i++) {
      const sy = Math.floor(rng(i * 7 + 1) * H);
      const sh = Math.max(6, Math.floor(rng(i * 7 + 2) * 90 * (0.4 + L)));
      const dx = Math.round((rng(i * 7 + 3) - 0.5) * 80 * L);
      ctx.globalAlpha = 0.10 + L * 0.30;
      ctx.fillStyle = (i & 1) ? "rgba(0,255,210,1)" : "rgba(255,40,170,1)";
      ctx.fillRect(dx, sy, W, Math.max(2, Math.floor(sh * 0.5)));
    }
    // 3) STATIC FUZZ — scattered blocky noise, density scales w/ level.
    ctx.globalCompositeOperation = "source-over";
    const grid = Math.max(8, blk);
    const cells = Math.floor((W / grid) * (H / grid));
    const n = Math.floor(cells * (0.05 + L * 0.55));
    for (let i = 0; i < n; i++) {
      const gx = Math.floor(rng(i * 5 + 11) * (W / grid)) * grid;
      const gy = Math.floor(rng(i * 5 + 12) * (H / grid)) * grid;
      const v = rng(i * 5 + 13);
      ctx.globalAlpha = 0.10 + L * 0.32; // keep the image readable thru fuzz
      ctx.fillStyle = v < 0.5 ? "#000" : (v < 0.8 ? "#fff" : "#aef240");
      ctx.fillRect(gx, gy, grid, grid);
    }
    // 4) sink toward black ONLY at the extreme — the mid range stays a
    //    chunky pixel-static image, going fully black just at the very
    //    end of shutdown / the instant before boot.
    const sink = L < 0.82 ? 0 : (L - 0.82) / 0.18;
    if (sink > 0) {
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = sink * sink;
      ctx.fillStyle = "#000";
      ctx.fillRect(0, 0, W, H);
    }
    // 5) BLINK / STROBE — random dropped frames + rare white pop;
    //    frequency climbs with level. Partial (never fully solid until
    //    the sink takes over) so it reads as flicker, not just black.
    const br = rng(999);
    if (br < L * 0.45) {
      ctx.globalAlpha = 0.35 + L * 0.40;
      ctx.fillStyle = "#000";
      ctx.fillRect(0, 0, W, H);
    } else if (br > 1 - L * 0.07) {
      ctx.globalAlpha = 0.65;
      ctx.fillStyle = "#fff";
      ctx.fillRect(0, 0, W, H);
    }
    ctx.restore();
  };

  // STARTUP — reverse decay, snapped to the boot beeps. Before beep-1
  // the frame is fully destroyed; each beep LOCKS it in a stage; just
  // after the last beep it is fully clean for the rest of the song.
  if (bootBeeps.length > 0) {
    const lastT = bootBeeps[bootBeeps.length - 1].t;
    if (audioT < lastT + 0.45) {
      let lastBeepIdx = -1;
      for (let i = 0; i < bootBeeps.length; i++) {
        if (audioT >= bootBeeps[i].t) lastBeepIdx = i; else break;
      }
      // resolve targets: -1 (pre) = 1.0, then step down per beep to 0.
      const targets = bootBeeps.map((_, i) => 1 - (i + 1) / bootBeeps.length);
      let level;
      if (lastBeepIdx < 0) {
        level = 1.0;
      } else {
        const prev = lastBeepIdx > 0 ? targets[lastBeepIdx - 1] : 1.0;
        const tgt = targets[lastBeepIdx];
        const since = audioT - bootBeeps[lastBeepIdx].t;
        const u = Math.min(1, Math.max(0, since / 0.16));
        const e = u * u * (3 - 2 * u);
        level = prev + (tgt - prev) * e;
        // each beep punches a brief re-fuzz JOLT.
        if (since < 0.10) level = Math.min(1, level + (1 - since / 0.10) * 0.30);
      }
      applyScreenDecay(level);
    }
  }

  // SHUTDOWN — forward decay over the tape-stop tail, snapped to the
  // shutdown beeps. Each beep steps the destruction UP; after the last
  // it holds fully destroyed (black) to the end.
  if (hasTapeStop) {
    const sb = shutdownBeeps.map((b) => b.t).sort((a, b) => a - b);
    const tailStart = duration - 3.5;
    if (audioT > tailStart) {
      let level;
      if (sb.length >= 1) {
        let lastIdx = -1;
        for (let i = 0; i < sb.length; i++) {
          if (audioT >= sb[i]) lastIdx = i; else break;
        }
        const steps = sb.map((_, i) => (i + 1) / sb.length); // 0.33,0.66,1
        if (lastIdx < 0) {
          // pre first beep: gentle ramp up to the first step.
          const u = Math.min(1, Math.max(0, (audioT - tailStart) / Math.max(0.01, sb[0] - tailStart)));
          level = 0.05 + u * (steps[0] * 0.6 - 0.05);
        } else {
          const prev = lastIdx > 0 ? steps[lastIdx - 1] : 0.2;
          const tgt = steps[lastIdx];
          const since = audioT - sb[lastIdx];
          const u = Math.min(1, since / 0.18);
          const e = u * u * (3 - 2 * u);
          level = prev + (tgt - prev) * e;
          if (since < 0.10) level = Math.min(1, level + (1 - since / 0.10) * 0.22);
        }
      } else {
        // no shutdown beeps → straight time ramp.
        level = Math.min(1, (audioT - tailStart) / 3.5);
      }
      applyScreenDecay(level);
    }
  }
}

// ── assets dir for any future intermediate files ─────────────────────
const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
mkdirSync(assetsDir, { recursive: true });

// ── ffmpeg encoder via stdin pipe ────────────────────────────────────
const totalFrames = Math.ceil(duration * FPS);
console.log(`▸ rendering ${OUT}`);
console.log(`  ${totalFrames} frames @ ${FPS}fps · ${W}x${H} canvas · ${LANES.length} lanes`);

// ── PROBE MODE — render a handful of still frames and exit ───────────
// `--probe-frames "5.2,18.7,46.0"` renders just those audio seconds to
// PNGs next to --out (…-probe-<t>.png) and skips ffmpeg entirely. Lets
// the stained-glass / chrome look be judged in seconds, not a 10-min
// full encode.
if (flags["probe-frames"]) {
  const ts = String(flags["probe-frames"]).split(",")
    .map((s) => Number(s.trim())).filter((n) => Number.isFinite(n));
  const base = OUT.replace(/\.mp4$/i, "");
  for (const sec of ts) {
    drawFrame(sec + AUDIO_DELAY_SEC); // undo the in-fn audio offset
    const p = `${base}-probe-${sec.toFixed(2)}.png`;
    writeFileSync(p, canvas.toBuffer("image/png"));
    console.log(`  ▸ probe ${sec.toFixed(2)}s → ${p}`);
  }
  console.log(`✓ probe complete (${ts.length} frame(s)) — no video encoded`);
  process.exit(0);
}

// node-canvas `toBuffer('raw')` emits BGRA in native byte order on
// little-endian systems (Apple silicon, x86_64). Feeding ffmpeg with
// -pix_fmt rgba was swapping the red and blue channels — switching
// to bgra resolves it.
const ff = spawn("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "rawvideo",
  "-pix_fmt", "bgra",
  "-s", `${W}x${H}`,
  "-r", String(FPS),
  "-i", "pipe:0",
  "-i", TRACK,
  "-c:v", "libx264",
  "-pix_fmt", "yuv420p",
  "-preset", "medium",
  "-crf", "20",
  // Re-encode to AAC for MP4 container compatibility (mp3-in-mp4
  // ships but QuickTime + most players drop the audio). The
  // encoder delay is small (~43 ms) and gapless metadata keeps it
  // imperceptible in playback.
  "-c:a", "aac", "-b:a", "192k",
  "-r", String(FPS),
  "-t", String(duration),
  "-shortest",
  OUT,
], { stdio: ["pipe", "inherit", "inherit"] });

ff.on("error", (e) => {
  console.error("ffmpeg spawn error:", e);
  process.exit(1);
});

// ── perfect-loop cross-dissolve (matches the audio loop) ─────────────
// Snapshot frame 0; over the last LOOP_S the finished frame dissolves
// back into that first frame so the VIDEO wraps seamlessly, same as the
// bar-aligned audio. (ported from the chillwave/undabeach pipeline.)
const LOOP_S = 1.4;
const loopCanvas = createCanvas(W, H);
const loopCtx = loopCanvas.getContext("2d");

// Stream frames. Pause-on-backpressure pattern so we don't OOM.
const t0 = Date.now();
let lastLog = t0;
async function streamFrames() {
  for (let f = 0; f < totalFrames; f++) {
    const t = f / FPS;
    drawFrame(t);
    if (f === 0) loopCtx.drawImage(canvas, 0, 0); // capture the wrap frame
    else if (t >= duration - LOOP_S) {
      let k = (t - (duration - LOOP_S)) / LOOP_S; // 0 → 1
      k = Math.max(0, Math.min(1, k));
      k = k * k * (3 - 2 * k);                     // smoothstep
      ctx.save();
      ctx.globalAlpha = k;
      ctx.drawImage(loopCanvas, 0, 0);
      ctx.restore();
    }
    // node-canvas's toBuffer("raw") returns a Buffer that shares
    // memory with the canvas pixel backing store. ffmpeg.stdin.write
    // is async — without an explicit copy the next drawFrame call
    // clobbers buffers still waiting to be consumed by ffmpeg, which
    // surfaces as pixel-identical "frozen" chunks in the output mp4.
    const buf = Buffer.from(canvas.toBuffer("raw"));
    if (!ff.stdin.write(buf)) {
      await new Promise((res) => ff.stdin.once("drain", res));
    }
    const now = Date.now();
    if (now - lastLog > 2000) {
      const pct = ((f / totalFrames) * 100).toFixed(1);
      const fps = (f / ((now - t0) / 1000)).toFixed(1);
      console.log(`  · frame ${f}/${totalFrames} (${pct}%) · ${fps} fps`);
      lastLog = now;
    }
  }
  ff.stdin.end();
}

streamFrames().catch((e) => {
  console.error("frame streaming error:", e);
  ff.kill();
  process.exit(1);
});

ff.on("exit", (code) => {
  if (code !== 0) {
    console.error(`✗ ffmpeg exited with code ${code}`);
    process.exit(1);
  }
  console.log(`✓ ${OUT}`);
});
