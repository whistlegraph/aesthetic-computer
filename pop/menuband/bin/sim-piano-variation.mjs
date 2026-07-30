#!/usr/bin/env node
// Render one seeded MenuBand variation from a { defaults, variations } manifest.
// The score and audio are produced separately; this file owns only the real-strip
// choreography, persistent falling glyphs, performance light, and video encode.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, isAbsolute, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import {
  W, H, OUT, STRIP_MIDIS, makeStage, loadStripRig, drawStrip,
  stripKeyX, stripKeyRect, stripKeyColor, litAt, renderVideo,
} from "./reel-lib.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const FPS = 60;
const KEY_LABELS = new Map([
  [60, "C"], [62, "D"], [64, "E"], [65, "F"], [67, "G"], [69, "A"], [71, "B"],
  [72, "H"], [74, "I"], [76, "J"], [77, "K"], [79, "L"], [81, "M"], [83, "N"],
]);

function die(message) {
  console.error(`✗ ${message}`);
  process.exit(1);
}

function valueAfter(flag) {
  const at = process.argv.indexOf(flag);
  return at >= 0 ? process.argv[at + 1] : null;
}

function usage() {
  console.log(`usage:
  node pop/menuband/bin/sim-piano-variation.mjs --manifest <file> --index <0-based>
  node pop/menuband/bin/sim-piano-variation.mjs --manifest <file> --id <variation-id>

options:
  --dry-run                 validate and print the resolved render plan
  --still <seconds>         render one PNG after simulating from frame zero
  --still-out <file.png>    destination for --still
  --total <seconds>         bounded QA render override (does not alter score)
  --out <file.mp4>          alternate video destination for QA
`);
}

if (process.argv.includes("--help") || process.argv.includes("-h")) {
  usage();
  process.exit(0);
}

const manifestArg = valueAfter("--manifest") || process.argv[2];
if (!manifestArg || manifestArg.startsWith("--")) die("--manifest <file> is required");
const manifestPath = resolve(process.cwd(), manifestArg);
if (!existsSync(manifestPath)) die(`manifest not found: ${manifestPath}`);

let manifest;
try {
  manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
} catch (error) {
  die(`cannot read manifest: ${error.message}`);
}
if (!Array.isArray(manifest.variations) || manifest.variations.length === 0) {
  die("manifest.variations must be a non-empty array");
}

const idArg = valueAfter("--id");
const indexArg = valueAfter("--index");
let index = indexArg == null ? null : Number(indexArg);
if (indexArg != null && (!Number.isInteger(index) || index < 0)) die("--index must be a non-negative integer");
if (idArg) index = manifest.variations.findIndex((entry) => (entry.id || entry.slug) === idArg);
if (index == null && /^\d+$/.test(process.argv[3] || "")) index = Number(process.argv[3]);
if (index == null) die("choose a variation with --index or --id");
if (index < 0 || index >= manifest.variations.length) die(`variation not found (index ${index}${idArg ? `, id ${idArg}` : ""})`);

const defaults = manifest.defaults || {};
const raw = manifest.variations[index] || {};
const defaultVisual = defaults.visual || {};
const rawVisual = raw.visual || {};
const selectedPalette = rawVisual.palette ?? defaultVisual.palette;
const selectedMotion = rawVisual.motion ?? defaultVisual.motion;
const selectedReveal = rawVisual.reveal ?? defaultVisual.reveal;
const selectedParticles = rawVisual.particles ?? defaultVisual.particles;
const selectedLighting = rawVisual.lighting ?? rawVisual.spotlight ?? defaultVisual.lighting ?? defaultVisual.spotlight;
const entry = {
  ...defaults,
  ...raw,
  visual: {
    ...defaultVisual,
    ...rawVisual,
    palette: Array.isArray(selectedPalette) || typeof selectedPalette === "string" ? selectedPalette
      : { ...objectOrEmpty(defaultVisual.palette), ...objectOrEmpty(rawVisual.palette) },
    motion: typeof selectedMotion === "string" ? selectedMotion
      : { ...objectOrEmpty(defaultVisual.motion), ...objectOrEmpty(rawVisual.motion) },
    reveal: typeof selectedReveal === "string" ? selectedReveal
      : { ...objectOrEmpty(defaultVisual.reveal), ...objectOrEmpty(rawVisual.reveal) },
    particles: typeof selectedParticles === "string" ? selectedParticles
      : { ...objectOrEmpty(defaultVisual.particles), ...objectOrEmpty(rawVisual.particles) },
    lighting: typeof selectedLighting === "string" ? selectedLighting
      : { ...objectOrEmpty(defaultVisual.lighting), ...objectOrEmpty(rawVisual.lighting) },
  },
};
const id = entry.id || entry.slug;
if (!id || !/^[a-zA-Z0-9][a-zA-Z0-9._-]*$/.test(id)) die("variation id must be a filesystem-safe string");

const manifestDir = dirname(manifestPath);
const baseDir = resolve(manifestDir, manifest.baseDir || defaults.baseDir || ".");
const variationDir = resolve(REPO, `pop/menuband/out/variations/${id}`);
function inputPath(value, fallback) {
  if (!value) return fallback;
  return isAbsolute(value) ? value : resolve(baseDir, value);
}
function outputPath(value, fallback) {
  if (!value) return fallback;
  return isAbsolute(value) ? value : resolve(baseDir, value);
}
const scorePath = inputPath(entry.notesPath || entry.notes || entry.score, `${variationDir}/${id}.notes.json`);
const audioPath = inputPath(entry.audioPath || entry.audio, `${variationDir}/${id}.wav`);
const outPath = outputPath(valueAfter("--out") || entry.outPath || entry.output, `${variationDir}/${id}.mp4`);
if (!existsSync(scorePath)) die(`score not found: ${scorePath}`);
if (!existsSync(audioPath)) die(`audio not found: ${audioPath}`);

let score;
try {
  score = JSON.parse(readFileSync(scorePath, "utf8"));
} catch (error) {
  die(`cannot read score: ${error.message}`);
}
const duration = Number(entry.durationSec ?? score.durationSec);
if (!Number.isFinite(duration) || duration <= 0) die("score.durationSec must be positive");
const totalOverride = valueAfter("--total");
const total = totalOverride == null ? duration : Math.min(duration, Math.max(0.1, Number(totalOverride)));
if (!Number.isFinite(total)) die("--total must be a number");

function visualMidi(note) {
  return Number(note.displayMidi ?? note.keyMidi ?? note.visualMidi ?? note.midi);
}
const notes = (score.notes || [])
  .map((note, eventIndex) => ({ ...note, eventIndex, visualMidi: visualMidi(note) }))
  .filter((note) => note.t >= 0 && note.t <= duration)
  .sort((a, b) => a.t - b.t || a.eventIndex - b.eventIndex);
const unmapped = notes.filter((note) => !STRIP_MIDIS.includes(note.visualMidi));
if (unmapped.length) {
  const sample = unmapped.slice(0, 5).map((note) => `${note.visualMidi}@${note.t}`).join(", ");
  die(`${unmapped.length} events fall outside the MenuBand display range (${sample}); provide displayMidi/keyMidi in ${STRIP_MIDIS.join(",")}`);
}
const displayNotes = notes.map((note) => ({ ...note, midi: note.visualMidi }));

const PALETTES = {
  lilac: ["rgb(240,235,248)", "rgb(218,205,235)", "rgb(194,176,218)"],
  sunrise: ["rgb(255,239,224)", "rgb(244,202,203)", "rgb(203,183,231)"],
  lagoon: ["rgb(225,249,246)", "rgb(183,226,224)", "rgb(141,181,210)"],
  midnight: ["rgb(31,29,55)", "rgb(61,45,91)", "rgb(115,70,117)"],
  citrus: ["rgb(255,250,213)", "rgb(247,222,157)", "rgb(220,174,157)"],
  silver: ["rgb(245,246,249)", "rgb(213,217,227)", "rgb(178,184,201)"],
};
const paletteSpec = entry.visual.palette;
const paletteName = typeof paletteSpec === "string" ? paletteSpec : paletteSpec.name;
const palette = paletteName && PALETTES[paletteName] ? PALETTES[paletteName] : PALETTES.lilac;
const stops = Array.isArray(paletteSpec) && paletteSpec.length >= 2 ? paletteSpec
  : Array.isArray(paletteSpec.stops) && paletteSpec.stops.length >= 2 ? paletteSpec.stops : palette;
const authoredSeed = entry.seed ?? manifest.seed;
const seed = Number.isInteger(Number(authoredSeed)) ? Number(authoredSeed) >>> 0 : hash32(id);
const motion = normalizeMotion(entry.visual.motion, seed);
const revealSpec = objectOrEmpty(entry.visual.reveal);
const particleSpec = normalizeParticles(entry.visual.particles, seed, entry.density);
const lighting = normalizeLighting(entry.visual.lighting, seed);
const highway = normalizeHighway(entry.visual.highway);

const revealAt = finite(entry.revealAtSec ?? revealSpec.at ?? score.revealAtSec, 3.2);
const revealDuration = positive(entry.revealDurationSec ?? revealSpec.duration ?? score.revealDurationSec, 1.15);
const exitDuration = positive(entry.exitDurationSec ?? revealSpec.exitDuration ?? score.exitDurationSec, 1.1);
const exitLead = positive(revealSpec.exitLead ?? entry.exitLeadSec, 1.5);
const exitAt = Math.max(revealAt + revealDuration, finite(entry.exitAtSec ?? revealSpec.exitAt ?? score.exitAtSec, duration - exitLead));
const startsOnScreen = entry.startsOnScreen === true || revealSpec.startsOnScreen === true;
const melodyOnly = entry.melodyOnly === true;

const scoreTransitions = melodyOnly ? [] : [
  { at: score.splitAtSec, mode: "right" },
  { at: score.fullPercussionAtSec, mode: "full" },
  { at: score.percussionOffAtSec, mode: "tone" },
].filter((transition) => Number.isFinite(Number(transition.at)));
const explicitTransitions = normalizeTransitions(entry.transitions || entry.visual.transitions || score.transitions, duration);
const transitions = explicitTransitions.length ? explicitTransitions : normalizeTransitions(scoreTransitions, duration);

const plan = {
  id, index, scorePath, audioPath, outPath, durationSec: duration, renderSec: total,
  fps: FPS, size: `${W}x${H}`, events: notes.length, seed,
  palette: stops, revealAt, revealDuration, exitAt, exitDuration,
  startsOnScreen, melodyOnly, transitions, highway,
};
if (process.argv.includes("--dry-run")) {
  console.log(JSON.stringify(plan, null, 2));
  process.exit(0);
}

mkdirSync(dirname(outPath), { recursive: true });

const { canvas, ctx } = makeStage();
const rig = await loadStripRig();
const percussionRig = await loadStripRig("menubar-frames-percussion-right");
const fullPercussionRig = await loadStripRig("menubar-frames-percussion-full");
const particles = makePersistentParticles(ctx, particleSpec, seed);

const stripWidth = W * bounded(motion.stripWidth ?? 0.94, 0.72, 1.0);
const stripHeight = stripWidth / rig.aspect;
const baseY = H * bounded(motion.centerY ?? 0.50, 0.34, 0.66) - stripHeight / 2;
const onsetTimes = [...new Set(notes.map((note) => +note.t.toFixed(5)))];
let onsetCursor = 0;
let mostRecent = -Infinity;
let eventCursor = 0;

function activeMode(t) {
  let mode = "tone";
  for (const transition of transitions) {
    if (transition.at > t) break;
    mode = transition.mode;
  }
  return mode;
}

function rigFor(t, note = null) {
  const mode = activeMode(t);
  if (mode === "full") return fullPercussionRig;
  if (mode === "right" || (note && isDrum(note) && mode === "tone")) return percussionRig;
  return rig;
}

function recentOnset(t) {
  while (onsetCursor < onsetTimes.length && onsetTimes[onsetCursor] <= t) {
    mostRecent = onsetTimes[onsetCursor++];
  }
  return mostRecent;
}

function onsetsThrough(t) {
  const hits = [];
  while (eventCursor < notes.length && notes[eventCursor].t <= t + 1e-7) {
    hits.push(notes[eventCursor++]);
  }
  return hits;
}

function drawBackground() {
  const gradient = ctx.createLinearGradient(0, 0, 0, H);
  for (let i = 0; i < stops.length; i++) gradient.addColorStop(i / (stops.length - 1), cssColor(stops[i]));
  ctx.fillStyle = gradient;
  ctx.fillRect(0, 0, W, H);
  if (paletteSpec.sheen !== false) {
    const sheen = ctx.createLinearGradient(0, H, W, 0);
    sheen.addColorStop(0, "rgba(255,255,255,0)");
    sheen.addColorStop(0.58, `rgba(255,255,255,${bounded(paletteSpec.sheenAlpha ?? 0.14, 0, 0.5)})`);
    sheen.addColorStop(1, "rgba(255,255,255,0)");
    ctx.fillStyle = sheen;
    ctx.fillRect(0, 0, W, H);
  }
}

function drawPerformanceLight(t, activeRig, stripRect, visibility) {
  if (visibility <= 0.005) return;
  const active = [...new Set(litAt(displayNotes, t, finite(lighting.hold, 0.18)))];
  if (!active.length) return;
  const colors = active.map((midi) => stripKeyColor(activeRig, midi));
  const strength = bounded(lighting.strength ?? 1, 0, 2);
  const radius = positive(lighting.radius, 360);
  const perKeyAlpha = Math.min(0.34, 0.48 / Math.sqrt(active.length)) * visibility * strength;

  ctx.save();
  ctx.globalCompositeOperation = lighting.blend || "screen";
  for (let i = 0; i < active.length; i++) {
    const x = stripKeyX(activeRig, active[i], stripRect);
    const y = stripRect.y + stripRect.h * 0.55;
    const color = colors[i];
    const glow = ctx.createRadialGradient(x, y, 8, x, y, radius);
    glow.addColorStop(0, rgb(color, perKeyAlpha));
    glow.addColorStop(0.34, rgb(color, perKeyAlpha * 0.42));
    glow.addColorStop(1, rgb(color, 0));
    ctx.fillStyle = glow;
    ctx.fillRect(0, 0, W, H);
  }
  const average = colors.reduce((sum, color) => [sum[0] + color[0], sum[1] + color[1], sum[2] + color[2]], [0, 0, 0])
    .map((value) => Math.round(value / colors.length));
  ctx.globalCompositeOperation = "soft-light";
  ctx.fillStyle = rgb(average, bounded(lighting.globalAlpha ?? 0.11, 0, 0.5) * visibility * strength);
  ctx.fillRect(0, 0, W, H);
  ctx.restore();
}

function drawNoteHighway(t, activeRig, stripRect) {
  if (!highway.enabled || highway.layout === "paper-loop-3d") return;
  const travelSec = positive(highway.travelSec, 3.0);
  const strikeY = stripRect.y + finite(highway.strikeOffsetPx, 2);
  const loop = scoreLoopGeometry(stripRect);
  const startY = highway.layout === "loop-feed" ? loop.exitY : finite(highway.startY, -56);
  const speed = (strikeY - startY) / travelSec;
  const arrivals = [];
  for (const note of displayNotes) {
    const until = note.t - t;
    if (until > travelSec) break;
    const duration = positive(note.dur, 0.2);
    if (t < note.t + duration) arrivals.push({ note, until, duration });
  }
  arrivals.sort((a, b) => b.until - a.until);

  ctx.save();
  ctx.globalCompositeOperation = highway.blend || "source-over";
  for (const arrival of arrivals) {
    const { note, until, duration } = arrival;
    const keyRect = stripKeyRect(activeRig, note.visualMidi, stripRect);
    const inset = positive(highway.keyInsetPx, 2) * (stripRect.w / (W * 0.94));
    const x = keyRect.x + inset / 2;
    const width = Math.max(12, keyRect.w - inset);
    // The leading edge reaches the key on onset. The duration-sized body keeps
    // feeding into the key until its trailing edge is ingested on note-off.
    const bottom = strikeY - until * speed;
    const top = bottom - duration * speed;
    const visibleTop = Math.max(startY, top);
    const visibleBottom = Math.min(strikeY, bottom);
    if (visibleBottom <= visibleTop) continue;
    const height = visibleBottom - visibleTop;
    const color = stripKeyColor(activeRig, note.visualMidi);
    const alpha = bounded(highway.alpha ?? 0.92, 0, 1);
    const radius = Math.min(width / 2, positive(highway.radiusPx, 9));

    ctx.save();
    ctx.shadowColor = rgb(color, bounded(highway.glowAlpha ?? 0.38, 0, 1));
    ctx.shadowBlur = positive(highway.glowPx, 14);
    ctx.fillStyle = rgb(color, alpha);
    roundRectPath(ctx, x, visibleTop, width, height, radius);
    ctx.fill();

    // The timing marks belong to the feed strip itself: one rail along each
    // key boundary, with no guide through the lane's center.
    if (highway.guides !== false && height > 4) {
      const guideScale = stripRect.w / (W * 0.94);
      const luminance = color[0] * 0.299 + color[1] * 0.587 + color[2] * 0.114;
      const railAlpha = bounded(highway.guideAlpha ?? 0.68, 0, 1);
      ctx.beginPath();
      ctx.moveTo(x + 2, visibleTop + radius);
      ctx.lineTo(x + 2, visibleBottom - radius);
      ctx.moveTo(x + width - 2, visibleTop + radius);
      ctx.lineTo(x + width - 2, visibleBottom - radius);
      ctx.setLineDash([
        positive(highway.dotPx, 3) * guideScale,
        positive(highway.dashGapPx, 9) * guideScale,
        positive(highway.dashPx, 14) * guideScale,
        positive(highway.dashGapPx, 9) * guideScale,
      ]);
      ctx.lineDashOffset = -t * positive(highway.dashSpeedPx, 54);
      ctx.lineCap = "round";
      ctx.lineWidth = positive(highway.guideWidthPx, 2.5) * guideScale;
      ctx.shadowColor = "transparent";
      ctx.strokeStyle = luminance > 155
        ? `rgba(30,24,42,${railAlpha})`
        : `rgba(255,255,255,${railAlpha})`;
      ctx.stroke();
      ctx.setLineDash([]);
      ctx.lineDashOffset = 0;
    }

    const label = KEY_LABELS.get(note.visualMidi);
    const capWidth = Math.max(28, Math.min(width - 10, 58));
    const capHeight = Math.max(30, Math.min(54, capWidth * 0.88));
    const labelY = bottom - capHeight / 2 - 8;
    if (label && labelY - capHeight / 2 >= visibleTop && labelY + capHeight / 2 <= visibleBottom) {
      drawComputerKeycap(label, x + width / 2, labelY, capWidth, capHeight, false);
    }
    ctx.restore();
  }
  ctx.restore();
}

function scoreLoopGeometry(stripRect) {
  const cx = stripRect.x + stripRect.w / 2;
  const cy = stripRect.y - H * bounded(highway.loopLift ?? 0.245, 0.16, 0.34);
  const rx = stripRect.w * bounded(highway.loopWidth ?? 0.46, 0.32, 0.58);
  const ry = H * bounded(highway.loopDepth ?? 0.15, 0.09, 0.22);
  return { cx, cy, rx, ry, exitY: cy + ry };
}

function drawScoreLoop(t, activeRig, stripRect) {
  if (!highway.enabled || highway.layout !== "loop-feed") return;
  const feedSec = positive(highway.travelSec, 3.2);
  const loopSec = Math.max(feedSec + 1, positive(highway.loopSec, 9.6));
  const loop = scoreLoopGeometry(stripRect);
  const orbiting = [];

  for (const note of displayNotes) {
    const until = note.t - t;
    if (until > loopSec) break;
    if (until <= feedSec) continue;
    const progress = clamp01(1 - (until - feedSec) / (loopSec - feedSec));
    const angle = -Math.PI / 2 + progress * Math.PI * 3;
    const depth = (Math.sin(angle) + 1) / 2;
    orbiting.push({ note, progress, angle, depth });
  }
  orbiting.sort((a, b) => a.depth - b.depth || a.note.t - b.note.t);

  ctx.save();
  for (const item of orbiting) {
    const { note, progress, angle, depth } = item;
    const keyRect = stripKeyRect(activeRig, note.visualMidi, stripRect);
    const targetCenter = keyRect.x + keyRect.w / 2;
    const exitMixRaw = clamp01((progress - 0.82) / 0.18);
    const exitMix = exitMixRaw * exitMixRaw * (3 - 2 * exitMixRaw);
    const ringX = loop.cx + Math.cos(angle) * loop.rx;
    const chordSpread = (targetCenter - loop.cx) * 0.18 * (0.38 + depth * 0.62);
    const x = ringX + chordSpread + (targetCenter - ringX - chordSpread) * exitMix;
    const y = loop.cy + Math.sin(angle) * loop.ry;
    const scale = 0.46 + depth * 0.54;
    const width = Math.max(24, (keyRect.w - positive(highway.keyInsetPx, 3)) * scale);
    const height = Math.max(38, Math.min(112, (48 + positive(note.dur, 0.2) * 54) * scale));
    const color = stripKeyColor(activeRig, note.visualMidi);
    const darkness = (1 - depth) * bounded(highway.loopBackDarken ?? 0.72, 0, 0.9);
    const fogColor = color.map((channel) => Math.round(channel * (1 - darkness) + 12 * darkness));
    const alpha = bounded((0.16 + depth * 0.84) * (0.68 + progress * 0.32), 0, 1);
    const radius = Math.min(width / 2, positive(highway.radiusPx, 9) * scale);

    ctx.save();
    ctx.globalAlpha = alpha;
    ctx.filter = `blur(${(1 - depth) * positive(highway.loopBackBlurPx, 4.5)}px)`;
    ctx.shadowColor = rgb(fogColor, 0.34 + depth * 0.34);
    ctx.shadowBlur = 8 + depth * 12;
    ctx.fillStyle = rgb(fogColor, 0.98);
    roundRectPath(ctx, x - width / 2, y - height / 2, width, height, radius);
    ctx.fill();

    const label = KEY_LABELS.get(note.visualMidi);
    if (label && width >= 28 && height >= 34) {
      const capWidth = Math.max(24, Math.min(width - 8, 54 * scale));
      const capHeight = Math.max(26, Math.min(height - 8, capWidth * 0.88));
      drawComputerKeycap(label, x, y + Math.min(height * 0.12, 9), capWidth, capHeight, false);
    }
    ctx.restore();
  }
  ctx.restore();
}

function drawPaperLoopScore(t, activeRig, stripRect) {
  if (!highway.enabled || highway.layout !== "paper-loop-3d") return;
  const straight = positive(highway.paperStraight, 6.2);
  const radius = positive(highway.paperRadius, 1.28);
  const halfWidth = positive(highway.paperHalfWidth, 5.55);
  const loopLength = straight * 2 + Math.PI * radius * 2;
  const loopSec = positive(highway.loopSec, 9.6);
  const speed = loopLength / loopSec;
  const camera = paperLoopCamera(t, stripRect, { straight, radius, halfWidth });
  const project = camera.project;
  const entries = [];
  const slices = Math.max(72, Math.round(positive(highway.paperSlices, 112)));

  for (let index = 0; index < slices; index++) {
    const s0 = loopLength * index / slices;
    const s1 = loopLength * (index + 1) / slices;
    const a = paperLoopPoint(s0, straight, radius, loopLength);
    const b = paperLoopPoint(s1, straight, radius, loopLength);
    const corners = [
      project({ x: -halfWidth, y: a.y, z: a.z }),
      project({ x: halfWidth, y: a.y, z: a.z }),
      project({ x: halfWidth, y: b.y, z: b.z }),
      project({ x: -halfWidth, y: b.y, z: b.z }),
    ];
    if (corners.some((point) => !point.ok)) continue;
    const localZ = (a.z + b.z) / 2;
    const back = clamp01((-localZ / radius + 1) / 2);
    entries.push({
      depth: average(corners.map((point) => point.depth)), layer: 0,
      draw() {
        ctx.save();
        ctx.filter = `blur(${back * positive(highway.paperBackBlurPx, 2.2)}px)`;
        const paperShade = 1 - back;
        const paper = [
          Math.round(56 + paperShade * 199),
          Math.round(48 + paperShade * 203),
          Math.round(64 + paperShade * 177),
        ];
        ctx.fillStyle = `rgb(${paper[0]},${paper[1]},${paper[2]})`;
        projectedQuad(corners);
        ctx.fill();
        ctx.restore();
      },
    });
  }

  for (const note of displayNotes) {
    const until = note.t - t;
    if (until > loopSec) break;
    const duration = positive(note.dur, 0.2);
    if (t >= note.t + duration) continue;
    const head = until * speed;
    const tail = head + duration * speed;
    const from = Math.max(0, head);
    const to = Math.min(loopLength, tail);
    if (to <= from) continue;

    const keyRect = stripKeyRect(activeRig, note.visualMidi, stripRect);
    const centerWorld = ((keyRect.x + keyRect.w / 2) - (stripRect.x + stripRect.w / 2))
      / stripRect.w * halfWidth * 2;
    const widthWorld = Math.max(0.18, keyRect.w / stripRect.w * halfWidth * 2 * 0.92);
    const x0 = centerWorld - widthWorld / 2;
    const x1 = centerWorld + widthWorld / 2;
    const color = stripKeyColor(activeRig, note.visualMidi);
    const step = Math.max(0.08, loopLength / slices);

    for (let s0 = from; s0 < to - 1e-6; s0 += step) {
      const s1 = Math.min(to, s0 + step);
      const a = paperLoopPoint(s0, straight, radius, loopLength);
      const b = paperLoopPoint(s1, straight, radius, loopLength);
      const corners = [
        project({ x: x0, y: a.y, z: a.z }),
        project({ x: x1, y: a.y, z: a.z }),
        project({ x: x1, y: b.y, z: b.z }),
        project({ x: x0, y: b.y, z: b.z }),
      ];
      if (corners.some((point) => !point.ok)) continue;
      const localZ = (a.z + b.z) / 2;
      const front = clamp01((localZ / radius + 1) / 2);
      const darkness = (1 - front) * bounded(highway.loopBackDarken ?? 0.74, 0, 0.95);
      const printed = color.map((channel) => Math.round(channel * (1 - darkness) + 14 * darkness));
      const dashIndex = Math.floor((s0 + t * speed) / (step * 1.8));
      entries.push({
        // Printed ink sits just above the paper so adjacent mesh tiles cannot
        // win the coplanar depth sort and create translucent horizontal cuts.
        depth: average(corners.map((point) => point.depth)) - 0.045, layer: 1,
        draw() {
          ctx.save();
          ctx.filter = `blur(${(1 - front) * positive(highway.loopBackBlurPx, 4.5)}px)`;
          ctx.fillStyle = rgb(printed, 0.16 + front * 0.84);
          projectedQuad(corners);
          ctx.fill();
          if (front > 0.48 && dashIndex % 3 !== 1) {
            const luminance = printed[0] * 0.299 + printed[1] * 0.587 + printed[2] * 0.114;
            ctx.strokeStyle = luminance > 155 ? "rgba(28,22,40,.72)" : "rgba(255,255,255,.78)";
            ctx.lineWidth = 2;
            ctx.beginPath();
            ctx.moveTo(corners[0].x, corners[0].y); ctx.lineTo(corners[3].x, corners[3].y);
            ctx.moveTo(corners[1].x, corners[1].y); ctx.lineTo(corners[2].x, corners[2].y);
            ctx.stroke();
          }
          ctx.restore();
        },
      });
    }

    if (head >= 0 && head <= loopLength) {
      const capSpan = Math.min(duration * speed * 0.72, 0.62);
      const s0 = Math.max(0, head + 0.08);
      const s1 = Math.min(loopLength, s0 + capSpan);
      if (s1 > s0 + 0.04) {
        const a = paperLoopPoint(s0, straight, radius, loopLength);
        const b = paperLoopPoint(s1, straight, radius, loopLength);
        const inset = widthWorld * 0.11;
        const corners = [
          project({ x: x0 + inset, y: a.y, z: a.z }),
          project({ x: x1 - inset, y: a.y, z: a.z }),
          project({ x: x1 - inset, y: b.y, z: b.z }),
          project({ x: x0 + inset, y: b.y, z: b.z }),
        ];
        if (corners.every((point) => point.ok)) {
          const localZ = (a.z + b.z) / 2;
          const front = clamp01((localZ / radius + 1) / 2);
          const label = KEY_LABELS.get(note.visualMidi);
          entries.push({
            depth: average(corners.map((point) => point.depth)) - 0.075, layer: 2,
            draw() { drawProjectedKeycap(corners, label, front); },
          });
        }
      }
    }
  }

  entries.sort((a, b) => b.depth - a.depth || a.layer - b.layer);
  for (const entry of entries) entry.draw();

  const readerLeft = project({ x: -halfWidth, y: -straight / 2, z: radius + 0.025 });
  const readerRight = project({ x: halfWidth, y: -straight / 2, z: radius + 0.025 });
  if (readerLeft.ok && readerRight.ok) {
    ctx.save();
    ctx.lineCap = "round";
    ctx.shadowColor = "rgba(24,17,36,.38)";
    ctx.shadowBlur = 10;
    ctx.strokeStyle = "rgba(34,26,48,.68)";
    ctx.lineWidth = 5;
    ctx.beginPath();
    ctx.moveTo(readerLeft.x, readerLeft.y); ctx.lineTo(readerRight.x, readerRight.y);
    ctx.stroke();
    ctx.restore();
  }
}

function paperLoopPoint(value, straight, radius, loopLength) {
  let s = ((value % loopLength) + loopLength) % loopLength;
  if (s <= straight) return { y: -straight / 2 + s, z: radius };
  s -= straight;
  if (s <= Math.PI * radius) {
    const angle = s / radius;
    return { y: straight / 2 + Math.sin(angle) * radius, z: Math.cos(angle) * radius };
  }
  s -= Math.PI * radius;
  if (s <= straight) return { y: straight / 2 - s, z: -radius };
  s -= straight;
  const angle = s / radius;
  return { y: -straight / 2 - Math.sin(angle) * radius, z: -Math.cos(angle) * radius };
}

function paperLoopCamera(t, stripRect, geometry) {
  const tourSec = positive(highway.paperCameraTourSec, 10);
  const cycle = Math.floor(t / tourSec);
  const phase = (t % tourSec) / tourSec;
  let tour = 0;
  if (phase >= 0.50 && phase < 0.72) tour = smooth01((phase - 0.50) / 0.22);
  else if (phase >= 0.72 && phase < 0.86) tour = 1;
  else if (phase >= 0.86) tour = 1 - smooth01((phase - 0.86) / 0.14);
  const direction = cycle % 2 === 0 ? 1 : -1;
  const azimuth = direction * tour * positive(highway.paperCameraYawRad, 0.34);
  const elevation = tour * positive(highway.paperCameraTiltRad, 0.15);
  const distance = positive(highway.paperCameraDistance, 14.4)
    + tour * positive(highway.paperCameraZoomRange, 1.8);
  const horizontal = Math.cos(elevation) * distance;
  const camera = {
    x: Math.sin(azimuth) * horizontal,
    y: Math.sin(elevation) * distance,
    z: Math.cos(azimuth) * horizontal,
  };
  const target = { x: 0, y: 0, z: 0 };
  const roll = direction * tour * positive(highway.paperCameraRollRad, 0.075);
  const forward = normalize3(subtract3(target, camera));
  let right = normalize3(cross3(forward, { x: 0, y: 1, z: 0 }));
  let up = cross3(right, forward);
  if (Math.abs(roll) > 1e-6) {
    const cosine = Math.cos(roll), sine = Math.sin(roll);
    const rolledRight = add3(scale3(right, cosine), scale3(up, sine));
    const rolledUp = subtract3(scale3(up, cosine), scale3(right, sine));
    right = rolledRight; up = rolledUp;
  }
  const focal = W * positive(highway.paperCameraFocal, 1.14);
  const raw = (point) => {
    const relative = subtract3(point, camera);
    const depth = dot3(relative, forward);
    if (depth < 0.12) return { x: 0, y: 0, depth, ok: false };
    return {
      x: dot3(relative, right) * focal / depth,
      y: -dot3(relative, up) * focal / depth,
      depth, ok: true,
    };
  };
  const reader = raw({ x: 0, y: -geometry.straight / 2, z: geometry.radius });
  const offsetX = stripRect.x + stripRect.w / 2 - reader.x;
  const offsetY = stripRect.y - reader.y;
  return {
    project(point) {
      const projected = raw(point);
      return { ...projected, x: projected.x + offsetX, y: projected.y + offsetY };
    },
  };
}

function drawProjectedKeycap(corners, label, front) {
  if (!label) return;
  const alpha = 0.18 + front * 0.80;
  ctx.save();
  ctx.filter = `blur(${(1 - front) * positive(highway.loopBackBlurPx, 4.5)}px)`;
  ctx.fillStyle = `rgba(255,255,255,${alpha})`;
  ctx.strokeStyle = `rgba(31,24,44,${0.14 + front * 0.76})`;
  ctx.lineWidth = Math.max(1, front * 2.5);
  projectedQuad(corners);
  ctx.fill(); ctx.stroke();
  if (front > 0.46) {
    const cx = average(corners.map((point) => point.x));
    const cy = average(corners.map((point) => point.y));
    const width = Math.hypot(corners[1].x - corners[0].x, corners[1].y - corners[0].y);
    const height = Math.hypot(corners[3].x - corners[0].x, corners[3].y - corners[0].y);
    const angle = Math.atan2(corners[1].y - corners[0].y, corners[1].x - corners[0].x);
    ctx.filter = "none";
    ctx.translate(cx, cy);
    ctx.rotate(angle);
    ctx.fillStyle = `rgba(25,19,37,${0.46 + front * 0.54})`;
    ctx.font = `900 ${Math.max(12, Math.min(42, width * 0.62, height * 0.72))}px MBSansRounded`;
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    ctx.fillText(label, 0, 1);
  }
  ctx.restore();
}

function projectedQuad(corners) {
  ctx.beginPath();
  ctx.moveTo(corners[0].x, corners[0].y);
  for (let index = 1; index < corners.length; index++) ctx.lineTo(corners[index].x, corners[index].y);
  ctx.closePath();
}

function subtract3(a, b) { return { x: a.x - b.x, y: a.y - b.y, z: a.z - b.z }; }
function add3(a, b) { return { x: a.x + b.x, y: a.y + b.y, z: a.z + b.z }; }
function scale3(a, scale) { return { x: a.x * scale, y: a.y * scale, z: a.z * scale }; }
function dot3(a, b) { return a.x * b.x + a.y * b.y + a.z * b.z; }
function cross3(a, b) {
  return { x: a.y * b.z - a.z * b.y, y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x };
}
function normalize3(value) {
  const length = Math.hypot(value.x, value.y, value.z) || 1;
  return scale3(value, 1 / length);
}
function average(values) { return values.reduce((sum, value) => sum + value, 0) / Math.max(1, values.length); }

function drawKeyIntakes(t, activeRig, stripRect) {
  if (!highway.enabled) return;
  const held = [...new Set(displayNotes
    .filter((note) => t >= note.t && t < note.t + positive(note.dur, 0.2))
    .map((note) => note.visualMidi))];
  if (!held.length) return;

  ctx.save();
  ctx.lineCap = "round";
  for (const midi of held) {
    const keyRect = stripKeyRect(activeRig, midi, stripRect);
    const color = stripKeyColor(activeRig, midi);
    const inset = Math.max(4, keyRect.w * 0.12);
    ctx.shadowColor = "rgba(12,8,24,0.58)";
    ctx.shadowBlur = 10;
    ctx.strokeStyle = "rgba(20,14,32,0.62)";
    ctx.lineWidth = 8;
    ctx.beginPath();
    ctx.moveTo(keyRect.x + inset, stripRect.y + 1);
    ctx.lineTo(keyRect.x + keyRect.w - inset, stripRect.y + 1);
    ctx.stroke();
    ctx.shadowColor = rgb(color, 0.7);
    ctx.shadowBlur = 8;
    ctx.strokeStyle = rgb(color, 0.96);
    ctx.lineWidth = 3;
    ctx.stroke();
  }
  ctx.restore();
}

function drawKeyboardLabels(t, activeRig, stripRect) {
  if (!highway.enabled) return;
  const held = new Set(displayNotes
    .filter((note) => t >= note.t && t < note.t + positive(note.dur, 0.2))
    .map((note) => note.visualMidi));
  const labelY = stripRect.y + stripRect.h * 0.72;
  ctx.save();
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";
  for (const midi of STRIP_MIDIS) {
    const label = KEY_LABELS.get(midi);
    if (!label) continue;
    const keyRect = stripKeyRect(activeRig, midi, stripRect);
    const active = held.has(midi);
    const size = Math.max(20, Math.min(31, keyRect.w * 0.46));
    ctx.font = `${active ? 900 : 800} ${size}px MBSansRounded`;
    ctx.shadowColor = active ? "rgba(255,255,255,0.9)" : "transparent";
    ctx.shadowBlur = active ? 7 : 0;
    ctx.fillStyle = active ? "rgba(255,255,255,0.96)" : "rgba(24,18,36,0.68)";
    ctx.fillText(label, keyRect.x + keyRect.w / 2, labelY);
  }
  ctx.restore();
}

function drawComputerKeycap(label, cx, cy, width, height, active) {
  const x = cx - width / 2;
  const y = cy - height / 2;
  const radius = Math.min(10, height * 0.22);
  ctx.save();
  ctx.shadowColor = active ? "rgba(255,255,255,0.9)" : "rgba(18,12,30,0.3)";
  ctx.shadowBlur = active ? 14 : 6;
  ctx.shadowOffsetY = active ? 0 : 3;
  ctx.fillStyle = active ? "rgba(22,17,34,0.98)" : "rgba(255,255,255,0.96)";
  ctx.strokeStyle = active ? "rgba(255,255,255,0.98)" : "rgba(30,23,44,0.9)";
  ctx.lineWidth = active ? 3.5 : 2.5;
  roundRectPath(ctx, x, y, width, height, radius);
  ctx.fill();
  ctx.shadowColor = "transparent";
  ctx.stroke();
  ctx.fillStyle = active ? "white" : "rgb(24,18,36)";
  ctx.font = `900 ${Math.max(22, Math.min(42, height * 0.72))}px MBSansRounded`;
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";
  ctx.fillText(label, cx, cy + height * 0.025);
  ctx.restore();
}

function sceneCamera(t, stripRect) {
  if (highway.layout !== "loop-feed") return { cx: W / 2, cy: H * 0.42, zoom: 1, rotate: 0 };
  const barSec = positive(score.barSec, 2);
  const phrase = Math.sin((t / (barSec * 4)) * Math.PI * 2 + (seed % 19) * 0.08);
  return {
    cx: W / 2,
    cy: stripRect.y - H * 0.12,
    zoom: bounded(0.925 + phrase * 0.035, 0.86, 0.98),
    rotate: Math.sin(t * positive(highway.cameraRotateSpeed, 0.22) + seed * 0.001) * positive(highway.cameraRotateRad, 0.018),
  };
}

function stripPose(t) {
  const onset = recentOnset(t);
  const age = t - onset;
  const struck = Number.isFinite(age) && age >= 0 && age < 0.85;
  const bounceDecay = positive(motion.bounceDecay, 5.2);
  const bounceFrequency = positive(motion.bounceFrequency, 18.5);
  const spring = struck ? Math.exp(-age * bounceDecay) * Math.sin(age * bounceFrequency) : 0;
  const barSec = positive(score.barSec, 2.0);
  const continuousWalk = startsOnScreen && melodyOnly;
  const walkDepth = continuousWalk ? Math.sin(Math.PI * clamp01(t / duration)) : 1;
  const breath = continuousWalk ? 0
    : Math.sin((t / barSec) * Math.PI * 2 + (seed % 31) * 0.03) * finite(motion.breathPx, 8);
  const reveal = clamp01((t - revealAt) / revealDuration);
  const revealEase = 1 - Math.pow(1 - reveal, 3);
  const exit = clamp01((t - exitAt) / exitDuration);
  const exitCurve = exit * exit * exit;
  const exitEase = startsOnScreen ? 0 : exitCurve;
  const walkScale = continuousWalk ? 0.76 + walkDepth * 0.24 : 1;
  const poseWidth = stripWidth * walkScale;
  const poseHeight = stripHeight * walkScale;
  const hiddenY = -poseHeight - positive(revealSpec.topPadding, 24);
  const nearCenterY = baseY + stripHeight / 2;
  const walkCenterY = nearCenterY - (1 - walkDepth) * H * 0.07;
  const restingY = walkCenterY - poseHeight / 2 + breath - spring * finite(motion.bouncePx, 46);
  const enteredY = startsOnScreen ? restingY : hiddenY + (restingY - hiddenY) * revealEase;
  const y = enteredY + (H + poseHeight * 2 - enteredY) * exitEase;
  const swayFrequency = positive(motion.swayFrequency, 1.35);
  const swayPhase = (seed % 360) * Math.PI / 180;
  const swayUnit = Math.sin(t * swayFrequency + swayPhase)
    + Math.sin(t * swayFrequency * 2.31 + swayPhase * 0.47) * bounded(motion.swayHarmonic ?? 0.18, 0, 0.6);
  const sway = swayUnit * finite(motion.swayPx, 0);
  let transitionAge = Infinity;
  for (const transition of transitions) transitionAge = Math.min(transitionAge, Math.abs(t - transition.at));
  const shake = transitionAge < positive(motion.shakeDuration, 0.7)
    ? Math.exp(-transitionAge * positive(motion.shakeDecay, 5.2)) * Math.sin(transitionAge * positive(motion.shakeFrequency, 42))
    : 0;
  return {
    x: (W - poseWidth) / 2 + sway + shake * finite(motion.shakePx, 22), y,
    w: poseWidth, h: poseHeight, spring, reveal, revealEase, exitEase, shake,
    boardVisible: startsOnScreen || reveal > 0,
    illumination: continuousWalk ? walkDepth : revealEase * (1 - exitCurve),
    curtainClosed: continuousWalk ? 1 - walkDepth : Math.max(1 - revealEase, exitCurve),
    walkDepth, swayUnit,
  };
}

function drawCurtain(pose) {
  if (!startsOnScreen) return;
  const closed = pose.curtainClosed;
  if (closed <= 0.001) return;
  const alpha = bounded(revealSpec.curtainAlpha ?? 0.84, 0, 0.95) * closed;
  const shade = ctx.createLinearGradient(0, pose.y, 0, pose.y + pose.h);
  shade.addColorStop(0, `rgba(2,4,10,${alpha})`);
  shade.addColorStop(0.55, `rgba(8,10,22,${alpha * 0.9})`);
  shade.addColorStop(1, `rgba(2,4,10,${alpha})`);
  ctx.save();
  ctx.fillStyle = shade;
  ctx.fillRect(pose.x, pose.y, pose.w, pose.h);
  ctx.restore();
}

function drawFrame(t) {
  const pose = stripPose(t);
  const activeRig = rigFor(t);
  drawBackground();
  const camera = sceneCamera(t, pose);
  ctx.save();
  ctx.translate(camera.cx, camera.cy);
  ctx.rotate(camera.rotate);
  ctx.scale(camera.zoom, camera.zoom);
  ctx.translate(-camera.cx, -camera.cy);
  drawPerformanceLight(t, activeRig, pose, startsOnScreen
    ? pose.illumination : pose.revealEase * (1 - pose.exitEase));
  drawPaperLoopScore(t, activeRig, pose);
  drawScoreLoop(t, activeRig, pose);
  drawNoteHighway(t, activeRig, pose);

  if (pose.boardVisible && pose.exitEase < 1) {
    const scale = 1 + pose.spring * finite(motion.bounceScale, 0.020);
    const tilt = Math.sin(t * positive(motion.floatFrequency, 0.72)) * finite(motion.floatTilt, 0.0032)
      + pose.swayUnit * finite(motion.swayTilt, 0)
      + pose.spring * finite(motion.bounceTilt, 0.0024) + pose.shake * finite(motion.shakeTilt, 0.018);
    const cx = pose.x + pose.w / 2;
    const cy = pose.y + pose.h / 2;
    ctx.save();
    ctx.translate(cx, cy); ctx.rotate(tilt); ctx.scale(scale, scale); ctx.translate(-cx, -cy);
    ctx.save();
    ctx.globalAlpha = bounded(motion.shadowAlpha ?? 0.18, 0, 0.5);
    ctx.filter = `blur(${positive(motion.shadowBlur, 18)}px)`;
    ctx.fillStyle = motion.shadowColor || "rgb(45,28,70)";
    ctx.fillRect(pose.x + 24, pose.y + 24, pose.w - 48, pose.h);
    ctx.restore();
    const lit = litAt(displayNotes, t, finite(lighting.keyHold, 0.16));
    drawStrip(ctx, activeRig, lit, pose.x, pose.y, pose.w);
    drawKeyIntakes(t, activeRig, pose);
    drawKeyboardLabels(t, activeRig, pose);
    ctx.restore();
  }

  for (const note of onsetsThrough(t)) {
    const eventRig = rigFor(note.t, note);
    const x = stripKeyX(eventRig, note.visualMidi, pose);
    const y = pose.y + pose.h + 8;
    const color = stripKeyColor(eventRig, note.visualMidi);
    particles.spawn({
      x, y, color, midi: note.soundMidi ?? note.midi, drum: isDrum(note), eventIndex: note.eventIndex,
    });
  }
  // The departing board pulls the existing stream down with it. Marks remain
  // fully opaque and are never cleared; the stronger exit gravity only makes
  // sure they cross the bottom naturally before the empty final frame.
  particles.stepAndDraw(1 / FPS, 1 + pose.exitEase * 10);
  ctx.restore();
  drawCurtain(pose);
}

const stillArg = valueAfter("--still");
if (stillArg != null) {
  const stillAt = bounded(Number(stillArg), 0, total);
  if (!Number.isFinite(stillAt)) die("--still must be a number");
  const frames = Math.round(stillAt * FPS);
  for (let frame = 0; frame <= frames; frame++) drawFrame(frame / FPS);
  const stillOut = outputPath(valueAfter("--still-out"), `${variationDir}/${id}-${stillAt.toFixed(2)}s.png`);
  mkdirSync(dirname(stillOut), { recursive: true });
  writeFileSync(stillOut, canvas.toBuffer("image/png"));
  console.log(`✓ still ${stillOut}`);
  process.exit(0);
}

await renderVideo({
  canvas, audioPath, outPath, total, drawFrame,
  label: `MenuBand variation ${index + 1}/${manifest.variations.length} · ${id}`,
  fps: FPS,
});

function makePersistentParticles(context, spec, variationSeed) {
  const falling = [];
  const gravity = positive(spec.gravity, 320);
  const drift = positive(spec.drift, 60);
  const size = positive(spec.size, 1);
  const density = bounded(spec.density ?? 1, 0, 4);
  const percussionCharacter = spec.percussionCharacter || "kit";

  function copiesFor(eventIndex) {
    const whole = Math.floor(density);
    return whole + (unitHash(variationSeed ^ eventIndex) < density - whole ? 1 : 0);
  }

  function stroke(color, width = 4) {
    context.strokeStyle = rgb(color);
    context.lineWidth = width;
    context.lineCap = "round";
    context.lineJoin = "round";
  }

  function drawNote(color) {
    context.fillStyle = rgb(color);
    context.strokeStyle = rgb(color);
    context.lineWidth = 4;
    context.save(); context.translate(0, 14); context.rotate(-0.3);
    context.beginPath(); context.ellipse(0, 0, 11, 8, 0, 0, Math.PI * 2); context.fill(); context.restore();
    context.fillRect(9, -22, 4.5, 36);
    context.beginPath(); context.moveTo(13, -22); context.quadraticCurveTo(30, -14, 20, 2);
    context.quadraticCurveTo(26, -10, 13, -10); context.closePath(); context.fill();
  }

  function drawDrum(midi, color) {
    const kind = ((midi % 12) + 12) % 12;
    stroke(color);
    context.fillStyle = rgb(color);
    if (percussionCharacter === "marks") {
      const arms = kind % 2 ? 6 : 4;
      for (let i = 0; i < arms; i++) {
        const angle = i * Math.PI * 2 / arms;
        context.beginPath(); context.moveTo(Math.cos(angle) * 5, Math.sin(angle) * 5);
        context.lineTo(Math.cos(angle) * 20, Math.sin(angle) * 20); context.stroke();
      }
    } else if (kind === 0) {
      // Bass drum: one solid particle color around an open negative center.
      // The old translucent face read as a second gray material.
      stroke(color, 6);
      context.beginPath(); context.arc(0, 0, 16, 0, Math.PI * 2); context.stroke();
      context.beginPath(); context.moveTo(-9, 13); context.lineTo(-16, 23);
      context.moveTo(9, 13); context.lineTo(16, 23); context.stroke();
      context.beginPath(); context.moveTo(25, -15); context.lineTo(8, -3); context.stroke();
      context.fillStyle = rgb(color);
      context.beginPath(); context.arc(6, -2, 4, 0, Math.PI * 2); context.fill();
    } else if (kind === 2) {
      context.beginPath(); context.ellipse(0, -8, 18, 6, 0, 0, Math.PI * 2); context.fill(); context.stroke();
      context.beginPath(); context.moveTo(-18, -8); context.lineTo(-15, 10);
      context.quadraticCurveTo(0, 16, 15, 10); context.lineTo(18, -8); context.stroke();
    } else if (kind === 4 || kind === 5) {
      for (let i = 0; i < 8; i++) {
        const angle = i * Math.PI / 4;
        context.beginPath(); context.moveTo(Math.cos(angle) * 7, Math.sin(angle) * 7);
        context.lineTo(Math.cos(angle) * 20, Math.sin(angle) * 20); context.stroke();
      }
      context.beginPath(); context.arc(0, 0, 5, 0, Math.PI * 2); context.fill();
    } else {
      context.beginPath(); context.moveTo(-20, 1); context.quadraticCurveTo(0, -13, 20, 1);
      context.quadraticCurveTo(0, 8, -20, 1); context.closePath(); context.fill(); context.stroke();
      context.beginPath(); context.moveTo(0, 4); context.lineTo(0, 22); context.stroke();
    }
  }

  return {
    spawn({ x, y, color, midi, drum, eventIndex }) {
      const count = drum ? 1 : copiesFor(eventIndex);
      for (let copy = 0; copy < count; copy++) {
        const hash = hash32(`${variationSeed}:${eventIndex}:${copy}`);
        falling.push({
          x: x + (unitHash(hash) - 0.5) * 14,
          y: y - copy * 8,
          color, midi, drum,
          vx: (unitHash(hash ^ 0x9e3779b9) - 0.5) * drift * 2,
          vy: 40 + unitHash(hash ^ 0x85ebca6b) * 48,
          rot: (unitHash(hash ^ 0xc2b2ae35) - 0.5) * 0.7,
          spin: (unitHash(hash ^ 0x27d4eb2f) - 0.5) * finite(spec.spin, 1.0),
          scale: size * (0.86 + unitHash(hash ^ 0x165667b1) * 0.28),
        });
      }
    },
    stepAndDraw(dt, gravityScale = 1) {
      for (const particle of falling) {
        particle.x += particle.vx * dt;
        particle.y += particle.vy * dt;
        particle.vy += gravity * gravityScale * dt;
        particle.rot += particle.spin * dt;
      }
      for (let i = falling.length - 1; i >= 0; i--) {
        if (falling[i].y > H + 90) falling.splice(i, 1);
      }
      for (const particle of falling) {
        context.save();
        context.globalAlpha = 1;
        context.translate(particle.x, particle.y);
        context.rotate(particle.rot);
        context.scale(particle.scale, particle.scale);
        context.shadowColor = "rgba(25,18,35,0.32)";
        context.shadowBlur = 5;
        context.shadowOffsetX = 2;
        context.shadowOffsetY = 3;
        if (particle.drum) drawDrum(particle.midi, particle.color);
        else drawNote(particle.color);
        context.restore();
      }
    },
  };
}

function normalizeTransitions(items, scoreDuration) {
  if (!Array.isArray(items)) return [];
  const aliases = { none: "tone", normal: "tone", notes: "tone", split: "right", percussionRight: "right", percussionFull: "full" };
  const out = items.map((item) => {
    const at = Number(item.at ?? item.t ?? item.atSec);
    const rawMode = String(item.mode || item.rig || item.type || "tone");
    return { at, mode: aliases[rawMode] || rawMode };
  }).filter((item) => Number.isFinite(item.at) && item.at >= 0 && item.at <= scoreDuration && ["tone", "right", "full"].includes(item.mode));
  return out.sort((a, b) => a.at - b.at);
}

function normalizeMotion(value, seedValue) {
  if (value && typeof value === "object" && !Array.isArray(value)) return value;
  const words = String(value || "").toLowerCase();
  const amount = (salt, lo, hi) => lo + unitHash(seedValue ^ salt) * (hi - lo);
  let motion = {
    breathPx: amount(0x11, 5, 13),
    bouncePx: amount(0x22, 34, 58),
    bounceDecay: amount(0x33, 4.5, 6.8),
    bounceFrequency: amount(0x44, 15, 23),
    floatFrequency: amount(0x55, 0.45, 1.05),
    floatTilt: amount(0x66, 0.0018, 0.0052),
    shakePx: amount(0x77, 14, 29),
    shakeFrequency: amount(0x88, 34, 52),
    shakeTilt: amount(0x99, 0.010, 0.024),
  };
  if (/near-still|restrained|shallow|imperceptible/.test(words)) {
    motion = { ...motion, breathPx: motion.breathPx * 0.55, bouncePx: motion.bouncePx * 0.68, floatTilt: motion.floatTilt * 0.55 };
  }
  if (/buoyant|elastic|hops|rebound|spring/.test(words)) {
    motion = { ...motion, bouncePx: motion.bouncePx * 1.24, bounceDecay: motion.bounceDecay * 0.82 };
  }
  if (/slow|long|glacial|tidal|viscous/.test(words)) {
    motion = { ...motion, floatFrequency: motion.floatFrequency * 0.65, bounceFrequency: motion.bounceFrequency * 0.78 };
  }
  if (/quick|fast|snap|jitter|peck|piston/.test(words)) {
    motion = { ...motion, bounceFrequency: motion.bounceFrequency * 1.3, shakeFrequency: motion.shakeFrequency * 1.18 };
  }
  if (/tilt|roll|pendulum|orbit|corkscrew|rotation|figure-eight/.test(words)) {
    motion = { ...motion, floatTilt: motion.floatTilt * 1.55 };
  }
  if (/tectonic|storm|jolt|shudder/.test(words)) {
    motion = { ...motion, shakePx: motion.shakePx * 1.35, shakeTilt: motion.shakeTilt * 1.25 };
  }
  return motion;
}

function normalizeParticles(value, seedValue, densityText) {
  if (value && typeof value === "object" && !Array.isArray(value)) return value;
  const text = `${value || ""} ${densityText || ""}`.toLowerCase();
  let density = 0.9 + unitHash(seedValue ^ 0xa531) * 0.75;
  const range = text.match(/([\d.]+)\s+to\s+([\d.]+)\s+notes/);
  if (range) density = Math.max(0.65, Math.min(2.2, (Number(range[1]) + Number(range[2])) / 3.2));
  return {
    density,
    gravity: 285 + unitHash(seedValue ^ 0xb642) * 105,
    drift: 42 + unitHash(seedValue ^ 0xc753) * 52,
    spin: 0.55 + unitHash(seedValue ^ 0xd864) * 0.9,
    size: 0.88 + unitHash(seedValue ^ 0xe975) * 0.28,
    melodicCharacter: "eighth",
    percussionCharacter: seedValue % 5 === 0 ? "marks" : "kit",
  };
}

function normalizeLighting(value, seedValue) {
  if (value && typeof value === "object" && !Array.isArray(value)) return value;
  const words = String(value || "").toLowerCase();
  let strength = 0.78 + unitHash(seedValue ^ 0x1357) * 0.42;
  if (/dim|subtle|narrow|restrained/.test(words)) strength *= 0.78;
  if (/neon|electric|solar|bright|flare/.test(words)) strength *= 1.2;
  return {
    strength,
    radius: 290 + unitHash(seedValue ^ 0x2468) * 150,
    globalAlpha: /dim|narrow/.test(words) ? 0.07 : 0.10 + unitHash(seedValue ^ 0x369a) * 0.05,
  };
}

function normalizeHighway(value) {
  if (value === true) return { enabled: true };
  if (!value || value === false) return { enabled: false };
  if (typeof value === "object" && !Array.isArray(value)) {
    return { enabled: value.enabled !== false, ...value };
  }
  return { enabled: false };
}

function isDrum(note) {
  return ["drum", "perc", "percussion", "kit"].includes(String(note.lane || note.type || "").toLowerCase());
}

function objectOrEmpty(value) {
  return value && typeof value === "object" && !Array.isArray(value) ? value : {};
}

function cssColor(color) {
  return Array.isArray(color) ? rgb(color) : color;
}

function rgb(color, alpha = 1) {
  return `rgba(${color[0]},${color[1]},${color[2]},${alpha})`;
}

function roundRectPath(context, x, y, width, height, radius) {
  const r = Math.max(0, Math.min(radius, width / 2, height / 2));
  context.beginPath();
  context.moveTo(x + r, y);
  context.arcTo(x + width, y, x + width, y + height, r);
  context.arcTo(x + width, y + height, x, y + height, r);
  context.arcTo(x, y + height, x, y, r);
  context.arcTo(x, y, x + width, y, r);
  context.closePath();
}

function finite(value, fallback) {
  const number = Number(value);
  return Number.isFinite(number) ? number : fallback;
}

function positive(value, fallback) {
  const number = Number(value);
  return Number.isFinite(number) && number > 0 ? number : fallback;
}

function bounded(value, min, max) {
  const number = Number(value);
  return Math.max(min, Math.min(max, Number.isFinite(number) ? number : min));
}

function clamp01(value) {
  return Math.max(0, Math.min(1, value));
}

function smooth01(value) {
  const x = clamp01(value);
  return x * x * (3 - 2 * x);
}

function hash32(value) {
  const string = String(value);
  let hash = 2166136261;
  for (let i = 0; i < string.length; i++) {
    hash ^= string.charCodeAt(i);
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0;
}

function unitHash(value) {
  let x = Number(value) >>> 0;
  x ^= x >>> 16; x = Math.imul(x, 0x7feb352d);
  x ^= x >>> 15; x = Math.imul(x, 0x846ca68b);
  x ^= x >>> 16;
  return (x >>> 0) / 0x100000000;
}
