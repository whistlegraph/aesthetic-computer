#!/usr/bin/env node
// Render one seeded MenuBand variation from a { defaults, variations } manifest.
// The score and audio are produced separately; this file owns only the real-strip
// choreography, persistent falling glyphs, performance light, and video encode.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, isAbsolute, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import {
  W, H, OUT, STRIP_MIDIS, makeStage, loadStripRig, drawStrip,
  stripKeyX, stripKeyColor, litAt, renderVideo,
} from "./reel-lib.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const FPS = 60;

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
const outPath = outputPath(entry.outPath || entry.output, `${variationDir}/${id}.mp4`);
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
  startsOnScreen, melodyOnly, transitions,
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
  let transitionAge = Infinity;
  for (const transition of transitions) transitionAge = Math.min(transitionAge, Math.abs(t - transition.at));
  const shake = transitionAge < positive(motion.shakeDuration, 0.7)
    ? Math.exp(-transitionAge * positive(motion.shakeDecay, 5.2)) * Math.sin(transitionAge * positive(motion.shakeFrequency, 42))
    : 0;
  return {
    x: (W - poseWidth) / 2 + shake * finite(motion.shakePx, 22), y,
    w: poseWidth, h: poseHeight, spring, reveal, revealEase, exitEase, shake,
    boardVisible: startsOnScreen || reveal > 0,
    illumination: continuousWalk ? walkDepth : revealEase * (1 - exitCurve),
    curtainClosed: continuousWalk ? 1 - walkDepth : Math.max(1 - revealEase, exitCurve),
    walkDepth,
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
  drawPerformanceLight(t, activeRig, pose, startsOnScreen
    ? pose.illumination : pose.revealEase * (1 - pose.exitEase));

  if (pose.boardVisible && pose.exitEase < 1) {
    const scale = 1 + pose.spring * finite(motion.bounceScale, 0.020);
    const tilt = Math.sin(t * positive(motion.floatFrequency, 0.72)) * finite(motion.floatTilt, 0.0032)
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
