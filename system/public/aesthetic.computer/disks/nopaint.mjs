// No Paint, 26.07.17.15.39
// Collaborate with the proposing machine: press No to discard or Paint to keep.

import {
  NOPAINT_LOOP_STATES,
  NOPAINT_MAX_RANDOM_ALPHA,
  NOPAINT_VERSION,
  makeProposal,
  proposalDefinition,
  seededRandom,
  seedFrom,
} from "../lib/nopaint-proposals.mjs";
import { nopaintProposal as lineProposal } from "./line.mjs";
import { darkWindowProposal, gridWormProposal } from "../lib/nopaint-construct-brushes.mjs";
import { nonConflictingConstructProposals } from "../lib/nopaint-construct-catalog.mjs";
import { recoveredConstructTransforms } from "../lib/nopaint-construct-transforms.mjs";

const COMPATIBLE_BRUSHES = Object.freeze(new Map([
  [lineProposal.slug, lineProposal],
  [gridWormProposal.slug, gridWormProposal],
  [darkWindowProposal.slug, darkWindowProposal],
  ...nonConflictingConstructProposals.map((contract) => [contract.slug, contract]),
  ...recoveredConstructTransforms.map((contract) => [contract.slug, contract]),
]));

let loopState = "choosing";
let stateBeforePause = "proposing";
let proposal = null;
let proposalFrame = 0;
let proposalPixels = null;
let proposalNumber = 0;
let sessionSeed = 0;
let freshStart = false;
let random = seededRandom(1);
let cameraFeed = null;
let decisions = [];
let saveCount = 0;
let lastDownload = null;
let testApi = null;
let testChannel = null;
let archiveOrigin = null;
let testMode = false;
let paintingResolution = null;
let finishMode = false;
let doneCount = 0;
let completionBusy = false;
let completionProgress = 0;
let completionCode = null;
let completionError = null;
let paintingDragPaused = false;
let paintingPressed = false;
let hoveredDecision = null;
let cursorSheet = null;
let cursorPoint = null;
let cursorFrame = 0;
let cursorWagFrames = 0;
const cueSamples = new Map();
let cueEvents = [];
const PROPOSAL_MERRY_FRAMES = 5 * 60;

const LEGACY_CUES = Object.freeze({
  "no-down": "generic - no button pressed (metal brush).webm",
  no: "generic - no button released (middle).webm",
  "paint-down": "generic - paint button pressed (psst).webm",
  paint: "generic - paint button released (cha).webm",
  rollover: "generic - button rollover.webm",
  "button-down": "generic - button press.webm",
  "button-up": "generic - button release.webm",
  back: "generic - pause release.webm",
  "done-down": "generic - save button pressed.webm",
  done: "generic - save button released.webm",
  "pause-down": "generic - pressing pause.webm",
  "pause-in": "generic - entering pause.webm",
  "pause-out": "generic - pause release.webm",
  "primitive-release": "primitive - released.webm",
});

const BRUSH_CUES = Object.freeze({
  rect: "box - start.webm",
  oval: "elipse - start.webm",
  line: "line - start.webm",
  softy: "softy - landed.webm",
  bubbles: "bubbles - theme.webm",
  "grid-worm": "grid worm - theme.webm",
  "dark-window:1": "dark window - note 1.webm",
  "dark-window:2": "dark window - note 2.webm",
  "dark-window:3": "dark window - note 3.webm",
  "dark-window:4": "dark window - note 4.webm",
  aura: "aura - theme.webm",
  build: "build - builder's beat.webm",
  breathe: "breathe - theme.webm",
  caterpillar: "caterpillar - trotting along.webm",
  ellipse: "elipse - start.webm",
  frame: "frame - knock.webm",
  rainbow: "rainbow - theme.webm",
  triangle: "triangle - start.webm",
  vignette: "vignette - theme.webm",
  contrast: "contrast - theme.webm",
  flip: "flip - first flipping.webm",
  invert: "invert - on.webm",
  "light-bump": "light bump - theme.webm",
  mirror: "mirror - theme.webm",
  quicksand: "quicksand - brief theme.webm",
  recurse: "recurse - thup.webm",
  saturate: "saturate - theme.webm",
  scroll: "scroll - theme.webm",
  sharpen: "sharpen - theme.webm",
  spin: "spin - theme.webm",
  turn: "turn - note 1.webm",
  zoom: "zoom - in.webm",
  walker: "common - jitter.webm",
  banner: "banner - theme.webm",
  wafer: "wafer - nibble appear.webm",
  wipe: "wipe - individual wipe.webm",
  camera: "camera - fx.webm",
});

const brushCueSamples = new Map();
let brushCueProposal = 0;
let activeBrushSound = null;
let activeBrushKind = null;
let decisionHeld = false;
let heldKeyboardDecision = null;

// Exact frame rectangles and origins from Construct's Cursor object (ID 90).
// The animated hand is the original `Over Button` sequence in cursor-sheet0.
const CURSOR_FRAMES = Object.freeze({
  normal: Object.freeze([
    { x: 52, y: 33, w: 11, h: 15, ox: 1 / 11, oy: 1 / 15 },
    { x: 1, y: 33, w: 12, h: 19, ox: 8 / 12, oy: 0.22180451127819545 },
  ]),
  hand: Object.freeze([
    { x: 35, y: 33, w: 15, h: 17, ox: 0.2, oy: 2 / 17 },
    { x: 35, y: 1, w: 15, h: 17, ox: 0.2, oy: 2 / 17 },
    { x: 18, y: 20, w: 15, h: 17, ox: 0.2, oy: 2 / 17 },
    { x: 1, y: 1, w: 15, h: 18, ox: 1 / 15, oy: 0 },
    { x: 15, y: 39, w: 15, h: 15, ox: 4 / 15, oy: 2 / 15 },
    { x: 18, y: 1, w: 15, h: 17, ox: 0.2, oy: 1 / 17 },
    { x: 33, y: 65, w: 14, h: 16, ox: 2 / 14, oy: 0 },
  ]),
});

function playCue(api, name) {
  const fallback = () => api.sound?.synth?.({
    type: name === "no" || name === "back" ? "triangle" : "sine",
    tone: name === "no" ? 180 : name === "back" ? 260 : name === "done" ? 880 : 520,
    duration: name === "done" ? 0.14 : 0.07,
    volume: 0.16,
    attack: 0.003,
    decay: 0.08,
    immediate: true,
  });
  const sample = cueSamples.get(name) ||
    (name === "painting-hover" ? cueSamples.get("rollover") : null);
  if (sample && api.sound?.play) {
    cueEvents.push({ name, path: "legacy" });
    return api.sound.play(sample, { volume: 0.72 });
  }
  cueEvents.push({ name, path: "synth" });
  return fallback();
}

function playBrushCue(api, kind) {
  if (brushCueProposal === proposalNumber) return;
  const sampleKey = kind === "dark-window" ? `${kind}:${(proposal?.note ?? 0) + 1}` : kind;
  const sample = brushCueSamples.get(sampleKey);
  if (!sample || !api.sound?.play) return;
  stopBrushCue();
  brushCueProposal = proposalNumber;
  activeBrushKind = kind;
  cueEvents.push({ name: `brush:${kind}`, path: "legacy" });
  const playing = api.sound.play(sample, { volume: 0.48 }, {
    kill: () => {
      if (activeBrushSound !== playing) return;
      activeBrushSound = null;
      activeBrushKind = null;
      publishTestState();
    },
  });
  activeBrushSound = playing;
  publishTestState();
}

function stopBrushCue() {
  if (!activeBrushSound) return;
  const kind = activeBrushKind;
  const playing = activeBrushSound;
  activeBrushSound = null;
  activeBrushKind = null;
  playing.kill?.(0.04);
  cueEvents.push({ name: `brush-stop:${kind}`, path: "lifecycle" });
  publishTestState();
}

function setDecisionHeld(held) {
  if (decisionHeld === held) return;
  decisionHeld = held;
  activeBrushSound?.update?.({ sampleSpeed: held ? 0.18 : 1 });
  cueEvents.push({
    name: held ? "brush-scratch-slow" : "brush-scratch-resume",
    path: "lifecycle",
  });
  publishTestState();
}

function resumeBrushCue(api) {
  brushCueProposal = 0;
  if (proposal?.kind) playBrushCue(api, proposal.kind);
}

function initialNavigationURL() {
  if (typeof window === "undefined") return null;
  const initial = performance.getEntriesByType?.("navigation")?.[0]?.name;
  try {
    return new URL(initial || window.location.href);
  } catch {
    return null;
  }
}

function freshLaunchRequested(url, colon = [], params = []) {
  const queryValue = url?.searchParams?.get("fresh");
  const queryRequestsFresh = url?.searchParams?.has("fresh") &&
    !["0", "false", "no", "off"].includes(String(queryValue).toLowerCase());
  const launchRequestsFresh = [...colon, ...params].some((value) =>
    ["fresh", "fresh=1", "new"].includes(String(value).trim().toLowerCase()));
  return Boolean(queryRequestsFresh || launchRequestsFresh);
}
let noButton;
let paintButton;
let doneButton;
let backButton;

function transition(next) {
  if (!NOPAINT_LOOP_STATES.includes(next)) {
    throw new Error(`Unknown No Paint loop state: ${next}`);
  }
  loopState = next;
}

function interfaceLayout(screen) {
  const barHeight = Math.max(96, Math.floor(screen.height * 0.2));
  const available = { x: 0, y: 0, w: screen.width, h: screen.height - barHeight };
  const source = paintingResolution || { width: available.w, height: available.h };
  const scale = Math.min(available.w / source.width, available.h / source.height);
  const viewport = {
    x: Math.floor((available.w - source.width * scale) / 2),
    y: Math.floor((available.h - source.height * scale) / 2),
    w: Math.floor(source.width * scale),
    h: Math.floor(source.height * scale),
  };
  return {
    stage: viewport,
    bar: { x: 0, y: screen.height - barHeight, w: screen.width, h: barHeight },
    status: {
      x: 0,
      y: screen.height,
      w: screen.width,
      h: 0,
    },
    scale,
  };
}

function positionButtons(screen) {
  // The recovered instrument keeps the decision pair together along the
  // bottom edge: No on the left, Paint larger on the right. They are the
  // architecture of the surface, not ordinary toolbar buttons.
  const { bar } = interfaceLayout(screen);
  const gap = Math.max(4, Math.floor(screen.width * 0.006));
  const available = screen.width - gap;
  const noWidth = Math.floor(available * 0.38);
  const paintWidth = available - noWidth;
  const buttonY = bar.y;
  const decisionHeight = bar.h;
  const no = noButton.btn || noButton;
  const paint = paintButton.btn || paintButton;
  no.box ||= {};
  paint.box ||= {};
  Object.assign(no.box, {
    x: 0, y: buttonY,
    w: noWidth, h: decisionHeight,
  });
  Object.assign(paint.box, {
    x: noWidth + gap, y: buttonY,
    w: paintWidth, h: decisionHeight,
  });
  const done = doneButton?.btn || doneButton;
  const back = backButton?.btn || backButton;
  if (done) {
    done.box ||= {};
    Object.assign(done.box, {
      x: noWidth + gap, y: buttonY,
      w: paintWidth, h: decisionHeight,
    });
  }
  if (back) {
    back.box ||= {};
    Object.assign(back.box, {
      x: 0, y: buttonY,
      w: noWidth, h: decisionHeight,
    });
  }
}

const MARKER_GLYPHS = Object.freeze({
  N: [[[0, 1], [0, 0], [1, 1], [1, 0]]],
  o: [[[0.2, 0.3], [0.8, 0.3], [1, 0.48], [1, 0.82], [0.8, 1], [0.2, 1], [0, 0.82], [0, 0.48], [0.2, 0.3]]],
  P: [[[0, 1], [0, 0], [0.72, 0], [1, 0.2], [1, 0.45], [0.72, 0.58], [0, 0.58]]],
  a: [[[0, 0.55], [0.2, 0.32], [0.75, 0.32], [0.92, 0.5], [0.92, 1]], [[0.92, 0.55], [0.2, 0.55], [0, 0.72], [0.18, 0.95], [0.92, 0.72]]],
  i: [[[0.5, 0.35], [0.5, 1]], [[0.5, 0.08], [0.5, 0.12]]],
  n: [[[0, 1], [0, 0.35], [0.72, 0.35], [1, 0.55], [1, 1]]],
  t: [[[0.5, 0.05], [0.5, 0.82], [0.7, 1], [0.95, 0.92]], [[0.12, 0.35], [0.9, 0.35]]],
  B: [[[0, 1], [0, 0], [0.65, 0], [0.92, 0.18], [0.65, 0.5], [0, 0.5]], [[0.65, 0.5], [1, 0.68], [0.72, 1], [0, 1]]],
  c: [[[0.95, 0.42], [0.75, 0.3], [0.2, 0.3], [0, 0.5], [0, 0.82], [0.2, 1], [0.8, 1], [1, 0.88]]],
  k: [[[0, 0], [0, 1]], [[0.9, 0.32], [0, 0.7], [1, 1]]],
  D: [[[0, 1], [0, 0], [0.55, 0], [1, 0.25], [1, 0.75], [0.55, 1], [0, 1]]],
  e: [[[0, 0.68], [1, 0.68], [0.88, 0.42], [0.2, 0.3], [0, 0.52], [0.08, 0.88], [0.35, 1], [0.9, 0.92]]],
});

function paintMarkerLabel($, button, label, color) {
  const letters = label.split("");
  const glyphWidth = 0.72;
  const gap = 0.3;
  const unitsWide = letters.length * glyphWidth + Math.max(0, letters.length - 1) * gap;
  const scale = Math.max(4, Math.min(button.box.h * 0.42, button.box.w * 0.68 / unitsWide));
  const originX = button.box.x + (button.box.w - unitsWide * scale) / 2;
  const originY = button.box.y + (button.box.h - scale) / 2;
  const thickness = Math.max(2, Math.round(scale * 0.11));

  letters.forEach((letter, letterIndex) => {
    const paths = MARKER_GLYPHS[letter] || [];
    const offsetX = originX + letterIndex * (glyphWidth + gap) * scale;
    for (let pass = 0; pass < 3; pass += 1) {
      const jitterX = Math.sin((letterIndex + 1) * 17 + pass * 11) * scale * 0.018;
      const jitterY = Math.cos((letterIndex + 1) * 13 + pass * 7) * scale * 0.018;
      paths.forEach((path) => {
        for (let point = 1; point < path.length; point += 1) {
          $.ink(color[0], color[1], color[2], pass === 1 ? 175 : 105).line(
            offsetX + path[point - 1][0] * glyphWidth * scale + jitterX,
            originY + path[point - 1][1] * scale + jitterY,
            offsetX + path[point][0] * glyphWidth * scale + jitterX,
            originY + path[point][1] * scale + jitterY,
            thickness,
          );
        }
      });
    }
  });
}

function paintDecisionButton($, button, label, flavor = "no") {
  const active = button.down || button.over;
  const palette = flavor === "paint"
    ? { fill: [45, 170, 76, active ? 255 : 235], ink: [245, 255, 245] }
    : flavor === "back"
      ? { fill: [232, 119, 28, active ? 255 : 235], ink: [255, 250, 235] }
      : flavor === "done"
        ? { fill: [45, 170, 76, active ? 255 : 235], ink: [245, 255, 245] }
        : { fill: [205, 38, 48, active ? 255 : 235], ink: [255, 245, 235] };
  $.ink(palette.fill)
    .box(button.box, "fill")
    .ink(palette.ink)
    .box(button.box, "outline");
  paintMarkerLabel($, button, label, palette.ink);
}

function paintOriginalCursor($) {
  if (!cursorSheet || !cursorPoint) return;
  const frames = hoveredDecision ? CURSOR_FRAMES.hand : CURSOR_FRAMES.normal;
  const frame = frames[Math.min(cursorFrame, frames.length - 1)];
  const scale = Math.max(1, Math.round(Math.min($.screen.width, $.screen.height) / 300));
  $.paste(
    {
      painting: cursorSheet,
      crop: { x: frame.x, y: frame.y, w: frame.w, h: frame.h },
    },
    Math.round(cursorPoint.x - frame.w * frame.ox * scale),
    Math.round(cursorPoint.y - frame.h * frame.oy * scale),
    scale,
  );
}

function clearProposal({ flatten, needsPaint, page, screen, system }) {
  if (!system.nopaint.buffer) return;
  page(system.nopaint.buffer).wipe(255, 255, 255, 0);
  flatten();
  page(screen);
  system.nopaint.needsPresent = true;
  needsPaint();
}

function paintHasContent(painting) {
  if (!painting?.pixels) return false;
  for (let index = 3; index < painting.pixels.length; index += 4) {
    if (painting.pixels[index] > 0) return true;
  }
  return false;
}

function seedNoiseSubstrate(painting, seed) {
  if (!painting?.pixels) return;
  const substrateRandom = seededRandom(`${seed}:noise-substrate`);
  const base = [
    112 + Math.floor(substrateRandom() * 32),
    112 + Math.floor(substrateRandom() * 32),
    112 + Math.floor(substrateRandom() * 32),
  ];
  painting.pixels.fill(0);
  const rectWidth = Math.min(192, Math.max(48, Math.floor(painting.width * 0.4)));
  const rectHeight = Math.min(192, Math.max(48, Math.floor(painting.height * 0.4)));
  const left = Math.floor((painting.width - rectWidth) / 2);
  const top = Math.floor((painting.height - rectHeight) / 2);
  for (let y = top; y < top + rectHeight; y += 1) {
    for (let x = left; x < left + rectWidth; x += 1) {
      // A stippled rectangle reads as paper grain while preserving transparent
      // interstices for AC's sparse compositor and the checker field beneath it.
      if (substrateRandom() > 0.38) continue;
      const index = (y * painting.width + x) * 4;
      const grain = Math.floor(substrateRandom() * 25) - 12;
      painting.pixels[index] = Math.max(0, Math.min(255, base[0] + grain));
      painting.pixels[index + 1] = Math.max(0, Math.min(255, base[1] + grain));
      painting.pixels[index + 2] = Math.max(0, Math.min(255, base[2] + grain));
      painting.pixels[index + 3] = 150 + Math.floor(substrateRandom() * 70);
    }
  }
}

function chooseProposal(api) {
  stopBrushCue();
  decisionHeld = false;
  transition("choosing");
  const resolution = paintingResolution || {
    width: api.system.painting.width,
    height: api.system.painting.height,
  };
  const baseProposal = makeProposal(
    random,
    resolution.width,
    resolution.height,
  );
  const compatibleBrush = COMPATIBLE_BRUSHES.get(baseProposal.kind);
  proposal = compatibleBrush
    ? compatibleBrush.generate({
        random,
        width: resolution.width,
        height: resolution.height,
        base: baseProposal,
      })
    : baseProposal;
  proposalPixels = compatibleBrush?.applyPixels
    ? compatibleBrush.applyPixels(
        api.system.painting.pixels,
        resolution.width,
        resolution.height,
        proposal.brush.parameters,
      )
    : null;
  proposalNumber += 1;
  proposalFrame = 0;
  transition("proposing");
  playBrushCue(api, proposal.kind);
  api.system.nopaint.needsPresent = true;
  api.needsPaint();
}

function discardProposal(api) {
  if (loopState !== "proposing" && loopState !== "paused") return;
  playCue(api, "no");
  transition("discarding");
  recordDecision(api, "no");
  clearProposal(api);
  chooseProposal(api);
  publishTestState();
}

function merryProposal(api) {
  if (loopState !== "proposing") return;
  cueEvents.push({ name: `brush-timeout:${proposal?.kind}`, path: "merry" });
  clearProposal(api);
  chooseProposal(api);
  publishTestState();
}

function persistPainting({ store, system }) {
  store["painting"] = {
    width: system.painting.width,
    height: system.painting.height,
    pixels: new Uint8ClampedArray(system.painting.pixels),
  };
  store.persist("painting", "nopaint:session", "local:db");
}

async function loadArchivePainting(api, archiveId) {
  const id = String(archiveId || "").trim();
  if (!/^[A-Za-z0-9]{4,32}$/.test(id)) return;

  archiveOrigin = {
    type: "nopaint-archive",
    id,
    record: `https://nopaint.art/${id}`,
    image: `https://pix.nopaint.art/${id}.png`,
    status: "loading",
  };
  api.store["nopaint:origin"] = { ...archiveOrigin };
  api.store.persist("nopaint:origin", "local:db");

  try {
    const loaded = await api.net.preload({
      path: archiveOrigin.image,
      extension: "png",
    });
    const bitmap = loaded?.img || loaded;
    const source = api.painting(256, 256, (p) => p.paste(bitmap, 0, 0));
    api.system.nopaint.replace(api, source, `nopaint-archive:${id}`);
    api.system.nopaint.buffer = api.painting(256, 256, (p) =>
      p.wipe(255, 255, 255, 0)
    );
    archiveOrigin.status = "ready";
    api.store["nopaint:origin"] = { ...archiveOrigin };
    api.store.persist("nopaint:origin", "local:db");
    proposalNumber = 0;
    chooseProposal(api);
    api.hud.label(`No Paint ← archive ${id}`);
    publishTestState();
  } catch (error) {
    archiveOrigin.status = "error";
    archiveOrigin.error = error?.message || "Archive painting could not load";
    api.store["nopaint:origin"] = { ...archiveOrigin };
    api.store.persist("nopaint:origin", "local:db");
    console.error(`No Paint archive load failed for ${id}:`, error);
  }
}

function recordDecision({ store }, decision) {
  decisions.push(Object.freeze({
    number: proposalNumber,
    operation: proposal?.kind,
    decision,
  }));
  const session = {
    version: NOPAINT_VERSION,
    seed: sessionSeed,
    decisions: decisions.slice(),
  };
  if (archiveOrigin) session.origin = { ...archiveOrigin };
  store["nopaint:session"] = session;
  store.persist("nopaint:session", "local:db");
}

function commitProposal(api) {
  if (loopState !== "proposing" && loopState !== "paused") return;
  playCue(api, "paint");
  if (["rect", "oval", "line"].includes(proposal?.kind)) {
    playCue(api, "primitive-release");
  }
  transition("committing");
  recordDecision(api, "paint");

  if (proposalPixels) api.system.painting.pixels.set(api.system.nopaint.buffer.pixels);
  else api.page(api.system.painting).paste(api.system.nopaint.buffer);
  api.flatten();
  api.system.nopaint.addUndoPainting(
    api.system.painting,
    `nopaint:${proposal.kind}`,
  );
  persistPainting(api);
  clearProposal(api);
  chooseProposal(api);
  publishTestState();
}

function togglePaused(api) {
  const { needsPaint } = api;
  if (loopState === "paused") {
    playCue(api, "pause-out");
    transition(stateBeforePause);
    resumeBrushCue(api);
  } else if (loopState === "proposing") {
    playCue(api, "pause-in");
    stopBrushCue();
    stateBeforePause = loopState;
    transition("paused");
  }
  needsPaint();
  publishTestState();
}

function savePainting({ canShare, download, num, system }) {
  saveCount += 1;
  lastDownload = `nopaint-${num.timestamp()}.png`;
  download(lastDownload, system.painting, {
    scale: 2,
    cropToScreen: true,
    // Browser journeys need a deterministic downloaded artifact; native share
    // sheets detach the page/iframe and are covered by their own integration.
    sharing: canShare && !initialNavigationURL()?.searchParams.has("test"),
  });
  publishTestState();
}

function paintingFingerprint(painting) {
  if (!painting?.pixels) return null;
  let hash = 2166136261;
  const stride = Math.max(1, Math.floor(painting.pixels.length / 4096));
  for (let index = 0; index < painting.pixels.length; index += stride) {
    hash ^= painting.pixels[index];
    hash = Math.imul(hash, 16777619);
  }
  return (hash >>> 0).toString(16).padStart(8, "0");
}

function testSnapshot() {
  if (testApi?.screen && noButton && paintButton) positionButtons(testApi.screen);
  const controlBox = (control) => {
    const box = control?.box || control?.btn?.box;
    return box ? { x: box.x, y: box.y, w: box.w, h: box.h } : null;
  };
  const layout = testApi?.screen ? interfaceLayout(testApi.screen) : null;
  return {
    version: NOPAINT_VERSION,
    state: loopState,
    seed: sessionSeed,
    freshStart,
    proposalNumber,
    proposalFrame,
    merry: {
      durationFrames: PROPOSAL_MERRY_FRAMES,
      remainingFrames: Math.max(0, PROPOSAL_MERRY_FRAMES - proposalFrame),
    },
    operation: proposal?.kind || null,
    brush: proposal?.brush ? {
      slug: proposal.brush.slug,
      params: [...proposal.brush.params],
      colon: [...proposal.brush.colon],
      parameters: { ...proposal.brush.parameters },
    } : null,
    decisions: decisions.map((decision) => ({ ...decision })),
    saveCount,
    finishMode,
    doneCount,
    completion: {
      busy: completionBusy,
      progress: completionProgress,
      code: completionCode,
      error: completionError,
      stayedInNoPaint: true,
    },
    audio: {
      ready: [...cueSamples.keys()],
      brushReady: [...brushCueSamples.keys()],
      activeBrush: activeBrushKind,
      decisionHeld,
      hovered: hoveredDecision,
      events: cueEvents.map((event) => ({ ...event })),
    },
    cursor: {
      ready: Boolean(cursorSheet),
      visible: Boolean(cursorPoint),
      animation: hoveredDecision ? "Over Button" : "Normal",
      frame: cursorFrame,
    },
    paintingButton: layout ? {
      x: 0,
      y: 0,
      w: testApi.screen.width,
      h: layout.bar.y,
      down: paintingPressed,
      over: hoveredDecision === "painting",
    } : null,
    lastDownload,
    ready: Boolean(proposal && testApi?.system?.nopaint?.buffer),
    controls: finishMode
      ? { back: controlBox(backButton), done: controlBox(doneButton) }
      : { no: controlBox(noButton), paint: controlBox(paintButton) },
    layout: layout ? {
      paintingViewport: { ...layout.stage },
      paintingResolution: { ...paintingResolution },
      screenResolution: {
        width: testApi.screen.width,
        height: testApi.screen.height,
      },
      controlBar: { ...layout.bar },
      modeline: { ...layout.status },
    } : null,
    paintingFingerprint: paintingFingerprint(testApi?.system?.painting),
    origin: archiveOrigin ? { ...archiveOrigin } : null,
  };
}

function publishTestState() {
  testChannel?.postMessage(testSnapshot());
}

function installTestHook(debug) {
  const explicitlyTesting = initialNavigationURL()?.searchParams.has("test");
  const windowDebug = typeof window !== "undefined" && window.acDEBUG;
  if (!debug && !explicitlyTesting && !windowDebug) return;
  testMode = true;

  if (typeof BroadcastChannel !== "undefined") {
    testChannel?.close();
    testChannel = new BroadcastChannel("ac-nopaint-test");
    testChannel.onmessage = ({ data }) => {
      if (data?.type !== "configure" || !data.seed || !testApi) return;
      const configuredSeed = /^\d+$/.test(String(data.seed))
        ? Number(data.seed) >>> 0
        : seedFrom(String(data.seed));
      if (configuredSeed === sessionSeed) return;
      sessionSeed = configuredSeed;
      random = seededRandom(sessionSeed);
      proposalNumber = 0;
      brushCueProposal = 0;
      clearProposal(testApi);
      chooseProposal(testApi);
      publishTestState();
    };
  }

  if (typeof window !== "undefined") {
    window.__acNoPaintTest = () => Object.freeze(testSnapshot());
  }
}

// 🥾 Boot
function boot({ colon, debug, hud, net, num, params, query = {}, screen, store, system, ui, ...api }) {
  // The runtime may rewrite the visible route before the piece boots. The
  // Navigation Timing entry retains the original tutorial/test URL.
  const navigationURL = initialNavigationURL();
  let visibleURL = null;
  try {
    if (typeof window !== "undefined") visibleURL = new URL(window.location.href);
  } catch {}
  const archiveId = params[0] === "archive" ? params[1] : null;
  const freshFromId = params[0] === "new" ? params[1] : null;
  const urlSeed = query.seed || navigationURL?.searchParams.get("seed") || null;
  freshStart = freshLaunchRequested(navigationURL, colon, params) ||
    freshLaunchRequested(visibleURL, colon, params) ||
    Boolean(freshFromId) ||
    (Object.hasOwn(query, "fresh") &&
      !["0", "false", "no", "off"].includes(String(query.fresh).toLowerCase()));
  const launchSeed = [...colon, ...params].find((value) =>
    !["fresh", "fresh=1", "new"].includes(String(value).trim().toLowerCase()));
  const requestedSeed = urlSeed || archiveId || freshFromId || launchSeed;
  const numericSeed = /^\d+$/.test(requestedSeed || "")
    ? Number(requestedSeed) >>> 0
    : null;
  sessionSeed = numericSeed || seedFrom(
    requestedSeed || `${num.timestamp()}-${num.randIntRange(0, 0x7fffffff)}`,
  );
  random = seededRandom(sessionSeed);
  proposal = null;
  proposalFrame = 0;
  proposalNumber = 0;
  cameraFeed = null;
  decisions = [];
  saveCount = 0;
  finishMode = false;
  doneCount = 0;
  completionBusy = false;
  completionProgress = 0;
  completionCode = null;
  completionError = null;
  paintingDragPaused = false;
  paintingPressed = false;
  hoveredDecision = null;
  cursorSheet = null;
  cursorPoint = null;
  cursorFrame = 0;
  cursorWagFrames = 0;
  cueEvents = [];
  brushCueProposal = 0;
  activeBrushSound = null;
  activeBrushKind = null;
  decisionHeld = false;
  heldKeyboardDecision = null;
  lastDownload = null;
  archiveOrigin = archiveId ? {
    type: "nopaint-archive",
    id: archiveId,
    record: `https://nopaint.art/${archiveId}`,
    image: `https://pix.nopaint.art/${archiveId}.png`,
    status: "queued",
  } : freshFromId ? {
    type: "nopaint-archive",
    id: freshFromId,
    record: `https://nopaint.art/${freshFromId}`,
    action: "rejected-as-start",
  } : null;
  // AC's painting contract: the initial canvas establishes this painting's
  // pixel resolution. Window changes after boot only alter presentation.
  paintingResolution = {
    width: system.painting.width,
    height: system.painting.height,
  };
  const needsStarterSubstrate = freshStart || (!archiveId && !paintHasContent(system.painting));
  if (needsStarterSubstrate) {
    seedNoiseSubstrate(system.painting, sessionSeed);
    api.flatten();
    api.page(screen);
  }
  store["painting:resolution-lock"] = true;
  store.persist("painting:resolution-lock", "local:db");
  testApi = { ...api, hud, net, screen, store, system };
  testMode = false;
  api.cursor?.("none");
  stateBeforePause = "proposing";

  noButton = new ui.TextButton("No");
  paintButton = new ui.TextButton("Paint");
  doneButton = new ui.TextButton("Done");
  backButton = new ui.TextButton("Back");
  positionButtons(screen);

  if (typeof net.preload === "function") {
    net.preload("/nopaint.art/images/cursor-sheet0.png")
      .then((image) => {
        cursorSheet = image.img || image;
        api.needsPaint();
      })
      .catch(() => {});
    const interactionLoads = new Map();
    for (const [name, filename] of Object.entries(LEGACY_CUES)) {
      let loading = interactionLoads.get(filename);
      if (!loading) {
        loading = net.preload(`/nopaint.art/media/${filename}`);
        interactionLoads.set(filename, loading);
      }
      loading
        .then((sample) => cueSamples.set(name, sample))
        .catch(() => {}); // playCue supplies an immediate native synth fallback.
    }
    for (const [kind, filename] of Object.entries(BRUSH_CUES)) {
      net.preload(`/nopaint.art/media/${filename}`)
        .then((sample) => {
          brushCueSamples.set(kind, sample);
          if (proposal?.kind === kind || (proposal?.kind === "dark-window" && kind.startsWith("dark-window:"))) {
            playBrushCue(testApi, proposal.kind);
          }
          publishTestState();
        })
        .catch(() => {});
    }
  }

  // A non-empty blank suppresses the runtime's default slug fallback without
  // drawing instructional chrome over the painting.
  hud.label(" ", [0, 0, 0, 0]);
  net.rewrite(archiveId ? `/nopaint~archive~${archiveId}` : `/nopaint:${sessionSeed}`);
  installTestHook(debug);
  chooseProposal(testApi);
  publishTestState();
  if (archiveId) loadArchivePainting(testApi, archiveId);
}

// 🧮 Sim
function sim({ needsPaint }) {
  if (cursorWagFrames > 0) {
    cursorWagFrames -= 1;
    if (cursorWagFrames === 0 && cursorFrame !== 0) {
      cursorFrame = 0;
      needsPaint();
    }
  }
  if (loopState === "proposing" && !decisionHeld) {
    proposalFrame += 1;
    if (proposalFrame >= PROPOSAL_MERRY_FRAMES) {
      merryProposal(testApi);
      return;
    }
    needsPaint();
    if (proposalFrame % 12 === 0) publishTestState();
  }
}

function animatedColor(color, phase) {
  const pulse = Math.round(Math.sin(phase) * 14);
  return [
    Math.max(0, Math.min(255, color[0] + pulse)),
    Math.max(0, Math.min(255, color[1] + pulse)),
    Math.max(0, Math.min(255, color[2] + pulse)),
    color[3],
  ];
}

function paintParallaxCheckers($, bar) {
  const size = Math.max(18, Math.round(Math.min($.screen.width, $.screen.height) / 18));
  const pointerX = cursorPoint?.x ?? $.screen.width / 2;
  const pointerY = cursorPoint?.y ?? bar.y / 2;
  const offsetX = Math.round((pointerX / $.screen.width - 0.5) * size * 0.7);
  const offsetY = Math.round((pointerY / Math.max(1, bar.y) - 0.5) * size * 0.7);
  $.ink(22, 22, 24).box(0, 0, $.screen.width, bar.y);
  for (let y = -size + offsetY; y < bar.y; y += size) {
    for (let x = -size + offsetX; x < $.screen.width; x += size) {
      const column = Math.floor((x - offsetX) / size);
      const row = Math.floor((y - offsetY) / size);
      if ((column + row) % 2 === 0) {
        $.ink(34, 34, 38).box(x, y, size, size);
      }
    }
  }
}

function renderProposal($) {
  const { ink, page, paste, system, video } = $;
  const buffer = system.nopaint.buffer;
  const phase = proposalFrame / 24;
  const wave = Math.sin(phase);
  const drift = Math.round(wave * proposal.drift);
  const color = animatedColor(proposal.color, phase);
  color[3] = Math.min(color[3], NOPAINT_MAX_RANDOM_ALPHA);

  page(buffer).wipe(255, 255, 255, 0);

  const contract = COMPATIBLE_BRUSHES.get(proposal.kind);
  if (proposalPixels) {
    buffer.pixels.set(proposalPixels);
  } else if (contract?.render && proposal.kind !== "line") {
    contract.render($, proposal, proposalFrame);
  } else

  if (proposal.kind === "rect") {
    ink(color).box(
      proposal.x + drift,
      proposal.y - drift,
      proposal.w,
      proposal.h,
    );
  } else if (proposal.kind === "oval") {
    const pulse = 1 + wave * 0.12;
    ink(color).oval(
      proposal.x + proposal.w / 2,
      proposal.y + proposal.h / 2,
      proposal.w * pulse,
      proposal.h * pulse,
      true,
    );
  } else if (proposal.kind === "line") {
    lineProposal.render($, proposal, proposalFrame);
  } else if (proposal.kind === "softy") {
    proposal.points.slice(0, 12).forEach((point, index) => {
      const breathe = 1 + Math.sin(phase + index * 0.7) * 0.18;
      ink(color[0], color[1], color[2], 22).oval(
        point.x + drift,
        point.y - drift,
        point.size * 2.8 * breathe,
        point.size * 2.8 * breathe,
        true,
      );
    });
  } else if (proposal.kind === "bubbles") {
    proposal.points.forEach((point, index) => {
      const rise = (proposalFrame * (0.08 + (index % 4) * 0.03)) % buffer.height;
      ink(color[0], color[1], color[2], 150).oval(
        point.x,
        (point.y - rise + buffer.height) % buffer.height,
        point.size,
        point.size,
        false,
        Math.max(1, proposal.thickness / 3),
      );
    });
  } else if (proposal.kind === "grid-worm") {
    const cell = Math.max(8, Math.floor(Math.min(buffer.width, buffer.height) / 14));
    const path = proposal.points.map((point, index) => ({
      x: Math.floor(point.x / cell) * cell + Math.sin(phase + index) * 2,
      y: Math.floor(point.y / cell) * cell + Math.cos(phase + index) * 2,
    }));
    for (let index = 1; index < path.length; index += 1) {
      ink(color).line(path[index - 1].x, path[index - 1].y, path[index].x, path[index].y, proposal.thickness);
    }
  } else if (proposal.kind === "walker") {
    let x = proposal.x;
    let y = proposal.y;
    proposal.points.forEach((point, index) => {
      const nextX = x + Math.cos(proposal.phase + index * 1.7) * point.size;
      const nextY = y + Math.sin(proposal.phase + index * 1.3) * point.size;
      ink(color).line(x + drift, y, nextX + drift, nextY, proposal.thickness);
      x = nextX;
      y = nextY;
    });
  } else if (proposal.kind === "banner") {
    const stripes = 9;
    for (let index = 0; index < stripes; index += 1) {
      const y = proposal.y + (proposal.h / stripes) * index;
      const sway = Math.sin(phase + index * 0.55) * proposal.drift;
      ink(color[0], color[1], color[2], 80 + index * 14).box(
        proposal.x + sway,
        y,
        proposal.w,
        Math.max(2, proposal.h / stripes - 1),
      );
    }
  } else if (proposal.kind === "wafer") {
    const cols = 7;
    const rows = 7;
    const cw = proposal.w / cols;
    const ch = proposal.h / rows;
    for (let row = 0; row < rows; row += 1) {
      for (let col = 0; col < cols; col += 1) {
        const lift = Math.sin(phase + row * 0.5 + col * 0.35) * 3;
        ink(color[0], color[1], color[2], (row + col) % 2 ? 70 : 150).box(
          proposal.x + col * cw,
          proposal.y + row * ch + lift,
          Math.max(1, cw - 1),
          Math.max(1, ch - 1),
        );
      }
    }
  } else if (proposal.kind === "wipe") {
    // A recovered "wipe" is a translucent wash in No Paint 3.0. Random
    // proposals must accumulate history; they may never replace it opaquely.
    ink(color[0], color[1], color[2], 96).box(
      0,
      0,
      system.painting.width,
      system.painting.height,
    );
  } else if (proposal.kind === "camera") {
    cameraFeed ||= video("camera", {
      width: system.painting.width,
      height: system.painting.height,
      facing: "environment",
    });
    const frame = cameraFeed();
    if (frame) {
      paste(frame);
    } else {
      ink(24, 24, 24, 230).box(0, 0, buffer.width, buffer.height);
      ink(255).write("waiting for camera", {
        x: Math.max(4, Math.floor(buffer.width / 2 - 54)),
        y: Math.floor(buffer.height / 2),
      });
    }
  }

  page($.screen);
}

// 🎨 Paint
function paint($) {
  if (!proposal || !$.system.nopaint.buffer) return false;
  renderProposal($);
  $.system.nopaint.needsPresent = true;

  const { bar, stage, status, scale } = interfaceLayout($.screen);
  const surface = { x: 0, y: 0, w: $.screen.width, h: bar.y };
  // Present the entire fixed-resolution painting above the controls. The
  // viewport may fit/letterbox responsively, but its pixels never reflow.
  $.wipe(18);
  paintParallaxCheckers($, bar);
  if (proposalPixels) {
    $.paste($.system.nopaint.buffer, stage.x, stage.y, scale);
  } else {
    $.paste($.system.painting, stage.x, stage.y, scale);
    $.paste($.system.nopaint.buffer, stage.x, stage.y, scale);
  }
  if (paintingPressed || hoveredDecision === "painting") {
    $.ink(255, 255, 255, paintingPressed ? 38 : 22).box(surface, "fill");
    $.ink(255, 255, 255, paintingPressed ? 235 : 145).box(surface, "outline");
  }
  const merryRemaining = Math.max(0, 1 - proposalFrame / PROPOSAL_MERRY_FRAMES);
  const merryBarHeight = Math.max(3, Math.round($.screen.height / 240));
  $.ink(10, 10, 12, 210).box(0, 0, surface.w, merryBarHeight);
  $.ink(92, 220, 128, 235).box(0, 0, Math.round(surface.w * merryRemaining), merryBarHeight);
  const definition = proposalDefinition(proposal.kind);
  $.ink(18).box(bar, "fill");
  $.ink(255, 180).write(
    completionBusy
      ? `Uploading ${Math.round(completionProgress * 100)}%`
      : completionCode
        ? `#${completionCode}`
        : completionError || definition?.label || proposal.kind,
    { x: 8, y: merryBarHeight + 6 },
  );

  positionButtons($.screen);
  if (finishMode) {
    paintDecisionButton($, backButton.btn, "Back", "back");
    paintDecisionButton($, doneButton.btn, "Done", "done");
  } else {
    paintDecisionButton($, noButton.btn, "No");
    paintDecisionButton($, paintButton.btn, "Paint", "paint");
  }
  paintOriginalCursor($);
  return loopState === "proposing";
}

function isAny(e, names) {
  return names.some((name) => e.is(name));
}

async function completePainting($) {
  if (completionBusy || completionCode) return;
  playCue($, "done");
  doneCount += 1;
  completionBusy = true;
  completionProgress = 0;
  completionError = null;
  $.needsPaint();
  publishTestState();

  try {
    const painting = {
      width: $.system.painting.width,
      height: $.system.painting.height,
      pixels: $.system.painting.pixels,
      ...($.store["painting:tags"] ? { tags: $.store["painting:tags"] } : {}),
    };
    const filename = `painting-${$.num.timestamp()}.png`;
    const data = testMode
      ? { code: "test" }
      : await $.upload(filename, painting, (progress) => {
          completionProgress = Math.max(0, Math.min(1, Number(progress) || 0));
          $.needsPaint();
          publishTestState();
        });
    if (!data?.code) throw new Error("Painting upload completed without a code");
    completionCode = data.code;
    completionProgress = 1;
    $.system.painting.code = data.code;
    $.store["painting:code"] = data.code;
    $.store.persist?.("painting:code", "local:db");
    const layout = interfaceLayout($.screen);
    const nextResolution = {
      w: $.screen.width,
      h: Math.max(1, layout.bar.y),
    };
    await $.system.nopaint.noBang($, nextResolution);
    paintingResolution = {
      width: nextResolution.w,
      height: nextResolution.h,
    };
    $.system.nopaint.buffer = $.painting(
      nextResolution.w,
      nextResolution.h,
      (page) => page.wipe(255, 255, 255, 0),
    );
    seedNoiseSubstrate($.system.painting, `${sessionSeed}:${doneCount}`);
    $.flatten();
  proposal = null;
  proposalFrame = 0;
  proposalPixels = null;
    decisions = [];
    finishMode = false;
    transition("proposing");
    chooseProposal($);
  } catch (error) {
    completionError = error?.message || "Upload failed";
  } finally {
    completionBusy = false;
    $.needsPaint();
    publishTestState();
  }
}

function xboxButtonPush(e) {
  const match = e.name?.match(/^gamepad:\d+:button:(\d+):push$/);
  return match ? Number(match[1]) : null;
}

export function nopaintXboxAction(button, completing = false) {
  if (completing) {
    if (button === 0 || button === 15) return "done";
    if ([1, 2, 3, 8, 14].includes(button)) return "back";
    return null;
  }
  if ([1, 2, 14].includes(button)) return "no";
  if (button === 0 || button === 15) return "paint";
  if (button === 3 || button === 8) return "finish";
  if (button === 9) return "pause";
  return null;
}

// 🎪 Act — every input surface reaches the same two decision functions.
function act($) {
  const { event: e } = $;
  const xboxButton = xboxButtonPush(e);
  const xboxAction = nopaintXboxAction(xboxButton, finishMode);
  let cursorDeltaX = 0;
  if (e.device === "mouse" && Number.isFinite(e.x) && Number.isFinite(e.y)) {
    cursorDeltaX = cursorPoint ? e.x - cursorPoint.x : 0;
    cursorPoint = { x: e.x, y: e.y };
  } else if (e.device === "touch") {
    cursorPoint = null;
  }
  const { stage, bar } = interfaceLayout($.screen);
  const surface = { x: 0, y: 0, w: $.screen.width, h: bar.y };
  const overSurface = (point) => point.x >= surface.x && point.x <= surface.x + surface.w &&
    point.y >= surface.y && point.y <= surface.y + surface.h;
  const leaveFinishMode = () => {
    playCue($, "back");
    finishMode = false;
    transition(stateBeforePause === "paused" ? "proposing" : stateBeforePause);
    resumeBrushCue($);
    $.needsPaint();
    publishTestState();
  };

  const leftControl = finishMode ? backButton.btn : noButton.btn;
  const rightControl = finishMode ? doneButton.btn : paintButton.btn;
  if ((e.device === "mouse" || e.is("move")) && !leftControl.down && !rightControl.down) {
    const target = leftControl.box.contains(e)
      ? finishMode ? "back" : "no"
      : rightControl.box.contains(e)
        ? finishMode ? "done" : "paint"
        : overSurface(e)
          ? "painting"
          : null;
    if (target !== hoveredDecision) {
      hoveredDecision = target;
      cursorFrame = 0;
      if (target === "painting") {
        cueEvents.push({
          name: "painting-hover",
          path: cueSamples.has("rollover") ? "legacy" : "synth",
        });
        playCue($, "rollover");
      } else if (target) {
        playCue($, "rollover");
      }
    }
    if (target && Math.abs(cursorDeltaX) > 1) {
      // Construct events 78/79 explicitly select directional hand poses;
      // WagCursorTimer later restores frame zero. These are logical states,
      // not a seven-frame playback loop.
      cursorFrame = cursorDeltaX > 0 ? 2 : 1;
      cursorWagFrames = 6;
      $.needsPaint();
    }
  }

  if (finishMode) {
    if (completionBusy) return;
    backButton.btn.act(e, {
      down: () => playCue($, "button-down"),
      push: leaveFinishMode,
    });
    doneButton.btn.act(e, {
      down: () => playCue($, "done-down"),
      push: () => completePainting($),
    });
    // Xbox parity: A/Right confirms Done; B/X/Left goes Back; Y/View toggles
    // the same completion surface as tapping the painting.
    if (xboxAction === "done") {
      completePainting($);
      return;
    }
    if (xboxAction === "back") {
      leaveFinishMode();
      return;
    }
    if (
      e.is("lift:1") &&
      overSurface(e)
    ) leaveFinishMode();
    return;
  }

  const overPainting = overSurface(e);
  if (e.is("touch:1") && overPainting) {
    paintingPressed = true;
    // The painting is the pause surface. Construct uses the same tactile
    // press vocabulary heard on Space here, not a generic UI-button click.
    playCue($, "pause-down");
    setDecisionHeld(true);
    $.needsPaint();
    publishTestState();
  }

  if (
    loopState === "proposing" &&
    e.is("draw:1") &&
    e.drag &&
    overSurface(e.drag)
  ) {
    paintingDragPaused = true;
    paintingPressed = false;
    setDecisionHeld(false);
    playCue($, "pause-down");
    togglePaused($);
    return;
  }

  noButton.btn.act(e, {
    down: () => {
      playCue($, "no-down");
      setDecisionHeld(true);
    },
    cancel: () => {
      playCue($, "back");
      setDecisionHeld(false);
    },
    push: () => discardProposal($),
  });
  paintButton.btn.act(e, {
    down: () => {
      playCue($, "paint-down");
      setDecisionHeld(true);
    },
    cancel: () => {
      playCue($, "back");
      setDecisionHeld(false);
    },
    push: () => commitProposal($),
  });
  if (
    e.is("lift:1") &&
    overPainting
  ) {
    if (paintingDragPaused) {
      paintingDragPaused = false;
      paintingPressed = false;
      return;
    }
    if (!paintingPressed) return;
    paintingPressed = false;
    setDecisionHeld(false);
    playCue($, "pause-in");
    stopBrushCue();
    finishMode = true;
    stateBeforePause = loopState;
    transition("paused");
    $.needsPaint();
    publishTestState();
    return;
  }
  if (e.is("lift:1") && paintingPressed) {
    paintingPressed = false;
    setDecisionHeld(false);
    playCue($, "back");
    $.needsPaint();
    publishTestState();
  }
  if (isAny(e, [
    "voice:no",
    "robot:no",
    "nopaint:no",
  ]) || xboxAction === "no") discardProposal($);

  if (isAny(e, [
    "voice:paint",
    "robot:paint",
    "nopaint:paint",
  ]) || xboxAction === "paint") commitProposal($);

  const keyDecision = isAny(e, [
    "keyboard:down:n", "keyboard:down:arrowleft", "keyboard:down:escape",
  ]) ? "no" : isAny(e, [
    "keyboard:down:enter", "keyboard:down:p", "keyboard:down:arrowright",
  ]) ? "paint" : null;
  if (keyDecision) {
    if (heldKeyboardDecision === keyDecision) return;
    if (heldKeyboardDecision) playCue($, "back");
    heldKeyboardDecision = keyDecision;
    playCue($, keyDecision === "no" ? "no-down" : "paint-down");
    setDecisionHeld(true);
    publishTestState();
    return;
  }

  const releasedDecision = isAny(e, [
    "keyboard:up:n", "keyboard:up:arrowleft", "keyboard:up:escape",
  ]) ? "no" : isAny(e, [
    "keyboard:up:enter", "keyboard:up:p", "keyboard:up:arrowright",
  ]) ? "paint" : null;
  if (releasedDecision && heldKeyboardDecision === releasedDecision) {
    heldKeyboardDecision = null;
    if (releasedDecision === "no") discardProposal($);
    else commitProposal($);
    return;
  }

  // Y and View are the pointer-free equivalent of tapping the painting.
  if (xboxAction === "finish") {
    playCue($, "pause-down");
    playCue($, "pause-in");
    stopBrushCue();
    finishMode = true;
    stateBeforePause = loopState;
    transition("paused");
    $.needsPaint();
    publishTestState();
    return;
  }

  if (e.is("keyboard:down:space") || xboxAction === "pause") {
    playCue($, "pause-down");
    togglePaused($);
  }
}

// The conductor makes decisions explicitly; generic pointer-lift baking must
// never commit a proposal behind the participant's back.
function bake() {}

function leave() {
  stopBrushCue();
  testApi?.cursor?.("native");
  if (typeof window !== "undefined") delete window.__acNoPaintTest;
  testChannel?.close();
  testChannel = null;
  testApi = null;
}

function meta() {
  return {
    title: "No Paint",
    desc: "Collaborate with a proposing machine: press No to discard or Paint to keep.",
    controls: "No: Left/N/Escape/B/X · Paint: Right/Enter/P/A · finish: Y/View · pause: Space/Menu or drag painting",
    params: "optional deterministic session seed; fresh starts with a blank painting",
    example: "nopaint fresh",
  };
}

// Pointer events still pass through the inherited nopaint gesture handler
// before this conductor sees its buttons. Bake-on-leave suppresses that
// handler's automatic lift bake; our no-op bake below also means leaving saves
// the accepted painting without silently accepting the live proposal.
export const system = "nopaint:bake-on-leave";

export { act, bake, boot, freshLaunchRequested, leave, meta, paint, sim };
