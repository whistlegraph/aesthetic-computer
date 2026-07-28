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

let loopState = "choosing";
let stateBeforePause = "proposing";
let proposal = null;
let proposalFrame = 0;
let proposalNumber = 0;
let sessionSeed = 0;
let random = seededRandom(1);
let cameraFeed = null;
let decisions = [];
let saveCount = 0;
let lastDownload = null;
let testApi = null;
let testChannel = null;
let archiveOrigin = null;
let paintingResolution = null;
let finishMode = false;
let doneCount = 0;
const cueSamples = new Map();
let cueEvents = [];

const LEGACY_CUES = Object.freeze({
  "no-down": "generic - no button pressed (metal brush).webm",
  no: "generic - no button released (middle).webm",
  "paint-down": "generic - paint button pressed (psst).webm",
  paint: "generic - paint button released (cha).webm",
  rollover: "generic - button rollover.webm",
  "button-down": "generic - button press.webm",
  back: "generic - pause release.webm",
  "done-down": "generic - save button pressed.webm",
  done: "generic - save button released.webm",
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
  walker: "common - jitter.webm",
  banner: "banner - theme.webm",
  wafer: "wafer - nibble appear.webm",
  wipe: "wipe - individual wipe.webm",
  camera: "camera - fx.webm",
});

const brushCueSamples = new Map();
let brushCueProposal = 0;

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
  const sample = cueSamples.get(name);
  if (sample && api.sound?.play) {
    cueEvents.push({ name, path: "legacy" });
    return api.sound.play(sample, { volume: 0.72 });
  }
  cueEvents.push({ name, path: "synth" });
  return fallback();
}

function playBrushCue(api, kind) {
  if (brushCueProposal === proposalNumber) return;
  const sample = brushCueSamples.get(kind);
  if (!sample || !api.sound?.play) return;
  brushCueProposal = proposalNumber;
  cueEvents.push({ name: `brush:${kind}`, path: "legacy" });
  api.sound.play(sample, { volume: 0.48 });
  publishTestState();
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
  const barHeight = Math.max(96, Math.floor(screen.height * 0.18));
  const statusHeight = Math.max(22, Math.floor(barHeight * 0.22));
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
    statusHeight,
    scale,
  };
}

function positionButtons(screen) {
  // The recovered instrument keeps the decision pair together along the
  // bottom edge: No on the left, Paint larger on the right. They are the
  // architecture of the surface, not ordinary toolbar buttons.
  const { bar, statusHeight } = interfaceLayout(screen);
  const gap = Math.max(4, Math.floor(screen.width * 0.006));
  const margin = Math.max(6, Math.floor(screen.width * 0.008));
  const available = screen.width - margin * 2 - gap;
  const noWidth = Math.floor(available * 0.38);
  const paintWidth = available - noWidth;
  const buttonY = bar.y + statusHeight;
  const decisionHeight = bar.h - statusHeight - margin;
  const no = noButton.btn || noButton;
  const paint = paintButton.btn || paintButton;
  no.box ||= {};
  paint.box ||= {};
  Object.assign(no.box, {
    x: margin, y: buttonY,
    w: noWidth, h: decisionHeight,
  });
  Object.assign(paint.box, {
    x: margin + noWidth + gap, y: buttonY,
    w: paintWidth, h: decisionHeight,
  });
  const done = doneButton?.btn || doneButton;
  const back = backButton?.btn || backButton;
  if (done) {
    done.box ||= {};
    Object.assign(done.box, {
      x: margin + noWidth + gap, y: buttonY,
      w: paintWidth, h: decisionHeight,
    });
  }
  if (back) {
    back.box ||= {};
    Object.assign(back.box, {
      x: margin, y: buttonY,
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

function paintDecisionButton($, button, label, light = false) {
  const active = button.down || button.over;
  $.ink(light ? [250, 250, 250, active ? 235 : 205] : [205, 38, 48, active ? 255 : 235])
    .box(button.box, "fill")
    .ink(light ? [20, 20, 20] : [255, 255, 255])
    .box(button.box, "outline");
  paintMarkerLabel($, button, label, light ? [20, 20, 20] : [255, 245, 235]);
}

function clearProposal({ flatten, needsPaint, page, screen, system }) {
  if (!system.nopaint.buffer) return;
  page(system.nopaint.buffer).wipe(255, 255, 255, 0);
  flatten();
  page(screen);
  system.nopaint.needsPresent = true;
  needsPaint();
}

function chooseProposal(api) {
  transition("choosing");
  const resolution = paintingResolution || {
    width: api.system.painting.width,
    height: api.system.painting.height,
  };
  proposal = makeProposal(
    random,
    resolution.width,
    resolution.height,
  );
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

  api.page(api.system.painting).paste(api.system.nopaint.buffer);
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
  } else if (loopState === "proposing") {
    playCue(api, "pause-in");
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
  const controlBox = (control) => {
    const box = control?.box || control?.btn?.box;
    return box ? { x: box.x, y: box.y, w: box.w, h: box.h } : null;
  };
  const layout = testApi?.screen ? interfaceLayout(testApi.screen) : null;
  return {
    version: NOPAINT_VERSION,
    state: loopState,
    seed: sessionSeed,
    proposalNumber,
    proposalFrame,
    operation: proposal?.kind || null,
    decisions: decisions.map((decision) => ({ ...decision })),
    saveCount,
    finishMode,
    doneCount,
    audio: {
      ready: [...cueSamples.keys()],
      brushReady: [...brushCueSamples.keys()],
      events: cueEvents.map((event) => ({ ...event })),
    },
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
function boot({ colon, debug, hud, net, num, params, screen, store, system, ui, ...api }) {
  // The runtime may rewrite the visible route before the piece boots. The
  // Navigation Timing entry retains the original tutorial/test URL.
  const urlSeed = initialNavigationURL()?.searchParams.get("seed") || null;
  const archiveId = params[0] === "archive" ? params[1] : null;
  const freshFromId = params[0] === "new" ? params[1] : null;
  const requestedSeed = urlSeed || archiveId || freshFromId || colon[0] || params[0];
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
  cueEvents = [];
  brushCueProposal = 0;
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
  store["painting:resolution-lock"] = true;
  store.persist("painting:resolution-lock", "local:db");
  testApi = { ...api, hud, net, screen, store, system };
  stateBeforePause = "proposing";

  noButton = new ui.TextButton("No");
  paintButton = new ui.TextButton("Paint");
  doneButton = new ui.TextButton("Done");
  backButton = new ui.TextButton("Back");
  positionButtons(screen);

  if (typeof net.preload === "function") {
    for (const [name, filename] of Object.entries(LEGACY_CUES)) {
      net.preload(`/nopaint.art/media/${filename}`)
        .then((sample) => cueSamples.set(name, sample))
        .catch(() => {}); // playCue supplies an immediate native synth fallback.
    }
    for (const [kind, filename] of Object.entries(BRUSH_CUES)) {
      net.preload(`/nopaint.art/media/${filename}`)
        .then((sample) => {
          brushCueSamples.set(kind, sample);
          if (proposal?.kind === kind) playBrushCue(testApi, kind);
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
  if (loopState === "proposing") {
    proposalFrame += 1;
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

function renderProposal($) {
  const { ink, page, paste, system, video } = $;
  const buffer = system.nopaint.buffer;
  const phase = proposalFrame / 24;
  const wave = Math.sin(phase);
  const drift = Math.round(wave * proposal.drift);
  const color = animatedColor(proposal.color, phase);
  color[3] = Math.min(color[3], NOPAINT_MAX_RANDOM_ALPHA);

  page(buffer).wipe(255, 255, 255, 0);

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
    ink(color).line(
      proposal.x - drift,
      proposal.y + drift,
      proposal.x + proposal.w + drift,
      proposal.y + proposal.h - drift,
      proposal.thickness,
    );
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

  const { bar, stage, scale } = interfaceLayout($.screen);
  // Present the entire fixed-resolution painting above the controls. The
  // viewport may fit/letterbox responsively, but its pixels never reflow.
  $.wipe(18);
  $.paste($.system.painting, stage.x, stage.y, scale);
  $.paste($.system.nopaint.buffer, stage.x, stage.y, scale);
  const paused = loopState === "paused" ? " · paused" : "";
  const definition = proposalDefinition(proposal.kind);
  $.ink(18).box(bar, "fill");
  $.ink(255, 180).write(
    `No Paint ${NOPAINT_VERSION} · ${definition?.label || proposal.kind} ${proposalNumber}${paused} · seed ${sessionSeed}`,
    { x: 8, y: bar.y + 5 },
  );

  positionButtons($.screen);
  if (finishMode) {
    paintDecisionButton($, backButton.btn, "Back");
    paintDecisionButton($, doneButton.btn, "Done", true);
  } else {
    paintDecisionButton($, noButton.btn, "No");
    paintDecisionButton($, paintButton.btn, "Paint", true);
  }
  return loopState === "proposing";
}

function isAny(e, names) {
  return names.some((name) => e.is(name));
}

// 🎪 Act — every input surface reaches the same two decision functions.
function act($) {
  const { event: e } = $;
  const leaveFinishMode = () => {
    playCue($, "back");
    finishMode = false;
    transition(stateBeforePause === "paused" ? "proposing" : stateBeforePause);
    $.needsPaint();
    publishTestState();
  };

  if (finishMode) {
    backButton.btn.act(e, {
      down: () => playCue($, "button-down"),
      push: leaveFinishMode,
    });
    doneButton.btn.act(e, {
      down: () => playCue($, "done-down"),
      push: () => {
        playCue($, "done");
        doneCount += 1;
        publishTestState();
        if (!initialNavigationURL()?.searchParams.has("test")) $.jump("done");
      },
    });
    const stage = interfaceLayout($.screen).stage;
    if (
      e.is("lift:1") &&
      e.x >= stage.x && e.x <= stage.x + stage.w &&
      e.y >= stage.y && e.y <= stage.y + stage.h
    ) leaveFinishMode();
    return;
  }

  noButton.btn.act(e, {
    down: () => playCue($, "no-down"),
    rollover: () => playCue($, "rollover"),
    cancel: () => playCue($, "back"),
    push: () => discardProposal($),
  });
  paintButton.btn.act(e, {
    down: () => playCue($, "paint-down"),
    rollover: () => playCue($, "rollover"),
    cancel: () => playCue($, "back"),
    push: () => commitProposal($),
  });
  const stage = interfaceLayout($.screen).stage;
  if (
    e.is("lift:1") &&
    e.x >= stage.x && e.x <= stage.x + stage.w &&
    e.y >= stage.y && e.y <= stage.y + stage.h
  ) {
    finishMode = true;
    stateBeforePause = loopState;
    transition("paused");
    $.needsPaint();
    publishTestState();
    return;
  }
  if (isAny(e, [
    "keyboard:down:n",
    "keyboard:down:escape",
    "voice:no",
    "robot:no",
    "nopaint:no",
  ])) discardProposal($);

  if (isAny(e, [
    "keyboard:down:enter",
    "keyboard:down:p",
    "voice:paint",
    "robot:paint",
    "nopaint:paint",
  ])) commitProposal($);

  if (e.is("keyboard:down:space")) togglePaused($);
}

// The conductor makes decisions explicitly; generic pointer-lift baking must
// never commit a proposal behind the participant's back.
function bake() {}

function leave() {
  if (typeof window !== "undefined") delete window.__acNoPaintTest;
  testChannel?.close();
  testChannel = null;
  testApi = null;
}

function meta() {
  return {
    title: "No Paint",
    desc: "Collaborate with a proposing machine: press No to discard or Paint to keep.",
    controls: "No: N/Escape · Paint: Enter/P · pause: Space · save/share: S",
    params: "optional deterministic session seed",
    example: "nopaint award-entry",
  };
}

// Pointer events still pass through the inherited nopaint gesture handler
// before this conductor sees its buttons. Bake-on-leave suppresses that
// handler's automatic lift bake; our no-op bake below also means leaving saves
// the accepted painting without silently accepting the live proposal.
export const system = "nopaint:bake-on-leave";

export { act, bake, boot, leave, meta, paint, sim };
