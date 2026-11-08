// 🔁 Loop

// These numbers define the budgeted frame-time (max) for CPU and Rendering.
// The updates will repeat multiple times per frame, but rendering will only
// ever happen once per display refresh.

const updateFps = /*120*/120; // This is constant and should be used for interpolation.
let renderFps = 165; // This is a maximum and will vary across environments.
const updateRate = 1000 / updateFps;
let renderRate = 1000 / renderFps;
let updateTime = 0;
let renderTime = 0;
let lastNow;
let input;
let updateAndRender;
let paused = false;

// 💤 Pace (2026-11-08): a window at rest — in the background, nothing
// painting — ticks on a timer every `paceMs` instead of on every display
// refresh, so the browser stops producing frames for it at all. Each tick
// still runs every update step the elapsed time is owed (about twelve at
// 100 ms), so timers and sound keep their pace. A hidden page always goes
// back to animation frames, which the browser suspends.
let paceMs = 0;
let paceTimer = 0;
let rafPending = false;

function rafTick(now) {
  rafPending = false;
  loop(now);
}

function paceTick() {
  paceTimer = 0;
  loop(performance.now());
}

function schedule() {
  if (paceMs > 0 && !document.hidden) {
    if (!paceTimer) paceTimer = setTimeout(paceTick, paceMs);
  } else if (!rafPending) {
    rafPending = true;
    window.requestAnimationFrame(rafTick);
  }
}

// 0 (or nothing) for every display frame; a number of ms to rest between ticks.
function pace(ms = 0) {
  ms = ms > 0 ? ms : 0;
  if (ms === paceMs) return;
  paceMs = ms;
  if (!paceMs && paceTimer) {
    // Waking: drop the pending timer and catch the next display frame.
    clearTimeout(paceTimer);
    paceTimer = 0;
    schedule();
  }
}

// ⏱️ Background Timer Worker
// When the page is hidden and a piece opts into `background: true`, we use a
// Web Worker setInterval to keep pumping sim-only frames. Worker timers are
// NOT throttled by the browser, unlike rAF (paused) or main-thread timers (1/sec).
let timerWorker = null;
let backgroundMode = false; // true when hidden AND piece wants background ticks
let backgroundEnabled = false; // set by bios when piece exports `background: true`

function ensureTimerWorker() {
  if (timerWorker) return timerWorker;
  try {
    let workerUrl;
    try {
      workerUrl = new URL("./timer-worker.mjs", import.meta.url);
    } catch {
      // In pack mode (blob: URLs), import.meta.url can't resolve relative paths.
      return null;
    }
    timerWorker = new Worker(workerUrl, { type: "module" });
    timerWorker.onmessage = onTimerWorkerTick;
  } catch (e) {
    console.warn("⏱️ Timer worker failed to start:", e);
  }
  return timerWorker;
}

function onTimerWorkerTick(e) {
  if (e.data.type !== "tick" || paused || !backgroundMode) return;
  // Drive a sim-only frame (no render).
  const now = performance.now();
  const rawDelta = now - lastNow;
  // Clamp delta: the main thread only wakes ~1/sec when hidden.
  // Cap to 50ms so we produce at most ~6 sim ticks per wake-up,
  // avoiding massive bursts that confuse piece timing heuristics.
  const delta = Math.min(rawDelta, 50);
  updateTime += delta;
  lastNow = now;

  let updateTimes = 0;
  while (updateTime >= updateRate) {
    updateTimes += 1;
    updateTime -= updateRate;
  }

  if (updateTimes > 0) {
    // needsRender = false → sim only, no paint
    updateAndRender(false, updateTimes, now);
  }
}

function startBackgroundTicks() {
  if (backgroundMode) return;
  backgroundMode = true;
  // Reset accumulators so the first background tick doesn't catch up
  updateTime = 0;
  lastNow = performance.now();
  const w = ensureTimerWorker();
  if (w) w.postMessage({ type: "start", rate: 8 }); // ~120fps
}

function stopBackgroundTicks() {
  if (!backgroundMode) return;
  backgroundMode = false;
  if (timerWorker) timerWorker.postMessage({ type: "stop" });
}

// Input runs once per loop.
// Update runs multiple times.
// Render runs once if enough time has passed.
let xrDriven = false; // An XR session calls the loop itself, frame by frame.

function loop(now, XR = false) {
  if (XR) xrDriven = true;
  if (paused) {
    // A paused loop sleeps: no frame is asked for until `resume`, so a page
    // held still (the parked mime feed under every prompt) costs the browser
    // nothing at all (2026-11-08: it used to ask for every display frame).
    lastNow = now;
    return;
  }

  input(now);

  const delta = now - lastNow;

  updateTime += delta;
  renderTime += delta;
  lastNow = now;

  let updateTimes = 0;

  while (updateTime >= updateRate) {
    updateTimes += 1;
    updateTime -= updateRate;
  }

  let needsRender = false;

  if (renderTime >= renderRate) {
    needsRender = true;
    renderTime -= renderRate;
  }

  updateAndRender(needsRender, updateTimes, now);

  // We don't use requestAnimationframe when
  // running the main loop in an VR/AR/XR environment.
  if (!XR) schedule();
}

// Handle visibility changes:
// - When hidden + backgroundEnabled → start timer worker for sim-only frames
// - When visible → stop timer worker, reset accumulators to prevent catch-up
document.addEventListener("visibilitychange", function () {
  if (document.hidden) {
    if (backgroundEnabled) {
      startBackgroundTicks();
    }
  } else {
    stopBackgroundTicks();
    updateTime = 0;
    renderTime = 0; // Also reset render time to prevent catch-up rendering
    lastNow = performance.now();
  }
});

// Allow bios to enable/disable background mode for the current piece.
function setBackgroundEnabled(enabled) {
  backgroundEnabled = !!enabled;
  // If we're already hidden and background just got enabled, start ticking.
  if (backgroundEnabled && document.hidden) {
    startBackgroundTicks();
  } else if (!backgroundEnabled) {
    stopBackgroundTicks();
  }
}

// Start the loop.
function start(inputFun, updateAndRenderFun) {
  input = inputFun;
  updateAndRender = updateAndRenderFun;
  lastNow = performance.now();
  paused = false;
  schedule();
}

function pause() {
  paused = true;
}

function resume() {
  const wasPaused = paused;
  paused = false;
  lastNow = performance.now();
  updateTime = 0;
  if (wasPaused && input && !xrDriven) schedule(); // Wake the sleeping loop (once started).
}

// Update the frame rate.
function frameRate(n = 165) {
  if (renderFps === n) return; // Skip if framerate hasn't changed
  renderFps = n;
  if (renderFps !== 165) console.log("🎞️ FPS:", renderFps);
  renderRate = 1000 / renderFps;
  renderTime = 0;
}

export const mainLoop = loop;
export { start, frameRate, pause, resume, setBackgroundEnabled, pace };
