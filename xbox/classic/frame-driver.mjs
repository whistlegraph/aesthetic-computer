// Browser game clock: render once per display refresh while combat advances on
// a deterministic fixed-rate timeline suitable for replay and networking.

export function createFrameDriver({
  simulate,
  paint,
  sample = () => {},
  simulationFps = 60,
  maxCatchUpTicks = 4,
  now = () => performance.now(),
  requestFrame = (callback) => requestAnimationFrame(callback),
  cancelFrame = (handle) => cancelAnimationFrame(handle),
  setTimer = (callback, delay) => setTimeout(callback, delay),
  clearTimer = (handle) => clearTimeout(handle),
  fallbackGraceMs = 2,
  timeScale = 1,
} = {}) {
  if (typeof simulate !== "function")
    throw new TypeError("simulate callback required");
  if (typeof paint !== "function") throw new TypeError("paint callback required");
  const interval = 1000 / simulationFps;
  const normalizeTimeScale = (value) => {
    const number = Number(value);
    if (!Number.isFinite(number)) return 1;
    if (number === 0) return 0;
    return Math.max(.05, Math.min(4, number));
  };
  let currentTimeScale = normalizeTimeScale(timeScale);
  // Offline capture can wait on fonts, Puppeteer, or a reviewer before its
  // first step. Anchor its authored clock when the driver is created beside
  // piece.boot(), otherwise that wait silently skips the beginning of a round.
  const offlineOrigin = now();
  const wallInterval = () => currentTimeScale > 0
    ? interval / currentTimeScale : Infinity;
  const earlyTolerance = .35;
  let running = false;
  let visible = true;
  let nextSimulationAt = 0;
  let simulationTime = 0;
  let timerHandle = null;
  let rafHandle = null;
  let offlineStarted = false;
  let offlinePresentationTime = 0;
  let offlineAccumulator = 0;
  const stats = {
    simulationFps,
    timeScale: currentTimeScale,
    renderFrames: 0,
    simulationTicks: 0,
    inputSamples: 0,
    droppedSimulationTicks: 0,
    startedAt: 0,
    lastRenderAt: 0,
    lastSimulationAt: 0,
    lastRenderCostMs: 0,
    lastSimulationCostMs: 0,
    // Averaged frame timing, named to match what App.cpp already puts on the
    // native runtime object (`frame_ms`, `render_cpu_ms`, `refresh_hz`), so a
    // browser read-out and an AC_NATIVE_PROFILE line can be compared without
    // translating. Zero until the first window closes: a number nobody has
    // measured yet is absent, never reported as 0.00.
    frameMs: 0,
    renderCpuMs: 0,
    refreshHz: 0,
  };

  // @jeffrey plays in a browser on an Xbox, where there are no devtools, so the
  // frame rate the console is actually drawing had no way out of the page. The
  // window is the same 120 frames App.cpp averages over. Both inputs are
  // measurements this driver already takes -- the rAF timestamp it is handed,
  // and the paint cost it already clocks -- so a profiled frame costs two adds
  // and a compare and never asks for another clock reading.
  const profileWindow = 120;
  let profileFrames = 0;
  let profileSpanMs = 0;
  let profileRenderMs = 0;
  let profileAt = 0;

  const profileFrame = (current) => {
    if (profileAt) {
      profileSpanMs += current - profileAt;
      profileRenderMs += stats.lastRenderCostMs;
      profileFrames++;
    }
    profileAt = current;
    if (profileFrames < profileWindow) return;
    stats.frameMs = profileSpanMs / profileFrames;
    stats.renderCpuMs = profileRenderMs / profileFrames;
    stats.refreshHz = stats.frameMs > 0 ? 1000 / stats.frameMs : 0;
    profileFrames = 0;
    profileSpanMs = 0;
    profileRenderMs = 0;
  };

  // A hidden tab stops receiving frames, and the one span that bridges the gap
  // is minutes long. Dropping the anchor throws that span away instead of
  // letting a backgrounded tab report a two-second frame.
  const forgetProfileFrame = () => { profileAt = 0; };

  const sampleInput = () => {
    sample();
    stats.inputSamples++;
  };

  const runSimulation = (sampleFirst, presentationTime = now()) => {
    if (sampleFirst) sampleInput();
    const started = now();
    simulate(simulationTime, presentationTime);
    stats.simulationTicks++;
    stats.lastSimulationAt = simulationTime;
    stats.lastSimulationCostMs = Math.max(0, now() - started);
  };

  const clearSimulationTimer = () => {
    if (timerHandle === null) return;
    clearTimer(timerHandle);
    timerHandle = null;
  };

  const armSimulationTimer = () => {
    clearSimulationTimer();
    if (!running || !visible || currentTimeScale === 0) return;
    // A timely animation frame gets first refusal. The timer preserves the
    // fixed simulation rate on displays running below 60 Hz.
    const delay = Math.max(0, nextSimulationAt + fallbackGraceMs - now());
    timerHandle = setTimer(() => {
      timerHandle = null;
      pumpSimulation(now(), true);
    }, delay);
  };

  const pumpSimulation = (current, sampleEachTick,
    maxTicks = maxCatchUpTicks) => {
    if (!running || currentTimeScale === 0) return;
    const dueEvery = wallInterval();
    let ticks = 0;
    while (current + earlyTolerance >= nextSimulationAt &&
      ticks < maxTicks) {
      simulationTime += interval;
      runSimulation(sampleEachTick, current);
      nextSimulationAt += dueEvery;
      ticks++;
    }
    if (current + earlyTolerance >= nextSimulationAt) {
      const dropped = Math.floor((current - nextSimulationAt) / dueEvery) + 1;
      stats.droppedSimulationTicks += Math.max(1, dropped);
      nextSimulationAt = current + dueEvery;
    }
    armSimulationTimer();
  };

  // A hidden or fully occluded window loses rAF and its tight fallback timer,
  // which used to park the whole timeline. A backgrounded versus host then
  // stopped publishing its room, and a waiting challenger's claim clock never
  // ran -- two overlapping windows on the same room could both go dead. While
  // hidden, the owed ticks are settled in one burst per second instead: the
  // cadence a throttled background tab actually grants a timer.
  const maintenanceIntervalMs = 1000;
  const maintenanceCatchUpTicks = simulationFps * 4;
  let maintenanceHandle = null;
  const clearMaintenanceTimer = () => {
    if (maintenanceHandle === null) return;
    clearTimer(maintenanceHandle);
    maintenanceHandle = null;
  };
  const armMaintenanceTimer = () => {
    clearMaintenanceTimer();
    if (!running || visible || currentTimeScale === 0) return;
    maintenanceHandle = setTimer(() => {
      maintenanceHandle = null;
      pumpSimulation(now(), true, maintenanceCatchUpTicks);
      armMaintenanceTimer();
    }, maintenanceIntervalMs);
  };

  const rafTick = (timestamp) => {
    if (!running) return;
    if (visible) {
      const current = Number.isFinite(timestamp) ? timestamp : now();
      sampleInput();
      pumpSimulation(current, false);
      const started = now();
      const dueEvery = wallInterval();
      const alpha = currentTimeScale === 0 ? 0 : Math.max(0, Math.min(1,
        (current - nextSimulationAt + dueEvery) / dueEvery));
      paint(current, alpha,
        simulationTime - interval * (1 - alpha));
      stats.renderFrames++;
      stats.lastRenderAt = current;
      stats.lastRenderCostMs = Math.max(0, now() - started);
      profileFrame(current);
    }
    rafHandle = requestFrame(rafTick);
  };

  return {
    stats,
    // Deterministic capture lane. The caller advances exactly one simulation
    // tick and one paint per invocation; no rAF/timer or wall-clock catch-up
    // participates. This is deliberately unavailable while the live driver
    // is running.
    stepOffline() {
      if (running) throw new Error("cannot step offline while frame driver is running");
      if (!offlineStarted) {
        offlineStarted = true;
        stats.startedAt = offlineOrigin;
        simulationTime = stats.startedAt - interval;
        offlinePresentationTime = stats.startedAt - interval;
        offlineAccumulator = 1;
        sampleInput();
      }
      offlinePresentationTime += interval;
      offlineAccumulator += stats.renderFrames ? currentTimeScale : 0;
      let simulationTicks = 0;
      while (offlineAccumulator >= 1) {
        simulationTime += interval;
        runSimulation(false, offlinePresentationTime);
        offlineAccumulator -= 1;
        simulationTicks++;
      }
      const started = now();
      // At fractional speed, walk monotonically from the state entering the
      // latest authored tick to the state leaving it. Starting the first
      // frame at 1 and the next at .5 made the opening visibly step backward.
      const alpha = currentTimeScale >= 1 ? 1
        : Math.max(0, Math.min(1, offlineAccumulator + currentTimeScale));
      paint(offlinePresentationTime, alpha,
        simulationTime - interval * (1 - alpha));
      stats.renderFrames++;
      stats.lastRenderAt = simulationTime;
      stats.lastRenderCostMs = Math.max(0, now() - started);
      return { frame: stats.renderFrames, simulationTime,
        presentationTime: offlinePresentationTime, simulationTicks,
        timeScale: currentTimeScale };
    },
    start() {
      if (running) return;
      running = true;
      visible = true;
      forgetProfileFrame();
      stats.startedAt = now();
      simulationTime = stats.startedAt;
      nextSimulationAt = stats.startedAt;
      sampleInput();
      runSimulation(false);
      nextSimulationAt += wallInterval();
      armSimulationTimer();
      rafHandle = requestFrame(rafTick);
    },
    stop() {
      if (!running) return;
      running = false;
      forgetProfileFrame();
      clearSimulationTimer();
      clearMaintenanceTimer();
      if (rafHandle !== null) cancelFrame(rafHandle);
      rafHandle = null;
    },
    setVisible(value) {
      visible = Boolean(value);
      forgetProfileFrame();
      clearSimulationTimer();
      clearMaintenanceTimer();
      if (!running) return;
      if (!visible) { armMaintenanceTimer(); return; }
      nextSimulationAt = now();
      sampleInput();
      simulationTime += interval;
      runSimulation(false, nextSimulationAt);
      nextSimulationAt += wallInterval();
      armSimulationTimer();
    },
    setTimeScale(value) {
      currentTimeScale = normalizeTimeScale(value);
      stats.timeScale = currentTimeScale;
      clearSimulationTimer();
      if (running && visible && currentTimeScale > 0) {
        nextSimulationAt = now() + wallInterval();
        armSimulationTimer();
      }
      armMaintenanceTimer();
      return currentTimeScale;
    },
    getTimeScale() { return currentTimeScale; },
  };
}

export default createFrameDriver;
