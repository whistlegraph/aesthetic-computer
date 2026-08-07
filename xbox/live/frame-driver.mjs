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
} = {}) {
  if (typeof simulate !== "function")
    throw new TypeError("simulate callback required");
  if (typeof paint !== "function") throw new TypeError("paint callback required");
  const interval = 1000 / simulationFps;
  const earlyTolerance = .35;
  let running = false;
  let visible = true;
  let nextSimulationAt = 0;
  let simulationTime = 0;
  let timerHandle = null;
  let rafHandle = null;
  const stats = {
    simulationFps,
    renderFrames: 0,
    simulationTicks: 0,
    inputSamples: 0,
    droppedSimulationTicks: 0,
    startedAt: 0,
    lastRenderAt: 0,
    lastSimulationAt: 0,
    lastRenderCostMs: 0,
    lastSimulationCostMs: 0,
  };

  const sampleInput = () => {
    sample();
    stats.inputSamples++;
  };

  const runSimulation = (sampleFirst) => {
    if (sampleFirst) sampleInput();
    const started = now();
    simulate(simulationTime);
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
    if (!running || !visible) return;
    // A timely animation frame gets first refusal. The timer preserves the
    // fixed simulation rate on displays running below 60 Hz.
    const delay = Math.max(0, nextSimulationAt + fallbackGraceMs - now());
    timerHandle = setTimer(() => {
      timerHandle = null;
      pumpSimulation(now(), true);
    }, delay);
  };

  const pumpSimulation = (current, sampleEachTick) => {
    if (!running || !visible) return;
    let ticks = 0;
    while (current + earlyTolerance >= nextSimulationAt &&
      ticks < maxCatchUpTicks) {
      simulationTime += interval;
      runSimulation(sampleEachTick);
      nextSimulationAt += interval;
      ticks++;
    }
    if (current + earlyTolerance >= nextSimulationAt) {
      const dropped = Math.floor((current - nextSimulationAt) / interval) + 1;
      stats.droppedSimulationTicks += Math.max(1, dropped);
      nextSimulationAt = current + interval;
    }
    armSimulationTimer();
  };

  const rafTick = (timestamp) => {
    if (!running) return;
    if (visible) {
      const current = Number.isFinite(timestamp) ? timestamp : now();
      sampleInput();
      pumpSimulation(current, false);
      const started = now();
      const alpha = Math.max(0, Math.min(1,
        (current - nextSimulationAt + interval) / interval));
      paint(current, alpha);
      stats.renderFrames++;
      stats.lastRenderAt = current;
      stats.lastRenderCostMs = Math.max(0, now() - started);
    }
    rafHandle = requestFrame(rafTick);
  };

  return {
    stats,
    start() {
      if (running) return;
      running = true;
      visible = true;
      stats.startedAt = now();
      simulationTime = stats.startedAt;
      nextSimulationAt = stats.startedAt;
      sampleInput();
      runSimulation(false);
      nextSimulationAt += interval;
      armSimulationTimer();
      rafHandle = requestFrame(rafTick);
    },
    stop() {
      if (!running) return;
      running = false;
      clearSimulationTimer();
      if (rafHandle !== null) cancelFrame(rafHandle);
      rafHandle = null;
    },
    setVisible(value) {
      visible = Boolean(value);
      clearSimulationTimer();
      if (!running || !visible) return;
      nextSimulationAt = now();
      sampleInput();
      simulationTime += interval;
      runSimulation(false);
      nextSimulationAt += interval;
      armSimulationTimer();
    },
  };
}

export default createFrameDriver;
