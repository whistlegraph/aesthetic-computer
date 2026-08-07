// Adaptive browser frame driver: stay on vsync when it is timely and fill
// missing deadlines when requestAnimationFrame is throttled below 60 Hz.

export function createFrameDriver({
  paint,
  targetFps = 60,
  now = () => performance.now(),
  requestFrame = (callback) => requestAnimationFrame(callback),
  cancelFrame = (handle) => cancelAnimationFrame(handle),
  setTimer = (callback, delay) => setTimeout(callback, delay),
  clearTimer = (handle) => clearTimeout(handle),
  fallbackGraceMs = 2,
} = {}) {
  if (typeof paint !== "function") throw new TypeError("paint callback required");
  const interval = 1000 / targetFps;
  const earlyTolerance = .35;
  let running = false;
  let visible = true;
  let nextFrameAt = 0;
  let timerHandle = null;
  let rafHandle = null;
  const stats = { targetFps, frames: 0, rafFrames: 0, timerFrames: 0,
    startedAt: 0, lastFrameAt: 0, lastCostMs: 0 };

  const clearDeadlineTimer = () => {
    if (timerHandle === null) return;
    clearTimer(timerHandle);
    timerHandle = null;
  };

  const armDeadlineTimer = () => {
    clearDeadlineTimer();
    if (!running || !visible) return;
    // Give a healthy 60 Hz rAF first refusal. The timer only fills a deadline
    // when the browser's animation clock is late or capped below targetFps.
    const delay = Math.max(0, nextFrameAt + fallbackGraceMs - now());
    timerHandle = setTimer(() => {
      timerHandle = null;
      tick(now(), "timer");
    }, delay);
  };

  const tick = (timestamp, source) => {
    if (!running || !visible) return false;
    const current = Number.isFinite(timestamp) ? timestamp : now();
    if (current + earlyTolerance < nextFrameAt) {
      if (source === "timer") armDeadlineTimer();
      return false;
    }
    const started = now();
    paint();
    stats.frames++;
    if (source === "raf") stats.rafFrames++;
    else if (source === "timer") stats.timerFrames++;
    stats.lastFrameAt = current;
    stats.lastCostMs = Math.max(0, now() - started);

    // Keep the ideal timeline when one deadline is late, but skip missed
    // deadlines instead of issuing catch-up paints.
    if (nextFrameAt < current - interval) nextFrameAt = current;
    do nextFrameAt += interval;
    while (nextFrameAt <= current + earlyTolerance);
    armDeadlineTimer();
    return true;
  };

  const rafTick = (timestamp) => {
    if (!running) return;
    tick(timestamp, "raf");
    rafHandle = requestFrame(rafTick);
  };

  return {
    stats,
    start() {
      if (running) return;
      running = true;
      visible = true;
      stats.startedAt = now();
      nextFrameAt = stats.startedAt;
      tick(nextFrameAt, "start");
      rafHandle = requestFrame(rafTick);
    },
    stop() {
      if (!running) return;
      running = false;
      clearDeadlineTimer();
      if (rafHandle !== null) cancelFrame(rafHandle);
      rafHandle = null;
    },
    setVisible(value) {
      visible = Boolean(value);
      clearDeadlineTimer();
      if (!running || !visible) return;
      nextFrameAt = now();
      tick(nextFrameAt, "resume");
    },
  };
}

export default createFrameDriver;
