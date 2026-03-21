// ⏱️ Timer Worker
// Provides reliable timing ticks even when the page is hidden.
// Web Worker setInterval is NOT throttled by the browser when the tab is hidden,
// unlike requestAnimationFrame (paused) or main-thread setInterval (throttled to 1/sec).

let interval = null;

self.onmessage = function (e) {
  const { type, rate } = e.data;

  if (type === "start") {
    if (interval) clearInterval(interval);
    // Default rate: ~120fps to match the sim update rate
    const ms = rate || 8;
    interval = setInterval(() => {
      self.postMessage({ type: "tick", now: performance.now() });
    }, ms);
  }

  if (type === "stop") {
    if (interval) {
      clearInterval(interval);
      interval = null;
    }
  }
};
