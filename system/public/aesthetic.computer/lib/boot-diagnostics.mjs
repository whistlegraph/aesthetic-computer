// Operational boot timing only; sent to AC's boot log, never product analytics.
// Visibility is not proof of user attention. Timer gaps can be CPU contention
// or browser/OS suspension, so keep them separate from observed hidden time.
export function createBootDiagnostics({
  document, window, performance, origin, start = performance.now(),
  setInterval, clearInterval, onLifecycle = () => {},
}) {
  let last = start;
  let hidden = document.visibilityState !== "visible";
  const initialVisibility = hidden ? "hidden" : "visible";
  let hiddenMs = 0, visibleMs = 0, changes = 0, freezes = 0;
  let maxTimerGapMs = 0, lastTick = start, stopped = null;
  let lifecycleEvents = 0;
  const account = () => {
    const now = performance.now();
    const delta = Math.max(0, now - last);
    if (hidden) hiddenMs += delta;
    else visibleMs += delta;
    last = now;
  };
  const snapshot = () => {
    if (stopped) return stopped;
    account();
    maxTimerGapMs = Math.max(maxTimerGapMs, last - lastTick - 1000);
    return {
      initialVisibility, visibility: hidden ? "hidden" : "visible",
      visibleMs: Math.round(visibleMs), hiddenMs: Math.round(hiddenMs),
      visibilityChanges: changes, freezes,
      maxTimerGapMs: Math.round(maxTimerGapMs),
    };
  };
  const emit = (event) => {
    if (lifecycleEvents++ < 20) onLifecycle(event);
  };
  const visibility = () => {
    account();
    hidden = document.visibilityState !== "visible";
    changes++;
    emit(hidden ? "hidden" : "visible");
  };
  const freeze = () => { freezes++; emit("freeze"); };
  const resume = () => { emit("resume"); };
  const pagehide = () => emit("pagehide");
  document.addEventListener("visibilitychange", visibility);
  document.addEventListener("freeze", freeze);
  document.addEventListener("resume", resume);
  window.addEventListener("pagehide", pagehide);
  const timer = setInterval(() => {
    const now = performance.now();
    maxTimerGapMs = Math.max(maxTimerGapMs, now - lastTick - 1000);
    lastTick = now;
  }, 1000);
  return {
    snapshot,
    finish() {
      if (!stopped) {
        stopped = snapshot();
        clearInterval(timer);
        document.removeEventListener("visibilitychange", visibility);
        document.removeEventListener("freeze", freeze);
        document.removeEventListener("resume", resume);
        window.removeEventListener("pagehide", pagehide);
      }
      return stopped;
    },
    resources() {
      // Fixed public module labels only: no piece names, query strings, URLs,
      // auth requests, content, headers, or user resources enter this payload.
      const labels = new Map([
        ["/aesthetic.computer/boot.mjs", "boot"],
        ["/aesthetic.computer/bios.mjs", "bios"],
        ["/aesthetic.computer/lib/parse.mjs", "parse"],
        ["/aesthetic.computer/lib/disk.mjs", "disk"],
        ["/aesthetic.computer/lib/disk-worker-manifest.json", "worker-manifest"],
      ]);
      try {
        return performance.getEntriesByType("resource").flatMap((entry) => {
          const url = new URL(entry.name);
          if (url.origin !== origin) return [];
          const module = labels.get(url.pathname) ||
            (/^\/aesthetic\.computer\/lib\/disk\.worker\.[a-f0-9]+\.mjs$/.test(url.pathname) ? "worker" : null);
          if (!module) return [];
          const ms = (v) => Math.round(Math.max(0, Number(v) || 0));
          return [{ module, durationMs: ms(entry.duration),
            requestWaitMs: ms(entry.responseStart - entry.requestStart),
            downloadMs: ms(entry.responseEnd - entry.responseStart),
            transferBytes: ms(entry.transferSize),
            serviceWorker: entry.workerStart > 0 }];
        }).sort((a, b) => b.durationMs - a.durationMs).slice(0, 12);
      } catch { return []; }
    },
  };
}
