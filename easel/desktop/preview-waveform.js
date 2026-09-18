window.installPreviewWaveform = (preview) => {
  const ns = "http://www.w3.org/2000/svg";
  const line = document.createElementNS(ns, "svg");
  line.id = "preview-waveform";
  line.setAttribute("viewBox", "0 0 32 511");
  line.setAttribute("preserveAspectRatio", "none");
  line.setAttribute("aria-hidden", "true");
  const path = document.createElementNS(ns, "path");
  path.setAttribute("vector-effect", "non-scaling-stroke");
  path.setAttribute("transform", "matrix(0 1 1 0 0 0)");
  line.append(path);
  document.body.append(line);
  let version;
  const align = () => {
    const inset = version?.offsetWidth ? version.offsetWidth + (parseFloat(getComputedStyle(version).right) || 0) + 12 : 80;
    document.documentElement.style.setProperty("--preview-audio-inset", `${inset}px`);
  };
  const placement = new ResizeObserver(align);
  queueMicrotask(() => {
    version = document.getElementById("credit-label");
    if (version) placement.observe(version);
    align();
  });
  window.addEventListener("resize", align);
  align();
  const motion = matchMedia("(prefers-reduced-motion: reduce)");
  const historyMs = 4000; // Two 4/4 bars at 120 BPM, oldest at the top.
  let history = [];
  let ready = false,
    timer = 0,
    generation = 0,
    previousRead = 0,
    peakEnvelope = 0,
    frame = 0;
  const clear = () => {
    line.classList.remove("sounding");
    path.setAttribute("d", "M0 16H511");
    previousRead = 0;
    peakEnvelope = 0;
    history = [];
    cancelAnimationFrame(frame);
    frame = 0;
  };
  function draw(now) {
    frame = 0;
    if (!ready || document.hidden || preview.hidden || preview.isAudioMuted() || document.body.classList.contains("preview-fullscreen")) { clear(); return; }
    history = history.filter(point => now - point.at < historyMs);
    if (!history.some(point => point.active)) { clear(); return; }
    line.classList.add("sounding");
    // Envelope slices retain transients instead of aliasing an entire audio
    // cycle down to a pixel. Their timestamps set the upward scroll speed.
    const position = point => (511 * (1 - (now - point.at) / historyMs)).toFixed(2);
    const edge = (point, side) => `${position(point)} ${(16 + point[side] * 13).toFixed(2)}`;
    path.setAttribute("d", `M${position(history[0])} 16${history.map(point => `L${edge(point, "low")}`).join("")}L511 16${history.toReversed().map(point => `L${edge(point, "high")}`).join("")}Z`);
    frame = requestAnimationFrame(draw);
  }
  async function poll() {
    const current = generation;
    const started = performance.now();
    let interval = 200;
    try {
      if (
        !ready ||
        document.hidden ||
        document.body.classList.contains("preview-fullscreen") ||
        preview.hidden ||
        preview.isAudioMuted()
      ) {
        clear();
        return;
      }
      // Keep listening during silence: the bird's whole first note is 75 ms.
      // Only one guest read is in flight, and hidden/muted previews back off.
      interval = motion.matches ? 33 : 16;
      const samples = await preview.executeJavaScript(
        "window.AC?.readOutputWaveform?.(512) || []",
      );
      if (current !== generation || !ready) return;
      const values = Array.isArray(samples)
        ? samples
            .slice(0, 512)
            .map((v) => (Number.isFinite(v) ? Math.max(-1, Math.min(1, v)) : 0))
        : [];
      const mean = values.reduce((sum, v) => sum + v, 0) / (values.length || 1);
      const centered = values.map(v => v - mean);
      const peak = centered.reduce((max, v) => Math.max(max, Math.abs(v)), 0);
      const active = peak > 0.0005;
      const now = performance.now();
      // Normalize the display only. Follow peaks immediately, release gain
      // gently, and cap amplification so near-silence doesn't fill the strip.
      peakEnvelope = Math.max(peak, peakEnvelope * Math.exp(-(now - previousRead) / 180));
      previousRead = now;
      const gain = Math.min(32, 0.85 / (peakEnvelope || 1));
      if (motion.matches) {
        cancelAnimationFrame(frame); frame = 0; history = [];
        line.classList.toggle("sounding", active);
        path.setAttribute("d", "M0 16H511");
      } else {
        // Normalization changes only each new slice, never the recorded past.
        history.push({at: now, active,
          low: active ? Math.min(...centered) * gain : 0,
          high: active ? Math.max(...centered) * gain : 0});
        // No second animation loop or queue of guest requests.
        if (!frame) draw(now);
      }
    } catch {
      clear();
    } finally {
      if (current === generation && ready)
        timer = setTimeout(poll, Math.max(4, interval - (performance.now() - started)));
    }
  }
  const stop = () => {
    ready = false;
    generation++;
    clearTimeout(timer);
    clear();
  };
  preview.addEventListener("did-start-loading", stop);
  preview.addEventListener("destroyed", stop);
  const start = () => {
    if (ready) return;
    ready = true;
    void poll();
  };
  preview.addEventListener("dom-ready", start);
  // Loading can restart without a fresh DOM (including same-document loads).
  preview.addEventListener("did-stop-loading", start);
  // The guest may already be ready when this optional decoration is installed.
  queueMicrotask(() => { try { if (preview.getWebContentsId() && !preview.isLoading()) start(); } catch {} });
  document.addEventListener("visibilitychange", () => {
    if (document.hidden) clear();
  });
  window.addEventListener("beforeunload", () => { stop(); placement.disconnect(); window.removeEventListener("resize", align); }, { once: true });
};
