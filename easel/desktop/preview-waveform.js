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
  const motion = matchMedia("(prefers-reduced-motion: reduce)");
  let ready = false,
    timer = 0,
    generation = 0,
    frame = 0,
    previousFrame = 0,
    target = [],
    displayed = [],
    lastSound = 0;
  const clear = () => {
    line.classList.remove("sounding");
    lastSound = 0;
    cancelAnimationFrame(frame);
    frame = 0;
    previousFrame = 0;
    target = [];
    displayed = [];
  };
  function animate(now) {
    frame = 0;
    if (!ready || document.hidden || !line.classList.contains("sounding")) return;
    const blend = 1 - Math.exp(-Math.min(64, now - (previousFrame || now - 16)) / 35);
    previousFrame = now;
    displayed = target.map((v, i) => (displayed[i] ?? v) + (v - (displayed[i] ?? v)) * blend);
    path.setAttribute("d", displayed.map((v, i) => `${i ? "L" : "M"}${i} ${(16 - Math.tanh(v * 4) * 13).toFixed(2)}`).join(""));
    frame = requestAnimationFrame(animate);
  }
  async function poll() {
    const current = generation;
    let active = false;
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
      const samples = await preview.executeJavaScript(
        "window.AC?.readOutputWaveform?.(512) || []",
      );
      if (current !== generation || !ready) return;
      const values = Array.isArray(samples)
        ? samples
            .slice(0, 512)
            .map((v) => (Number.isFinite(v) ? Math.max(-1, Math.min(1, v)) : 0))
        : [];
      active = values.some((v) => Math.abs(v) > 0.0005);
      if (active) lastSound = performance.now();
      line.classList.toggle(
        "sounding",
        !!lastSound && performance.now() - lastSound < 600,
      );
      if (!line.classList.contains("sounding")) return;
      if (motion.matches) {
        cancelAnimationFrame(frame); frame = 0;
        path.setAttribute("d", "M0 16H511");
      } else {
        // Resample older guests too, so a loading piece never changes geometry.
        target = Array.from({length:512}, (_, i) => {
          const at = i * Math.max(0, values.length - 1) / 511;
          const lo = Math.floor(at), fraction = at - lo;
          return (values[lo] || 0) * (1 - fraction) + (values[Math.min(lo + 1, values.length - 1)] || 0) * fraction;
        });
        if (!frame) frame = requestAnimationFrame(animate);
      }
    } catch {
      clear();
    } finally {
      if (current === generation && ready)
        timer = setTimeout(poll, active && !motion.matches ? 33 : 200);
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
  window.addEventListener("beforeunload", stop, { once: true });
};
