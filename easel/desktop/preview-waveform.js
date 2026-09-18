window.installPreviewWaveform = (preview) => {
  const ns = "http://www.w3.org/2000/svg";
  const line = document.createElementNS(ns, "svg");
  line.id = "preview-waveform";
  line.setAttribute("viewBox", "0 0 127 32");
  line.setAttribute("preserveAspectRatio", "none");
  line.setAttribute("aria-hidden", "true");
  const path = document.createElementNS(ns, "path");
  path.setAttribute("vector-effect", "non-scaling-stroke");
  line.append(path);
  document.body.append(line);
  const motion = matchMedia("(prefers-reduced-motion: reduce)");
  let ready = false,
    timer = 0,
    generation = 0,
    lastSound = 0;
  const clear = () => {
    line.classList.remove("sounding");
    lastSound = 0;
  };
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
        "window.AC?.readOutputWaveform?.() || []",
      );
      if (current !== generation || !ready) return;
      const values = Array.isArray(samples)
        ? samples
            .slice(0, 128)
            .map((v) => (Number.isFinite(v) ? Math.max(-1, Math.min(1, v)) : 0))
        : [];
      active = values.some((v) => Math.abs(v) > 0.0005);
      if (active) lastSound = performance.now();
      line.classList.toggle(
        "sounding",
        !!lastSound && performance.now() - lastSound < 600,
      );
      if (!line.classList.contains("sounding")) return;
      const box = document.getElementById("artifact-shell").getBoundingClientRect();
      line.style.top = `${Math.max(0, box.top + box.height / 2 - 24)}px`;
      path.setAttribute(
        "d",
        motion.matches || !active
          ? "M0 16H127"
          : values
              .map(
                (v, i) =>
                  `${i ? "L" : "M"}${(i * 127) / Math.max(1, values.length - 1)} ${(16 - Math.tanh(v * 4) * 13).toFixed(2)}`,
              )
              .join(""),
      );
    } catch {
      clear();
    } finally {
      if (current === generation && ready)
        timer = setTimeout(poll, active && !motion.matches ? 50 : 200);
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
