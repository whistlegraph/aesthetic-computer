// Ableton - AC Max for Live Plugin Manager, 2026.02.12
// Browse and download Max for Live devices

let plugins = [];
let loading = true;
let error = null;
let downloadButtons = [];
let hoveredButton = null;

export const boot = () => {
  fetch("/m4l-plugins")
    .then((res) => {
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      return res.json();
    })
    .then((data) => {
      plugins = Array.isArray(data) ? data : [];
      loading = false;
    })
    .catch((err) => {
      console.error("Failed to load plugins:", err);
      error = "Failed to load plugins.";
      loading = false;
    });
};

function paint({ wipe, ink, screen, write, line, box, pen, dark }) {
  const bg = dark ? [20, 20, 25] : [240, 240, 235];
  const fg = dark ? 255 : 0;
  const dim = dark ? 100 : 130;
  const accent = dark ? [100, 160, 255] : [30, 90, 200];
  const btnBg = dark ? [50, 100, 180] : [30, 90, 200];
  const btnHover = dark ? [80, 150, 255] : [60, 130, 255];
  const sep = dark ? 35 : 210;
  const cardDim = dark ? 140 : 100;

  wipe(...bg);

  const { width, height } = screen;
  const margin = 12;
  let y = margin;

  // Header
  ink(fg).write("AC Max for Live", { x: margin, y });
  y += 12;
  ink(dim).write("Download devices for Ableton Live", { x: margin, y });
  y += 16;
  ink(sep).line(margin, y, width - margin, y);
  y += 12;

  if (loading) {
    ink(dim).write("Loading...", { x: margin, y });
    return;
  }

  if (error) {
    ink(255, 80, 80).write(error, { x: margin, y });
    return;
  }

  if (plugins.length === 0) {
    ink(dim).write("No plugins available", { x: margin, y });
    return;
  }

  // Plugin list
  downloadButtons = [];

  for (const plugin of plugins) {
    // Name + icon
    ink(fg).write(
      `${plugin.device.icon} ${plugin.device.displayName}`,
      { x: margin, y },
    );
    y += 12;

    // Version + category
    ink(...accent).write(
      `v${plugin.version.string} — ${plugin.device.category}`,
      { x: margin, y },
    );
    y += 12;

    // Description
    ink(cardDim).write(plugin.metadata.description || "", { x: margin, y });
    y += 14;

    // Download button
    const btnW = 108;
    const btnH = 14;
    const btnX = margin;
    const btnY = y;

    const isHovered =
      pen &&
      pen.x >= btnX &&
      pen.x < btnX + btnW &&
      pen.y >= btnY &&
      pen.y < btnY + btnH;

    ink(isHovered ? btnHover : btnBg);
    box(btnX, btnY, btnW, btnH);
    ink(255).write("Download .amxd", { x: btnX + 4, y: btnY + 3 });

    downloadButtons.push({
      plugin,
      bounds: { x: btnX, y: btnY, w: btnW, h: btnH },
    });

    y += btnH + 16;

    // Separator
    ink(sep).line(margin, y, width - margin, y);
    y += 12;
  }
}

function act({ event: e, pen, sound, download }) {
  if (!plugins.length) return;

  if (e.is("move") || e.is("draw")) {
    hoveredButton = null;
    for (const btn of downloadButtons) {
      const { x, y, w, h } = btn.bounds;
      if (pen && pen.x >= x && pen.x < x + w && pen.y >= y && pen.y < y + h) {
        hoveredButton = btn;
        break;
      }
    }
  }

  if (e.is("touch") && hoveredButton) {
    const plugin = hoveredButton.plugin;
    sound.synth({ type: "sine", tone: 440, beats: 0.1, volume: 0.3 });

    // Fetch the .amxd binary then trigger a file save via the download API
    fetch(plugin.m4l.downloadUrl)
      .then((res) => res.arrayBuffer())
      .then((buf) => {
        download(plugin.m4l.fileName, new Uint8Array(buf));
      })
      .catch((err) => console.error("Download failed:", err));

    fetch(`/m4l-plugins/${plugin.code}/download`, { method: "POST" }).catch(
      () => {},
    );
  }
}

export { paint, act };
