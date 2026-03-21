// Ableton - AC Max for Live Plugin Manager, 2026.02.12
// Browse and download Max for Live devices

let plugins = [];
let loading = true;
let error = null;
let downloadButtons = [];
let hoveredButton = null;
let frame = 0;
let downloading = null; // plugin code currently downloading

const { sin, cos, PI, floor, abs } = Math;

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

function sim() {
  frame += 1;
}

// Strip emoji (AC bitmap font can't render them)
function stripEmoji(str) {
  return str.replace(/[\u{1F000}-\u{1FFFF}]/gu, "").trim();
}

// Draw animated waveform header
function drawWave(ink, line, width, y, dark, t) {
  const amp = 3;
  const wavColor = dark ? [40, 60, 100] : [180, 200, 230];
  for (let x = 0; x < width; x += 2) {
    const v = sin((x * 0.04) + t * 0.03) * amp +
              sin((x * 0.07) - t * 0.02) * amp * 0.5;
    ink(...wavColor, 80 + abs(v) * 15);
    line(x, y + v, x + 1, y + v);
  }
}

// Pixel icon: mini keyboard (midi)
function iconMidi(ink, box, x, y, t) {
  const pulse = sin(t * 0.08) * 0.3 + 0.7;
  const b = floor(180 * pulse);
  ink(b, b, 255).box(x, y, 9, 7);
  ink(0).box(x + 2, y, 1, 4);
  ink(0).box(x + 5, y, 1, 4);
}

// Pixel icon: waveform (instrument)
function iconInstrument(ink, plot, x, y, t) {
  for (let i = 0; i < 9; i++) {
    const v = sin((i * 0.8) + t * 0.06) * 3;
    ink(100, 200, 255).plot(x + i, y + 3 + floor(v));
  }
}

// Pixel icon: knob (effect)
function iconEffect(ink, circle, line, x, y, t) {
  const angle = t * 0.04;
  ink(255, 150, 80).circle(x + 4, y + 3, 3, false);
  const dx = floor(cos(angle) * 2);
  const dy = floor(sin(angle) * 2);
  ink(255, 200, 100).line(x + 4, y + 3, x + 4 + dx, y + 3 + dy);
}

function paint({ wipe, ink, screen, box, line, circle, plot, pen, dark }) {
  const bg = dark ? [15, 15, 20] : [242, 242, 238];
  const fg = dark ? 240 : 10;
  const dim = dark ? 80 : 140;
  const accent = dark ? [100, 170, 255] : [20, 80, 200];
  const btnBg = dark ? [40, 80, 160] : [30, 90, 200];
  const btnHover = dark ? [70, 140, 255] : [50, 120, 255];
  const sep = dark ? 30 : 215;
  const cardDim = dark ? 120 : 110;

  wipe(...bg);

  const { width } = screen;
  const margin = 12;
  let y = 20;

  // Animated wave behind header
  drawWave(ink, line, width, y + 4, dark, frame);

  // Header
  ink(fg).write("AC Max for Live", { x: margin, y });
  y += 12;

  // Subtle pulsing subtitle
  const subAlpha = floor(dim + sin(frame * 0.03) * 20);
  ink(subAlpha).write("Download devices for Ableton Live", { x: margin, y });
  y += 14;

  // Animated separator - a dot travels along the line
  ink(sep).line(margin, y, width - margin, y);
  const dotX = margin + ((frame * 0.5) % (width - margin * 2));
  ink(...accent).plot(floor(dotX), y);
  ink(...accent).plot(floor(dotX) + 1, y);
  y += 12;

  if (loading) {
    // Animated loading dots
    const dots = ".".repeat((floor(frame / 15) % 3) + 1);
    ink(dim).write("Loading" + dots, { x: margin, y });
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

  downloadButtons = [];

  for (let i = 0; i < plugins.length; i++) {
    const plugin = plugins[i];
    const cat = plugin.device.category;
    const name = stripEmoji(plugin.device.displayName);

    // Animated icon by category
    if (cat === "midi") iconMidi(ink, box, margin, y, frame + i * 40);
    else if (cat === "effect") iconEffect(ink, circle, line, margin, y, frame + i * 40);
    else iconInstrument(ink, plot, margin, y, frame + i * 40);

    ink(fg).write(name, { x: margin + 12, y });
    y += 12;

    // Version + category with accent
    ink(...accent).write(`v${plugin.version.string} - ${cat}`, {
      x: margin,
      y,
    });
    y += 12;

    // Description
    ink(cardDim).write(stripEmoji(plugin.metadata.description || ""), {
      x: margin,
      y,
    });
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

    const isDownloading = downloading === plugin.code;

    if (isDownloading) {
      // Animated progress fill
      const progress = (frame % 60) / 60;
      ink(dark ? [30, 50, 80] : [200, 220, 240]);
      box(btnX, btnY, btnW, btnH);
      ink(dark ? [60, 160, 80] : [40, 140, 60]);
      box(btnX, btnY, floor(btnW * progress), btnH);
      ink(255).write("Downloading...", { x: btnX + 4, y: btnY + 3 });
    } else {
      // Hover glow
      if (isHovered) {
        const glow = floor(sin(frame * 0.1) * 15);
        ink(btnHover[0] + glow, btnHover[1] + glow, btnHover[2]);
      } else {
        ink(...btnBg);
      }
      box(btnX, btnY, btnW, btnH);
      ink(255).write("Download .amxd", { x: btnX + 4, y: btnY + 3 });
    }

    downloadButtons.push({
      plugin,
      bounds: { x: btnX, y: btnY, w: btnW, h: btnH },
    });

    y += btnH + 14;

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
    sound.synth({ type: "sine", tone: 660, beats: 0.05, volume: 0.2 });
    sound.synth({ type: "sine", tone: 880, beats: 0.05, volume: 0.15, delay: 0.05 });

    downloading = plugin.code;

    fetch(plugin.m4l.downloadUrl)
      .then((res) => res.arrayBuffer())
      .then((buf) => {
        download(plugin.m4l.fileName, new Uint8Array(buf));
        downloading = null;
      })
      .catch((err) => {
        console.error("Download failed:", err);
        downloading = null;
      });

    fetch(`/m4l-plugins/${plugin.code}/download`, { method: "POST" }).catch(
      () => {},
    );
  }
}

export { paint, act, sim };
