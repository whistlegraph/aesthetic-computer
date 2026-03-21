// theme.mjs — Prompt theme chooser for AC Native OS
// Displays theme cards, keyboard nav (up/down/enter/escape).
// Persists selection to /mnt/config.json so it survives reboots.

// Build theme list from __theme.presets (defined in C runtime)
const THEMES = [{ id: "default", label: "default", desc: "auto dark/light" }];
for (const id in __theme.presets) {
  const p = __theme.presets[id];
  THEMES.push({ id, label: p.label || id, desc: p.desc || id });
}

let selected = 0;
let frame = 0;
let T;

function boot({ system }) {
  T = __theme.update();
  // Read current theme from config
  let currentId = __theme._overrideId || "default";
  selected = THEMES.findIndex((t) => t.id === currentId);
  if (selected < 0) selected = 0;
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:arrowup")) {
    selected = (selected - 1 + THEMES.length) % THEMES.length;
  }
  if (e.is("keyboard:down:arrowdown")) {
    selected = (selected + 1) % THEMES.length;
  }
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    const id = THEMES[selected].id;
    system?.saveConfig?.("theme", id);
    __theme.apply(id);
    system?.jump?.("prompt");
  }
  if (e.is("keyboard:down:escape")) {
    system?.jump?.("prompt");
  }
}

function paint({ wipe, ink, box, write, screen }) {
  frame++;
  T = __theme.update();
  const W = screen.width;
  const H = screen.height;

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  const font = "6x10";
  const charW = 6;
  const total = THEMES.length;

  // Card layout
  const cardH = 28;
  const cardGap = 4;
  const totalH = total * cardH + (total - 1) * cardGap;
  const startY = Math.floor((H - totalH) / 2);
  const cardX = 8;
  const cardW = W - 16;

  for (let i = 0; i < total; i++) {
    const t = THEMES[i];
    const y = startY + i * (cardH + cardGap);
    const isSel = i === selected;

    // Get preview colors for this theme
    const preset = __theme.presets[t.id];
    const colors = preset ? (T.dark ? preset.dark : preset.light) : null;
    const previewBg = colors ? colors.bg : T.bg;
    const previewFg = colors
      ? (typeof colors.fg === "number" ? [colors.fg, colors.fg, colors.fg] : colors.fg)
      : [T.fg, T.fg, T.fg];
    const previewAccent = colors ? colors.accent : T.accent;
    const previewCursor = colors ? colors.cursor : T.cursor;

    // Card background (use preview bg)
    ink(previewBg[0], previewBg[1], previewBg[2]);
    box(cardX, y, cardW, cardH, true);

    // Selection indicator
    if (isSel) {
      ink(previewCursor[0], previewCursor[1], previewCursor[2]);
      box(cardX - 2, y - 1, cardW + 4, cardH + 2, false);
      if (frame % 40 < 28) {
        ink(previewCursor[0], previewCursor[1], previewCursor[2]);
        write(">", { x: cardX + 3, y: y + 5, size: 1, font });
      }
    }

    // Theme label
    ink(previewFg[0], previewFg[1], previewFg[2]);
    write(t.label, { x: cardX + 14, y: y + 3, size: 1, font });

    // Description (dimmed)
    const dimR = Math.floor(previewFg[0] * 0.5);
    const dimG = Math.floor(previewFg[1] * 0.5);
    const dimB = Math.floor(previewFg[2] * 0.5);
    ink(dimR, dimG, dimB);
    write(t.desc, { x: cardX + 14, y: y + 15, size: 1, font });

    // Accent + cursor color swatches on right
    const swSize = 8;
    const swY = y + Math.floor((cardH - swSize) / 2);
    ink(previewAccent[0], previewAccent[1], previewAccent[2]);
    box(cardX + cardW - swSize * 2 - 12, swY, swSize, swSize, true);
    ink(previewCursor[0], previewCursor[1], previewCursor[2]);
    box(cardX + cardW - swSize - 6, swY, swSize, swSize, true);
  }

  // Hint at bottom
  ink(T.fgMute, T.fgMute, T.fgMute);
  write("up/down + enter  esc to cancel", { x: 4, y: H - 14, size: 1, font });
}

function sim() {}
function leave() {}

export { boot, paint, act, sim, leave };
