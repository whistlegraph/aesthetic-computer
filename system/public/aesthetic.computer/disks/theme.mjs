// 🎨 Theme — Prompt theme chooser
// Displays available prompt themes and lets you pick one.
// Stores the selection to `prompt:theme` and jumps back to `prompt`.

import { scheme } from "./prompt.mjs";

const THEMES = [
  {
    id: "default",
    label: "default",
    desc: "purple & yellow",
    dark: { bg: [70, 50, 100], fg: [255, 255, 255], accent: [200, 30, 100] },
    light: { bg: [252, 247, 197], fg: [40, 30, 90], accent: [56, 122, 223] },
  },
  {
    id: "serious",
    label: "serious",
    desc: "black & white",
    dark: { bg: [0, 0, 0], fg: [255, 255, 255], accent: [128, 128, 128] },
    light: { bg: [255, 255, 255], fg: [0, 0, 0], accent: [128, 128, 128] },
  },
  {
    id: "neo",
    label: "neo",
    desc: "lime & black",
    dark: { bg: [0, 0, 0], fg: [0, 255, 0], accent: [0, 100, 255] },
    light: { bg: [0, 255, 0], fg: [0, 0, 0], accent: [255, 0, 200] },
  },
];

let selected = 0;
let hovered = -1;
let cards = []; // [{x, y, w, h}] built each paint frame
let jumped = false;
let cachedStore, cachedJump;

function boot({ store, dark }) {
  const current = store["prompt:theme"] || "default";
  selected = THEMES.findIndex((t) => t.id === current);
  if (selected < 0) selected = 0;
  jumped = false;
}

function paint({ wipe, ink, screen, dark }) {
  const { width: W, height: H } = screen;
  const isDark = dark;

  wipe(isDark ? 10 : 230);

  const total = THEMES.length;
  const cardH = Math.floor(H * 0.22);
  const cardW = Math.floor(W * 0.86);
  const cardX = Math.floor((W - cardW) / 2);
  const totalCardsH = cardH * total + 8 * (total - 1);
  let cardY = Math.floor((H - totalCardsH) / 2);

  cards = [];

  for (let i = 0; i < total; i++) {
    const t = THEMES[i];
    const colors = isDark ? t.dark : t.light;
    const isSelected = i === selected;
    const isHovered = i === hovered;

    cards.push({ x: cardX, y: cardY, w: cardW, h: cardH });

    // Card fill
    ink(...colors.bg).box(cardX, cardY, cardW, cardH);

    // Border: thick for selected, thin for hovered
    if (isSelected) {
      ink(...colors.accent).box(cardX - 3, cardY - 3, cardW + 6, cardH + 6, "outline");
      ink(...colors.accent).box(cardX - 2, cardY - 2, cardW + 4, cardH + 4, "outline");
    } else if (isHovered) {
      const hc = isDark ? [200, 200, 200] : [60, 60, 60];
      ink(...hc).box(cardX - 2, cardY - 2, cardW + 4, cardH + 4, "outline");
    }

    // Theme name
    const labelY = cardY + Math.floor(cardH / 2) - 4;
    ink(...colors.fg).write(t.label, { x: cardX + 10, y: labelY });

    // Desc text (dimmed)
    const dimFg = colors.fg.map((v) => Math.floor(v * 0.55));
    ink(...dimFg).write(t.desc, { x: cardX + 10, y: labelY + 14 });

    // Accent swatch on right
    const sw = 14;
    ink(...colors.accent).box(
      cardX + cardW - sw - 10,
      cardY + Math.floor((cardH - sw) / 2),
      sw,
      sw
    );

    cardY += cardH + 8;
  }

  // Instruction hint
  const hintColor = isDark ? [90, 90, 90] : [160, 160, 160];
  ink(...hintColor).write("↑↓ or tap · enter to apply", { center: "x", x: 0, y: H - 16, screen });
}

function act({ event: e, store, jump, needsPaint }) {
  if (jumped) return;
  cachedStore = store;
  cachedJump = jump;

  // Hover detection (runs on every event since act always fires with current position)
  const prev = hovered;
  hovered = -1;
  for (let i = 0; i < cards.length; i++) {
    const c = cards[i];
    if (e.x >= c.x && e.x <= c.x + c.w && e.y >= c.y && e.y <= c.y + c.h) {
      hovered = i;
      break;
    }
  }
  if (hovered !== prev) needsPaint();

  if (e.is("lift")) {
    for (let i = 0; i < cards.length; i++) {
      const c = cards[i];
      if (e.x >= c.x && e.x <= c.x + c.w && e.y >= c.y && e.y <= c.y + c.h) {
        selected = i;
        needsPaint();
        applyTheme();
        return;
      }
    }
  }

  if (e.is("keyboard:down:arrowup")) {
    selected = (selected - 1 + THEMES.length) % THEMES.length;
    needsPaint();
  }
  if (e.is("keyboard:down:arrowdown")) {
    selected = (selected + 1) % THEMES.length;
    needsPaint();
  }
  if (e.is("keyboard:down:enter")) {
    applyTheme();
  }
  if (e.is("keyboard:down:escape")) {
    jumped = true;
    jump("prompt");
  }
}

function applyTheme() {
  if (!cachedStore || !cachedJump) return;
  jumped = true;
  cachedStore["prompt:theme"] = THEMES[selected].id;
  cachedStore.persist("prompt:theme");
  cachedJump("prompt");
}

function leave() {
  jumped = false;
  hovered = -1;
  cards = [];
}

export { boot, paint, act, leave };
