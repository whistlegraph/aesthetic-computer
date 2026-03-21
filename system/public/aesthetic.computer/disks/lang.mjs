// 🌍 Lang — Interface language chooser
// Displays available languages and lets you pick one.
// Stores the selection to `prompt:lang` and jumps back to `prompt`.

import { languages } from "./prompt.mjs";

const LANGS = Object.entries(languages).map(([id, l]) => ({
  id,
  label: l.label,
  sample: l.buttons.enter + " / " + l.buttons.paste.label,
}));

let selected = 0;
let hovered = -1;
let cards = [];
let jumped = false;
let cachedStore, cachedJump;

function boot({ store }) {
  const current = store["prompt:lang"] || "en";
  selected = LANGS.findIndex((l) => l.id === current);
  if (selected < 0) selected = 0;
  jumped = false;
}

function paint({ wipe, ink, screen, dark }) {
  const { width: W, height: H } = screen;

  wipe(dark ? 10 : 230);

  const total = LANGS.length;
  const cardH = Math.floor(H * 0.18);
  const cardW = Math.floor(W * 0.86);
  const cardX = Math.floor((W - cardW) / 2);
  const totalCardsH = cardH * total + 8 * (total - 1);
  let cardY = Math.floor((H - totalCardsH) / 2);

  cards = [];

  for (let i = 0; i < total; i++) {
    const l = LANGS[i];
    const isSelected = i === selected;
    const isHovered = i === hovered;

    cards.push({ x: cardX, y: cardY, w: cardW, h: cardH });

    // Card fill
    const bg = isSelected
      ? (dark ? [40, 30, 70] : [220, 210, 250])
      : (dark ? [25, 25, 25] : [245, 245, 245]);
    ink(...bg).box(cardX, cardY, cardW, cardH);

    // Border
    if (isSelected) {
      const accent = dark ? [180, 100, 255] : [100, 50, 200];
      ink(...accent).box(cardX - 3, cardY - 3, cardW + 6, cardH + 6, "outline");
      ink(...accent).box(cardX - 2, cardY - 2, cardW + 4, cardH + 4, "outline");
    } else if (isHovered) {
      const hc = dark ? [80, 80, 80] : [180, 180, 180];
      ink(...hc).box(cardX - 2, cardY - 2, cardW + 4, cardH + 4, "outline");
    }

    // Language name
    const fg = dark ? [255] : [0];
    const labelY = cardY + Math.floor(cardH / 2) - 4;
    ink(...fg).write(l.label, { x: cardX + 10, y: labelY });

    // Sample text (dimmed)
    const dim = dark ? [120] : [140];
    ink(...dim).write(l.sample, { x: cardX + 10, y: labelY + 14 });

    // Language code on right
    const codeColor = dark ? [100] : [170];
    ink(...codeColor).write(l.id.toUpperCase(), {
      x: cardX + cardW - 30,
      y: cardY + Math.floor(cardH / 2) - 3,
    });

    cardY += cardH + 8;
  }

  // Instruction hint
  const hintColor = dark ? [90] : [160];
  ink(...hintColor).write("tap or enter to apply", { center: "x", x: 0, y: H - 16, screen });
}

function act({ event: e, store, jump, needsPaint }) {
  if (jumped) return;
  cachedStore = store;
  cachedJump = jump;

  // Hover detection
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
        applyLang();
        return;
      }
    }
  }

  if (e.is("keyboard:down:arrowup")) {
    selected = (selected - 1 + LANGS.length) % LANGS.length;
    needsPaint();
  }
  if (e.is("keyboard:down:arrowdown")) {
    selected = (selected + 1) % LANGS.length;
    needsPaint();
  }
  if (e.is("keyboard:down:enter")) {
    applyLang();
  }
  if (e.is("keyboard:down:escape")) {
    jumped = true;
    jump("prompt");
  }
}

function applyLang() {
  if (!cachedStore || !cachedJump) return;
  jumped = true;
  cachedStore["prompt:lang"] = LANGS[selected].id;
  cachedStore.persist("prompt:lang");
  cachedJump("prompt");
}

function leave() {
  jumped = false;
  hovered = -1;
  cards = [];
}

export { boot, paint, act, leave };
