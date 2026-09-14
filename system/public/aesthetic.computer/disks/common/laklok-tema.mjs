// laklok-tema — the shared dress of the Laer Klokken rooms: `laklok` (the
// chat) and `amail` (the post). One roster of temas, one saved choice
// (`laklok:theme`), one interface language (`laklok:lang`), one census, and
// the corner chrome both rooms wear — the QR, the ⚙ gear, the envelope with
// its count, and the indstillinger pane.
//
// The vector sister (system/public/html/index.html) mirrors LAK_THEMES and
// LAK_REALTIME_CYCLES by name; toolchain/laklok-sisters/parity.mjs reads this
// file to check them, so add or rename a tema in both places.

import { hslToRgb } from "../../lib/num.mjs";
import { qrcode as qr, ErrorCorrectLevel } from "../../dep/@akamfoad/qr/qr.mjs";

export const TEMA_KEY = "laklok:theme";
export const LANG_KEY = "laklok:lang";

// 🌅 `realtime` — the ambient tema: a palette that is a pure function of the
// date, so the room is a little different every day and the same for
// everyone in it. `t` is fractional UTC days; the hue walks ~6° a day around
// a 61-day lap while saturation and lightness breathe on their own cycles,
// so no midnight is a cut. Text sits at a fixed lightness above the ground.
// Mirrored in the vector client as `realtimeTheme` — same cycles, same slots.
export const LAK_REALTIME_CYCLES = [61, 23, 17]; // hue lap, saturation, lightness (days)
export function realtimeTheme(now = Date.now()) {
  const t = Math.floor(now / 60000) / 1440; // quantized to the minute
  const [hueDays, satDays, lightDays] = LAK_REALTIME_CYCLES;
  const hue = (t * 360) / hueDays;
  const sat = 34 + 12 * Math.sin((t * 2 * Math.PI) / satDays);
  const light = 22 + 5 * Math.sin((t * 2 * Math.PI) / lightDays + 2);
  const c = (h, s, l) => hslToRgb(((h % 360) + 360) % 360, s, l);
  const bg = c(hue, sat, light);
  return {
    bg,
    stripeA: c(hue, sat + 6, light * 0.62),
    stripeB: c(hue, sat + 6, light * 0.8),
    chat: {
      background: bg,
      chromeBg: bg,
      lines: [...c(hue, sat, 62), 64],
      scrollbar: c(hue, 60, 78),
      messageText: c(hue, 30, 95),
      messageBox: c(hue, 45, 85),
      log: [100, 255, 220],
      logHover: [255, 240, 120],
      handle: c(hue + 30, 80, 80),
      handleHover: [255, 240, 120],
      url: c(hue + 180, 85, 80),
      urlHover: [255, 240, 120],
      prompt: c(hue + 120, 80, 80),
      promptContent: c(hue + 180, 85, 80),
      promptHover: [255, 240, 120],
      promptContentHover: [255, 240, 120],
      painting: c(hue + 60, 85, 80),
      paintingHover: [255, 240, 120],
      kidlisp: c(hue + 300, 85, 80),
      kidlispHover: [255, 240, 120],
      timestamp: c(hue, 25, 68),
      timestampHover: [255, 240, 120],
      heart: [255, 220, 240],
    },
  };
}

// The `realtime` slot in LAK_THEMES is refilled once a minute while it is the
// active tema (sim). Returns true when the palette actually moved.
let realtimeMinute = 0;
export function realtimeTick() {
  const minute = Math.floor(Date.now() / 60000);
  if (minute === realtimeMinute) return false;
  realtimeMinute = minute;
  Object.assign(LAK_THEMES.realtime, realtimeTheme());
  return true;
}

// 🎨 Themes — each recolors the whole room. `ler` (clay) is the historical
// terracotta; the others keep the same relationships in new light. The vector
// client mirrors these by name, so add/rename in both places.
export const LAK_THEMES = {
  ler: {
    bg: [180, 100, 60],
    stripeA: [122, 60, 26],
    stripeB: [150, 78, 34],
    chat: {
      background: [180, 100, 60],
      chromeBg: [180, 100, 60],
      lines: [220, 150, 100, 64],
      scrollbar: [255, 180, 100],
      messageText: [255, 255, 240],
      messageBox: [255, 220, 180],
      log: [100, 255, 220],
      logHover: [255, 240, 120],
      handle: [255, 160, 120],
      handleHover: [255, 240, 120],
      url: [120, 220, 255],
      urlHover: [255, 240, 120],
      prompt: [200, 255, 180],
      promptContent: [120, 220, 255],
      promptHover: [255, 240, 120],
      promptContentHover: [255, 240, 120],
      painting: [255, 200, 140],
      paintingHover: [255, 240, 120],
      kidlisp: [255, 140, 200],
      kidlispHover: [255, 240, 120],
      timestamp: [220, 180, 150],
      timestampHover: [255, 240, 120],
      heart: [255, 220, 240],
    },
  },
  nat: {
    bg: [26, 30, 62],
    stripeA: [20, 24, 50],
    stripeB: [32, 38, 76],
    chat: {
      background: [26, 30, 62],
      chromeBg: [26, 30, 62],
      lines: [90, 110, 190, 64],
      scrollbar: [120, 150, 255],
      messageText: [235, 240, 255],
      messageBox: [180, 200, 255],
      log: [100, 255, 220],
      logHover: [255, 240, 120],
      handle: [150, 180, 255],
      handleHover: [255, 240, 120],
      url: [120, 220, 255],
      urlHover: [255, 240, 120],
      prompt: [200, 255, 180],
      promptContent: [120, 220, 255],
      promptHover: [255, 240, 120],
      promptContentHover: [255, 240, 120],
      painting: [255, 200, 140],
      paintingHover: [255, 240, 120],
      kidlisp: [255, 140, 200],
      kidlispHover: [255, 240, 120],
      timestamp: [150, 160, 210],
      timestampHover: [255, 240, 120],
      heart: [255, 220, 240],
    },
  },
  skov: {
    bg: [24, 56, 36],
    stripeA: [18, 44, 28],
    stripeB: [30, 66, 42],
    chat: {
      background: [24, 56, 36],
      chromeBg: [24, 56, 36],
      lines: [90, 150, 110, 64],
      scrollbar: [130, 220, 150],
      messageText: [235, 255, 240],
      messageBox: [190, 230, 200],
      log: [100, 255, 220],
      logHover: [255, 240, 120],
      handle: [170, 230, 150],
      handleHover: [255, 240, 120],
      url: [120, 220, 255],
      urlHover: [255, 240, 120],
      prompt: [220, 255, 170],
      promptContent: [120, 220, 255],
      promptHover: [255, 240, 120],
      promptContentHover: [255, 240, 120],
      painting: [255, 210, 140],
      paintingHover: [255, 240, 120],
      kidlisp: [255, 150, 190],
      kidlispHover: [255, 240, 120],
      timestamp: [150, 190, 160],
      timestampHover: [255, 240, 120],
      heart: [255, 215, 235],
    },
  },
  lakrids: {
    bg: [22, 20, 24],
    stripeA: [14, 12, 16],
    stripeB: [30, 27, 34],
    chat: {
      background: [22, 20, 24],
      chromeBg: [22, 20, 24],
      lines: [90, 80, 95, 64],
      scrollbar: [200, 190, 210],
      messageText: [240, 238, 244],
      messageBox: [210, 205, 215],
      log: [100, 255, 220],
      logHover: [255, 240, 120],
      handle: [240, 170, 190],
      handleHover: [255, 240, 120],
      url: [130, 210, 255],
      urlHover: [255, 240, 120],
      prompt: [190, 240, 170],
      promptContent: [130, 210, 255],
      promptHover: [255, 240, 120],
      promptContentHover: [255, 240, 120],
      painting: [250, 200, 150],
      paintingHover: [255, 240, 120],
      kidlisp: [255, 150, 210],
      kidlispHover: [255, 240, 120],
      timestamp: [150, 145, 160],
      timestampHover: [255, 240, 120],
      heart: [255, 210, 230],
    },
  },
  realtime: realtimeTheme(), // 🌅 ambient — see realtimeTheme / realtimeTick
};

export function isTema(name) {
  return typeof name === "string" && Object.hasOwn(LAK_THEMES, name);
}

// 👗 Which tema to wear. A colon or `~` token pins one (`laklok:nat`,
// `amail~skov` — shareable themed URLs); otherwise whatever the store already
// has synchronously, falling back to `ler`. The retrieved store settles later
// — see restoreTema.
export function pickTema(tokens, store) {
  const pinned = (tokens || []).find((t) => isTema(t));
  if (pinned) return { name: pinned, pinned: true };
  return { name: isTema(store[TEMA_KEY]) ? store[TEMA_KEY] : "ler", pinned: false };
}

// `store.retrieve` resolves via .then — awaiting it in boot stalls the piece
// (see cal.mjs for the same pattern). Hands the saved tema over when it lands,
// or null if a token already pinned one or nothing was saved.
export function restoreTema(store, pinned, onTema) {
  store.retrieve(TEMA_KEY).then((v) => onTema(!pinned && isTema(v) ? v : null));
}

export function saveTema(store, name) {
  store[TEMA_KEY] = name;
  store.persist(TEMA_KEY);
}

// 📊 Theme census — tell /api/laklok-theme which tema this visitor is on (boot
// = heartbeat, chip tap = switch). Needs a login; anonymous visitors are not
// counted. Fire-and-forget: the room never waits on it.
export function reportTema(net, name) {
  net.userRequest("POST", "/api/laklok-theme", { theme: name });
}

// 🗣️ Interface language — the words on the chrome, not the letters or the
// chat. `da` is the rooms' mother tongue; `en` for everyone else. One saved
// choice for both rooms, like the tema. Each room names its own fallback.
export const LANGS = ["da", "en"];
const STRINGS = {
  da: {
    settings: "indstillinger",
    mode: "tilstand",
    tema: "tema",
    filter: "filter",
    all: "alle",
    links: "links",
    language: "sprog",
    // amail
    inbox: "indbakke",
    sent: "sendt",
    prefs: "valg",
    write: "skriv",
    send: "send",
    markRead: "markér læst",
    noMail: "ingen Amail endnu",
    nothingSent: "intet sendt endnu",
    tryHint: "prøv: amail @jeffrey hej",
    composeHint: "enter går ned  ·  tryk på en række",
    whoTo: "til hvem?",
    nothingToSay: "intet at sige endnu",
    noOne: "ingen svarer på",
    couldntSend: "kunne ikke sendes",
    to: "til",
    re: "re",
    say: "sig",
    optional: "(valgfri)",
    loading: "henter",
    loadingPrefs: "henter valg…",
    updating: "opdaterer…",
    subscribed: "tilmeldt",
    unsubscribed: "afmeldt",
    subscribe: "tilmeld",
    unsubscribe: "afmeld",
    blastHistory: "udsendelser",
    noBlasts: "ingen udsendelser endnu",
    unverified: "ubekræftet",
    outside: "udefra",
    error: "fejl",
    adTitle: "breve mellem @handles",
    adBody:
      "Amail er posten på aesthetic.computer — intet forlader computeren. log ind, tag et @handle, og din boks er klar.",
    adPrompt: "fra enhver prompt:  amail @handle dine ord",
    signup: "opret dig",
    login: "log ind",
  },
  en: {
    settings: "settings",
    mode: "mode",
    tema: "theme",
    filter: "filter",
    all: "all",
    links: "links",
    language: "language",
    // amail
    inbox: "inbox",
    sent: "sent",
    prefs: "prefs",
    write: "write",
    send: "send",
    markRead: "mark read",
    noMail: "no Amail yet",
    nothingSent: "nothing sent yet",
    tryHint: "try: amail @jeffrey hello",
    composeHint: "enter moves down  ·  tap a row to jump",
    whoTo: "who is it to?",
    nothingToSay: "nothing to say yet",
    noOne: "no one answers to",
    couldntSend: "couldn't send that",
    to: "to",
    re: "re",
    say: "say",
    optional: "(optional)",
    loading: "loading",
    loadingPrefs: "loading prefs…",
    updating: "updating…",
    subscribed: "subscribed",
    unsubscribed: "unsubscribed",
    subscribe: "subscribe",
    unsubscribe: "unsubscribe",
    blastHistory: "blast history",
    noBlasts: "no blasts sent yet",
    unverified: "unverified",
    outside: "outside",
    error: "error",
    adTitle: "letters between @handles",
    adBody:
      "Amail is the post of aesthetic.computer — nothing leaves the computer. log in, take a @handle, and your box is ready.",
    adPrompt: "from any prompt:  amail @handle your words",
    signup: "sign up",
    login: "log in",
  },
};

export function isLang(name) {
  return LANGS.includes(name);
}

export function strings(lang) {
  return STRINGS[isLang(lang) ? lang : "en"];
}

// A `~da` / `:en` token pins the language; otherwise the saved one; otherwise
// the room's own fallback.
export function pickLang(tokens, store, fallback = "en") {
  const pinned = (tokens || []).find((t) => isLang(t));
  if (pinned) return { name: pinned, pinned: true };
  return { name: isLang(store[LANG_KEY]) ? store[LANG_KEY] : fallback, pinned: false };
}

export function restoreLang(store, pinned, onLang) {
  store.retrieve(LANG_KEY).then((v) => onLang(!pinned && isLang(v) ? v : null));
}

export function saveLang(store, name) {
  store[LANG_KEY] = name;
  store.persist(LANG_KEY);
}

// 📱 A QR's cells for a url, or null when the encoder balks.
export function makeQR(url) {
  try {
    return qr(url, { errorCorrectLevel: ErrorCorrectLevel.L }).modules;
  } catch (e) {
    console.error("QR generation failed:", url, e);
    return null;
  }
}

// 1px per cell on a `paper` ground with a 1px border of the same. The two
// rooms use different paper so their corners read apart at a glance.
export function paintQR($, cells, x, y, paper = [255, 255, 255]) {
  const { ink } = $;
  const size = cells.length;
  ink(...paper).box(x, y, size + 2, size + 2);
  for (let cy = 0; cy < size; cy++) {
    for (let cx = 0; cx < size; cx++) {
      if (cells[cy][cx]) ink(0, 0, 0).box(x + 1 + cx, y + 1 + cy, 1, 1);
    }
  }
  return { x, y, w: size + 2, h: size + 2 };
}

// ⚙️ The settings toggle — a little hamburger. Returns its padded hit box.
export const GEAR = 13;
export function paintGear($, x, y, open) {
  const { ink } = $;
  ink(open ? [255, 240, 120] : [255, 255, 255, 200]).box(x, y, GEAR, GEAR, "outline");
  const lineCol = open ? [255, 240, 120] : [255, 255, 255, 220];
  for (let li = 0; li < 3; li++) {
    ink(...lineCol).box(x + 3, y + 3 + li * 3, GEAR - 6, 1);
  }
  return { x: x - 2, y: y - 2, w: GEAR + 4, h: GEAR + 4 };
}

// 📬 The envelope — shut and grey when the box is empty, lit teal with its
// flap open when something is waiting. Same palette as the prompt curtain.
export const ENVELOPE = { w: 15, h: 10 };
export function paintEnvelope($, x, y, { lit = false, hot = false, down = false } = {}) {
  const { ink, line } = $;
  const { w, h } = ENVELOPE;
  const fill = down
    ? [0, 80, 100, 235]
    : lit
      ? hot ? [0, 150, 175, 230] : [0, 120, 140, 210]
      : hot ? [40, 46, 58, 205] : [26, 30, 38, 170];
  const edge = down
    ? [210, 255, 255]
    : lit
      ? hot ? [200, 255, 255] : [120, 255, 255]
      : hot ? [150, 162, 186] : [80, 88, 104];
  ink(fill).box(x, y, w, h);
  ink(edge).box(x, y, w, h, "outline");
  ink(lit || hot ? [190, 255, 255] : [60, 66, 80]);
  line(x, y, x + (w >> 1), y + (h >> 1));
  line(x + w - 1, y, x + (w >> 1), y + (h >> 1));
}

// 🔴 A red count badge. MatrixChunky8 digits are 3×7 at rows 0–6 of their
// cell; 11 tall with the glyph at +2 gives them two clear pixels above and
// below. Returns its width so a caller can centre the cluster it sits on.
export const BADGE_H = 11;
export function badgeWidth(label) {
  return label.length * 4 + 5;
}
export function paintBadge($, x, y, label, hot = false) {
  const { ink } = $;
  const w = badgeWidth(label);
  ink(hot ? [255, 60, 70] : [220, 30, 40]).box(x, y, w, BADGE_H);
  ink(hot ? [255, 180, 185] : [255, 120, 130]).box(x, y, w, BADGE_H, "outline");
  ink(255, 240, 240).write(label, { x: x + 3, y: y + 2 }, undefined, undefined, false, "MatrixChunky8");
  return w;
}

// 🏷️ A chip — the unit of the pane and of amail's compact controls. Filled
// gold when selected, outlined otherwise; `tint` colours an outlined chip so
// an action can carry its own hue. Returns its hit box.
export const CHIP_H = 11;
export const CHIP_FONT = "MatrixChunky8";
export function chipWidth(text) {
  return text.length * 5 + 8;
}
export function paintChip($, x, y, text, { selected = false, tint = null, dim = false } = {}) {
  const { ink } = $;
  const w = chipWidth(text);
  if (selected) {
    ink(255, 240, 120).box(x, y, w, CHIP_H);
    ink(20, 10, 6).write(text, { x: x + 4, y: y + 2 }, undefined, undefined, false, CHIP_FONT);
  } else {
    const edge = tint ? [...tint, dim ? 110 : 200] : [255, 255, 255, dim ? 70 : 120];
    const face = tint ? [...tint, dim ? 170 : 255] : [255, 255, 255, dim ? 140 : 200];
    ink(edge).box(x, y, w, CHIP_H, "outline");
    ink(face).write(text, { x: x + 4, y: y + 2 }, undefined, undefined, false, CHIP_FONT);
  }
  return { x, y, w, h: CHIP_H };
}

// ⚙️ The indstillinger pane — rows of chips hung from the top-right under the
// corner chrome. `rows` is [{ label, chips: [{ text, selected, action }] }].
// Returns the chip hit boxes, with the pane's own bounds on `.pane` so act()
// can tell a tap inside from a tap that should close it.
export function paintTemaPane($, { theme, rows, right, top, title = "indstillinger" }) {
  const { ink } = $;
  const hits = [];

  const lineH = 16;
  const padX = 6;
  const labelW = 42;

  // The pane is as wide as its widest row wants, but never wider than the
  // screen allows; a row that doesn't fit flows its chips onto more lines
  // (phones are ~256px, and the tema row alone is wider than that).
  let paneW = 0;
  for (const row of rows) {
    let w = labelW;
    for (const chip of row.chips) w += chipWidth(chip.text) + 4;
    paneW = Math.max(paneW, w);
  }
  paneW = Math.min(paneW + padX * 2, right - 2);
  const chipsLeft = padX + labelW;
  const chipsRight = paneW - padX;

  // Lay the chips out once (relative to the pane) so the height is known
  // before anything is drawn.
  const placed = []; // [{ chip, x, y }]
  let lineY = 14 + 4;
  for (const row of rows) {
    let chipX = chipsLeft;
    let y = lineY;
    for (const chip of row.chips) {
      const w = chipWidth(chip.text);
      if (chipX + w > chipsRight && chipX > chipsLeft) {
        chipX = chipsLeft;
        y += CHIP_H + 3;
      }
      placed.push({ chip, x: chipX, y, label: chipX === chipsLeft && y === lineY ? row.label : null });
      chipX += w + 4;
    }
    lineY = y + lineH;
  }
  const paneH = lineY + 4;
  const paneX = Math.max(2, right - paneW);
  const paneY = top;

  ink(theme.stripeA[0], theme.stripeA[1], theme.stripeA[2], 245).box(paneX, paneY, paneW, paneH);
  ink(255, 240, 120).box(paneX, paneY, paneW, paneH, "outline");
  ink(255, 240, 120).write(title, { x: paneX + padX, y: paneY + 4 }, undefined, undefined, false, CHIP_FONT);

  for (const { chip, x, y, label } of placed) {
    if (label) {
      ink(255, 255, 255, 180).write(label, { x: paneX + padX, y: paneY + y + 2 }, undefined, undefined, false, CHIP_FONT);
    }
    const box = paintChip($, paneX + x, paneY + y, chip.text, { selected: chip.selected });
    hits.push({ ...box, action: chip.action });
  }

  hits.pane = { x: paneX, y: paneY, w: paneW, h: paneH };
  return hits;
}

// The two rows every room's pane carries: tema and sprog.
export function temaRow(current, s) {
  return {
    label: s.tema,
    chips: Object.keys(LAK_THEMES).map((name) => ({
      text: name,
      selected: current === name,
      action: { type: "theme", value: name },
    })),
  };
}
export function langRow(current, s) {
  return {
    label: s.language,
    chips: LANGS.map((name) => ({
      text: name,
      selected: current === name,
      action: { type: "lang", value: name },
    })),
  };
}
