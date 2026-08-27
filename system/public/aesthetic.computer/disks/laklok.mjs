// laklok, 2025.5.08.16.31.51.182
// Learn the 'clock'! (formerly `laer-klokken`; that path still aliases here.)
// 🌐 Backed by the branded domain laklok.com.

/* 📝 Notes
  The raster piece and the vector client (`/html/` on laklok.com, source at
  `system/public/html/index.html`) are sisters — the settings pane (mode,
  tema, filter) exists on both and any change here should land there too.
  The update path lives in `toolchain/laklok-sisters/PARITY.md`:
  `parity.mjs` diffs the mirrored constants (run it after touching either
  side) and `sisters.mjs` renders them side by side for the visual half.
 */

import { Chat } from "../lib/chat.mjs"; // TODO: Eventually expand to `net.Socket`
import * as chat from "./chat.mjs"; // Import chat everywhere.
import { qrcode as qr, ErrorCorrectLevel } from "../dep/@akamfoad/qr/qr.mjs";

let client;

// 📐 Top chrome height — shared between chat.paint (topMargin) and the circus
// marquee so the banner fills the whole header down to the margin line.
const LAK_TOP_MARGIN = 34;

// 📱 laklok.com QR rendered in the top-right corner (see paintQR).
let lakQRCells = null;

// 🎨 Themes — each recolors the whole room. `ler` (clay) is the historical
// terracotta; the others keep the same relationships in new light. The vector
// client mirrors these by name, so add/rename in both places.
const LAK_THEMES = {
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
};

// ⚙️ Settings pane state — mode (raster here / vector on laklok.com/html),
// tema, and the media-links filter. The same pane exists on the vector side.
let lakTheme = "ler";
let lakLinksOnly = false;
let settingsOpen = false;
let gearBox = null; // {x, y, w, h} hit area for the ⚙ toggle
let settingsHits = []; // [{x, y, w, h, action}] chips, rebuilt each paint

// 🔗 What counts as a media link — hosts that ARE media plus direct files.
// Mirrored verbatim in the vector client; edit both or the sisters drift.
const LAK_MEDIA_LINK =
  /(youtube\.com\/(watch|shorts|embed)|youtu\.be\/|vimeo\.com\/|tiktok\.com\/|soundcloud\.com\/|bandcamp\.com|\.(png|jpe?g|gif|webp|mp4|mov|webm|mp3|wav|ogg|m4a)\b)/i;
function hasMediaLink(text) {
  if (!text) return false;
  const urls = text.match(/https?:\/\/[^\s]+|www\.[^\s]+/gi);
  return !!urls && urls.some((u) => LAK_MEDIA_LINK.test(u));
}

// The chat system, filtered down to media links when the filter is on. A
// fresh shallow view per frame — message objects keep their identity so the
// renderer's per-message caches stay warm.
function chatView() {
  const sys = client.system;
  if (!lakLinksOnly) return sys;
  return { ...sys, messages: sys.messages.filter((m) => hasMediaLink(m.text)) };
}

function boot({ api, wipe, debug, send, hud, store, colon, jump }) {
  client = new Chat(debug, send);
  client.connect("clock"); // Connect to 'clock' chat. (DB stays `chat-clock`.)
  chat.boot(api, client.system); // Use default font

  // 🚫 chat.boot stamps a prompt.ac/chat QR to the LEFT of the HUD label; clear
  // it so laklok shows only its own laklok.com QR in the top-right (paintQR).
  hud.qr(null);

  // ⚙️ Colon overrides first (`laklok:nat:links` — handy for the sisters
  // suite and shareable themed URLs; `laklok:vector` jumps straight to the
  // vector client), then saved preferences fill whatever colon left alone.
  // `store.retrieve` resolves via .then — awaiting it in boot stalls the
  // piece (see cal.mjs for the same pattern).
  settingsOpen = false;
  lakTheme = "ler";
  lakLinksOnly = false;
  let colonTheme = false;
  let colonLinks = false;
  for (const token of colon || []) {
    if (LAK_THEMES[token]) { lakTheme = token; colonTheme = true; }
    if (token === "links") { lakLinksOnly = true; colonLinks = true; }
    if (token === "alle") { lakLinksOnly = false; colonLinks = true; }
    if (token === "vector") jump("out:https://laklok.com/html/");
  }
  if (!colonTheme && LAK_THEMES[store["laklok:theme"]]) {
    lakTheme = store["laklok:theme"];
  }
  if (!colonLinks && typeof store["laklok:links"] === "boolean") {
    lakLinksOnly = store["laklok:links"];
  }
  store.retrieve("laklok:theme").then((v) => {
    if (!colonTheme && LAK_THEMES[v]) {
      lakTheme = v;
      chat.refresh(client.system);
    }
  });
  store.retrieve("laklok:links").then((v) => {
    if (!colonLinks && typeof v === "boolean") {
      lakLinksOnly = v;
      chat.refresh(client.system);
    }
  });

  // 📱 Generate the laklok.com QR once; painted top-right each frame.
  try {
    lakQRCells = qr("https://laklok.com", {
      errorCorrectLevel: ErrorCorrectLevel.L,
    }).modules;
  } catch (e) {
    console.error("laklok QR generation failed:", e);
    lakQRCells = null;
  }
  // 🏷️ Ensure label shows piece name (not "chat"), pinned white so it doesn't
  // ride the red/orange/lime connection-status color.
  hud.label("laklok", "white");
  // 🌐 Complete the laklok.com wordmark on the same baseline.
  hud.suffix(".com");
}

// 🎪 "Laer Klokken" circus banner — a striped, gold-trimmed marquee in the
// top-right header strip, in GNU Unifont, with every character a different
// circus color and its own little bounce. Masked to the top header area.
const LAK_SIGN_FONT = "unifont";
const LAK_CIRCUS_COLS = [
  [255, 80, 80],   // red
  [255, 210, 70],  // gold
  [120, 210, 255], // sky
  [140, 240, 150], // green
  [255, 150, 220], // pink
  [180, 150, 255], // violet
];

// The canvas uses logical pixels, so phone browsers are typically around 256px
// wide even when their CSS viewport is much larger. Below this breakpoint the
// full 8px circus spacing collides with both the HUD label and the QR code.
const LAK_COMPACT_HEADER_MAX = 399;
const LAK_HUD_FALLBACK_WIDTH = 78;
const LAK_HEADER_GUTTER = 4;

function getSignLayout($, labelLength, charWidth) {
  const { screen, hud } = $;
  const qrSize = lakQRCells?.length || 0;

  if (screen.width > LAK_COMPACT_HEADER_MAX) {
    const gap = 8;
    const width = labelLength * charWidth + gap * Math.max(0, labelLength - 1);
    return width <= screen.width - LAK_HEADER_GUTTER
      ? { left: Math.round((screen.width - width) / 2), gap }
      : null;
  }

  const hudBox = hud?.currentLabel?.()?.btn?.box;
  const hudRight = hudBox
    ? (hudBox.x || 0) + (hudBox.w || 0)
    : 6 + LAK_HUD_FALLBACK_WIDTH;
  const left = hudRight + LAK_HEADER_GUTTER;
  const right = screen.width - qrSize - 4 - LAK_HEADER_GUTTER;
  const available = right - left;
  const glyphWidth = labelLength * charWidth;
  const gaps = Math.max(0, labelLength - 1);
  const gap = gaps > 0 ? Math.min(2, Math.floor((available - glyphWidth) / gaps)) : 0;

  return gap >= 0 ? { left, gap } : null;
}

function paintLaerKlokkenSign($, headerHeight = LAK_TOP_MARGIN) {
  const { ink, screen } = $;
  const now = typeof performance !== "undefined" ? performance.now() : 0;
  const t = now * 0.004;
  const label = "Laer Klokken";
  const themed = LAK_THEMES[lakTheme];

  // Unifont latin glyphs are a fixed 8px advance — no need to measure per frame.
  const CHAR_W = 8;
  const headerH = headerHeight; // marquee fills the header down to the margin
  const sx = 0;
  const sy = 0; // flush with the top of the screen (no gap)
  const sw = screen.width; // full-width banner across the header
  const sh = headerH;
  // Wide screens keep the spacious centered marquee. Phones compact it into
  // the measured gap between the HUD label and QR; if that gap is too small,
  // the branded HUD label remains as the sole title rather than overlapping.
  const signLayout = getSignLayout($, label.length, CHAR_W);

  // Striped circus backdrop, slowly scrolling like a barber pole, tuned per
  // theme so the banner reads as one piece with the room.
  const stripeW = 14;
  const scroll = Math.floor(t * 6);
  for (let bx = 0; bx < sw; bx += stripeW) {
    const odd = Math.floor((bx + scroll) / stripeW) % 2;
    const c = odd ? themed.stripeB : themed.stripeA;
    ink(c[0], c[1], c[2]).box(sx + bx, sy, Math.min(stripeW, sw - bx), sh);
  }
  // Each character: its own circus color + individual vertical bounce.
  if (!signLayout) return;
  let cx = signLayout.left;
  const baseY = sy + Math.round((sh - 16) / 2); // 16 ≈ unifont glyph height, vertically centered
  for (let i = 0; i < label.length; i++) {
    const ch = label[i];
    if (ch !== " ") {
      const bounce = Math.round(Math.sin(t * 2.6 + i * 0.7) * 2);
      const c = LAK_CIRCUS_COLS[i % LAK_CIRCUS_COLS.length];
      ink(20, 10, 6).write(ch, { x: cx + 1, y: baseY + bounce + 1 }, undefined, undefined, false, LAK_SIGN_FONT);
      ink(c[0], c[1], c[2]).write(ch, { x: cx, y: baseY + bounce }, undefined, undefined, false, LAK_SIGN_FONT);
    }
    cx += CHAR_W + signLayout.gap;
  }
}

// 📱 laklok.com QR code, pinned to the top-right corner with a white border,
// plus the ⚙ settings toggle just to its left.
function paintQR($) {
  if (!lakQRCells) return;
  const { ink, screen } = $;
  const cells = lakQRCells;
  const size = cells.length; // 1px per cell
  const qrBoxSize = size + 2;
  const rightInset = 3; // Match the QR's 3px top inset (4px below at this height).
  const qrX = screen.width - qrBoxSize - rightInset;
  const qrY = Math.floor((LAK_TOP_MARGIN - qrBoxSize) / 2);
  ink(255, 255, 255).box(qrX, qrY, size + 2, size + 2); // white background + border
  for (let y = 0; y < size; y++) {
    for (let x = 0; x < size; x++) {
      if (cells[y][x]) ink(0, 0, 0).box(qrX + 1 + x, qrY + 1 + y, 1, 1);
    }
  }

  // ⚙️ Settings toggle — a little hamburger by the QR ("here by the QR").
  const gw = 13;
  const gh = 13;
  const gx = qrX - gw - 4;
  const gy = qrY + Math.floor((qrBoxSize - gh) / 2);
  gearBox = { x: gx - 2, y: gy - 2, w: gw + 4, h: gh + 4 }; // padded hit area
  ink(settingsOpen ? [255, 240, 120] : [255, 255, 255, 200]).box(gx, gy, gw, gh, "outline");
  const lineCol = settingsOpen ? [255, 240, 120] : [255, 255, 255, 220];
  for (let li = 0; li < 3; li++) {
    ink(...lineCol).box(gx + 3, gy + 3 + li * 3, gw - 6, 1);
  }
}

// ⚙️ The settings pane — mode / tema / filter, drawn under the header by the
// QR. Chips register their hit boxes into `settingsHits` for act().
function paintSettings($) {
  settingsHits = [];
  if (!settingsOpen) return;
  const { ink, screen } = $;
  const themed = LAK_THEMES[lakTheme];

  const rows = [
    {
      label: "mode",
      chips: [
        { text: "raster", selected: true, action: { type: "mode", value: "raster" } },
        { text: "vector", selected: false, action: { type: "mode", value: "vector" } },
      ],
    },
    {
      label: "tema",
      chips: Object.keys(LAK_THEMES).map((name) => ({
        text: name,
        selected: lakTheme === name,
        action: { type: "theme", value: name },
      })),
    },
    {
      label: "filter",
      chips: [
        { text: "alle", selected: !lakLinksOnly, action: { type: "links", value: false } },
        { text: "links", selected: lakLinksOnly, action: { type: "links", value: true } },
      ],
    },
  ];

  const chipH = 11;
  const rowH = 16;
  const padX = 6;
  const labelW = 34;
  const chipFont = "MatrixChunky8";
  const chipW = (t) => t.length * 5 + 8;

  let paneW = 0;
  for (const row of rows) {
    let w = labelW;
    for (const chip of row.chips) w += chipW(chip.text) + 4;
    paneW = Math.max(paneW, w);
  }
  paneW += padX * 2;
  const paneH = rows.length * rowH + 14 + 8;
  const paneX = Math.max(2, screen.width - paneW - 3);
  const paneY = LAK_TOP_MARGIN + 2;

  ink(themed.stripeA[0], themed.stripeA[1], themed.stripeA[2], 245).box(paneX, paneY, paneW, paneH);
  ink(255, 240, 120).box(paneX, paneY, paneW, paneH, "outline");
  ink(255, 240, 120).write("indstillinger", { x: paneX + padX, y: paneY + 4 }, undefined, undefined, false, chipFont);

  let rowY = paneY + 14 + 4;
  for (const row of rows) {
    ink(255, 255, 255, 180).write(row.label, { x: paneX + padX, y: rowY + 2 }, undefined, undefined, false, chipFont);
    let chipX = paneX + padX + labelW;
    for (const chip of row.chips) {
      const w = chipW(chip.text);
      if (chip.selected) {
        ink(255, 240, 120).box(chipX, rowY, w, chipH);
        ink(20, 10, 6).write(chip.text, { x: chipX + 4, y: rowY + 2 }, undefined, undefined, false, chipFont);
      } else {
        ink(255, 255, 255, 120).box(chipX, rowY, w, chipH, "outline");
        ink(255, 255, 255, 200).write(chip.text, { x: chipX + 4, y: rowY + 2 }, undefined, undefined, false, chipFont);
      }
      settingsHits.push({ x: chipX, y: rowY, w, h: chipH, action: chip.action });
      chipX += w + 4;
    }
    rowY += rowH;
  }

  // Remember the pane bounds so act() can tell inside from outside.
  settingsHits.pane = { x: paneX, y: paneY, w: paneW, h: paneH };
}

function paint($) {
  const themed = LAK_THEMES[lakTheme];
  chat.paint($, {
    otherChat: chatView(),
    hideChrome: true,
    topMargin: LAK_TOP_MARGIN, // Shorter top chrome panel than the default 42.
    attachAfterInput: true,
    inputPlaceholder: "Chat...",
    presenceOnlineOnly: true,
    presenceRightInset: 50, // QR box + gear + breathing room.
    // 🎪 Circus marquee as the header backdrop — fills the whole chrome panel,
    // painted under the online counter so the counter stays readable on top.
    paintHeader: (api, tm) => paintLaerKlokkenSign(api, tm),
    presenceTop: 24, // "N online" counter sits low in the header, over the marquee.
    theme: themed.chat,
  });

  // 🎪 The circus marquee is painted as the chat header backdrop (via the
  // paintHeader option above), so it fills the header under the counter.
  // 📱 laklok.com QR + ⚙ gear, top-right corner (over everything).
  paintQR($);
  paintSettings($);
}

function act($) {
  const { event: e, jump, store, needsPaint } = $;

  const hit = (box) =>
    box && e.x >= box.x && e.x < box.x + box.w && e.y >= box.y && e.y < box.y + box.h;

  // ⚙️ Gear toggles the pane; while open, the pane owns every pointer event
  // so a tap on a chip never scrolls the chat underneath.
  if (e.is("touch") && hit(gearBox)) {
    settingsOpen = !settingsOpen;
    needsPaint?.();
    return;
  }

  if (settingsOpen && (e.is("touch") || e.is("draw") || e.is("lift"))) {
    if (e.is("touch")) {
      const chip = settingsHits.find((h) => hit(h));
      if (chip) {
        const { type, value } = chip.action;
        if (type === "mode" && value === "vector") {
          jump("out:https://laklok.com/html/");
        } else if (type === "theme") {
          lakTheme = value;
          store["laklok:theme"] = value;
          store.persist("laklok:theme");
          chat.refresh(client.system); // recolor cached message lines
        } else if (type === "links") {
          lakLinksOnly = value;
          store["laklok:links"] = value;
          store.persist("laklok:links");
          chat.refresh(client.system); // relayout the filtered feed
        }
      } else if (!hit(settingsHits.pane)) {
        settingsOpen = false; // Tap outside closes.
      }
      needsPaint?.();
    }
    return;
  }

  chat.act($, chatView(), { allowDelete: true });
}

function sim($) {
  chat.sim($);
}

function leave() {
  client.kill();
}


// 📚 Library

// function beat() {
//   // Runs once per system metronome (BPM) tick.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
