// laklok, 2025.5.08.16.31.51.182
// Learn the 'clock'! (formerly `laer-klokken`; that path still aliases here.)
// 🌐 Backed by the branded domain laklok.com.

/* 📝 Notes
 */

import { Chat } from "../lib/chat.mjs"; // TODO: Eventually expand to `net.Socket`
import * as chat from "./chat.mjs"; // Import chat everywhere.

let client;

function boot({ api, wipe, debug, send, hud }) {
  client = new Chat(debug, send);
  client.connect("clock"); // Connect to 'clock' chat. (DB stays `chat-clock`.)
  chat.boot(api, client.system); // Use default font

  // 📱 Set QR code to appear LEFT of the HUD label (qr-stamp-label mode)
  hud.qr("https://laklok.com");
  // 🏷️ Ensure label shows piece name (not "chat"), pinned white so it doesn't
  // ride the red/orange/lime connection-status color.
  hud.label("laklok", "white");
  // ✨ Show ".com" superscript in the HUD corner label (laklok.com branding,
  // same as notepat). disk.mjs renders the ".com" case specially.
  hud.superscript(".com");
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
function paintLaerKlokkenSign($) {
  const { ink, box, write, screen, text } = $;
  const now = typeof performance !== "undefined" ? performance.now() : 0;
  const t = now * 0.004;
  const label = "Laer Klokken";

  // Measure each glyph in Unifont so per-character placement is exact.
  const widths = [];
  let total = 0;
  for (let i = 0; i < label.length; i++) {
    const w = text?.width ? text.width(label[i], LAK_SIGN_FONT) || 8 : 8;
    widths.push(w);
    total += w;
  }
  const pad = 12;
  const headerH = 42; // chat's top header band (topMargin) — fill it fully
  const sx = 0;
  const sy = 1;
  const sw = screen.width; // full-width banner across the header
  const sh = headerH - 2;
  // Reserve the top-left for the HUD corner label (QR + "laklok" + ".com") so
  // the banner letters don't run underneath it.
  const leftClear = 108;
  const textLeft = sx + leftClear;
  const textRight = sx + sw - pad;
  const span = textRight - textLeft;
  if (span < total) return; // not enough room clear of the HUD corner label
  // Justify the letters across the remaining width (spread out, marquee style).
  const gap = label.length > 1 ? Math.max(4, Math.min(60, (span - total) / (label.length - 1))) : 0;

  // Striped circus backdrop (red / cream), slowly scrolling like a barber pole.
  const stripeW = 6;
  const scroll = Math.floor(t * 6);
  for (let bx = 0; bx < sw; bx += stripeW) {
    const odd = Math.floor((bx + scroll) / stripeW) % 2;
    ink(odd ? [92, 20, 28] : [54, 32, 20]).box(sx + bx, sy, Math.min(stripeW, sw - bx), sh);
  }
  // Each character: its own circus color + individual vertical bounce.
  let cx = textLeft;
  const baseY = sy + Math.round((sh - 16) / 2); // 16 ≈ unifont glyph height, vertically centered
  for (let i = 0; i < label.length; i++) {
    const ch = label[i];
    if (ch !== " ") {
      const bounce = Math.round(Math.sin(t * 2.6 + i * 0.7) * 2);
      const c = LAK_CIRCUS_COLS[i % LAK_CIRCUS_COLS.length];
      ink(20, 10, 6).write(ch, { x: cx + 1, y: baseY + bounce + 1 }, undefined, undefined, false, LAK_SIGN_FONT);
      ink(c[0], c[1], c[2]).write(ch, { x: cx, y: baseY + bounce }, undefined, undefined, false, LAK_SIGN_FONT);
    }
    cx += widths[i];
  }
}

function paint($) {
  // Custom warm color theme for laklok chat
  chat.paint($, {
    otherChat: client.system,
    hideChrome: true,
    theme: {
      background: [180, 100, 60], // Warm terracotta/rust background
      chromeBg: [180, 100, 60], // Match background — no dark banners above/below fold
      lines: [220, 150, 100, 64], // Soft peach lines
      scrollbar: [255, 180, 100], // Warm orange scrollbar
      messageText: [255, 255, 240], // Brighter cream/off-white text for better contrast
      messageBox: [255, 220, 180], // Warm beige for message boxes
      log: [100, 255, 220], // Bright cyan/teal for system log messages
      logHover: [255, 240, 120], // Bright golden yellow on hover
      handle: [255, 160, 120], // Brighter coral for handles
      handleHover: [255, 240, 120], // Bright golden yellow on hover
      url: [120, 220, 255], // Brighter light blue for contrast
      urlHover: [255, 240, 120], // Bright golden yellow on hover
      prompt: [200, 255, 180], // Brighter soft green for prompts
      promptContent: [120, 220, 255], // Light blue like urls
      promptHover: [255, 240, 120], // Bright golden yellow on hover
      promptContentHover: [255, 240, 120], // Bright golden yellow on hover
      painting: [255, 200, 140], // Brighter peachy orange for paintings
      paintingHover: [255, 240, 120], // Bright golden yellow on hover
      kidlisp: [255, 140, 200], // Warm pink/magenta for kidlisp
      kidlispHover: [255, 240, 120], // Bright golden yellow on hover
      timestamp: [220, 180, 150], // Much brighter/lighter brown for timestamps
      timestampHover: [255, 240, 120], // Bright golden yellow on hover
      heart: [255, 220, 240], // Light pink — pops on warm rust background
    }
  });

  // Decorative flashy sign on top of the header chrome.
  paintLaerKlokkenSign($);
}

function act($) {
  chat.act($, client.system, { allowDelete: true });
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
