// laklok, 2025.5.08.16.31.51.182
// Learn the 'clock'! (formerly `laer-klokken`; that path still aliases here.)
// 🌐 Backed by the branded domain laklok.com.

/* 📝 Notes
 */

import { Chat } from "../lib/chat.mjs"; // TODO: Eventually expand to `net.Socket`
import * as chat from "./chat.mjs"; // Import chat everywhere.
import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

let client;

// 📐 Top chrome height — shared between chat.paint (topMargin) and the circus
// marquee so the banner fills the whole header down to the margin line.
const LAK_TOP_MARGIN = 34;

// 📱 laklok.com QR rendered in the top-right corner (see paintQR).
let lakQRCells = null;

function boot({ api, wipe, debug, send, hud }) {
  client = new Chat(debug, send);
  client.connect("clock"); // Connect to 'clock' chat. (DB stays `chat-clock`.)
  chat.boot(api, client.system); // Use default font

  // 🚫 chat.boot stamps a prompt.ac/chat QR to the LEFT of the HUD label; clear
  // it so laklok shows only its own laklok.com QR in the top-right (paintQR).
  hud.qr(null);

  // 📱 Generate the laklok.com QR once; painted top-right each frame.
  try {
    lakQRCells = qr("https://laklok.com").modules;
  } catch (e) {
    console.error("laklok QR generation failed:", e);
    lakQRCells = null;
  }
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
function paintLaerKlokkenSign($, headerHeight = LAK_TOP_MARGIN) {
  const { ink, box, write, screen, text } = $;
  const now = typeof performance !== "undefined" ? performance.now() : 0;
  const t = now * 0.004;
  const label = "Laer Klokken";

  // Unifont latin glyphs are a fixed 8px advance — no need to measure per frame.
  const CHAR_W = 8;
  const total = label.length * CHAR_W;
  const headerH = headerHeight; // marquee fills the header down to the margin
  const sx = 0;
  const sy = 0; // flush with the top of the screen (no gap)
  const sw = screen.width; // full-width banner across the header
  const sh = headerH;
  // Spread the letters with a fixed gap, then center the whole word group on
  // the overall screen width (sits mid-screen, clear of the top-left HUD label).
  const gap = 8;
  const lettersW = total + gap * Math.max(0, label.length - 1);
  if (lettersW > sw - 4) return; // too wide for the screen
  const textLeft = Math.round(sx + (sw - lettersW) / 2);

  // Striped circus backdrop, slowly scrolling like a barber pole. Tuned to the
  // warm dark-orange of the laklok rust background so it reads as one piece.
  const stripeW = 14;
  const scroll = Math.floor(t * 6);
  for (let bx = 0; bx < sw; bx += stripeW) {
    const odd = Math.floor((bx + scroll) / stripeW) % 2;
    ink(odd ? [150, 78, 34] : [122, 60, 26]).box(sx + bx, sy, Math.min(stripeW, sw - bx), sh);
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
    cx += CHAR_W + gap;
  }
}

// 📱 laklok.com QR code, pinned to the top-right corner with a white border.
function paintQR($) {
  if (!lakQRCells) return;
  const { ink, screen } = $;
  const cells = lakQRCells;
  const size = cells.length; // 1px per cell
  const margin = 3;
  const qrX = screen.width - size - 1 - margin; // -1 leaves room for the border
  const qrY = margin;
  ink(255, 255, 255).box(qrX, qrY, size + 2, size + 2); // white background + border
  for (let y = 0; y < size; y++) {
    for (let x = 0; x < size; x++) {
      if (cells[y][x]) ink(0, 0, 0).box(qrX + 1 + x, qrY + 1 + y, 1, 1);
    }
  }
}

function paint($) {
  // Custom warm color theme for laklok chat
  chat.paint($, {
    otherChat: client.system,
    hideChrome: true,
    topMargin: LAK_TOP_MARGIN, // Shorter top chrome panel than the default 42.
    // 🎪 Circus marquee as the header backdrop — fills the whole chrome panel,
    // painted under the online counter so the counter stays readable on top.
    paintHeader: (api, tm) => paintLaerKlokkenSign(api, tm),
    presenceTop: 24, // "N online" counter sits low in the header, over the marquee.
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

  // 🎪 The circus marquee is painted as the chat header backdrop (via the
  // paintHeader option above), so it fills the header under the counter.
  // 📱 laklok.com QR, top-right corner (over everything).
  paintQR($);
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
