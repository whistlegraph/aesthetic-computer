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
  // 🏷️ Ensure label shows piece name (not "chat")
  hud.label("laklok");
  // ✨ Show ".com" superscript in the HUD corner label (laklok.com branding,
  // same as notepat). disk.mjs renders the ".com" case specially.
  hud.superscript(".com");
}

// 🪧 Flashy decorative "Laer Klokken" sign, masked into the top header strip
// at the top-right. Warm hues cycle over time for a little flair.
function paintLaerKlokkenSign($) {
  const { ink, box, write, screen } = $;
  const t = (typeof performance !== "undefined" ? performance.now() : 0) * 0.004;
  const label = "Laer Klokken";
  const sw = label.length * 6 + 7; // ~6px per glyph in the default font + padding
  const sx = screen.width - sw - 3;
  const sy = 2;
  const sh = 11;
  // Header mask: only paint when there's room in the top strip.
  if (sx < 2 || screen.width < sw + 8) return;
  // Flashy warm color cycle.
  const r = Math.max(0, Math.min(255, 225 + Math.round(Math.sin(t) * 30)));
  const g = Math.max(0, Math.min(255, 150 + Math.round(Math.sin(t + 2.1) * 70)));
  const b = Math.max(0, Math.min(255, 90 + Math.round(Math.sin(t + 4.2) * 60)));
  const glow = 0.5 + 0.5 * Math.sin(t * 1.7);
  ink(30, 14, 8, 220).box(sx - 1, sy - 1, sw + 2, sh + 2); // sign backing
  ink(r, g, b, 120 + Math.round(glow * 135)).box(sx - 1, sy - 1, sw + 2, sh + 2, "outline");
  ink(20, 10, 6).write(label, { x: sx + 4, y: sy + 3 }); // drop shadow
  ink(r, g, b).write(label, { x: sx + 3, y: sy + 2 });
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
