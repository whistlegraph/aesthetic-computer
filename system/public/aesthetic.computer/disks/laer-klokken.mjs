// Laer-klokken, 2025.5.08.16.31.51.182
// Learn the 'clock'!

/* 📝 Notes
 */

import { Chat } from "../lib/chat.mjs"; // TODO: Eventually expand to `net.Socket`
import * as chat from "./chat.mjs"; // Import chat everywhere.

let client;

function boot({ api, wipe, debug, send, hud }) {
  client = new Chat(debug, send);
  client.connect("clock"); // Connect to 'clock' chat.
  chat.boot(api, client.system); // Use default font
  
  // � Use MatrixChunky8 (tiny) font for HUD label
  hud.tinyLabel(true);
  // 📱 Set QR code to appear LEFT of the HUD label (qr-stamp-label mode)
  hud.qr("https://prompt.ac/laer-klokken");
  // 🏷️ Ensure label shows piece name (not "chat")
  hud.label("laer-klokken");
}

function paint($) {
  // Custom warm color theme for laer-klokken chat
  chat.paint($, { 
    otherChat: client.system,
    // r8dioPlayer removed per community request
    theme: {
      background: [180, 100, 60], // Warm terracotta/rust background
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
    }
  });
  
  // 🏷️ Draw animated "laer-klokken" label with shadow and character-by-character color cycling
  // (Styled like prompt HUD corner label)
  const labelText = "laer-klokken";
  const labelX = 6;
  const labelY = 2;
  const charWidth = 4; // MatrixChunky8 char width
  const frame = $.help?.repeat || 0;
  
  // Warm color palette for animation (matching theme)
  const colors = [
    [255, 180, 100], // Warm orange
    [255, 200, 140], // Peachy
    [255, 160, 120], // Coral
    [255, 220, 180], // Warm beige
    [220, 150, 100], // Soft peach
    [255, 140, 100], // Tangerine
  ];
  
  // Draw each character with shadow and cycling color
  for (let i = 0; i < labelText.length; i++) {
    const char = labelText[i];
    const charX = labelX + i * charWidth;
    
    // Shadow (1px right and down)
    $.ink(0, 0, 0, 200).write(char, { x: charX + 1, y: labelY + 1 }, false, undefined, false, "MatrixChunky8");
    
    // Main character with cycling color based on position and frame
    const colorIndex = (i + Math.floor(frame / 8)) % colors.length;
    const color = colors[colorIndex];
    $.ink(...color).write(char, { x: charX, y: labelY }, false, undefined, false, "MatrixChunky8");
  }
}

function act($) {
  chat.act($, client.system);
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
