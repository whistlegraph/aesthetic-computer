// Laer-klokken, 2025.5.08.16.31.51.182
// Learn the 'clock'!

/* 📝 Notes
 */

import { Chat } from "../lib/chat.mjs"; // TODO: Eventually expand to `net.Socket`
import * as chat from "./chat.mjs"; // Import chat everywhere.

let client;

function boot({ api, wipe, debug, send }) {
  wipe("aqua");
  client = new Chat(debug, send);
  client.connect("clock"); // Connect to 'clock' chat.
  chat.boot(api, client.system);
}

function paint($) {
  chat.paint($, { otherChat: client.system });
}

function act($) {
  chat.act($, client.system);
}

function sim($) {
  chat.sim($);
}

function leave() {
  console.log(client.kill);
  client.kill()
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
