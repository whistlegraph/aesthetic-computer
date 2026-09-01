// Hand the game's source straight to the page. mac-test.html checks
// __fightPieceSource before it fetches /oskiewar.js, and taking that branch
// also skips the two-second update poller behind it — a live-reload affordance
// for oskiewar.com that a shipped Steam build has no use for.

const { contextBridge } = require("electron");
const { existsSync, readFileSync } = require("node:fs");
const { join, resolve } = require("node:path");

// Mirrors main.js: staged beside a packaged app, xbox/live in the working tree.
const staged = join(process.resourcesPath || "", "staged");
const live = existsSync(staged) ? staged : resolve(__dirname, "../../live");

try {
  contextBridge.exposeInMainWorld(
    "__fightPieceSource",
    readFileSync(join(live, "oskiewar.js"), "utf8"),
  );
} catch (error) {
  // Falling through is survivable: the page fetches app://local/oskiewar.js
  // instead, which the protocol handler serves off the same disk.
  console.error(`[shell] could not preload the piece: ${error.message}`);
}
