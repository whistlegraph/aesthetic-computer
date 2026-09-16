#!/usr/bin/env node
// Copy the runtime into staged/ so a packaged build carries its own files
// instead of reading the repo. Deliberately a copy and not a symlink: the
// depot upload has to see real bytes, and a build that silently depends on a
// working tree is a build that breaks on someone else's machine.
//
// The page reaches outside xbox/live for eight things; that list is duplicated
// in main.js and xbox/tools/serve-live.mjs, and all three move together.

import { cp, mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const live = resolve(here, "../../live");
const repo = resolve(live, "../..");
const staged = join(here, "staged");

const outside = new Map([
  ["aesthetic.computer/dep/@akamfoad/qr/qr.mjs",
    "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs"],
  ["aesthetic.computer/lib/product-analytics.mjs",
    "system/public/aesthetic.computer/lib/product-analytics.mjs"],
  ["aesthetic.computer/lib/oskiewar-analytics.mjs",
    "system/public/aesthetic.computer/lib/oskiewar-analytics.mjs"],
  ["aesthetic.computer/cursors/precise.svg",
    "system/public/aesthetic.computer/cursors/precise.svg"],
  ["aesthetic.computer/cursors/active.svg",
    "system/public/aesthetic.computer/cursors/active.svg"],
  ["ComicRelief-Regular.ttf",
    "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf"],
  ["ComicRelief-Regular.woff2",
    "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.woff2"],
  // account.mjs imports this statically, so without it the page's whole
  // module graph fails and the window stays blank. It ships inert: the
  // account corner is hidden by the trim and nothing calls it.
  ["aesthetic.computer/lib/auth0-otp.mjs",
    "system/public/aesthetic.computer/lib/auth0-otp.mjs"],
]);

// The Steam page is oskiewar.com's page with the web-only furniture taken
// off at stage time rather than kept as a second copy that drifts. Every cut
// is an exact-match edit that throws when the page moves underneath it, so
// a stale trim fails the build instead of quietly shipping a login button.
//
//   - no Open Graph meta: nothing shares an Electron window
//   - no Auth0: the account corner is hidden and its module never preloads
//   - the FPS governor is pinned at full resolution
//   - no QR codes: they encode oskiewar.com links a Steam player cannot use
//   - no Web MIDI: the `?midi` lane is a browser affordance
//   - not versus-capable: no relay, no room link, the local front door
function trim(html) {
  const cuts = [
    [/^  <meta property="og:[^\n]*\n/gm, "", 15],
    ['  <link rel="modulepreload" href="/aesthetic.computer/lib/auth0-otp.mjs">\n', "", 1],
    ["  </style>", "    #account, #account-panel { display: none !important; }\n  </style>", 1],
    ["    let manualResolution = null;", "    let manualResolution = 1;", 1],
    ["    globalThis.qrcode = qrcode;", "    globalThis.qrcode = undefined;", 1],
    ['    if (midiMode !== null && midiMode !== "0" && midiMode !== "off") {',
      '    if (false) {', 1],
    // Not versus-capable: the web front door hosts a relay room and shows
    // "fight a friend oskiewar.com/<room>", a link a Steam player cannot
    // use. Unraised, the game keeps the local front the native shell gets.
    ['      globalThis.__oskiewarVersusCapable = !harness.has("social-preview") &&\n' +
      '        !harness.has("replay-oven") && !harness.has("self-play");',
      '      globalThis.__oskiewarVersusCapable = false; void harness;', 1],
  ];
  for (const [from, to, expected] of cuts) {
    const count = typeof from === "string" ? html.split(from).length - 1 : (html.match(from) || []).length;
    if (count !== expected)
      throw new Error(`trim: expected ${expected} of ${String(from).slice(0, 60)}…, found ${count}`);
    html = html.replaceAll(from, to);
  }
  return html;
}

await rm(staged, { recursive: true, force: true });
await mkdir(staged, { recursive: true });

// oskiewar.js is the game; every .mjs in xbox/live is either imported by the
// page or small enough that proving it isn't costs more than shipping it.
// The megabyte .js files in there belong to other pieces and stay behind.
const wanted = (await readdir(live))
  .filter((name) => name.endsWith(".mjs") || name === "oskiewar.js");

for (const name of wanted) await cp(join(live, name), join(staged, name));
await writeFile(join(staged, "index.html"), trim(await readFile(join(live, "mac-test.html"), "utf8")));

for (const [to, from] of outside) {
  const target = join(staged, to);
  await mkdir(dirname(target), { recursive: true });
  await cp(join(repo, from), target);
}

console.log(`staged ${wanted.length + 1 + outside.size} files → ${staged}`);
