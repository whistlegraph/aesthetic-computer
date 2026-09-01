#!/usr/bin/env node
// Copy the runtime into staged/ so a packaged build carries its own files
// instead of reading the repo. Deliberately a copy and not a symlink: the
// depot upload has to see real bytes, and a build that silently depends on a
// working tree is a build that breaks on someone else's machine.
//
// The page reaches outside xbox/live for six things; that list is duplicated
// in main.js and xbox/tools/serve-live.mjs, and all three move together.

import { cp, mkdir, readdir, rm } from "node:fs/promises";
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
]);

await rm(staged, { recursive: true, force: true });
await mkdir(staged, { recursive: true });

// oskiewar.js is the game; every .mjs in xbox/live is either imported by the
// page or small enough that proving it isn't costs more than shipping it.
// The megabyte .js files in there belong to other pieces and stay behind.
const wanted = (await readdir(live))
  .filter((name) => name.endsWith(".mjs") || name === "oskiewar.js");

for (const name of wanted) await cp(join(live, name), join(staged, name));
await cp(join(live, "mac-test.html"), join(staged, "index.html"));

for (const [to, from] of outside) {
  const target = join(staged, to);
  await mkdir(dirname(target), { recursive: true });
  await cp(join(repo, from), target);
}

console.log(`staged ${wanted.length + 1 + outside.size} files → ${staged}`);
