#!/usr/bin/env node
// catalog-art.mjs — derive the small images pop.aesthetic.computer serves
// from each release's full-size cover: thumbs/<slug>.jpg (112²) for the
// catalog rows, art/<slug>.jpg (480²) for the jukebox deck, and pop-og.jpg
// (1200×630, the eight newest covers) for link previews. Run after
// publish-release-records.mjs whenever a cover is added or replaced.
//
//   node pop/bin/catalog-art.mjs            # every track in the catalog
//   node pop/bin/catalog-art.mjs <slug>     # just one

import { mkdir, readFile, writeFile } from "node:fs/promises";
import { execFileSync } from "node:child_process";
import { dirname, join, resolve } from "node:path";
import { tmpdir } from "node:os";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const SITE = join(ROOT, "system/public/pop.aesthetic.computer");
const CACHE = join(tmpdir(), "pop-covers");
const only = process.argv[2];

const catalog = JSON.parse(await readFile(join(SITE, "releases/catalog.json"), "utf8"));
const tracks = catalog.tracks.filter((t) => t.cover && (!only || t.slug === only));
await Promise.all([mkdir(CACHE, { recursive: true }), mkdir(join(SITE, "thumbs"), { recursive: true }), mkdir(join(SITE, "art"), { recursive: true })]);

const covers = [];
for (const track of tracks) {
  const src = join(CACHE, `${track.slug}.jpg`);
  const response = await fetch(track.cover);
  if (!response.ok) { console.warn(`skip ${track.slug}: cover ${response.status}`); continue; }
  await writeFile(src, Buffer.from(await response.arrayBuffer()));
  for (const [dir, size, quality] of [["thumbs", 112, 80], ["art", 480, 84]]) {
    execFileSync("magick", [src, "-resize", `${size}x${size}^`, "-gravity", "center", "-extent", `${size}x${size}`,
      "-strip", "-quality", String(quality), join(SITE, dir, `${track.slug}.jpg`)]);
  }
  covers.push(src);
  console.log(`${track.slug}: thumbs/ + art/`);
}

if (!only && covers.length) {
  const og = join(SITE, "pop-og.jpg");
  const tile = (files) => ["(", ...files, "-resize", "300x300^", "-gravity", "center", "-extent", "300x300", "+append", ")"];
  execFileSync("magick", [...tile(covers.slice(0, 4)), ...tile(covers.slice(4, 8)), "-append",
    "-background", "#1d1d24", "-gravity", "center", "-extent", "1200x630", "-strip", "-quality", "84", og]);
  console.log("pop-og.jpg");
}
