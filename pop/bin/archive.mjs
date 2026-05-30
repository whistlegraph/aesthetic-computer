#!/usr/bin/env node
// archive.mjs — per-track media stager for pop/.
//
// Copies a track's DURABLE media out of pop/<lane>/out/ into the
// assets-system mirror system/public/assets/pop/<lane>/<slug>/ so
// `npm run pop:assets:up` can back it to DigitalOcean Spaces.
//
// Durable = the deliverable + the bed + the BILLABLE jeffrey-pvc vocal
// stem and its say.mjs cache keys (restoring those makes future
// sing.mjs re-renders free on any machine). Intermediates
// (*-pitched.mp3, *-stretched.mp3, out/.tmp/) are NOT staged — they
// regenerate locally from the vocal stem in seconds.
//
// See pop/ASSETS.md for the full story.
//
// Usage:
//   node pop/bin/archive.mjs <lane> <slug>
//   node pop/bin/archive.mjs hippyhayzard hippyhayzard

import { existsSync, mkdirSync, copyFileSync, statSync, readdirSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const [lane, slug] = process.argv.slice(2);
if (!lane || !slug) {
  console.error("usage: node pop/bin/archive.mjs <lane> <slug>");
  process.exit(1);
}

const HERE = dirname(fileURLToPath(import.meta.url));   // pop/bin
const POP = resolve(HERE, "..");                         // pop
const REPO = resolve(POP, "..");                         // repo root
const OUT = `${POP}/${lane}/out`;
const DEST = `${REPO}/system/public/assets/pop/${lane}/${slug}`;
const VDEST = `${DEST}/vocal`;

if (!existsSync(OUT)) {
  console.error(`✗ no out dir for lane '${lane}': ${OUT}`);
  process.exit(1);
}
mkdirSync(DEST, { recursive: true });

let n = 0;
function take(srcName, destDir, destName = srcName) {
  const src = `${OUT}/${srcName}`;
  if (!existsSync(src)) return false;
  mkdirSync(destDir, { recursive: true });
  const dst = `${destDir}/${destName}`;
  copyFileSync(src, dst);
  const kb = (statSync(dst).size / 1024).toFixed(0);
  console.log(`  + ${basename(destDir)}/${destName}  (${kb} KB)`);
  n++;
  return true;
}

console.log(`▸ archiving ${lane}/${slug} → ${DEST.replace(REPO + "/", "")}`);

// final deliverable: prefer the sung song, else the bed itself
if (!take(`${slug}-song.mp3`, DEST, `${slug}.mp3`)) {
  take(`${slug}.mp3`, DEST, `${slug}.mp3`);
}
// the instrumental bed (cheap, kept for convenience / remixing)
take(`${slug}.mp3`, DEST, "bed.mp3");

// the billable vocal stem + its cache keys (the must-preserve assets)
for (const f of [
  `${slug}-vocal.mp3`,
  `${slug}-vocal.mp3.hash`,
  `${slug}-vocal.mp3.alignment.json`,
  `${slug}-vocal-words.json`,
  `${slug}-vocal-words.json.hash`,
  `${slug}-pitched-alignment.json`,
]) take(f, VDEST);

// pixel-art section panels (from pop/bin/gen-pixel-sections.mjs) —
// landed beside the source illys; mirror them straight into
// system/public/assets/pop/<slug>/sec-<N>.pixel.png so they ship
// alongside the audio on `npm run pop:assets:up`.
const SEC_PIXEL_RE = new RegExp(`^${slug}-p-sec-(\\d{1,2})-[^.]+\\.pixel\\.png$`);
const pixelMirror = `${REPO}/system/public/assets/pop/${slug}`;
for (const f of readdirSync(OUT)) {
  const m = f.match(SEC_PIXEL_RE);
  if (!m) continue;
  const idx = parseInt(m[1], 10);
  mkdirSync(pixelMirror, { recursive: true });
  const src = `${OUT}/${f}`;
  const dst = `${pixelMirror}/sec-${idx}.pixel.png`;
  copyFileSync(src, dst);
  const kb = (statSync(dst).size / 1024).toFixed(1);
  console.log(`  + ${slug}/sec-${idx}.pixel.png  (${kb} KB)`);
  n++;
}

if (n === 0) {
  console.error(`✗ nothing staged — no known media in ${OUT}`);
  process.exit(1);
}
console.log(`✓ staged ${n} file(s). next: npm run pop:assets:up`);
