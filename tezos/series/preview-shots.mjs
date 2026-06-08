#!/usr/bin/env node
// tezos/series/preview-shots.mjs
//
// Preview the series WITHOUT the Electron app or login: every piece runs on
// production AC via an inline-source URL. Prints clickable links (live,
// animated) and headlessly screenshots each (via chrome-shot) so we can verify
// each one actually renders before minting.
//
// Usage:
//   node tezos/series/preview-shots.mjs            # links + stills for all
//   node tezos/series/preview-shots.mjs --series clock
//   node tezos/series/preview-shots.mjs --no-shots # just print URLs
//   node tezos/series/preview-shots.mjs --one feedback/drift   # single piece

import fs from "fs";
import path from "path";
import { spawnSync } from "child_process";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const REPO = path.resolve(__dirname, "../..");
const MANIFEST = path.join(__dirname, "manifest.json");
const THUMBS = path.join(__dirname, "thumbs");
const CHROME_SHOT = path.join(REPO, "toolchain/macos/chrome-shot.mjs");

const argv = process.argv.slice(2);
const getArg = (k, d) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : d; };
const ONLY = getArg("series", null);
const ONE = getArg("one", null);
const noShots = argv.includes("--no-shots");

// Inline copy of encodeKidlispForUrl (kidlisp.mjs:15041) — avoids importing the
// giant browser module into node. Keep in sync if the encoder changes.
function encodeKidlispForUrl(source) {
  return source
    .replace(/ /g, "_")
    .replace(/\n/g, "§")
    .replace(/%/g, "¤")
    .replace(/;/g, "¨")
    .replace(/#/g, "%23");
}

// Comment lines (the `; series · title` marker) are noise in a preview URL —
// strip them so the link is clean and starts at the first real expression.
// The minted source keeps its comment; this only affects the preview link.
function stripForUrl(source) {
  return source
    .split("\n")
    .filter((l) => l.trim() && !l.trim().startsWith(";"))
    .join("\n");
}

const manifest = JSON.parse(fs.readFileSync(MANIFEST, "utf8"));
if (!noShots) fs.mkdirSync(THUMBS, { recursive: true });

const pieces = [];
for (const [name, list] of Object.entries(manifest.series)) {
  if (ONLY && name !== ONLY) continue;
  for (const it of list) {
    if (ONE && `${name}/${it.title}` !== ONE) continue;
    pieces.push({ name, ...it });
  }
}

console.log(`\n🔗 Live preview links (production — click to run, animated):\n`);
for (const p of pieces) {
  const enc = encodeKidlispForUrl(stripForUrl(p.source));
  p.url = `https://aesthetic.computer/${enc}`;
  console.log(`  ${`${p.name}/${p.title}`.padEnd(18)} ${p.url}`);
}

if (noShots) process.exit(0);

console.log(`\n📸 Rendering stills (headless, sequential) → ${path.relative(REPO, THUMBS)}/\n`);
const results = [];
for (const p of pieces) {
  const out = path.join(THUMBS, `${p.name}-${p.title}.png`);
  const r = spawnSync("node", [
    CHROME_SHOT, `${p.url}?nogap=true`, out,
    "--size", "480x480", "--budget", "6000", "--wait", "40000",
  ], { stdio: ["ignore", "pipe", "pipe"], encoding: "utf8" });
  let ok = r.status === 0 && fs.existsSync(out);
  let bytes = ok ? fs.statSync(out).size : 0;
  if (ok && bytes < 800) ok = false; // suspiciously tiny = blank/error
  results.push({ id: `${p.name}/${p.title}`, ok, bytes, out });
  console.log(`  ${ok ? "✅" : "❌"} ${`${p.name}/${p.title}`.padEnd(18)} ${ok ? (bytes + "b") : ("status " + r.status)}`);
}

const okN = results.filter((r) => r.ok).length;
console.log(`\n${okN}/${results.length} rendered. Stills in ${path.relative(REPO, THUMBS)}/`);
const bad = results.filter((r) => !r.ok);
if (bad.length) console.log(`Needs a look: ${bad.map((b) => b.id).join(", ")}`);
