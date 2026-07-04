#!/usr/bin/env node
// gen-pals.mjs — generate themed "pals" icon variations with gpt-image-2,
// using the pals mark as the image reference (same technique as the /pop
// Spotify avatar + fuser icon). Candidate artwork for the AC Readings /
// "Aesthetic Dot Computer" podcast cover.
//
// Usage: node bin/gen-pals.mjs [--force] [--only crystal,felt] [--size 1024x1024]
//   → out/pals/<slug>.png (+ a contact sheet out/pals/contact.png)

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..", "..");
const OUT = resolve(ROOT, "out", "pals");
mkdirSync(OUT, { recursive: true });

const argv = process.argv.slice(2);
const flag = (k) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : null; };
const FORCE = argv.includes("--force");
const SIZE = flag("size") || "1024x1024";
const ONLY = flag("only") ? new Set(flag("only").split(",")) : null;

// ── API key (env → vault devcontainer.env) ─────────────────────────────
function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const envFile = resolve(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  const line = readFileSync(envFile, "utf8").split("\n").find((l) => l.startsWith("OPENAI_API_KEY="));
  if (!line) throw new Error("OPENAI_API_KEY not found");
  return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
}
const KEY = loadKey();

// ── pals reference raster (pre-baked 2048² mark) ───────────────────────
const REF = [
  resolve(REPO, "pop/hellsine/assets/pals-logo.png"),
  resolve(REPO, "assets/aesthetic-inc/pals.png"),
].find(existsSync);
if (!REF) throw new Error("no pals reference raster found");

// The pals mark = a plump, rounded, organic hand-drawn squiggle (flat pink
// #cd5c9b). Every prompt keeps its SHAPE and swaps the material/vibe.
const MARK = "the plump, rounded, organic hand-drawn squiggle brand mark from the reference image, reproduced faithfully as the single central emblem — its exact looping shape preserved";
const RULES = "Square composition, the mark centered with a generous calm margin so a round or square crop never clips it. One coherent studio object, no collage. Original artwork — no real brand names, no trademarked wordmarks, no other logos, and absolutely no lettering or text anywhere. No motion blur; crisp, sharp, high detail throughout.";

const THEMES = [
  { slug: "crystal", prompt: `A hyper-real faceted emerald-green crystal sculpture of ${MARK}, rendered as a glowing gemstone with fine internal glitch-crack fractures lit from within, floating on a near-black background with subtle green bloom. Spotify-green and black. ${RULES}` },
  { slug: "felt", prompt: `A cozy needle-felted wool version of ${MARK} in warm magenta-pink, soft fuzzy fibers and gentle hand-craft imperfection, resting on a cream felt background under soft daylight. Pastel, tactile, handmade. ${RULES}` },
  { slug: "glass", prompt: `An iridescent hand-blown glass sculpture of ${MARK}, translucent with soft internal rainbow refractions and glossy highlights, on a pale pastel gradient background under soft studio light. Dreamy, delicate. ${RULES}` },
  { slug: "chrome", prompt: `A mirror-polished liquid chrome sculpture of ${MARK}, high-gloss reflective metal with smooth Y2K blob highlights, on a cool lilac-to-silver gradient background. Sleek, futuristic. ${RULES}` },
  { slug: "ceramic", prompt: `A glossy kiln-glazed ceramic sculpture of ${MARK} in soft blush-pink porcelain with a subtle crackle glaze and gentle specular sheen, on a warm neutral studio background. Handmade, ceramic-studio. ${RULES}` },
  { slug: "neon", prompt: `A glowing hot-pink neon tube shaped as ${MARK}, luminous with soft bloom and a faint holographic sheen, mounted on a deep midnight-blue wall. Nightlife, electric. ${RULES}` },
  { slug: "risograph", prompt: `A layered cut-paper and risograph-textured illustration of ${MARK} in flat pastel pink and coral with visible paper grain and slight ink misregistration, on a warm off-white background. Flat, printy, editorial. ${RULES}` },
  { slug: "wood", prompt: `A hand-carved warm walnut wood sculpture of ${MARK} with visible grain and soft rounded edges, tinted faintly rosy, on a honey-toned background under soft directional light. Warm, crafted, organic. ${RULES}` },
];

// ── avatars tray: SMALLER, circle-safe pals for IG/Spotify/round crops —
// varied focal size, colorway, material, contrast, and vibe. ─────────────
const AV = "Square composition on a clean flat background. Original artwork — no real brand names, no wordmarks, no other logos, no lettering or text anywhere. No motion blur; crisp and sharp.";
const AVATARS = [
  { slug: "av-glass-pastel",  prompt: `A SMALL iridescent hand-blown glass ${MARK}, sitting compact in the exact center with a lot of soft empty margin all around it (fits easily inside a circle crop). Pale pastel pink-lilac background, soft studio light, low contrast, dreamy and calm. ${AV}` },
  { slug: "av-neon-punch",    prompt: `A small glowing hot-magenta neon-tube ${MARK}, centered with generous black margin around it. Deep pure-black background, very high contrast, punchy and electric, bold nightlife vibe. ${AV}` },
  { slug: "av-chrome-cool",   prompt: `A small mirror-polished liquid chrome ${MARK}, centered with wide margin. Cool blue-to-silver gradient background, high contrast, sleek Y2K futuristic. ${AV}` },
  { slug: "av-felt-cozy",     prompt: `A small needle-felted wool ${MARK} in warm rose-pink, centered with lots of cream margin. Soft cream felt background, low contrast, cozy and tactile. ${AV}` },
  { slug: "av-holo-foil",     prompt: `A small holographic iridescent foil ${MARK}, centered with clean margin. Soft silver-white background with rainbow sheen, high contrast, shiny and modern. ${AV}` },
  { slug: "av-jelly-candy",   prompt: `A small glossy translucent candy-jelly ${MARK} in bright cherry-pink, centered with airy margin. Soft mint background, medium-high contrast, playful and juicy. ${AV}` },
  { slug: "av-clay-terra",    prompt: `A small matte terracotta clay ${MARK}, centered with warm margin. Soft sand background, low contrast, earthy and handmade. ${AV}` },
  { slug: "av-amethyst",      prompt: `A medium faceted amethyst crystal ${MARK}, centered with clean margin. Deep violet background with soft purple bloom, high contrast, gemmy and rich. ${AV}` },
  { slug: "av-gold-lux",      prompt: `A small brushed 3D gold ${MARK}, centered with wide margin. Pure black background, high contrast, luxe and minimal. ${AV}` },
  { slug: "av-balloon",       prompt: `A small glossy inflatable balloon ${MARK} in candy red, centered with airy margin. Clean pale-blue background, high contrast, fun and bouncy. ${AV}` },
];

const TRAYS = { materials: THEMES, avatars: AVATARS };

async function gen(theme) {
  const out = resolve(OUT, `${theme.slug}.png`);
  if (existsSync(out) && !FORCE) { console.log(`  · ${theme.slug} cached`); return out; }
  process.stdout.write(`  → ${theme.slug} …`);
  const ATT = 4;
  for (let a = 1; a <= ATT; a++) {
    try {
      const fd = new FormData();
      fd.append("model", "gpt-image-2");
      fd.append("prompt", theme.prompt);
      fd.append("size", SIZE);
      fd.append("quality", "high");
      fd.append("n", "1");
      fd.append("image[]", new Blob([readFileSync(REF)], { type: "image/png" }), "pals.png");
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST", headers: { Authorization: `Bearer ${KEY}` }, body: fd,
      });
      if (!res.ok) { const t = await res.text(); if (res.status >= 500 && a < ATT) throw new Error(`HTTP ${res.status}`); process.stdout.write(" ✗\n"); throw new Error(`${res.status}: ${t.slice(0, 200)}`); }
      const j = await res.json();
      writeFileSync(out, Buffer.from(j.data[0].b64_json, "base64"));
      process.stdout.write(" ✓\n");
      return out;
    } catch (e) {
      if (a < ATT) { process.stdout.write(` retry${a}`); await new Promise((r) => setTimeout(r, 1500 * a)); }
      else { process.stdout.write(" ✗\n"); console.error(`    ${theme.slug}: ${e.message}`); return null; }
    }
  }
}

const tray = flag("tray") || "materials";
const themes = TRAYS[tray] || THEMES;
const list = themes.filter((t) => !ONLY || ONLY.has(t.slug));
console.log(`\nGenerating ${list.length} pals · tray "${tray}" (${SIZE}) from ${REF.split("/").slice(-2).join("/")}:\n`);
const made = [];
for (const t of list) { const f = await gen(t); if (f) made.push({ slug: t.slug, file: f }); }

// contact sheet for picking (untitled tiles — magick font annotation is flaky here)
if (made.length) {
  const tiles = made.map((m) => { const t = resolve(OUT, `.t-${m.slug}.png`); execFileSync("magick", [m.file, "-resize", "440x440", t]); return t; });
  const cols = made.length > 8 ? 5 : 4;
  const contact = resolve(OUT, `contact-${tray}.png`);
  execFileSync("magick", ["montage", ...tiles, "-tile", `${cols}x`, "-geometry", "+8+8", "-background", "#111", contact]);
  execFileSync("rm", ["-f", ...tiles]);
  console.log(`\n✓ ${made.length} generated → out/pals/`);
  console.log(`  contact sheet · ${contact}`);
}
