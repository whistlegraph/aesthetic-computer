#!/usr/bin/env node
// gen-keyframes.mjs — the 4 narrative key-frames (gpt-image-2), painterly
// /pop illustration style, jeffrey as a recurring faceless 3/4-back character.
// These seed the Seedance 2 image-to-video clips. See NARRATIVE.md.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
function key() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const v = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(v, "utf8").split("\n"))
    if (line.startsWith("OPENAI_API_KEY=")) return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("no OPENAI_API_KEY");
}

const STYLE =
  "Painterly digital illustration, cinematic 16:9, warm and moody, rich color, soft glow " +
  "and gentle film grain, hand-painted indie-animated-film concept-art feel. Recurring " +
  "character: a man seen from 3/4-BEHIND or in SILHOUETTE — NEVER his face, no front face — " +
  "casual hoodie, a creative coder/painter. Aesthetic Computer palette: warm cream, deep " +
  "violet, citrus-green terminal glow, coral accents. No readable text, no logos. ";

const BEATS = {
  "key-1-painter.png":
    STYLE + "A cluttered Los Angeles studio at night. He sits at a desk seen from 3/4-behind in a " +
    "painter's posture, brush in hand — but his canvas is a glowing laptop screen full of colored light. " +
    "A quiet stack of old, closed, dead laptops sits beside him. Warm desk lamp, faint green terminal " +
    "glow. Intimate and nocturnal — the origin of an idea.",
  "key-2-awakening.png":
    STYLE + "Close on his hands flipping open a small salvaged laptop and slotting in a USB stick. The " +
    "screen blooms to life with a vivid grid of glowing colored musical note-tiles and a bright waveform, " +
    "light and color flooding out of the machine into the dark room. A cheap laptop becoming a glowing " +
    "instrument. Magical, warm.",
  "key-3-commons.png":
    STYLE + "A vast pull-back view: a dark constellation where thousands of tiny glowing screens blink on, " +
    "each showing a small distinct piece of colorful generative art, connected by faint threads of light — " +
    "a galaxy of a creative community. His small silhouette stands at the lower edge looking up at it. Awe " +
    "and scale.",
  "key-4-invitation.png":
    STYLE + "A single refurbished laptop alone on a warm, softly-spotlit dark stage, glowing like a musical " +
    "instrument waiting to be played. His silhouette has just set it down and is stepping back into shadow. " +
    "Hopeful, reverent, an invitation. Cinematic rim light.",
};

const apiKey = key();
const force = process.argv.includes("--force");
for (const [name, prompt] of Object.entries(BEATS)) {
  const out = join(HERE, name);
  if (existsSync(out) && !force) { console.log(`· cached ${name}`); continue; }
  const t0 = Date.now();
  process.stdout.write(`▸ ${name} … `);
  const res = await fetch("https://api.openai.com/v1/images/generations", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
    body: JSON.stringify({ model: "gpt-image-2", prompt, size: "1536x1024", quality: "high", n: 1 }),
  });
  if (!res.ok) { console.log(`FAIL ${res.status}: ${(await res.text()).slice(0, 160)}`); continue; }
  const j = await res.json();
  const b64 = j.data?.[0]?.b64_json;
  if (!b64) { console.log("no image"); continue; }
  writeFileSync(out, Buffer.from(b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
console.log("done");
