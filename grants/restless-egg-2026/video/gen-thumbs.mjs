#!/usr/bin/env node
// gen-thumbs.mjs — generate a DISTINCT concept frame per shot (gpt-image-2)
// so the storyboard reads as 7 different beats, not the same emblem repeated.
// Shots 1 & 5 keep their real Seedance frames; the rest get fresh art.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");

function key() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const v = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(v, "utf8").split("\n"))
    if (line.startsWith("OPENAI_API_KEY="))
      return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("no OPENAI_API_KEY");
}

const STYLE =
  "Cinematic 16:9 concept-art storyboard frame, warm painterly light, soft film grain, " +
  "rich color, single clear subject, no people, no faces, no hands, no readable logos. ";

const SHOTS = {
  "thumb-2-morph.png":
    STYLE + "A laptop screen caught mid-transformation: the glowing screen dissolving from a " +
    "soft abstract emblem into a vivid grid of colorful glowing musical note-tiles, streaks of " +
    "motion blur showing the morph, magical and musical.",
  "thumb-3-notepat.png":
    STYLE + "Extreme close-up of a playable musical software instrument: a dense grid of colorful " +
    "glowing note-tiles lit up as if being played, a bright cyan audio waveform streaking across the " +
    "top, dark sleek background, energetic and tactile — an instrument you play.",
  "thumb-4-moat.png":
    STYLE + "Abstract visualization of a physical-modeling synthesizer: glowing vibrating strings and " +
    "resonating waveguides radiating warm light against a dark background, a sense of deep engineering " +
    "and sound made visible, premium and scientific.",
  "thumb-6-platform.png":
    STYLE + "A vast wall-grid of dozens of tiny distinct pieces of generative art, each a small glowing " +
    "colorful abstract square, like a gallery of creative-code programs made by a community, vibrant and " +
    "teeming with variety.",
  "thumb-7-ask.png":
    STYLE + "An inviting end-card scene: a single refurbished laptop sitting like a small instrument on a " +
    "warm softly-spotlit stage, a hopeful glow around it, the feeling of an invitation to play — warm, " +
    "optimistic, cinematic.",
};

const apiKey = key();
const force = process.argv.includes("--force");
for (const [name, prompt] of Object.entries(SHOTS)) {
  const out = join(HERE, name);
  if (existsSync(out) && !force) { console.log(`· cached ${name}`); continue; }
  const t0 = Date.now();
  process.stdout.write(`▸ ${name} … `);
  const res = await fetch("https://api.openai.com/v1/images/generations", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
    body: JSON.stringify({ model: "gpt-image-2", prompt, size: "1536x1024", quality: "medium", n: 1 }),
  });
  if (!res.ok) { console.log(`FAIL ${res.status}: ${(await res.text()).slice(0, 160)}`); continue; }
  const j = await res.json();
  const b64 = j.data?.[0]?.b64_json;
  if (!b64) { console.log("no image"); continue; }
  writeFileSync(out, Buffer.from(b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
console.log("done");
