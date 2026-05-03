#!/usr/bin/env node
// gen-cover.mjs — colored-pencil cover illustration for an AC paper
//
// Reads:    papers/<paper-dir>/figures/cover-prompt.txt
// Writes:   papers/<paper-dir>/figures/cover.png (1024x1536)
//
// Wraps every prompt in a canonical colored-pencil-on-cream-paper style
// preamble so all paper covers look like a series. Calls OpenAI gpt-image-2
// via the same path used by recap/bin/gen-photos.mjs (no reference images —
// the cover is generated from prompt only).
//
// Usage:
//   node papers/bin/gen-cover.mjs arxiv-rhizome
//   node papers/bin/gen-cover.mjs arxiv-rhizome --force
//   node papers/bin/gen-cover.mjs --all          (every arxiv-* with a prompt)

import { readFileSync, writeFileSync, mkdirSync, existsSync, readdirSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS_DIR = resolve(HERE, "..");
const REPO = resolve(PAPERS_DIR, "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (const a of argv) {
  if (a.startsWith("--")) flags[a.slice(2)] = true;
  else positional.push(a);
}

const STYLE_PREFIX =
  "Hand-rendered emblem on warm cream-colored paper — designed to read " +
  "like a record-label seal, a band-tee print, or a hand-painted shop " +
  "sign. ONE singular subject only: no clusters, no tangles, no " +
  "arrangements, no cornucopias, no sets of multiple objects. The " +
  "emblem fills the central two-thirds of the square and floats; " +
  "corners and periphery DISSOLVE INTO PURE CREAM PAPER on all four " +
  "sides with NO hard frame, NO border, NO background scene, NO " +
  "environmental context. Bold, simple, recognizable from across a " +
  "room. Drawn in colored pencil and gouache with thick confident " +
  "strokes and high tonal contrast at the subject's center, gradually " +
  "softening to nothing at the periphery. Hand-lettered text, proper " +
  "nouns, brand wordmarks, address numbers, and logo iconography ARE " +
  "WELCOME — render them with the visible imperfection of a silkscreen " +
  "DO NOT NAME THE SUBJECT INSTITUTION ANYWHERE IN THE IMAGE — no " +
  "hand-lettered names, no carved wordmarks, no chalk lettering, no " +
  "stamped or painted institution names, no readable signage spelling " +
  "out the paper's subject. Incidental product or manufacturer text " +
  "already physically belonging to a depicted object (a SONY mark on " +
  "the body of a vintage camera, a small disk-brand logo, the printed " +
  "model number on a floppy's metal shutter) is acceptable when it is " +
  "NOT the institution being illustrated. Letterforms may appear as " +
  "illegible scribble-gestalt where the form of letters is part of the " +
  "object's character (a stencil sleeve, a typed page, a chalkboard, " +
  "the rhythm of engraved name-bands on a plaque) but they must NEVER " +
  "spell readable institutional names. The institution's identity is " +
  "carried entirely by ICONOGRAPHIC EMBLEM, FORM, SILHOUETTE, COLOR, " +
  "and ERA (the elephant, the temple, the pixel-burst, the hexagon, " +
  "the inverted stoop, the brick arch, the brass seal). Muted natural " +
  "palette (terracotta, sage, ochre, dusty pink, slate blue, brick red, " +
  "warm grey, oxblood, brass). Square 1:1 composition. The subject: ";

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) {
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("OPENAI_API_KEY not found");
}

async function genCover(paperDir, { force }) {
  const figuresDir = join(PAPERS_DIR, paperDir, "figures");
  const promptPath = join(figuresDir, "cover-prompt.txt");
  const outPath = join(figuresDir, "cover.png");

  if (!existsSync(promptPath)) {
    return { paperDir, status: "skip", reason: "no cover-prompt.txt" };
  }
  mkdirSync(figuresDir, { recursive: true });

  if (existsSync(outPath) && !force) {
    return { paperDir, status: "cached", outPath };
  }

  const subject = readFileSync(promptPath, "utf8").trim();
  const fullPrompt = STYLE_PREFIX + subject;

  const t0 = Date.now();
  const apiKey = loadOpenAIKey();
  const res = await fetch("https://api.openai.com/v1/images/generations", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${apiKey}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      model: "gpt-image-2",
      prompt: fullPrompt,
      size: "1024x1024",
      quality: "high",
      n: 1,
    }),
  });
  if (!res.ok) {
    throw new Error(`OpenAI ${res.status}: ${(await res.text()).slice(0, 400)}`);
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) throw new Error("no image returned");
  writeFileSync(outPath, Buffer.from(b64, "base64"));
  return {
    paperDir,
    status: "fresh",
    outPath,
    durSec: (Date.now() - t0) / 1000,
  };
}

let targets = [];
if (flags.all) {
  for (const name of readdirSync(PAPERS_DIR)) {
    if (!name.startsWith("arxiv-")) continue;
    const promptPath = join(PAPERS_DIR, name, "figures", "cover-prompt.txt");
    if (existsSync(promptPath)) targets.push(name);
  }
} else if (positional.length) {
  targets = positional;
} else {
  console.error(
    "usage: gen-cover.mjs <arxiv-slug> [--force]\n" +
      "       gen-cover.mjs --all [--force]",
  );
  process.exit(1);
}

console.log(`▸ ${targets.length} target(s) · force=${!!flags.force}`);

for (const t of targets) {
  try {
    const r = await genCover(t, { force: !!flags.force });
    if (r.status === "cached") console.log(`  · cached: ${t}`);
    else if (r.status === "skip") console.log(`  · skip ${t}: ${r.reason}`);
    else console.log(`  ✓ ${t} (${r.durSec.toFixed(1)}s)`);
  } catch (e) {
    console.error(`  ✗ ${t}: ${e.message}`);
    process.exitCode = 2;
  }
}
