#!/usr/bin/env node
// marimba/bin/gen-lomo-restyle-fluttabap.mjs — the LOMO-REALISTIC illy set.
//
// An ALTERNATE aesthetic for the fluttabap360 bunny reel: instead of the
// hand-drawn colored-pencil + gouache panels, re-photograph each EXACT
// panel as a realistic photo shot through a Lomo/Holga toy camera. Uses
// images/edits with the finished drawn panel as the reference so the
// composition, characters, keyboards and story stay identical — only the
// MEDIUM changes (drawing → cross-processed 35mm photograph).
//
// Source: out/_fluttabap360-bunny-reel-beat-<name>.png (the drawn set)
// Output: out/_fluttabap360-bunny-reel-lomo-beat-<name>.png (+ Desktop mirror)
//
// Usage:
//   node pop/marimba/bin/gen-lomo-restyle-fluttabap.mjs               # all beats
//   node pop/marimba/bin/gen-lomo-restyle-fluttabap.mjs --only dance,space
//   node pop/marimba/bin/gen-lomo-restyle-fluttabap.mjs --force       # re-restyle

import { readFileSync, writeFileSync, existsSync, mkdirSync, copyFileSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const SLUG = "fluttabap360-bunny-reel";
const OUT = `${LANE}/out`;
const SIZE = "1024x1536";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}
const FORCE = flags.force === true;
const ONLY = typeof flags.only === "string" ? new Set(flags.only.split(",").map((s) => s.trim())) : null;

const BEATS = [
  "cocoon", "crack", "hatch", "hatchpov", "grow", "dance",
  "laptops", "mallets", "laptopspov", "molt", "wing", "drone", "spacepov", "space",
];

function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(vault, "utf8").split("\n"))
    if (line.startsWith("OPENAI_API_KEY=")) return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("OPENAI_API_KEY not found");
}
const apiKey = loadKey();
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// The restyle instruction — keep everything, change ONLY the medium.
const LOMO = `Re-photograph this EXACT scene as a REALISTIC PHOTOGRAPH shot on 35mm film through a LOMO / Holga toy camera. Keep the IDENTICAL composition, characters, poses, expressions, objects, layout, and color placement — do NOT redraw, re-pose, add, or rearrange anything, and keep every laptop screen and keyboard exactly where it is. Change ONLY the medium from a colored-pencil drawing into a real photograph: photoreal materials and light — real soft rabbit fur, real translucent monarch-wing membrane with real veins, real brushed metal, real glossy laptop screens and keycaps, real grass, real sky, real depth. Shot on cross-processed 35mm film: saturated punchy color, teal-cool shadows and warm-gold highlights, natural shallow depth of field with a soft toy-lens focus falloff toward the edges, a heavy dark vignette, a subtle warm light leak, and visible fine film grain. Cinematic, tactile, tangible, real — NOT an illustration, NOT a cartoon, NOT a 3D render, NOT flat. The creature stays the same bunny × monarch hybrid with its two long ears and round red glasses, now looking like a real photographed creature.`;

async function restyle(name) {
  const src = `${OUT}/_${SLUG}-beat-${name}.png`;
  const dst = `${OUT}/_${SLUG}-lomo-beat-${name}.png`;
  if (!existsSync(src)) { console.error(`✗ ${name}: source panel missing (${src})`); return false; }
  if (existsSync(dst) && !FORCE) { console.log(`✓ cached → ${basename(dst)}`); return true; }
  console.log(`▸ lomo restyle · ${name}`);
  for (let attempt = 1; attempt <= 4; attempt++) {
    try {
      const fd = new FormData();
      fd.append("model", "gpt-image-2");
      fd.append("prompt", LOMO);
      fd.append("size", SIZE);
      fd.append("quality", "high");
      fd.append("n", "1");
      fd.append("image[]", new Blob([readFileSync(src)], { type: "image/png" }), `${name}.png`);
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
      });
      if (!res.ok) {
        const err = await res.text();
        if ((res.status === 429 || res.status >= 500) && attempt < 4) { await sleep(4000 * attempt); continue; }
        console.error(`✗ ${name}: OpenAI ${res.status} ${err.slice(0, 200)}`); return false;
      }
      const b64 = (await res.json()).data?.[0]?.b64_json;
      if (!b64) { console.error(`✗ ${name}: no image`); return false; }
      writeFileSync(dst, Buffer.from(b64, "base64"));
      try { copyFileSync(dst, join(homedir(), "Desktop", `${SLUG}-lomo-beat-${name}.png`)); } catch {}
      console.log(`✓ → ${basename(dst)}`);
      return true;
    } catch (e) {
      if (attempt < 4) { await sleep(4000 * attempt); continue; }
      console.error(`✗ ${name}: ${e.message}`); return false;
    }
  }
  return false;
}

mkdirSync(OUT, { recursive: true });
const list = BEATS.filter((b) => (ONLY ? ONLY.has(b) : true));
console.log(`▸ lomo-realistic restyle · ${list.length} beat(s)`);
let ok = 0;
for (const name of list) if (await restyle(name)) ok++;
console.log(`\n✓ lomo restyle — ${ok}/${list.length} panels`);
