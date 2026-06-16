#!/usr/bin/env node
// gen-keyframes.mjs — the 4 narrative key-frames with JEFFREY's real likeness,
// via gpt-image-2 /v1/images/edits conditioned on the canonical jeffrey refs
// (marketing/lib/jeffrey-refs.mjs). Painterly /pop style; he's kept 3/4-back /
// profile / silhouette so it reads as him AND survives Seedance face moderation.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { jeffreyRefs } from "../../../marketing/lib/jeffrey-refs.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
function key() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const v = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(v, "utf8").split("\n"))
    if (line.startsWith("OPENAI_API_KEY=")) return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("no OPENAI_API_KEY");
}

const REFS = jeffreyRefs();
const STYLE =
  "Painterly digital illustration, cinematic 16:9, warm and moody, rich color, soft glow and gentle " +
  "film grain, hand-painted indie-animated-film concept-art feel. The MAN in the scene is the specific " +
  "person in the reference photos — match his face, hair, beard, build and vibe so it is clearly HIM — " +
  "but frame him from 3/4-behind, side profile, or silhouette (no straight-on front face). Aesthetic " +
  "Computer palette: warm cream, deep violet, citrus-green terminal glow, coral accents. No readable " +
  "text, no logos. ";

const BEATS = {
  "key-1-painter.png":
    STYLE + "He sits at a cluttered Los Angeles studio desk at night, seen from 3/4-behind so we catch " +
    "his profile in the warm lamp light — a painter's posture, brush in hand — but his canvas is a glowing " +
    "laptop screen full of colored light. A quiet stack of old, closed, dead laptops beside him. Faint " +
    "green terminal glow. Intimate, nocturnal — the origin of an idea.",
  "key-2-awakening.png":
    STYLE + "Close over-the-shoulder on him (his profile and hands visible) flipping open a small salvaged " +
    "laptop and slotting in a USB stick. The screen blooms with a vivid grid of glowing colored musical " +
    "note-tiles and a bright waveform; light and color flood out of the machine onto his face and into the " +
    "dark room. A cheap laptop becoming a glowing instrument. Magical, warm.",
  "key-3-commons.png":
    STYLE + "A vast pull-back: a dark constellation where thousands of tiny glowing screens blink on, each " +
    "a small distinct piece of colorful generative art, threads of light connecting them. He stands in the " +
    "lower foreground from behind, clearly him by his hair and build and outfit, looking up at the galaxy " +
    "of a creative community. Awe and scale.",
  "key-4-invitation.png":
    STYLE + "A single refurbished laptop alone on a warm, softly-spotlit dark stage, glowing like an " +
    "instrument waiting to be played. His silhouette — recognizably him — has just set it down and is " +
    "stepping back into the shadow. Hopeful, reverent, an invitation. Cinematic rim light.",
};

const apiKey = key();
const force = process.argv.includes("--force");
console.log(`refs: ${REFS.length} jeffrey photos`);
for (const [name, prompt] of Object.entries(BEATS)) {
  const out = join(HERE, name);
  if (existsSync(out) && !force) { console.log(`· cached ${name}`); continue; }
  const t0 = Date.now();
  process.stdout.write(`▸ ${name} … `);
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", prompt);
  fd.append("size", "1536x1024");
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
  });
  if (!res.ok) { console.log(`FAIL ${res.status}: ${(await res.text()).slice(0, 160)}`); continue; }
  const j = await res.json();
  const b64 = j.data?.[0]?.b64_json;
  if (!b64) { console.log("no image"); continue; }
  writeFileSync(out, Buffer.from(b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
console.log("done");
