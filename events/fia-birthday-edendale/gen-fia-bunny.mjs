#!/usr/bin/env node
// gen-fia-bunny.mjs — fia's birthday invite, take 2: a pixar-style 3D bunny
// whose face + spirit are based on real photos of fia (from the @whistlegraph /
// solafiya IG platter), celebrating at the edendale vintage bar. very bright.
// vertical 1024x1536, calm top/bottom safe zones for an event-text overlay.

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __dir = dirname(fileURLToPath(import.meta.url));
const REPO = join(__dir, "..", "..");

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = join(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) {
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("OPENAI_API_KEY not set and not found in vault devcontainer.env");
}

const JUNGLE_REFS = join(REPO, "pop/jungle/out/.refs");
const REFS = [
  // fia identity (real photos, normalized copies from the solafiya pipeline)
  join(JUNGLE_REFS, "_jas_Documents_Working_Desktop_gens_IMG_0933_jpg.png"), // clean frontal
  join(JUNGLE_REFS, "ments_Working_Desktop_gens__fia_platter_sec4_png.png"), // smiling 3/4
  join(JUNGLE_REFS, "ments_Working_Desktop_gens__fia_platter_sec2_png.png"), // smiling frontal
  // edendale room
  join(__dir, "refs/9addf5d8.jpg"), // starlet-portrait wall + red damask + banquette
  join(__dir, "refs/b308548b.jpg"), // pressed-tin ceiling + maroon wainscot
];

const PROMPT = `a charming 3D animated character in the polished style of a modern pixar feature film — glossy cinematic CG render, soft global illumination, big expressive sparkly eyes, fluffy fur with gentle subsurface scattering, rounded appealing shapes, warm and adorable.

the character is a cute fluffy bunny whose face and spirit are based on the woman in the first three reference photos: keep HER warm dark-brown eyes, HER long dark wavy/curly hair framing the face (rendered as soft flowing hair around and between the bunny's ears), HER genuine friendly smile, HER warm olive skin tone in the little muzzle and cheeks. she is recognizably the same person, lovingly reimagined as a bunny. keep the real warm human-like eyes and gentle happy expression — that is what makes her HER.

she wears a little cone-shaped birthday party hat and a tiny delicate star necklace. she sits at a small round marble bistro table inside a vintage 1920s silver lake bar — the room from the other reference photos: wine-red damask wallpaper, a gallery wall of small gold-framed vintage portraits, a tufted red leather banquette, a pressed-tin ceiling. she raises a pretty pink cocktail in a coupe glass in a joyful birthday toast. one or two soft pastel balloons drift nearby.

the scene is VERY BRIGHTLY LIT — airy, sunny, light-filled, fresh and cheerful daylight glow. NOT dim, NOT moody, NOT candlelit.

vertical portrait composition, the bunny centered in the bright middle band. keep the very top of the frame quiet — ceiling and balloon tops — and the very bottom quiet — table edge and floor — so there is calm empty space at top and bottom for text.

only the ONE bunny — no other animals, no crowd. no human people in frame. no text, no lettering, no words anywhere. no harsh edges, no motion blur.`;

async function main() {
  const apiKey = loadOpenAIKey();
  const outDir = join(__dir, "out");
  mkdirSync(outDir, { recursive: true });

  for (const ref of REFS) {
    if (!existsSync(ref)) throw new Error(`ref missing: ${ref}`);
  }

  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", PROMPT);
  fd.append("size", "1024x1536");
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }

  console.log("→ gpt-image-2 edit, 1024x1536, 3 fia + 2 edendale refs…");
  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}` },
    body: fd,
  });
  const json = await res.json();
  if (!res.ok || !json.data?.[0]?.b64_json) {
    console.error("FAILED:", JSON.stringify(json, null, 2));
    process.exit(1);
  }
  const stamp = process.argv[2] || "v1";
  const out = join(outDir, `fia-bunny-${stamp}.png`);
  writeFileSync(out, Buffer.from(json.data[0].b64_json, "base64"));
  console.log("✓ wrote", out);
}

main();
