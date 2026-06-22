#!/usr/bin/env node
// gen-fia-human.mjs — fia's birthday invite: a pixar-style 3D animated version
// of fia HERSELF (human), based on real photos from the @whistlegraph/solafiya
// IG platter, celebrating at the edendale vintage bar. very bright.
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
  // edendale room — the magical back patio (bougainvillea walls, arches, string lights)
  join(__dir, "refs/d7ba1a64.jpg"),
];

const PROMPT = `a charming 3D animated character in the polished style of a modern pixar feature film — glossy cinematic CG render, soft global illumination, big warm expressive eyes, soft skin with gentle subsurface scattering, appealing stylized proportions, warm and adorable.

the character is a young woman based closely on the woman in the first three reference photos — recognizably HER: keep her warm dark-brown eyes, her long dark wavy/curly hair, her genuine friendly smile, her warm olive skin tone and her real facial features and bone structure. she wears a soft grey button-up shirt and a tiny delicate star necklace, just like in the photos. lovingly stylized as a pixar character but unmistakably the same person.

the setting is fantastical and magical: edendale's enchanted back garden patio from the reference photo — tall old walls covered in climbing bougainvillea and vines, romantic stone archways, and crisscrossing strings of warm glowing fairy lights overhead. but dialed up into a dreamy storybook fantasy — soft floating sparkles and bokeh in the air, a gentle luminous glow, magical and whimsical.

she is celebrating her birthday: a little cone-shaped party hat perched on her curls, sitting at a small round bistro table. she raises a pretty pink cocktail in a coupe glass in a joyful birthday toast, beaming. her other hand rests on the very edge of the table in the foreground, wearing a glowing MOOD RING — a chunky ring with a domed gemstone that radiates shifting jewel colors (teal, violet, amber), casting a soft magical glow. soft pastel balloons drift nearby.

gathered lovingly around her are several adorable, real, fluffy pet bunnies — soft rabbits of different colors (cream, grey, brown, white) perched on the bench, on the table, and nestled beside her, all cute and happy, joining the party. they are ordinary cute bunnies (four-legged little rabbits), NOT human-like, NOT wearing clothes.

bright, colorful confetti is tossed through the air and scattered across the table, falling festively around her.

the light is LUMINOUS and bright-magical — glowing fairy lights, a warm radiant dreamy glow, fresh and enchanting. NOT dim, NOT dark, NOT gloomy.

vertical portrait composition, she sits centered in the glowing middle band. keep the very top of the frame quiet — fairy lights and balloon tops — and the very bottom quiet — table edge and her mood-ring hand — so there is calm space at top and bottom for text.

no other PEOPLE besides her — just fia and her cute bunnies. no text, no lettering, no words anywhere. no harsh edges, no motion blur.`;

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

  console.log("→ gpt-image-2 edit, 1024x1536, 3 fia + 2 edendale refs (human)…");
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
  const out = join(outDir, `fia-human-${stamp}.png`);
  writeFileSync(out, Buffer.from(json.data[0].b64_json, "base64"));
  console.log("✓ wrote", out);
}

main();
