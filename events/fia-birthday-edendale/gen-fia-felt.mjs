#!/usr/bin/env node
// gen-fia-felt.mjs — fia's birthday invite, felt edition. everything is a
// handmade needle-felted wool diorama, lightly colored (pale pastels).
// fia recognizable (from real IG-platter photos), bunnies, edendale patio.
// two scenes: A = patio toast w/ bunnies, B = cozy close-up w/ bunny + cake.
// usage: node gen-fia-felt.mjs <A|B> [stamp]

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
  join(JUNGLE_REFS, "_jas_Documents_Working_Desktop_gens_IMG_0933_jpg.png"), // fia frontal (primary)
  join(JUNGLE_REFS, "ments_Working_Desktop_gens__fia_platter_sec4_png.png"),
  join(JUNGLE_REFS, "ments_Working_Desktop_gens__fia_platter_sec2_png.png"),
  join(__dir, "refs/d7ba1a64.jpg"), // edendale string-lit patio
];

const FELT = `everything in the image is a handmade NEEDLE-FELTED WOOL DIORAMA — every surface is soft fuzzy felted wool with visible wool fibers, fuzzy soft edges, little imperfections, a cozy handmade craft look, as if photographed up close on a tabletop. LIGHTLY COLORED — a soft pale pastel wool palette (cream, dusty rose, pale sage, soft lavender, butter yellow), gentle and airy. soft warm diffuse light, shallow depth of field. NOT plastic, NOT glossy, NOT cgi-smooth — soft matte wool throughout.`;

const FIA = `a fully needle-felted WOOL figure of a young woman — her ENTIRE figure including her FACE, SKIN, and HAIR is made of soft felted wool: fuzzy matte wool skin with visible fibers, hair felted from dark-brown wool yarn in soft curls, warm dark-brown eyes as tiny round glass beads, a small stitched felt smile. she still clearly resembles the woman in the first three reference photos — same hair, same warm friendly face shape and expression — but rendered entirely as a handmade felt doll. she wears a soft grey felted button-up shirt and a tiny star-bead necklace. NO real human skin anywhere — she is all wool.`;

const SCENES = {
  A: `${FELT}

${FIA} she wears a little felt cone party hat and sits at a tiny round felt bistro table on a felted version of the edendale garden patio from the reference photo — felted bougainvillea climbing the walls, tiny felt string-light bulbs strung overhead, little felt archways. she raises a tiny felt cocktail coupe (pale pink wool) in a joyful birthday toast, beaming.

gathered lovingly around her are several adorable fluffy felted bunnies in pale wool (cream, grey, soft brown) — little four-legged felt rabbits, NOT human-like, NOT clothed. tiny felt confetti bits are scattered on the table and tossed in the air. a soft felt balloon or two. a small glowing bead 'mood ring' on her felt hand.

vertical portrait composition, the felt fia centered. keep the very top quiet (felt string lights, balloon tops) and the very bottom quiet (table edge) for a text overlay. no text, no lettering. no other people — just felt fia and her felt bunnies.`,

  B: `${FELT}

a cozy close-up: ${FIA} she leans in cheek-to-cheek with one big fluffy felted bunny (soft cream wool), both beaming with joy. she wears her little felt party hat. in front of them sits a tiny felt birthday cake with a single soft felt candle-flame. behind them, soft felt balloons and a few warm felt string-lights of the edendale patio, felted bougainvillea. tiny felt confetti drifting.

vertical portrait composition, the two faces in the upper-middle band, warm and intimate. keep the very bottom quiet (the little cake and table) for a text overlay. no text, no lettering. just felt fia and her felt bunny.`,
};

async function main() {
  const scene = (process.argv[2] || "A").toUpperCase();
  if (!SCENES[scene]) throw new Error(`scene must be A or B (got ${scene})`);
  const stamp = process.argv[3] || "v1";
  const apiKey = loadOpenAIKey();
  const outDir = join(__dir, "out");
  mkdirSync(outDir, { recursive: true });
  for (const ref of REFS) if (!existsSync(ref)) throw new Error(`ref missing: ${ref}`);

  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", SCENES[scene]);
  fd.append("size", "1024x1536");
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }

  console.log(`→ gpt-image-2 felt scene ${scene}, 1024x1536…`);
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
  const out = join(outDir, `fia-felt-${scene}-${stamp}.png`);
  writeFileSync(out, Buffer.from(json.data[0].b64_json, "base64"));
  console.log("✓ wrote", out);
}

main();
