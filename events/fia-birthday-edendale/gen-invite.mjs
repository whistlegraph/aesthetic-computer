#!/usr/bin/env node
// gen-invite.mjs — fia's birthday invite: edendale vintage bar reimagined as a
// soft pastel-surreal party of fancy fluffy animals. gpt-image-2 image-edit,
// conditioned on real edendale interior reference photos. vertical 1024x1536,
// calm top/bottom safe zones reserved for an event-text overlay added in post.

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

const REFS = [
  join(__dir, "refs/9addf5d8.jpg"), // starlet-portrait wall + red damask + banquette
  join(__dir, "refs/b308548b.jpg"), // pressed-tin ceiling + maroon wainscot tile
];

const PROMPT = `a soft, dreamy, painterly illustration — a single adorable fluffy bunny celebrating a birthday inside a vintage 1920s silver lake bar, exactly the room in the reference photos: wine-red damask wallpaper, a gallery wall of small gold-framed vintage portraits, a tufted red leather banquette, a pressed-tin ceiling.

but the scene is VERY BRIGHTLY LIT — airy, sunny, light-filled, fresh and cheerful, bright clean daylight glow. NOT dim, NOT moody, NOT candlelit.

just one bunny — plush, huggable, big soft eyes, wearing a little cone-shaped birthday party hat, sitting at a small table holding a pretty cocktail in a coupe glass, raising it in a happy toast. cute, tender, slightly mischievous. only the ONE bunny — no other animals, no crowd, no guests. one or two soft pastel balloons nearby.

soft pastel-surreal storybook treatment: gentle bright pastel palette, dreamy painterly brushwork, a touch of sparkle. joyful and fresh.

vertical portrait composition, the bunny centered in the bright middle band. keep the very top quiet — wallpaper, ceiling, balloon tops — and keep the very bottom quiet — table edge and floor — so there is calm empty space at top and bottom for text.

no text, no lettering, no signage, no words anywhere in the image. no human people. no harsh edges, no motion blur.`;

async function main() {
  const apiKey = loadOpenAIKey();
  const outDir = join(__dir, "out");
  mkdirSync(outDir, { recursive: true });

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

  console.log("→ gpt-image-2 edit, 1024x1536, 2 edendale refs…");
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
  const out = join(outDir, `invite-${stamp}.png`);
  writeFileSync(out, Buffer.from(json.data[0].b64_json, "base64"));
  console.log("✓ wrote", out);
}

main();
