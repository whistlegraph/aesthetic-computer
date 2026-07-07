// Tree-on-blacktop storyboard stills. Two chained gpt-image-2 edits:
//   1. start-blacktop.png — the REAL source frame re-staged: same hand, now
//      holding colored sidewalk chalk over asphalt blacktop.
//   2. end-tree.png — that start still with the finished colored-chalk tree.
// Each validated by eye before any Seedance spend.
import { readFileSync, writeFileSync, existsSync } from "node:fs";

const REPO = "/Users/jas/aesthetic-computer";
const HERE = "/Users/jas/Desktop/seedance-variations/storyboard";

function openaiKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(vault, "utf8").split("\n")) {
    if (line.startsWith("OPENAI_API_KEY=")) {
      return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
    }
  }
  throw new Error("OPENAI_API_KEY not found");
}

async function edit(refPath, prompt, outPath) {
  if (existsSync(outPath) && !process.argv.includes("--force")) {
    console.log(`· cached: ${outPath}`);
    return;
  }
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", prompt);
  fd.append("size", "1024x1536");
  fd.append("quality", "high");
  fd.append("n", "1");
  fd.append("image[]", new Blob([readFileSync(refPath)], { type: "image/png" }), "ref.png");
  const t0 = Date.now();
  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${openaiKey()}` },
    body: fd,
  });
  if (!res.ok) throw new Error(`OpenAI ${res.status}: ${(await res.text()).slice(0, 400)}`);
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) throw new Error(`no image: ${JSON.stringify(json).slice(0, 300)}`);
  writeFileSync(outPath, Buffer.from(b64, "base64"));
  console.log(`✓ ${outPath} (${((Date.now() - t0) / 1000).toFixed(1)}s)`);
}

const which = process.argv[2] || "start";

if (which === "start") {
  await edit(
    `${HERE}/start-frame.png`,
    "Keep the same real hand and the same natural handheld phone framing and " +
      "daylight from this photo, but re-stage the scene outdoors: the hand now " +
      "holds a thick piece of PINK sidewalk chalk, poised about to draw on dark " +
      "asphalt blacktop pavement (a playground or driveway surface with fine " +
      "texture, small stones, a few faint old chalk smudges). Top-down phone " +
      "view. No drawing on the blacktop yet. Photorealistic, like a frame from " +
      "a casual phone video.",
    `${HERE}/start-blacktop.png`,
  );
} else {
  await edit(
    `${HERE}/start-blacktop.png`,
    "Edit this photo minimally. Keep the exact same asphalt blacktop, the same " +
      "real hand, the same framing, daylight and colors. Change only two " +
      "things: (1) the blacktop now shows a completed hand-drawn sidewalk-chalk " +
      "drawing of a simple friendly tree — a thick BROWN chalk trunk with two " +
      "short root flares, a big round GREEN chalk canopy scribbled loosely, and " +
      "three small PINK chalk blossoms dotted in the canopy; casual playful " +
      "chalk line quality; (2) the hand holding the pink chalk now rests at the " +
      "last blossom, chalk tip touching it. Photorealistic, like the next " +
      "moment of the same phone video.",
    `${HERE}/end-tree.png`,
  );
}
