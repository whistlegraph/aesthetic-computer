// Storyboard still: gpt-image-2 EDIT of a real source frame → end keyframe
// showing the finished chalk drawing. Validated locally before any Seedance
// spend (the /marketing and /pop validate-before-animate loop).
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

const [name, motionDesc] = [process.argv[2], process.argv[3]];
if (!name || !motionDesc) {
  console.error("usage: gen-still.mjs <name> <finished-drawing description>");
  process.exit(1);
}
const outPath = `${HERE}/end-${name}.png`;
if (existsSync(outPath) && !process.argv.includes("--force")) {
  console.log(`· cached: ${outPath}`);
  process.exit(0);
}

const prompt =
  "Edit this photo minimally. Keep the exact same scene: the same green " +
  "chalkboard with its smudges, the same real hand holding white chalk, the " +
  "same natural handheld phone framing, daylight and colors. Change only two " +
  "things: (1) the board now shows a completed hand-drawn white chalk drawing " +
  `of ${motionDesc}, drawn in the same casual chalk line quality as the ` +
  "existing marks; (2) the hand has moved to rest at the final stroke of that " +
  "drawing, chalk tip touching where the last line ends. Photorealistic, " +
  "looks like the next moment of the same phone video.";

const fd = new FormData();
fd.append("model", "gpt-image-2");
fd.append("prompt", prompt);
fd.append("size", "1024x1536");
fd.append("quality", "high");
fd.append("n", "1");
fd.append("image[]", new Blob([readFileSync(`${HERE}/start-frame.png`)], { type: "image/png" }), "start-frame.png");

console.log(`▸ end keyframe: ${name} — ${motionDesc}`);
const t0 = Date.now();
const res = await fetch("https://api.openai.com/v1/images/edits", {
  method: "POST",
  headers: { Authorization: `Bearer ${openaiKey()}` },
  body: fd,
});
if (!res.ok) {
  console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 400)}`);
  process.exit(2);
}
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) { console.error(`✗ no image: ${JSON.stringify(json).slice(0, 300)}`); process.exit(2); }
writeFileSync(outPath, Buffer.from(b64, "base64"));
console.log(`✓ ${outPath} (${((Date.now() - t0) / 1000).toFixed(1)}s)`);
