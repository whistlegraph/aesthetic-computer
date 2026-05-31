// gen-cover.mjs — slippy fish (oskie) single cover via gpt-image-2 image-edit.
// Mirrors pop/hellsine/bin/gen-illy.mjs conventions: key from env or vault,
// images/edits with the offline raycast render as the structure reference.
//   node pop/slippy-fish/bin/gen-cover.mjs [--force]

import { readFileSync, writeFileSync, existsSync } from "fs";
import { homedir } from "os";
import { spawnSync } from "child_process";

const REPO = "/Users/jas/aesthetic-computer";
const HOME = homedir();
const LANE = `${REPO}/pop/slippy-fish`;
const SIZE = "1024x1024";
const FORCE = process.argv.includes("--force");
const arg = (k, d) => {
  const hit = process.argv.find((a) => a.startsWith(`--${k}=`));
  return hit ? hit.slice(k.length + 3) : d;
};
const LABEL = arg("label", "cover"); // output suffix: slippy-fish-<label>-*.png
const PROMPT_PATH = arg("prompt", `${LANE}/prompt.txt`);
// Structure/style reference: the offline fish.mjs render. Prefer the Shelf
// working copy (the Desktop copy can be auto-cleaned), fall back to Desktop.
const REF =
  [`${HOME}/Documents/Shelf/slippy-fish/slippy-fish-render-3000.jpg`,
   `${HOME}/Desktop/slippy-fish-render-3000.jpg`].find(existsSync);
const OUT_1024 = `${LANE}/slippy-fish-${LABEL}-1024.png`;
const OUT_3000 = `${LANE}/slippy-fish-${LABEL}-3000.png`;
const DESKTOP_OUT = `${HOME}/Desktop/slippy-fish-${LABEL}-3000.png`;

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
  throw new Error("OPENAI_API_KEY not set and not found in vault");
}

if (!REF) throw new Error("reference render not found — run pop/slippy-fish/bin/render.mjs first");
if (existsSync(OUT_3000) && !FORCE) {
  console.log(`✓ cached cover → ${OUT_3000.replace(REPO + "/", "")} (use --force to regen)`);
  process.exit(0);
}

const apiKey = loadOpenAIKey();
const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
console.log(`▸ slippy fish cover · ${SIZE} · ref ${REF.replace(HOME, "~")}`);
const t0 = Date.now();

const sharp = (await import(`${REPO}/ac-electron/node_modules/sharp/lib/index.js`)).default;

// Downscale the reference to the gen size before upload — a 3000² file stalls
// the request and gpt-image-2 resizes internally anyway. 1024² guides structure
// just as well and uploads in a blink.
const REF_1024 = `${LANE}/.ref-${LABEL}-1024.jpg`;
await sharp(readFileSync(REF)).resize(1024, 1024).jpeg({ quality: 90 }).toFile(REF_1024);

// Use curl for the upload — node's undici fetch stalls on this multipart POST
// in some network environments; curl is reliable. Response b64 -> base64 file.
const RESP = `${LANE}/.resp-${LABEL}.json`;
const r = spawnSync(
  "curl",
  [
    "-s", "--max-time", "360",
    "https://api.openai.com/v1/images/edits",
    "-H", `Authorization: Bearer ${apiKey}`,
    "-F", "model=gpt-image-2",
    "-F", "size=" + SIZE,
    "-F", "quality=high",
    "-F", "n=1",
    "-F", `image[]=@${REF_1024};type=image/jpeg`,
    "-F", `prompt=${prompt}`,
    "-o", RESP,
  ],
  { stdio: "inherit" },
);
if (r.status !== 0) {
  console.error(`✗ curl failed (status ${r.status})`);
  process.exit(1);
}
const json = JSON.parse(readFileSync(RESP, "utf8"));
const b64 = json.data?.[0]?.b64_json;
if (!b64) {
  console.error(`✗ no image: ${JSON.stringify(json).slice(0, 400)}`);
  process.exit(1);
}
writeFileSync(OUT_1024, Buffer.from(b64, "base64"));
console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT_1024.replace(REPO + "/", "")}`);

// Upscale to 3000² delivery size with sharp (already imported above).
await sharp(OUT_1024).resize(3000, 3000, { kernel: "lanczos3" }).png().toFile(OUT_3000);
console.log(`✓ upscaled → ${OUT_3000.replace(REPO + "/", "")}`);

// Place a copy on the Desktop (write last, after the file is complete).
spawnSync("cp", [OUT_3000, DESKTOP_OUT]);
console.log(`✓ desktop copy → ${DESKTOP_OUT.replace(HOME, "~")}`);
