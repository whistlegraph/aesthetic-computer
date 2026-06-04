#!/usr/bin/env node
// bills/bin/gen-illy.mjs — generate the editorial "illy" that heads each
// bills update. A calm colored-pencil billing-desk still life via gpt-image-2.
//
// Prompt: bills/illy.txt
// Output: bills/out/bills-illy.png            (archival original)
//         system/public/bills.aesthetic.computer/illy.png   (served on the site)
//
// Usage:
//   node bills/bin/gen-illy.mjs            # cached if the site illy exists
//   node bills/bin/gen-illy.mjs --force    # regenerate

import { readFileSync, writeFileSync, existsSync, mkdirSync, copyFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");          // bills/
const REPO = resolve(LANE, "..");          // repo root

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  if (process.argv[i].startsWith("--")) flags[process.argv[i].slice(2)] = true;
}
const FORCE = flags.force === true;
const SIZE = "1536x1024"; // wide banner

const PROMPT_PATH = resolve(LANE, "illy.txt");
const OUT_PATH    = resolve(LANE, "out", "bills-illy.png");          // archival original
const SITE_PATH   = resolve(REPO, "system/public/bills.aesthetic.computer", "illy.jpg"); // web banner
mkdirSync(resolve(LANE, "out"), { recursive: true });

// Optimize the served banner: 1280px-wide JPEG (~400KB) via macOS sips,
// falling back to a raw PNG copy if sips is unavailable.
function writeServed(srcPng) {
  const sips = spawnSync("sips", ["-Z", "1280", "-s", "format", "jpeg",
    "-s", "formatOptions", "80", srcPng, "--out", SITE_PATH], { stdio: "ignore" });
  if (sips.status === 0 && existsSync(SITE_PATH)) return true;
  copyFileSync(srcPng, SITE_PATH.replace(/\.jpg$/, ".png"));
  return false;
}

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

if (existsSync(SITE_PATH) && !FORCE) {
  console.log(`✓ cached illy → ${SITE_PATH.replace(REPO + "/", "")} (use --force to regen)`);
  process.exit(0);
}

const apiKey = loadOpenAIKey();
const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
console.log(`▸ bills illy · ${SIZE}`);
const t0 = Date.now();

const res = await fetch("https://api.openai.com/v1/images/generations", {
  method: "POST",
  headers: {
    Authorization: `Bearer ${apiKey}`,
    "Content-Type": "application/json",
  },
  body: JSON.stringify({
    model: "gpt-image-2",
    prompt,
    size: SIZE,
    quality: "high",
    n: 1,
  }),
});
if (!res.ok) {
  console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 600)}`);
  process.exit(1);
}
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) {
  console.error(`✗ no image: ${JSON.stringify(json).slice(0, 280)}`);
  process.exit(1);
}
const buf = Buffer.from(b64, "base64");
writeFileSync(OUT_PATH, buf);
const optimized = writeServed(OUT_PATH);
console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT_PATH.replace(REPO + "/", "")}`);
console.log(`✓ served ${optimized ? "JPEG" : "PNG"} → ${SITE_PATH.replace(REPO + "/", "")}`);
