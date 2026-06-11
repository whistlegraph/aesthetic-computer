#!/usr/bin/env node
// clip-wizard/bin/gen-mascot.mjs — generate the ClipWizard app mascot.
//
// Prompt: clip-wizard/Sources/ClipWizard/Assets/clipwizard-mascot-prompt.txt
// Output: clip-wizard/Sources/ClipWizard/Assets/clipwizard-mascot.png
//
// Usage:
//   node clip-wizard/bin/gen-mascot.mjs           # cached if mascot exists
//   node clip-wizard/bin/gen-mascot.mjs --force   # regenerate

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  if (process.argv[i].startsWith("--")) flags[process.argv[i].slice(2)] = true;
}
const FORCE = flags.force === true;
const SIZE = "1024x1024";

const PROMPT_PATH = `${LANE}/Sources/ClipWizard/Assets/clipwizard-mascot-prompt.txt`;
const OUT_PATH    = `${LANE}/Sources/ClipWizard/Assets/clipwizard-mascot.png`;
mkdirSync(dirname(OUT_PATH), { recursive: true });

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

if (existsSync(OUT_PATH) && !FORCE) {
  console.log(`✓ cached mascot → ${OUT_PATH.replace(REPO + "/", "")} (use --force to regen)`);
  process.exit(0);
}

const apiKey = loadOpenAIKey();
const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
console.log(`▸ clip-wizard mascot · ${SIZE}`);
const t0 = Date.now();

const body = {
  model: "gpt-image-2",
  prompt,
  size: SIZE,
  quality: "high",
  background: "opaque",
  n: 1,
};

const res = await fetch("https://api.openai.com/v1/images/generations", {
  method: "POST",
  headers: {
    Authorization: `Bearer ${apiKey}`,
    "Content-Type": "application/json",
  },
  body: JSON.stringify(body),
});
if (!res.ok) {
  console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 800)}`);
  process.exit(1);
}
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) {
  console.error(`✗ no image: ${JSON.stringify(json).slice(0, 280)}`);
  process.exit(1);
}
writeFileSync(OUT_PATH, Buffer.from(b64, "base64"));
console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT_PATH.replace(REPO + "/", "")}`);
