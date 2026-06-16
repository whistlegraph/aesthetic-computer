#!/usr/bin/env node
// shot-wizard/bin/gen-mascot.mjs — generate the ShotWizard app mascot.
//
// Prompt: Sources/ShotWizard/Assets/shotwizard-mascot-prompt.txt
// Output: Sources/ShotWizard/Assets/shotwizard-mascot.png
//
// Usage:  node shot-wizard/bin/gen-mascot.mjs [--force]

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..");
const FORCE = process.argv.includes("--force");

const PROMPT_PATH = `${LANE}/Sources/ShotWizard/Assets/shotwizard-mascot-prompt.txt`;
const OUT_PATH = `${LANE}/Sources/ShotWizard/Assets/shotwizard-mascot.png`;
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
console.log("▸ shot-wizard mascot · 1024x1024");
const t0 = Date.now();
const res = await fetch("https://api.openai.com/v1/images/generations", {
  method: "POST",
  headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
  body: JSON.stringify({ model: "gpt-image-2", prompt, size: "1024x1024", quality: "high", n: 1 }),
});
if (!res.ok) throw new Error(`OpenAI ${res.status}: ${(await res.text()).slice(0, 400)}`);
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) throw new Error("no image returned");
writeFileSync(OUT_PATH, Buffer.from(b64, "base64"));
console.log(`✓ mascot → ${OUT_PATH.replace(REPO + "/", "")} in ${((Date.now() - t0) / 1000).toFixed(0)}s`);
