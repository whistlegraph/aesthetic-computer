#!/usr/bin/env node
// gen-illy.mjs — generate the hopehop album cover (text-to-image).
//
// Lean standalone generator (no jeffrey-platter identity refs — hopehop's
// cover is a panther/milkshake emblem, not a portrait). Calls gpt-image-2
// /v1/images/generations with the prompt in hopehop.illy.txt.
//
//   node pop/hopehop/bin/gen-illy.mjs            # cached; skips if cover exists
//   node pop/hopehop/bin/gen-illy.mjs --force    # regenerate
//
// Output: pop/hopehop/out/hopehop-cover.png (1024x1024)

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const FORCE = process.argv.includes("--force");
const PROMPT_PATH = resolve(LANE, "hopehop.illy.txt");
const OUT_PATH = resolve(LANE, "out", "hopehop-cover.png");

function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = resolve(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
    }
  }
  throw new Error("OPENAI_API_KEY not set and not found in vault devcontainer.env");
}

if (existsSync(OUT_PATH) && !FORCE) { console.log(`✓ cover exists (use --force): ${OUT_PATH}`); process.exit(0); }

const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
const apiKey = loadKey();
mkdirSync(dirname(OUT_PATH), { recursive: true });

console.log("• generating hopehop cover (gpt-image-2, 1024x1024) …");
const res = await fetch("https://api.openai.com/v1/images/generations", {
  method: "POST",
  headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
  body: JSON.stringify({ model: "gpt-image-2", prompt, size: "1024x1024", n: 1 }),
});
if (!res.ok) { console.error(`✗ image API ${res.status}: ${await res.text()}`); process.exit(1); }
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) { console.error("✗ no image in response"); process.exit(1); }
writeFileSync(OUT_PATH, Buffer.from(b64, "base64"));
console.log(`✓ ${OUT_PATH}`);
