#!/usr/bin/env node
// slab/menuband/marketing/notepat-launch/gen-illy.mjs
// Hand-drawn launch illustration of the neo laptop with the notepat.com
// note-key layout glowing, generated via gpt-image-2 (images/edits) from
// the finished two-tone colorway renders. Follows the repo illy convention
// (see papers/notepat-charts/gen-cover-illy.mjs).
//
//   node gen-illy.mjs            # generate both colorways (cached)
//   node gen-illy.mjs --force    # regenerate
//   node gen-illy.mjs citrus     # just the citrus/blue variant
//   node gen-illy.mjs indigo     # just the indigo/green variant

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..", "..");
const PROMPT_PATH = resolve(HERE, "illy.txt");

const args = process.argv.slice(2);
const flags = Object.fromEntries(
  args.filter((a) => a.startsWith("--")).map((a) => [a.slice(2), true]),
);
const which = args.filter((a) => !a.startsWith("--"));
const FORCE = flags.force === true;

const VARIANTS = [
  { key: "citrus", ref: "refs/neo-twotone-citrus-blue.jpg", out: "illy-citrus-blue.png" },
  { key: "indigo", ref: "refs/neo-twotone-indigo-green.jpg", out: "illy-indigo-green.png" },
].filter((v) => which.length === 0 || which.includes(v.key));

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

const apiKey = loadOpenAIKey();
const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
const size = "1536x1024"; // landscape, matches the overhead laptop composition

async function generate(v) {
  const out = resolve(HERE, v.out);
  if (existsSync(out) && !FORCE) {
    console.log(`✓ cached: ${out.replace(REPO + "/", "")} (--force to regen)`);
    return;
  }
  const refPath = resolve(HERE, v.ref);
  if (!existsSync(refPath)) {
    console.warn(`  ⚠ ref missing, skipping ${v.key}: ${v.ref}`);
    return;
  }

  console.log(`▸ menuband launch illy · ${v.key} · ${size}`);
  const t0 = Date.now();

  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", prompt);
  fd.append("size", size);
  fd.append("quality", "high");
  fd.append("n", "1");
  const buf = readFileSync(refPath);
  fd.append("image[]", new Blob([buf], { type: "image/jpeg" }), v.ref.split("/").pop());

  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}` },
    body: fd,
  });

  if (!res.ok) {
    console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 600)}`);
    process.exitCode = 1;
    return;
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) {
    console.error(`✗ no image: ${JSON.stringify(json).slice(0, 280)}`);
    process.exitCode = 1;
    return;
  }
  writeFileSync(out, Buffer.from(b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${out.replace(REPO + "/", "")}`);
}

for (const v of VARIANTS) await generate(v);
