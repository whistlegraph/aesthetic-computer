#!/usr/bin/env node
// gen-still.mjs — generate a photoreal cinematic still (gpt-image-2) to use as
// a Seedance image-to-video source. No emblem/paper style — straight cinematic.
//
// Usage: node gen-still.mjs <out.png> "<prompt>"
//        node gen-still.mjs still-boot.png   (uses the built-in boot prompt)

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");

function key() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(vault, "utf8").split("\n")) {
    if (line.startsWith("OPENAI_API_KEY=")) {
      return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
    }
  }
  throw new Error("OPENAI_API_KEY not found");
}

const BOOT_PROMPT =
  "Cinematic photograph, dark moody room at night, a single small black laptop " +
  "open on a worn wooden desk, lit almost entirely by its own screen which is " +
  "just beginning to glow to life — a faint warm light blooming on the keyboard " +
  "and spilling onto the desk. A small USB thumb drive is plugged into the side. " +
  "Shallow depth of field, volumetric light, gentle film grain, dramatic low-key " +
  "lighting, the quiet feeling of a machine waking up for the first time. " +
  "No people, no hands, no brand logos or readable text anywhere on the laptop. " +
  "Shot on 35mm film, anamorphic, premium and reverent.";

const out = join(HERE, process.argv[2] || "still-boot.png");
const prompt = process.argv[3] || BOOT_PROMPT;

if (existsSync(out) && !process.argv.includes("--force")) {
  console.log(`[gen-still] ${out.split("/").pop()} exists (pass --force to redo)`);
  process.exit(0);
}

const t0 = Date.now();
const res = await fetch("https://api.openai.com/v1/images/generations", {
  method: "POST",
  headers: { Authorization: `Bearer ${key()}`, "Content-Type": "application/json" },
  body: JSON.stringify({ model: "gpt-image-2", prompt, size: "1536x1024", quality: "high", n: 1 }),
});
if (!res.ok) throw new Error(`OpenAI ${res.status}: ${(await res.text()).slice(0, 400)}`);
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) throw new Error("no image returned");
writeFileSync(out, Buffer.from(b64, "base64"));
console.log(`[gen-still] wrote ${out.split("/").pop()} in ${((Date.now() - t0) / 1000).toFixed(0)}s`);
