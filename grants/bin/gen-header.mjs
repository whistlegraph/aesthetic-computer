#!/usr/bin/env node
// gen-header.mjs — proposal-page header illustration for an AC grant
//
// Reads:    grants/<slug>/figures/cover-prompt.txt   (full standalone prompt)
// Writes:   system/public/<slug>/header.png          (deployed alongside index.html)
//
// Sibling to papers/bin/gen-cover.mjs. Two intentional differences:
//   1. Format is 1536x1024 (3:2 horizontal page-hero) instead of 1024x1024
//      (1:1 paper-cover emblem).
//   2. The prompt file is treated as the COMPLETE prompt — no STYLE_PREFIX
//      prepended. Each proposal carries its own context (Hockney palette,
//      cream paper, peer-horizontality, AC lore) directly in the file.
//
// The slug for the public path defaults to the grants folder slug. If the
// grants folder is named "<short>-2026" the public path matches; the one
// known mismatch is "printed-matter-project-space-2026" → "printed-matter-2026"
// on the public side, handled via the SLUG_MAP below.
//
// Usage:
//   node grants/bin/gen-header.mjs printed-matter-project-space-2026
//   node grants/bin/gen-header.mjs --all
//   node grants/bin/gen-header.mjs <slug> --force

import { readFileSync, writeFileSync, existsSync, readdirSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const GRANTS_DIR = resolve(HERE, "..");
const REPO = resolve(GRANTS_DIR, "..");
const PUBLIC_DIR = join(REPO, "system", "public");

const SLUG_MAP = {
  "printed-matter-project-space-2026": "printed-matter-2026",
  "culturehub-2026": "culture-hub-2026",
};

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (const a of argv) {
  if (a.startsWith("--")) flags[a.slice(2)] = true;
  else positional.push(a);
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
  throw new Error("OPENAI_API_KEY not found");
}

async function genHeader(grantSlug, { force }) {
  const figuresDir = join(GRANTS_DIR, grantSlug, "figures");
  const promptPath = join(figuresDir, "cover-prompt.txt");
  if (!existsSync(promptPath)) {
    return { grantSlug, status: "skip", reason: "no cover-prompt.txt" };
  }

  const publicSlug = SLUG_MAP[grantSlug] ?? grantSlug;
  const outDir = join(PUBLIC_DIR, publicSlug);
  if (!existsSync(outDir)) {
    return { grantSlug, status: "skip", reason: `no public dir at ${outDir}` };
  }
  const outPath = join(outDir, "header.png");
  if (existsSync(outPath) && !force) {
    return { grantSlug, status: "cached", outPath };
  }

  const prompt = readFileSync(promptPath, "utf8").trim();

  const t0 = Date.now();
  const apiKey = loadOpenAIKey();
  const res = await fetch("https://api.openai.com/v1/images/generations", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${apiKey}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      model: "gpt-image-2",
      prompt,
      size: "1536x1024",
      quality: "high",
      n: 1,
    }),
  });
  if (!res.ok) {
    throw new Error(`OpenAI ${res.status}: ${(await res.text()).slice(0, 400)}`);
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) throw new Error("no image returned");
  writeFileSync(outPath, Buffer.from(b64, "base64"));
  return {
    grantSlug,
    publicSlug,
    status: "fresh",
    outPath,
    durSec: (Date.now() - t0) / 1000,
  };
}

let targets = [];
if (flags.all) {
  for (const name of readdirSync(GRANTS_DIR)) {
    const promptPath = join(GRANTS_DIR, name, "figures", "cover-prompt.txt");
    if (existsSync(promptPath)) targets.push(name);
  }
} else if (positional.length) {
  targets = positional;
} else {
  console.error(
    "usage: gen-header.mjs <grant-slug> [--force]\n" +
      "       gen-header.mjs --all [--force]",
  );
  process.exit(1);
}

console.log(`▸ ${targets.length} target(s) · force=${!!flags.force}`);

for (const t of targets) {
  try {
    const r = await genHeader(t, { force: !!flags.force });
    if (r.status === "cached") console.log(`  · cached: ${t} → ${r.outPath}`);
    else if (r.status === "skip") console.log(`  · skip ${t}: ${r.reason}`);
    else
      console.log(
        `  ✓ ${t} → system/public/${r.publicSlug}/header.png (${r.durSec.toFixed(1)}s)`,
      );
  } catch (e) {
    console.error(`  ✗ ${t}: ${e.message}`);
    process.exitCode = 2;
  }
}
