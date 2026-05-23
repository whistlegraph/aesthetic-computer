#!/usr/bin/env node
// gen-promo.mjs — central marketing image-gen entrypoint.
//
// Reads a campaign directory (anywhere on disk — repo or Desktop):
//   <campaign>/cover-prompt.txt        prompt body (required)
//   <campaign>/refs/                   optional outside-world reference images
//   <campaign>/gens/                   output dir (created)
//
// Calls OpenAI gpt-image-2. By default uses /v1/images/edits with the
// shared jeffrey identity refs (SHOOT + SELFIE). Pass --no-jeffrey to call
// /v1/images/generations instead (no refs, prompt-only).
//
// Usage:
//   node marketing/bin/gen-promo.mjs ~/Desktop/nela-now-instant
//   node marketing/bin/gen-promo.mjs ~/Desktop/nela-now-instant --size 1536x1024
//   node marketing/bin/gen-promo.mjs ./marketing/campaigns/foo --no-jeffrey
//   node marketing/bin/gen-promo.mjs ~/Desktop/nela-now-instant --variant v2
//   node marketing/bin/gen-promo.mjs ~/Desktop/nela-now-instant --mirror ~/Desktop
//
// --mirror <dir>: also drop a flat copy at <dir>/<campaign>-<variant>.png
// (default ON when campaign-dir lives anywhere under ~/Desktop, mirroring
// to ~/Desktop itself — pass --no-mirror to disable).

import { readFileSync, writeFileSync, mkdirSync, existsSync, readdirSync, statSync, copyFileSync } from "node:fs";
import { resolve, dirname, join, basename, extname } from "node:path";
import { fileURLToPath } from "node:url";
import { jeffreyRefs } from "../lib/jeffrey-refs.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  } else positional.push(a);
}

const campaignArg = positional[0];
if (!campaignArg) {
  console.error("usage: gen-promo.mjs <campaign-dir> [--size WxH] [--variant NAME] [--no-jeffrey] [--force]");
  process.exit(1);
}

const campaignDir = resolve(campaignArg.replace(/^~/, process.env.HOME));
if (!existsSync(campaignDir) || !statSync(campaignDir).isDirectory()) {
  console.error(`✗ campaign dir not found: ${campaignDir}`);
  process.exit(1);
}

// --prompt-file <path> overrides the default cover-prompt.txt lookup,
// so a section can be regenerated against a tweaked prompt (e.g. a
// square-framing variant of the portrait prompt) without renaming the
// canonical file. Path is resolved relative to the campaign dir if
// not absolute.
const promptPath = flags["prompt-file"]
  ? resolve(campaignDir, String(flags["prompt-file"]).replace(/^~/, process.env.HOME))
  : join(campaignDir, "cover-prompt.txt");
if (!existsSync(promptPath)) {
  console.error(`✗ no prompt file at ${promptPath}`);
  process.exit(1);
}
const prompt = readFileSync(promptPath, "utf8").trim();

const size = flags.size || "1024x1536";
const variant = flags.variant || "v1";
const useJeffrey = !flags["no-jeffrey"];
const force = !!flags.force;

const gensDir = join(campaignDir, "gens");
mkdirSync(gensDir, { recursive: true });
const outPath = join(gensDir, `${variant}.png`);

if (existsSync(outPath) && !force) {
  console.log(`· cached: ${outPath} (--force to regen)`);
  process.exit(0);
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
const apiKey = loadOpenAIKey();

const campaignRefsDir = join(campaignDir, "refs");
const campaignRefs = existsSync(campaignRefsDir)
  ? readdirSync(campaignRefsDir)
      .filter((f) => /\.(jpe?g|png|webp)$/i.test(f))
      .map((f) => join(campaignRefsDir, f))
  : [];

console.log(`▸ campaign: ${basename(campaignDir)}`);
console.log(`  prompt: ${prompt.length} chars`);
console.log(`  size: ${size} · variant: ${variant} · jeffrey: ${useJeffrey}`);
console.log(`  campaign refs: ${campaignRefs.length}`);
console.log(`  → ${outPath}`);

const t0 = Date.now();

// Retry wrapper for the OpenAI image call. gpt-image-2 occasionally
// times out (ETIMEDOUT) or drops the socket (EPIPE) mid-upload of a
// large 14-ref bundle — those failures are transient and a single
// retry almost always succeeds. We also raise the per-request timeout
// to 10 minutes so the underlying fetch doesn't bail prematurely on
// the long server-side render.
async function callOpenAIWithRetry(maxAttempts = 4) {
  let lastErr = null;
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    const ac = new AbortController();
    const timer = setTimeout(() => ac.abort(), 10 * 60 * 1000);
    try {
      let res;
      if (useJeffrey || campaignRefs.length) {
        const refs = [...(useJeffrey ? jeffreyRefs() : []), ...campaignRefs];
        if (attempt === 1) console.log(`  total refs: ${refs.length}`);
        const fd = new FormData();
        fd.append("model", "gpt-image-2");
        fd.append("prompt", prompt);
        fd.append("size", size);
        fd.append("quality", "high");
        fd.append("n", "1");
        for (const ref of refs) {
          const buf = readFileSync(ref);
          const mime = extname(ref).toLowerCase() === ".png" ? "image/png"
                     : extname(ref).toLowerCase() === ".webp" ? "image/webp"
                     : "image/jpeg";
          fd.append("image[]", new Blob([buf], { type: mime }), basename(ref));
        }
        res = await fetch("https://api.openai.com/v1/images/edits", {
          method: "POST",
          headers: { Authorization: `Bearer ${apiKey}` },
          body: fd,
          signal: ac.signal,
        });
      } else {
        res = await fetch("https://api.openai.com/v1/images/generations", {
          method: "POST",
          headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
          body: JSON.stringify({ model: "gpt-image-2", prompt, size, quality: "high", n: 1 }),
          signal: ac.signal,
        });
      }
      clearTimeout(timer);
      return res;
    } catch (e) {
      clearTimeout(timer);
      lastErr = e;
      const cause = e?.cause?.code || e?.code || e?.name;
      const transient = ["ETIMEDOUT", "EPIPE", "ECONNRESET", "ECONNREFUSED", "UND_ERR_SOCKET", "AbortError"].includes(cause);
      if (attempt < maxAttempts && transient) {
        const backoff = Math.min(60, 3 * Math.pow(2, attempt - 1));
        console.error(`  ⚠ attempt ${attempt} failed (${cause}); retrying in ${backoff}s…`);
        await new Promise((r) => setTimeout(r, backoff * 1000));
        continue;
      }
      throw e;
    }
  }
  throw lastErr;
}
const res = await callOpenAIWithRetry();

if (!res.ok) {
  console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 500)}`);
  process.exit(2);
}
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) {
  console.error(`✗ no image returned: ${JSON.stringify(json).slice(0, 300)}`);
  process.exit(2);
}
writeFileSync(outPath, Buffer.from(b64, "base64"));

const dur = ((Date.now() - t0) / 1000).toFixed(1);
const usage = json.usage || {};
console.log(`✓ ${outPath} (${dur}s · in=${usage.input_tokens || "?"} out=${usage.output_tokens || "?"})`);

// Mirror a flat copy somewhere browsable. Default target = ~/Desktop when
// the campaign dir itself lives under ~/Desktop; otherwise mirroring is off
// unless --mirror <dir> is passed explicitly.
const desktop = `${process.env.HOME}/Desktop`;
let mirrorDir = null;
if (flags.mirror && flags.mirror !== true) {
  mirrorDir = resolve(String(flags.mirror).replace(/^~/, process.env.HOME));
} else if (!flags["no-mirror"] && campaignDir.startsWith(desktop)) {
  mirrorDir = desktop;
}
if (mirrorDir) {
  mkdirSync(mirrorDir, { recursive: true });
  const mirrorPath = join(mirrorDir, `${basename(campaignDir)}-${variant}.png`);
  copyFileSync(outPath, mirrorPath);
  console.log(`✓ mirror: ${mirrorPath}`);
}
