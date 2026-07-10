#!/usr/bin/env node
// fal-upscale.mjs — super-resolution for the pals logos (or any image) via
// fal.ai. AuraSR (default) is a clean 4× GAN upscaler ideal for graphic/logo
// art; clarity adds invented detail (better for photos). Reuses the FAL_KEY +
// queue pattern from pop/lib/fal.mjs.
//
// Usage:
//   node bin/fal-upscale.mjs <in.png> [out.png] [--model aura-sr|clarity|esrgan] [--factor 4]
//   node bin/fal-upscale.mjs out/pals/cf-sunset.png --model aura-sr

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname, basename, extname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");

function loadFalKey() {
  if (process.env.FAL_KEY) return process.env.FAL_KEY;
  const envFile = resolve(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  for (const line of readFileSync(envFile, "utf8").split("\n")) {
    if (line.startsWith("FAL_KEY=")) return line.slice("FAL_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
  }
  throw new Error("FAL_KEY not set and not in vault devcontainer.env");
}

const argv = process.argv.slice(2);
const flag = (k, d) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : d; };
const positional = argv.filter((a, i) => !a.startsWith("--") && argv[i - 1] !== `--model` && argv[i - 1] !== `--factor`);

const inPath = resolve(process.cwd(), positional[0] || "");
if (!positional[0]) { console.error("usage: fal-upscale.mjs <in> [out] [--model aura-sr|clarity|esrgan] [--factor 4]"); process.exit(1); }
const modelName = flag("model", "aura-sr");
const factor = Number(flag("factor", 4));
const outPath = positional[1]
  ? resolve(process.cwd(), positional[1])
  : inPath.replace(new RegExp(`${extname(inPath)}$`), `.x${factor}.png`);

const MODELS = {
  "aura-sr":  { endpoint: "fal-ai/aura-sr",           input: (u) => ({ image_url: u, upscaling_factor: factor }) },
  "clarity":  { endpoint: "fal-ai/clarity-upscaler",  input: (u) => ({ image_url: u, upscale_factor: factor, creativity: 0.2, resemblance: 0.9 }) },
  "esrgan":   { endpoint: "fal-ai/esrgan",            input: (u) => ({ image_url: u, scale: factor }) },
};
const model = MODELS[modelName];
if (!model) { console.error(`unknown model '${modelName}'. options: ${Object.keys(MODELS).join(", ")}`); process.exit(1); }

const KEY = loadFalKey();
const auth = { Authorization: `Key ${KEY}` };

const buf = readFileSync(inPath);
const dataUri = `data:image/png;base64,${buf.toString("base64")}`;

console.log(`▸ upscaling ${basename(inPath)} ×${factor} via fal-ai/${model.endpoint.split("/").pop()} …`);
// 1. submit
let res = await fetch(`https://queue.fal.run/${model.endpoint}`, {
  method: "POST", headers: { ...auth, "Content-Type": "application/json" },
  body: JSON.stringify(model.input(dataUri)),
});
if (!res.ok) { console.error(`✗ submit ${res.status}: ${(await res.text()).slice(0, 300)}`); process.exit(1); }
const job = await res.json();
const statusUrl = job.status_url;
const responseUrl = job.response_url;

// 2. poll
let status = job.status;
const t0 = Date.now();
while (status !== "COMPLETED") {
  if (status === "FAILED" || Date.now() - t0 > 300000) { console.error(`✗ job ${status || "timeout"}`); process.exit(1); }
  await new Promise((r) => setTimeout(r, 2500));
  const s = await fetch(statusUrl, { headers: auth });
  status = (await s.json()).status;
  process.stdout.write(`\r  ${status} · ${((Date.now() - t0) / 1000).toFixed(0)}s   `);
}
process.stdout.write("\n");

// 3. fetch result + download
const r = await fetch(responseUrl, { headers: auth });
const result = await r.json();
const url = result.image?.url || result.images?.[0]?.url || result.output?.url;
if (!url) { console.error(`✗ no image in result: ${JSON.stringify(result).slice(0, 300)}`); process.exit(1); }
const img = await fetch(url);
writeFileSync(outPath, Buffer.from(await img.arrayBuffer()));
console.log(`✓ ${outPath}${result.image?.width ? ` (${result.image.width}×${result.image.height})` : ""}`);
