#!/usr/bin/env node
// Render comparable fal.ai cover edits and assemble labeled 2x2 contact sheets.

import { existsSync, mkdirSync, readFileSync, writeFileSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { falKey, uploadToFalStorage } from "../../lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const SOURCE = resolve(LANE, "cover/special-sign-cosmic-turtle-colored-pencil-v2.png");
const OUT = resolve(LANE, "cover/fal-contact-sheets");
mkdirSync(OUT, { recursive: true });

const common = `Edit the supplied square album cover. Preserve Jeffrey's recognizable face, natural loose brown hair, tactile colored-pencil and graphite paper texture, muted cream/indigo/turquoise/ochre palette, geometric cosmic turtle shell, tiny Pals line seal on the shell, sparse scientific signal diagrams, and absence of all typography. Recompose the turtle into a believable low close three-quarter swimming pose seen through a pronounced circular fisheye lens. The head must emerge along a plausible sea-turtle neck axis rather than a vertical human torso. Exactly four anatomically coherent flippers: the nearest front flipper is strongly foreshortened toward the lens, the far front flipper is smaller and partially occluded, and the two rear flippers follow the shell perspective. Keep one head, one shell, no human shoulders, no extra limbs. The shell curves convincingly in fisheye perspective and may crop naturally at the square edges. Keep the strange restrained 1990s alternative-rock sleeve feeling rather than glossy fantasy key art. Add a small original early-2000s console-rendered sun-shine medallion motif: chunky rounded rays, pearly gold/cyan bloom, toy-like low-poly gloss, integrated as an uncanny object near the shell. It must not copy Mario, a Shine Sprite, Nintendo symbols, or any recognizable game asset. No words, watermark, dreadlocks, braids, white hair, cable hair, horror, mascot smile, or dense starfield.`;

const sheets = [
  {
    slug: "01-believable-fisheye",
    label: "BELIEVABLE FISHEYE",
    prompt: `${common} Camera direction: intimate but not comedic, approximately a 14mm full-frame fisheye feel. Keep the whole head and most of the shell readable. The medallion is subtle, about the apparent size of one eye, with restrained bloom.`,
  },
  {
    slug: "02-shine-push",
    label: "SHINE PUSH",
    prompt: `${common} Camera direction: energetic 8mm circular-fisheye exaggeration with the near flipper and shell nose entering the lens field. Keep Jeffrey's facial proportions recognizable despite lens distortion. Make the original sun medallion larger and more playful, with pearlescent GameCube-era material, rounded low-poly rays, cyan-gold lens bloom, and a faint colored-pencil halo; it remains a new generic object with no face and no copyrighted emblem.`,
  },
];

const models = [
  {
    slug: "gpt-image-2-high",
    label: "GPT IMAGE 2 · HIGH",
    endpoint: "openai/gpt-image-2/edit",
    input: (url, prompt) => ({ prompt, image_urls: [url], image_size: "square_hd", quality: "high", output_format: "png" }),
  },
  {
    slug: "nano-banana-pro",
    label: "NANO BANANA PRO · 2K",
    endpoint: "fal-ai/nano-banana-pro/edit",
    input: (url, prompt) => ({ prompt, image_urls: [url], aspect_ratio: "1:1", resolution: "2K", output_format: "png", num_images: 1 }),
  },
  {
    slug: "seedream-5-pro",
    label: "SEEDREAM 5 PRO",
    endpoint: "bytedance/seedream/v5/pro/edit",
    input: (url, prompt) => ({ prompt, image_urls: [url], image_size: "square_hd", output_format: "png" }),
  },
  {
    slug: "flux-kontext-max",
    label: "FLUX KONTEXT MAX",
    endpoint: "fal-ai/flux-pro/kontext/max",
    input: (url, prompt) => ({ prompt, image_url: url, aspect_ratio: "1:1", output_format: "png", safety_tolerance: "2" }),
  },
];

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const auth = { Authorization: `Key ${falKey()}`, "Content-Type": "application/json" };

async function renderOne(sheet, model, sourceUrl) {
  const out = resolve(OUT, `${sheet.slug}--${model.slug}.png`);
  const queue = `${out}.queue.json`;
  if (existsSync(out)) {
    console.log(`  ✓ ${sheet.slug}/${model.slug}: cached`);
    return out;
  }
  let queued = existsSync(queue) ? JSON.parse(readFileSync(queue, "utf8")) : null;
  if (!queued) {
    console.log(`  → ${sheet.slug}/${model.slug}: submit`);
    const res = await fetch(`https://queue.fal.run/${model.endpoint}`, {
      method: "POST", headers: auth, body: JSON.stringify(model.input(sourceUrl, sheet.prompt)), signal: AbortSignal.timeout(180000),
    });
    if (!res.ok) throw new Error(`${model.slug} submit ${res.status}: ${(await res.text()).slice(0, 500)}`);
    queued = await res.json();
    writeFileSync(queue, JSON.stringify(queued, null, 2));
  } else console.log(`  ↻ ${sheet.slug}/${model.slug}: resume`);

  let status = "";
  const started = Date.now();
  while (status !== "COMPLETED") {
    await sleep(4000);
    const res = await fetch(queued.status_url, { headers: auth });
    if (!res.ok) throw new Error(`${model.slug} status ${res.status}`);
    const body = await res.json();
    if (body.status !== status) {
      status = body.status;
      console.log(`    ${model.slug}: ${status.toLowerCase()} · ${((Date.now() - started) / 1000).toFixed(0)}s`);
    }
    if (status === "FAILED" || body.error) throw new Error(`${model.slug}: ${JSON.stringify(body).slice(0, 800)}`);
  }
  const resultRes = await fetch(queued.response_url, { headers: auth });
  if (!resultRes.ok) throw new Error(`${model.slug} result ${resultRes.status}`);
  const result = await resultRes.json();
  const url = result.images?.[0]?.url || result.image?.url;
  if (!url) throw new Error(`${model.slug}: no image in ${JSON.stringify(result).slice(0, 800)}`);
  const imageRes = await fetch(url);
  if (!imageRes.ok) throw new Error(`${model.slug} download ${imageRes.status}`);
  writeFileSync(out, Buffer.from(await imageRes.arrayBuffer()));
  unlinkSync(queue);
  writeFileSync(`${out}.json`, JSON.stringify({ provider: "fal.ai", endpoint: model.endpoint, source: SOURCE, prompt: sheet.prompt, result, generatedAt: new Date().toISOString() }, null, 2));
  console.log(`  ✓ ${sheet.slug}/${model.slug}: ${out}`);
  return out;
}

function contactSheet(sheet, images) {
  const out = resolve(OUT, `${sheet.slug}--contact-sheet.jpg`);
  const tiles = [];
  for (let i = 0; i < images.length; i++) {
    const tile = resolve(OUT, `.${sheet.slug}--tile-${i}.png`);
    const rendered = spawnSync("magick", [
      images[i], "-resize", "1200x1200", "-background", "#17151c",
      "-gravity", "center", "-extent", "1200x1200", "-gravity", "northwest",
      "-font", "/System/Library/Fonts/Supplemental/Arial Bold.ttf",
      "-pointsize", "54", "-fill", "white", "-undercolor", "#000000B8",
      "-annotate", "+28+28", ` ${models[i].label} `, tile,
    ], { stdio: "inherit" });
    if (rendered.status !== 0) throw new Error(`contact sheet tile failed: ${sheet.slug}/${models[i].slug}`);
    tiles.push(tile);
  }
  const assembled = spawnSync("magick", [
    "(", tiles[0], tiles[1], "+append", ")",
    "(", tiles[2], tiles[3], "+append", ")",
    "-append", "-quality", "94", out,
  ], { stdio: "inherit" });
  for (const tile of tiles) if (existsSync(tile)) unlinkSync(tile);
  if (assembled.status !== 0) throw new Error(`contact sheet failed: ${sheet.slug}`);
  return out;
}

if (!existsSync(SOURCE)) throw new Error(`source missing: ${SOURCE}`);
console.log("Uploading shared source once…");
const sourceUrl = await uploadToFalStorage(SOURCE);
const finalSheets = [];
for (const sheet of sheets) {
  console.log(`\n${sheet.label}`);
  const results = await Promise.allSettled(models.map((model) => renderOne(sheet, model, sourceUrl)));
  const failures = results.map((r, i) => r.status === "rejected" ? `${models[i].slug}: ${r.reason.message}` : null).filter(Boolean);
  if (failures.length) throw new Error(failures.join("\n"));
  finalSheets.push(contactSheet(sheet, results.map((r) => r.value)));
}
writeFileSync(resolve(OUT, "contact-sheets.json"), JSON.stringify({ source: SOURCE, sheets: sheets.map((s, i) => ({ ...s, contactSheet: finalSheets[i] })), models: models.map(({ input, ...m }) => m), generatedAt: new Date().toISOString() }, null, 2));
console.log(`\n✓ ${finalSheets.join("\n✓ ")}`);
