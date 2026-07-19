#!/usr/bin/env node
// Farm machine-read visual descriptions for every video post in the
// whistlegraph.org platter.
//
// This deliberately does not send TikTok captions or speech transcripts to the
// vision model: `desc` is author copy, while these records must describe only
// what is visible in six chronologically sampled frames.
//
// Examples:
//   node visual-farm.mjs --dry-run --limit 3
//   OPENAI_API_KEY=... node visual-farm.mjs --limit 3
//   OPENAI_API_KEY=... node visual-farm.mjs --all --concurrency 3
//   node visual-farm.mjs --merge-only

import { spawn } from "node:child_process";
import {
  createHash,
} from "node:crypto";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = join(HERE, "..", "..");
const POSTS_PATH = join(REPO, "system", "public", "whistlegraph.org", "posts.json");
const DOWNLOADS = join(HERE, "downloads");
const RECORDS_DIR = join(DOWNLOADS, "visual-records");
const MERGED_PATH = join(DOWNLOADS, "VISUALS.json");
const PROMPT_VERSION = "whistlegraph-visual-v1";
const DEFAULT_MODEL = "gpt-5.4-mini";
const SAMPLE_FRACTIONS = [0.05, 0.22, 0.4, 0.58, 0.76, 0.94];

const DESCRIPTION_SCHEMA = {
  type: "object",
  additionalProperties: false,
  properties: {
    summary: {
      type: "string",
      description: "Two or three concrete sentences describing the visible sequence.",
    },
    sequence: {
      type: "array",
      items: { type: "string" },
      description: "Chronological visible actions or changes across the six frames.",
    },
    setting: { type: "string", description: "The visible physical or digital setting." },
    subjects: {
      type: "array",
      items: { type: "string" },
      description: "Visible people, hands, objects, drawings, or screens, described neutrally.",
    },
    drawingSurface: {
      type: "string",
      description: "Visible drawing surface, or an empty string when none is visible.",
    },
    toolsAndMaterials: {
      type: "array",
      items: { type: "string" },
      description: "Only tools and materials visibly supported by the frames.",
    },
    marksAndForms: {
      type: "array",
      items: { type: "string" },
      description: "Visible lines, marks, symbols, figures, and how they develop.",
    },
    visibleText: {
      type: "array",
      items: { type: "string" },
      description: "Clearly legible on-screen or physical text; omit guesses.",
    },
    camera: {
      type: "string",
      description: "Framing, viewpoint, and visible camera changes across the samples.",
    },
    uncertainties: {
      type: "array",
      items: { type: "string" },
      description: "Important things the sampled frames do not establish.",
    },
    confidence: {
      type: "number",
      description: "Overall confidence from 0 to 1 in this sampled-frame description.",
    },
  },
  required: [
    "summary",
    "sequence",
    "setting",
    "subjects",
    "drawingSurface",
    "toolsAndMaterials",
    "marksAndForms",
    "visibleText",
    "camera",
    "uncertainties",
    "confidence",
  ],
};

const SYSTEM_PROMPT = `You are building a visual evidence archive for Whistlegraph videos.
Describe only what is visibly supported by the supplied 3-by-2 contact sheet. The
panels run chronologically from top-left to bottom-right. Do not use or invent an
author caption, transcript, melody, intent, identity, relationship, location, or
story that the frames do not establish. Do not identify people or infer sensitive
traits. Pay special attention to hands, drawing tools, surfaces, evolving marks,
figures, symbols, materials, and changes between panels. A contact sheet samples
the video rather than showing continuous motion, so state uncertainty instead of
turning a before/after difference into a definite action. Use plain descriptive
language suitable for search, accessibility, and later human curation.`;

function parseArgs(argv) {
  const out = { ids: [], concurrency: 2, model: process.env.WG_VISUAL_MODEL || DEFAULT_MODEL };
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const value = argv[i + 1];
    if (arg === "--id" && value) { out.ids.push(...value.split(",")); i += 1; }
    else if (arg === "--limit" && value) { out.limit = Number(value); i += 1; }
    else if (arg === "--concurrency" && value) { out.concurrency = Number(value); i += 1; }
    else if (arg === "--model" && value) { out.model = value; i += 1; }
    else if (arg === "--all") out.all = true;
    else if (arg === "--force") out.force = true;
    else if (arg === "--dry-run") out.dryRun = true;
    else if (arg === "--merge-only") out.mergeOnly = true;
    else if (arg === "--keep-montages") out.keepMontages = true;
    else if (arg === "--help" || arg === "-h") out.help = true;
    else throw new Error(`unknown or incomplete argument: ${arg}`);
  }
  if (!Number.isInteger(out.concurrency) || out.concurrency < 1 || out.concurrency > 8) {
    throw new Error("--concurrency must be an integer from 1 to 8");
  }
  if (out.limit !== undefined && (!Number.isInteger(out.limit) || out.limit < 1)) {
    throw new Error("--limit must be a positive integer");
  }
  return out;
}

function usage() {
  console.log(`Whistlegraph visual-description farm

  --id ID[,ID]       process specific post ids
  --limit N          process the first N missing records
  --all              process every missing video post (required for a full run)
  --concurrency N    simultaneous jobs, 1-8 (default 2)
  --model MODEL      vision-capable Responses API model (default ${DEFAULT_MODEL})
  --force            replace records even when prompt/model provenance matches
  --dry-run          report the queue without downloading or calling the API
  --merge-only       rebuild downloads/VISUALS.json from completed records
  --keep-montages    retain contact sheets under downloads/visual-montages/
`);
}

function run(command, args, options = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, { stdio: ["ignore", "pipe", "pipe"], ...options });
    let stdout = "";
    let stderr = "";
    child.stdout?.on("data", (chunk) => { stdout += chunk; });
    child.stderr?.on("data", (chunk) => { stderr += chunk; });
    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) resolve(stdout);
      else reject(new Error(`${command} exited ${code}: ${stderr.trim() || stdout.trim()}`));
    });
  });
}

function recordPath(id) {
  if (!/^\d+$/.test(String(id))) throw new Error(`unsafe post id: ${id}`);
  return join(RECORDS_DIR, `${id}.json`);
}

function readRecord(id) {
  const path = recordPath(id);
  if (!existsSync(path)) return null;
  try { return JSON.parse(readFileSync(path, "utf8")); }
  catch { return null; }
}

function writeJsonAtomic(path, value) {
  mkdirSync(dirname(path), { recursive: true });
  const temp = `${path}.tmp-${process.pid}-${Date.now()}`;
  writeFileSync(temp, `${JSON.stringify(value, null, 2)}\n`);
  renameSync(temp, path);
}

function mergeRecords() {
  mkdirSync(RECORDS_DIR, { recursive: true });
  const visuals = readdirSync(RECORDS_DIR)
    .filter((name) => /^\d+\.json$/.test(name))
    .map((name) => JSON.parse(readFileSync(join(RECORDS_DIR, name), "utf8")))
    .filter((record) => record.status === "complete" && record.visual)
    .sort((a, b) => String(a.postId).localeCompare(String(b.postId)));
  const merged = {
    version: 1,
    promptVersion: PROMPT_VERSION,
    generated: new Date().toISOString(),
    count: visuals.length,
    visuals,
  };
  writeJsonAtomic(MERGED_PATH, merged);
  console.log(`merged ${visuals.length} records -> ${MERGED_PATH}`);
  return merged;
}

async function extractContactSheet(post, workDir) {
  const videoPath = join(workDir, `${post.id}.mp4`);
  const sheetPath = join(workDir, `${post.id}.jpg`);
  // Very recent pulls may not have reached the public CDN yet. Prefer any
  // ignored local grab before fetching, which also makes retries on a farm
  // machine work after the source mp4 has been copied into downloads/.
  const localName = readdirSync(DOWNLOADS).find((name) =>
    name === `${post.id}.mp4` || name.endsWith(`-${post.id}.mp4`));
  if (localName) copyFileSync(join(DOWNLOADS, localName), videoPath);
  else {
    await run("curl", [
      "--location", "--fail", "--silent", "--show-error",
      "--retry", "3", "--retry-delay", "2",
      "--output", videoPath, post.src,
    ]);
  }
  const durationText = await run(process.env.FFPROBE_BIN || "ffprobe", [
    "-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", videoPath,
  ]);
  const duration = Number(durationText.trim());
  if (!Number.isFinite(duration) || duration <= 0) throw new Error("ffprobe returned no duration");
  const sampledSeconds = SAMPLE_FRACTIONS.map((fraction) =>
    Number(Math.min(Math.max(duration * fraction, 0.01), Math.max(duration - 0.01, 0.01)).toFixed(3)));
  const framePaths = [];
  for (let i = 0; i < sampledSeconds.length; i += 1) {
    const framePath = join(workDir, `frame-${i}.jpg`);
    framePaths.push(framePath);
    await run(process.env.FFMPEG_BIN || "ffmpeg", [
      "-hide_banner", "-loglevel", "error", "-y",
      "-ss", String(sampledSeconds[i]), "-i", videoPath,
      "-frames:v", "1",
      "-vf", "scale=512:512:force_original_aspect_ratio=decrease,pad=512:512:(ow-iw)/2:(oh-ih)/2:color=white",
      "-q:v", "3", framePath,
    ]);
  }
  const inputs = framePaths.flatMap((path) => ["-i", path]);
  await run(process.env.FFMPEG_BIN || "ffmpeg", [
    "-hide_banner", "-loglevel", "error", "-y", ...inputs,
    "-filter_complex",
    "[0:v][1:v][2:v]hstack=inputs=3[top];[3:v][4:v][5:v]hstack=inputs=3[bottom];[top][bottom]vstack=inputs=2[out]",
    "-map", "[out]", "-frames:v", "1", "-q:v", "3", sheetPath,
  ]);
  return { sheetPath, duration: Number(duration.toFixed(3)), sampledSeconds };
}

function responseText(payload) {
  for (const item of payload.output || []) {
    if (item.type !== "message") continue;
    for (const content of item.content || []) {
      if (content.type === "output_text" && typeof content.text === "string") return content.text;
    }
  }
  throw new Error("Responses API returned no output_text");
}

async function describeSheet(sheetPath, model, sampledSeconds) {
  const imageBytes = readFileSync(sheetPath);
  const imageUrl = `data:image/jpeg;base64,${imageBytes.toString("base64")}`;
  const body = {
    model,
    input: [
      { role: "system", content: [{ type: "input_text", text: SYSTEM_PROMPT }] },
      {
        role: "user",
        content: [
          {
            type: "input_text",
            text: `Sample times in panel order: ${sampledSeconds.join(", ")} seconds. Produce the visual evidence record.`,
          },
          { type: "input_image", image_url: imageUrl, detail: "high" },
        ],
      },
    ],
    text: {
      format: {
        type: "json_schema",
        name: "whistlegraph_visual_evidence",
        strict: true,
        schema: DESCRIPTION_SCHEMA,
      },
    },
    max_output_tokens: 1800,
  };
  let lastError;
  for (let attempt = 1; attempt <= 5; attempt += 1) {
    const response = await fetch("https://api.openai.com/v1/responses", {
      method: "POST",
      headers: {
        authorization: `Bearer ${process.env.OPENAI_API_KEY}`,
        "content-type": "application/json",
      },
      body: JSON.stringify(body),
    });
    if (response.ok) {
      const payload = await response.json();
      return {
        visual: JSON.parse(responseText(payload)),
        responseId: payload.id,
        usage: payload.usage || null,
        montageSha256: createHash("sha256").update(imageBytes).digest("hex"),
      };
    }
    const detail = await response.text();
    lastError = new Error(`OpenAI ${response.status}: ${detail.slice(0, 500)}`);
    if (response.status !== 429 && response.status < 500) break;
    await new Promise((resolve) => setTimeout(resolve, (2 ** attempt) * 1000 + Math.random() * 750));
  }
  throw lastError;
}

async function processPost(post, options, index, total) {
  const prefix = `[${index + 1}/${total}] ${post.id}`;
  const workDir = mkdtempSync(join(tmpdir(), `wg-visual-${post.id}-`));
  try {
    console.log(`${prefix} extracting six frames`);
    const extracted = await extractContactSheet(post, workDir);
    console.log(`${prefix} describing with ${options.model}`);
    const result = await describeSheet(extracted.sheetPath, options.model, extracted.sampledSeconds);
    const now = new Date().toISOString();
    const record = {
      version: 1,
      status: "complete",
      postId: String(post.id),
      source: {
        url: post.src,
        duration: extracted.duration,
        sampledSeconds: extracted.sampledSeconds,
      },
      visual: result.visual,
      provenance: {
        model: options.model,
        promptVersion: PROMPT_VERSION,
        responseId: result.responseId,
        montageSha256: result.montageSha256,
        createdAt: now,
        usage: result.usage,
      },
    };
    writeJsonAtomic(recordPath(post.id), record);
    if (options.keepMontages) {
      const montagePath = join(HERE, "downloads", "visual-montages", `${post.id}.jpg`);
      mkdirSync(dirname(montagePath), { recursive: true });
      writeFileSync(montagePath, readFileSync(extracted.sheetPath));
    }
    console.log(`${prefix} complete: ${record.visual.summary}`);
    return record;
  } catch (error) {
    console.error(`${prefix} failed: ${error.message}`);
    return null;
  } finally {
    rmSync(workDir, { recursive: true, force: true });
  }
}

async function main() {
  const options = parseArgs(process.argv.slice(2));
  if (options.help) { usage(); return; }
  if (options.mergeOnly) { mergeRecords(); return; }
  if (!options.all && options.limit === undefined && options.ids.length === 0) {
    usage();
    throw new Error("choose --id, --limit, or explicit --all");
  }
  const platter = JSON.parse(readFileSync(POSTS_PATH, "utf8"));
  const requested = new Set(options.ids.map(String));
  let queue = platter.posts.filter((post) => post.media === "video");
  if (requested.size) {
    queue = queue.filter((post) => requested.has(String(post.id)));
    const found = new Set(queue.map((post) => String(post.id)));
    const missing = [...requested].filter((id) => !found.has(id));
    if (missing.length) throw new Error(`video post ids not found: ${missing.join(", ")}`);
  }
  if (!options.force) {
    queue = queue.filter((post) => {
      const record = readRecord(post.id);
      return !record || record.status !== "complete" ||
        record.provenance?.promptVersion !== PROMPT_VERSION || record.provenance?.model !== options.model;
    });
  }
  if (options.limit !== undefined) queue = queue.slice(0, options.limit);
  console.log(`queue: ${queue.length} video posts · model ${options.model} · concurrency ${options.concurrency}`);
  if (options.dryRun) {
    console.log(queue.slice(0, 20).map((post) => post.id).join("\n"));
    if (queue.length > 20) console.log(`... ${queue.length - 20} more`);
    return;
  }
  if (!process.env.OPENAI_API_KEY) throw new Error("OPENAI_API_KEY is required");
  mkdirSync(RECORDS_DIR, { recursive: true });
  let cursor = 0;
  let completed = 0;
  async function worker() {
    while (cursor < queue.length) {
      const index = cursor;
      cursor += 1;
      if (await processPost(queue[index], options, index, queue.length)) completed += 1;
    }
  }
  await Promise.all(Array.from({ length: Math.min(options.concurrency, queue.length) }, worker));
  const merged = mergeRecords();
  const videoCount = platter.posts.filter((post) => post.media === "video").length;
  console.log(`run complete: ${completed}/${queue.length} new · ${merged.count}/${videoCount} video posts described`);
  if (completed !== queue.length) process.exitCode = 1;
}

main().catch((error) => {
  console.error(`visual-farm: ${error.message}`);
  process.exitCode = 1;
});
