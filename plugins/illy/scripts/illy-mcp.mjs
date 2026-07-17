#!/usr/bin/env node
// illy-mcp.mjs — provider-neutral illustration broker for AC marketing + pop.
// Node builtins only; JSON-RPC/MCP over newline-delimited stdio.

import { createHash } from "node:crypto";
import {
  copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync,
  renameSync, statSync, unlinkSync, writeFileSync,
} from "node:fs";
import { basename, dirname, extname, isAbsolute, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import * as readline from "node:readline";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const REPO = resolve(ROOT, "../..");
const PROVIDERS = JSON.parse(readFileSync(join(ROOT, "config/providers.json"), "utf8")).providers;
const PIPELINES = JSON.parse(readFileSync(join(ROOT, "config/pipelines.json"), "utf8")).pipelines;
const VAULT_ENV = join(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
const IMAGE_RE = /\.(png|jpe?g|webp|gif)$/i;

function envValue(name) {
  if (process.env[name]) return process.env[name];
  if (!existsSync(VAULT_ENV)) return null;
  const line = readFileSync(VAULT_ENV, "utf8").split("\n").find((x) => x.startsWith(`${name}=`));
  return line ? line.slice(name.length + 1).trim().replace(/^['"]|['"]$/g, "") : null;
}

function slugify(value) {
  return String(value || "illy").toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "") || "illy";
}

function mime(path) {
  const ext = extname(path).toLowerCase();
  return ext === ".jpg" || ext === ".jpeg" ? "image/jpeg" : ext === ".webp" ? "image/webp" : ext === ".gif" ? "image/gif" : "image/png";
}

function dataUri(path) {
  return `data:${mime(path)};base64,${readFileSync(path).toString("base64")}`;
}

function nested(obj, dotted) {
  return dotted.split(".").reduce((value, key) => value?.[/^\d+$/.test(key) ? Number(key) : key], obj);
}

function resolvePath(base, path) {
  return isAbsolute(path) ? resolve(path) : resolve(base, path);
}

function firstPromptFile(targetDir, pipeline) {
  if (pipeline.promptFile) {
    const path = join(targetDir, pipeline.promptFile);
    if (existsSync(path)) return path;
  }
  for (const pattern of pipeline.promptGlobs || []) {
    if (!pattern.includes("*")) {
      const path = join(targetDir, pattern);
      if (existsSync(path)) return path;
      continue;
    }
    const [prefix, suffix] = pattern.split("*");
    const match = readdirSync(targetDir).sort().find((name) => name.startsWith(prefix) && name.endsWith(suffix));
    if (match) return join(targetDir, match);
  }
  return null;
}

function collectRefs(targetDir, pipeline, requested = []) {
  const refs = requested.map((path) => resolvePath(targetDir, path));
  if (pipeline.refsDir) {
    const dir = join(targetDir, pipeline.refsDir);
    if (existsSync(dir)) {
      refs.push(...readdirSync(dir).filter((name) => IMAGE_RE.test(name)).sort().map((name) => join(dir, name)));
    }
  }
  return [...new Set(refs)].map((path) => resolve(path));
}

function supports(provider, model, mode, refCount) {
  const spec = PROVIDERS[provider]?.models?.[model];
  if (!spec || !spec.modes.includes(mode)) return false;
  return spec.maxReferences === undefined || refCount <= spec.maxReferences;
}

function selectProvider(pipeline, requested, modelRequested, mode, refCount) {
  const candidates = requested && requested !== "auto" ? [requested] : pipeline.providerOrder;
  for (const provider of candidates) {
    const spec = PROVIDERS[provider];
    if (!spec) throw new Error(`unknown provider: ${provider}`);
    const preferred = modelRequested || pipeline.modelByProvider[provider];
    const model = supports(provider, preferred, mode, refCount)
      ? preferred
      : Object.keys(spec.models).find((candidate) => supports(provider, candidate, mode, refCount));
    if (!supports(provider, model, mode, refCount)) continue;
    if (envValue(spec.credential)) return { provider, model };
  }
  const detail = candidates.map((p) => `${p}:${envValue(PROVIDERS[p]?.credential) ? "credentialed" : "no credential"}`).join(", ");
  throw new Error(`no configured backend supports ${mode} with ${refCount} reference(s) (${detail})`);
}

function buildPlan(args = {}) {
  const pipelineName = args.pipeline || "marketing";
  const pipelineFile = args.pipelineFile ? resolve(args.pipelineFile) : null;
  const custom = pipelineFile ? JSON.parse(readFileSync(pipelineFile, "utf8")) : null;
  const pipeline = custom?.pipeline || custom?.pipelines?.[pipelineName] || PIPELINES[pipelineName];
  if (!pipeline) throw new Error(`unknown pipeline: ${pipelineName}`);
  const targetDir = resolve(args.targetDir || process.cwd());
  if (!existsSync(targetDir) || !statSync(targetDir).isDirectory()) throw new Error(`targetDir not found: ${targetDir}`);
  const promptPath = args.promptFile ? resolvePath(targetDir, args.promptFile) : firstPromptFile(targetDir, pipeline);
  const prompt = String(args.prompt || (promptPath && readFileSync(promptPath, "utf8")) || "").trim();
  if (!prompt) throw new Error("provide prompt or a target directory containing the pipeline prompt file");
  const refs = collectRefs(targetDir, pipeline, args.references || []);
  for (const ref of refs) if (!existsSync(ref)) throw new Error(`reference not found: ${ref}`);
  const mode = refs.length ? "edit" : "generate";
  const picked = selectProvider(pipeline, args.provider || "auto", args.model, mode, refs.length);
  const slug = slugify(args.slug || basename(targetDir));
  const variant = slugify(args.variant || pipeline.defaults?.variant || "v1");
  const outputTemplate = args.output || pipeline.outputTemplate;
  const output = resolvePath(targetDir, outputTemplate.replaceAll("{slug}", slug).replaceAll("{variant}", variant));
  const size = args.size || pipeline.defaults?.size || "1024x1024";
  const quality = args.quality || pipeline.defaults?.quality || "high";
  return {
    pipeline: pipelineName, pipelineFile, description: pipeline.description, targetDir, prompt, promptPath,
    promptHash: createHash("sha256").update(prompt).digest("hex"), refs, mode,
    provider: picked.provider, model: picked.model, size, quality, slug, variant, output,
    stages: pipeline.stages, force: args.force === true,
  };
}

async function fetchRetry(url, options, attempts = 4) {
  let error;
  for (let attempt = 1; attempt <= attempts; attempt++) {
    try {
      const res = await fetch(url, options);
      if (res.ok || (res.status < 500 && res.status !== 429)) return res;
      error = new Error(`HTTP ${res.status}: ${(await res.text()).slice(0, 400)}`);
    } catch (caught) { error = caught; }
    if (attempt < attempts) await new Promise((done) => setTimeout(done, 1500 * attempt));
  }
  throw error;
}

async function openaiGenerate(plan, destination) {
  const key = envValue("OPENAI_API_KEY");
  let res;
  if (plan.mode === "edit") {
    const form = new FormData();
    for (const [name, value] of Object.entries({ model: plan.model, prompt: plan.prompt, size: plan.size, quality: plan.quality, n: "1" })) form.append(name, value);
    for (const ref of plan.refs) form.append("image[]", new Blob([readFileSync(ref)], { type: mime(ref) }), basename(ref));
    res = await fetchRetry("https://api.openai.com/v1/images/edits", { method: "POST", headers: { Authorization: `Bearer ${key}` }, body: form, signal: AbortSignal.timeout(600000) });
  } else {
    res = await fetchRetry("https://api.openai.com/v1/images/generations", {
      method: "POST", headers: { Authorization: `Bearer ${key}`, "Content-Type": "application/json" },
      body: JSON.stringify({ model: plan.model, prompt: plan.prompt, size: plan.size, quality: plan.quality, n: 1 }), signal: AbortSignal.timeout(600000),
    });
  }
  if (!res.ok) throw new Error(`OpenAI ${res.status}: ${(await res.text()).slice(0, 600)}`);
  const result = await res.json();
  const image = result.data?.[0];
  if (image?.b64_json) writeFileSync(destination, Buffer.from(image.b64_json, "base64"));
  else if (image?.url) writeFileSync(destination, Buffer.from(await (await fetch(image.url)).arrayBuffer()));
  else throw new Error("OpenAI returned no image");
  return { requestId: result.id || null, usage: result.usage || null };
}

async function falGenerate(plan, destination) {
  const key = envValue("FAL_KEY");
  const model = PROVIDERS.fal.models[plan.model];
  const [width, height] = plan.size.split("x").map(Number);
  const input = { prompt: plan.prompt, image_size: { width, height }, num_images: 1 };
  if (plan.refs[0]) input.image_url = dataUri(plan.refs[0]);
  const auth = { Authorization: `Key ${key}`, "Content-Type": "application/json" };
  const queuePath = `${plan.output}.queue.json`;
  let queued = existsSync(queuePath) ? JSON.parse(readFileSync(queuePath, "utf8")) : null;
  if (!queued) {
    const submit = await fetchRetry(`https://queue.fal.run/${plan.model}`, { method: "POST", headers: auth, body: JSON.stringify(input), signal: AbortSignal.timeout(180000) });
    if (!submit.ok) throw new Error(`fal submit ${submit.status}: ${(await submit.text()).slice(0, 600)}`);
    queued = await submit.json();
    writeFileSync(queuePath, JSON.stringify(queued, null, 2));
  }
  let status = "";
  while (status !== "COMPLETED") {
    await new Promise((done) => setTimeout(done, 3000));
    const response = await fetch(queued.status_url, { headers: auth });
    const body = await response.json();
    status = body.status;
    if (status === "FAILED" || body.error) {
      if (existsSync(queuePath)) unlinkSync(queuePath);
      throw new Error(`fal generation failed: ${JSON.stringify(body).slice(0, 600)}`);
    }
  }
  const result = await (await fetch(queued.response_url, { headers: auth })).json();
  const url = nested(result, model.output);
  if (!url) throw new Error(`fal returned no image at ${model.output}`);
  const image = await fetch(url);
  if (!image.ok) throw new Error(`fal image download ${image.status}`);
  writeFileSync(destination, Buffer.from(await image.arrayBuffer()));
  unlinkSync(queuePath);
  return { requestId: queued.request_id || null, seed: result.seed ?? null };
}

function archiveExisting(output) {
  if (!existsSync(output)) return null;
  const dir = join(dirname(output), "archive");
  mkdirSync(dir, { recursive: true });
  const stem = basename(output, extname(output));
  const stamp = new Date().toISOString().replace(/[:.]/g, "-");
  const archived = join(dir, `${stem}-${stamp}${extname(output)}`);
  renameSync(output, archived);
  const meta = `${output}.illy.json`;
  if (existsSync(meta)) renameSync(meta, `${archived}.illy.json`);
  return archived;
}

function writeProvenance(plan, extra = {}) {
  const meta = {
    schema: "illy/v1", createdAt: new Date().toISOString(), pipeline: plan.pipeline,
    provider: plan.provider, model: plan.model, mode: plan.mode, size: plan.size,
    quality: plan.quality, output: plan.output, promptPath: plan.promptPath,
    promptHash: plan.promptHash, references: plan.refs, stages: plan.stages, ...extra,
  };
  writeFileSync(`${plan.output}.illy.json`, JSON.stringify(meta, null, 2) + "\n");
  return meta;
}

async function generate(args) {
  const plan = buildPlan(args);
  if (existsSync(plan.output) && !plan.force) return { status: "cached", plan, provenance: existsSync(`${plan.output}.illy.json`) ? `${plan.output}.illy.json` : null };
  mkdirSync(dirname(plan.output), { recursive: true });
  const pending = `${plan.output}.pending${extname(plan.output) || ".png"}`;
  if (existsSync(pending)) unlinkSync(pending);
  const started = Date.now();
  const result = plan.provider === "openai" ? await openaiGenerate(plan, pending) : await falGenerate(plan, pending);
  const archived = archiveExisting(plan.output);
  renameSync(pending, plan.output);
  const meta = writeProvenance(plan, { durationSeconds: (Date.now() - started) / 1000, archived, ...result });
  return { status: "generated", plan, meta };
}

function record(args) {
  const output = resolve(args.output || "");
  if (!args.output || !existsSync(output)) throw new Error("output must name an existing image");
  const prompt = String(args.prompt || "").trim();
  if (!prompt) throw new Error("prompt is required");
  const plan = {
    pipeline: args.pipeline || "custom", provider: args.provider || "codex-built-in",
    model: args.model || "undisclosed", mode: (args.references || []).length ? "edit" : "generate",
    size: args.size || null, quality: args.quality || null, output,
    promptPath: args.promptFile ? resolve(args.promptFile) : null,
    promptHash: createHash("sha256").update(prompt).digest("hex"),
    refs: (args.references || []).map((path) => resolve(path)), stages: ["external-generate", "provenance"],
  };
  return writeProvenance(plan, { note: args.note || "Recorded external/interactively generated illy." });
}

const commonProperties = {
  pipeline: { type: "string", description: `Built-ins: ${Object.keys(PIPELINES).join(", ")}; custom names are allowed with pipelineFile.` },
  pipelineFile: { type: "string", description: "Optional JSON file containing {pipeline:{...}} or {pipelines:{name:{...}}}." }, targetDir: { type: "string" },
  prompt: { type: "string" }, promptFile: { type: "string" }, references: { type: "array", items: { type: "string" } },
  provider: { type: "string", description: "auto, openai, or fal" }, model: { type: "string" }, output: { type: "string" },
  slug: { type: "string" }, variant: { type: "string" }, size: { type: "string" }, quality: { type: "string" }, force: { type: "boolean" },
};

const TOOLS = [
  { name: "illy_backends", description: "List configured illy providers/models, capabilities, and credential availability without exposing secrets.", inputSchema: { type: "object", properties: {} } },
  { name: "illy_pipelines", description: "List the marketing/pop illy pipeline presets and their stages.", inputSchema: { type: "object", properties: {} } },
  { name: "illy_plan", description: "Resolve an illy request into a non-billing execution plan: prompt, refs, provider/model, stages, and output.", inputSchema: { type: "object", properties: commonProperties } },
  { name: "illy_generate", description: "Run a paid OpenAI or fal.ai illy pipeline. Existing outputs are cached unless force=true; successful rerolls archive the prior take and write provenance.", inputSchema: { type: "object", properties: commonProperties } },
  { name: "illy_record", description: "Write illy provenance for an image generated outside the MCP, including a Codex built-in image-tool render.", inputSchema: { type: "object", properties: { output: { type: "string" }, prompt: { type: "string" }, pipeline: { type: "string" }, provider: { type: "string" }, model: { type: "string" }, promptFile: { type: "string" }, references: { type: "array", items: { type: "string" } }, size: { type: "string" }, quality: { type: "string" }, note: { type: "string" } }, required: ["output", "prompt"] } },
];

async function callTool(name, args = {}) {
  if (name === "illy_backends") return Object.fromEntries(Object.entries(PROVIDERS).map(([id, provider]) => [id, { label: provider.label, credential: provider.credential, available: !!envValue(provider.credential), models: provider.models }]));
  if (name === "illy_pipelines") return PIPELINES;
  if (name === "illy_plan") return buildPlan(args);
  if (name === "illy_generate") return generate(args);
  if (name === "illy_record") return record(args);
  throw new Error(`unknown tool: ${name}`);
}

async function handle(message) {
  const { id, method, params } = message;
  try {
    if (method === "initialize") return { jsonrpc: "2.0", id, result: { protocolVersion: "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "illy", version: "0.1.0" } } };
    if (method === "initialized" || method === "notifications/initialized") return null;
    if (method === "ping") return { jsonrpc: "2.0", id, result: {} };
    if (method === "tools/list") return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    if (method === "tools/call") {
      const value = await callTool(params?.name, params?.arguments || {});
      return { jsonrpc: "2.0", id, result: { content: [{ type: "text", text: JSON.stringify(value, null, 2) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32601, message: `method not found: ${method}` } };
  } catch (error) {
    return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
  }
}

const rl = readline.createInterface({ input: process.stdin, terminal: false });
rl.on("line", async (line) => {
  if (!line.trim()) return;
  try { const response = await handle(JSON.parse(line)); if (response) process.stdout.write(JSON.stringify(response) + "\n"); }
  catch (error) { process.stdout.write(JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32700, message: error.message } }) + "\n"); }
});
console.error("🎨 illy MCP ready");
