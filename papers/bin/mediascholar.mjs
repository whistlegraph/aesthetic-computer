#!/usr/bin/env node
// Mediascholar — autonomously synthesize a new-media question and mill it into
// a review-only Botted Paper. systemd owns resource limits; this runner adds a
// live admission gate, cadence, single-flight locking, provenance, and provider
// checkpoints. It never commits, pushes, publishes, deploys, or reads the vault.

import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import {
  access, copyFile, mkdir, open, readFile, readdir, stat, statfs, unlink, writeFile,
} from "node:fs/promises";
import { homedir, hostname, loadavg, cpus, freemem } from "node:os";
import { basename, dirname, extname, isAbsolute, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";
import { createSourceBundle } from "../source-bundle.mjs";

const pexec = promisify(execFile);
const HERE = dirname(fileURLToPath(import.meta.url));
const SOURCE_REPO = resolve(HERE, "../..");
const MEDIA_DIR = join(SOURCE_REPO, "papers", "mediascholar");
const GiB = 1024 ** 3;
const MiB = 1024 ** 2;

const now = () => new Date().toISOString();
const exists = async (path) => access(path).then(() => true, () => false);
const envNumber = (env, key, fallback) => {
  const value = Number(env[key]);
  return Number.isFinite(value) ? value : fallback;
};
const envBool = (env, key, fallback = false) => {
  if (!(key in env)) return fallback;
  return !new Set(["0", "false", "no", "off", ""]).has(String(env[key]).toLowerCase());
};

export function configFromEnv(env = process.env) {
  const home = env.MEDIASCHOLAR_HOME || join(homedir(), ".local", "share", "mediascholar");
  const repo = resolve(env.MEDIASCHOLAR_REPO || join(homedir(), "aesthetic-computer"));
  return {
    home: resolve(home),
    repo,
    runsDir: join(resolve(home), "runs"),
    worktreesDir: join(resolve(home), "worktrees"),
    lockPath: join(resolve(home), "run.lock"),
    lastSuccessPath: join(resolve(home), "last-success.json"),
    advertisePath: resolve(env.MEDIASCHOLAR_ADVERTISE_PATH
      || join(homedir(), ".config", "slab", "ledger", "advertise", "mediascholar.json")),
    provider: String(env.MEDIASCHOLAR_PROVIDER || "auto").toLowerCase(),
    providerPreference: String(env.MEDIASCHOLAR_PROVIDER_PREFERENCE || "claude,openai")
      .split(",").map((item) => item.trim().toLowerCase()).filter(Boolean),
    proxyUrl: String(env.MEDIASCHOLAR_PROXY_URL || "").replace(/\/$/, ""),
    claudeBin: env.MEDIASCHOLAR_CLAUDE_BIN || "claude",
    codexBin: env.MEDIASCHOLAR_CODEX_BIN || "codex",
    claudeTokenFile: resolve(env.MEDIASCHOLAR_CLAUDE_TOKEN_FILE
      || join(homedir(), ".config", "claude", "oauth-token")),
    anthropicKeyFile: resolve(env.MEDIASCHOLAR_ANTHROPIC_KEY_FILE
      || join(homedir(), ".config", "mediascholar", "credentials", "anthropic")),
    openaiKeyFile: resolve(env.MEDIASCHOLAR_OPENAI_KEY_FILE
      || join(homedir(), ".config", "mediascholar", "credentials", "openai")),
    maxLoadPerCpu: envNumber(env, "MEDIASCHOLAR_MAX_LOAD_PER_CPU", 0.55),
    minAvailableMemoryBytes: envNumber(env, "MEDIASCHOLAR_MIN_AVAILABLE_MEMORY_MIB", 4096) * MiB,
    minFreeDiskBytes: envNumber(env, "MEDIASCHOLAR_MIN_FREE_DISK_GIB", 32) * GiB,
    minDaysBetweenPapers: envNumber(env, "MEDIASCHOLAR_MIN_DAYS_BETWEEN_PAPERS", 7),
    maxRetainedCandidates: envNumber(env, "MEDIASCHOLAR_MAX_RETAINED_CANDIDATES", 4),
    topicBudgetUsd: envNumber(env, "MEDIASCHOLAR_TOPIC_BUDGET_USD", 2),
    paperBudgetUsd: envNumber(env, "MEDIASCHOLAR_PAPER_BUDGET_USD", 12),
    stageTimeoutMs: envNumber(env, "MEDIASCHOLAR_STAGE_TIMEOUT_MINUTES", 150) * 60_000,
    enabled: envBool(env, "MEDIASCHOLAR_ENABLED", false),
    dryRun: envBool(env, "MEDIASCHOLAR_DRY_RUN", false),
  };
}

async function availableMemoryBytes() {
  try {
    const text = await readFile("/proc/meminfo", "utf8");
    const kib = Number(text.match(/^MemAvailable:\s+(\d+)\s+kB$/m)?.[1]);
    if (Number.isFinite(kib)) return kib * 1024;
  } catch {}
  return freemem();
}

export async function resourceSnapshot(config) {
  let diskFreeBytes = 0;
  try {
    const fs = await statfs(config.repo);
    diskFreeBytes = Number(fs.bavail) * Number(fs.bsize);
  } catch {}
  const cpuCount = Math.max(1, cpus().length);
  return {
    checkedAt: now(),
    cpuCount,
    load1: loadavg()[0],
    loadPerCpu: loadavg()[0] / cpuCount,
    availableMemoryBytes: await availableMemoryBytes(),
    diskFreeBytes,
  };
}

export function admissionVerdict(config, snapshot) {
  const reasons = [];
  if (snapshot.loadPerCpu > config.maxLoadPerCpu) {
    reasons.push(`load ${snapshot.loadPerCpu.toFixed(2)}/cpu exceeds ${config.maxLoadPerCpu.toFixed(2)}`);
  }
  if (snapshot.availableMemoryBytes < config.minAvailableMemoryBytes) {
    reasons.push(`available memory ${(snapshot.availableMemoryBytes / GiB).toFixed(1)} GiB is below ${(config.minAvailableMemoryBytes / GiB).toFixed(1)} GiB`);
  }
  if (snapshot.diskFreeBytes < config.minFreeDiskBytes) {
    reasons.push(`free disk ${(snapshot.diskFreeBytes / GiB).toFixed(1)} GiB is below ${(config.minFreeDiskBytes / GiB).toFixed(1)} GiB`);
  }
  return { accepted: reasons.length === 0, reasons, snapshot };
}

async function readJson(path, fallback = null) {
  try { return JSON.parse(await readFile(path, "utf8")); }
  catch { return fallback; }
}

async function writeJson(path, value) {
  await mkdir(dirname(path), { recursive: true });
  const temp = `${path}.${process.pid}.tmp`;
  await writeFile(temp, `${JSON.stringify(value, null, 2)}\n`, { mode: 0o600 });
  await import("node:fs/promises").then(({ rename }) => rename(temp, path));
}

async function nonEmptyFile(path) {
  try { return (await stat(path)).size > 0; }
  catch { return false; }
}

async function executable(command) {
  if (command.includes(sep)) return (await exists(command)) ? command : null;
  try { return (await pexec("/usr/bin/env", ["which", command])).stdout.trim() || null; }
  catch { return null; }
}

async function proxyHealth(config) {
  if (!config.proxyUrl) return null;
  try {
    const response = await fetch(`${config.proxyUrl}/health`, { signal: AbortSignal.timeout(3000) });
    if (!response.ok) return null;
    return await response.json();
  } catch { return null; }
}

export async function providerInventory(config, env = process.env) {
  const health = await proxyHealth(config);
  const claudeCredential = Boolean(
    health?.providers?.claude
    || env.CLAUDE_CODE_OAUTH_TOKEN
    || env.ANTHROPIC_API_KEY
    || await nonEmptyFile(config.claudeTokenFile)
    || await nonEmptyFile(config.anthropicKeyFile));
  const openaiCredential = Boolean(
    health?.providers?.openai
    || env.CODEX_API_KEY
    || env.OPENAI_API_KEY
    || await nonEmptyFile(config.openaiKeyFile));
  const inventory = {
    claude: {
      binary: await executable(config.claudeBin),
      credential: claudeCredential,
      viaProxy: Boolean(health?.providers?.claude),
    },
    openai: {
      binary: await executable(config.codexBin),
      credential: openaiCredential,
      viaProxy: Boolean(health?.providers?.openai),
    },
  };
  for (const value of Object.values(inventory)) value.ready = Boolean(value.binary && value.credential);
  return inventory;
}

export function chooseProvider(config, inventory) {
  if (!new Set(["auto", "claude", "openai"]).has(config.provider)) {
    throw new Error(`unknown provider: ${config.provider}`);
  }
  if (config.provider !== "auto") {
    if (!inventory[config.provider]?.ready) throw new Error(`${config.provider} provider is not ready`);
    return config.provider;
  }
  const order = [...config.providerPreference, "claude", "openai"];
  const selected = order.find((name, index) => order.indexOf(name) === index && inventory[name]?.ready);
  if (!selected) throw new Error("no provider is ready");
  return selected;
}

function redact(text) {
  return String(text || "")
    .replace(/sk-[A-Za-z0-9_-]{12,}/g, "[redacted-key]")
    .replace(/sk-ant-[A-Za-z0-9_-]{12,}/g, "[redacted-key]");
}

async function credentialEnv(config, provider, baseEnv = process.env) {
  const env = { ...baseEnv };
  if (config.proxyUrl) {
    if (provider === "claude") {
      env.ANTHROPIC_BASE_URL = `${config.proxyUrl}/anthropic`;
      env.CLAUDE_CODE_OAUTH_TOKEN = "sk-ant-oat01-mediascholar-local-proxy";
      delete env.ANTHROPIC_API_KEY;
    } else {
      env.CODEX_API_KEY = "sk-mediascholar-local-proxy";
      delete env.OPENAI_API_KEY;
    }
    return env;
  }
  if (provider === "claude" && !env.CLAUDE_CODE_OAUTH_TOKEN && !env.ANTHROPIC_API_KEY) {
    if (await nonEmptyFile(config.claudeTokenFile)) {
      env.CLAUDE_CODE_OAUTH_TOKEN = (await readFile(config.claudeTokenFile, "utf8")).trim();
    } else if (await nonEmptyFile(config.anthropicKeyFile)) {
      env.ANTHROPIC_API_KEY = (await readFile(config.anthropicKeyFile, "utf8")).trim();
    }
  }
  if (provider === "openai" && !env.CODEX_API_KEY && !env.OPENAI_API_KEY
      && await nonEmptyFile(config.openaiKeyFile)) {
    env.CODEX_API_KEY = (await readFile(config.openaiKeyFile, "utf8")).trim();
  }
  return env;
}

async function runLogged(command, args, options) {
  const startedAt = now();
  try {
    const result = await pexec(command, args, {
      cwd: options.cwd,
      env: options.env,
      timeout: options.timeout,
      maxBuffer: 64 * 1024 * 1024,
    });
    await writeFile(options.stdoutPath, redact(result.stdout), { mode: 0o600 });
    await writeFile(options.stderrPath, redact(result.stderr), { mode: 0o600 });
    return { startedAt, completedAt: now(), stdout: result.stdout, stderr: result.stderr };
  } catch (error) {
    await writeFile(options.stdoutPath, redact(error.stdout), { mode: 0o600 }).catch(() => {});
    await writeFile(options.stderrPath, redact(error.stderr || error.message), { mode: 0o600 }).catch(() => {});
    throw new Error(`${basename(command)} failed: ${redact(error.stderr || error.message).trim().slice(-1200)}`);
  }
}

function parseClaudeResult(stdout) {
  const envelope = JSON.parse(stdout);
  if (envelope.structured_output && typeof envelope.structured_output === "object") {
    return { value: envelope.structured_output, model: envelope.model || null, usage: envelope.usage || null };
  }
  const raw = typeof envelope.result === "string" ? envelope.result : JSON.stringify(envelope.result ?? envelope);
  return { value: JSON.parse(raw), model: envelope.model || null, usage: envelope.usage || null };
}

function mcpConfig(worktree) {
  return {
    mcpServers: {
      paper: {
        type: "stdio",
        command: "node",
        args: [join(SOURCE_REPO, "slab", "bin", "paper-mcp.mjs")],
        env: { PAPER_REPO: worktree, PAPER_DISABLE_VAULT: "1" },
      },
    },
  };
}

async function invokeClaude({ config, stage, prompt, schemaPath, worktree, runDir }) {
  const env = await credentialEnv(config, "claude");
  const schema = JSON.stringify(JSON.parse(await readFile(schemaPath, "utf8")));
  const args = [
    "--print", "--output-format", "json", "--json-schema", schema,
    "--no-session-persistence", "--permission-mode", "auto",
    "--max-budget-usd", String(stage === "topic" ? config.topicBudgetUsd : config.paperBudgetUsd),
    "--no-chrome",
  ];
  const tools = stage === "topic"
    ? "Read,Glob,Grep,WebSearch,WebFetch"
    : "Read,Glob,Grep,Write,Edit,WebSearch,WebFetch,mcp__paper__paper_list,mcp__paper__paper_find,mcp__paper__paper_read,mcp__paper__paper_build,mcp__paper__paper_figure_table_qa_check";
  args.push("--tools", tools, "--allowedTools", tools);
  const configPath = join(runDir, `${stage}-mcp.json`);
  await writeJson(configPath, stage === "paper" ? mcpConfig(worktree) : { mcpServers: {} });
  args.push("--mcp-config", configPath, "--strict-mcp-config");
  args.push(prompt);
  const log = await runLogged(config.claudeBin, args, {
    cwd: worktree,
    env,
    timeout: config.stageTimeoutMs,
    stdoutPath: join(runDir, `${stage}.stdout.json`),
    stderrPath: join(runDir, `${stage}.stderr.log`),
  });
  return { ...parseClaudeResult(log.stdout), startedAt: log.startedAt, completedAt: log.completedAt };
}

async function invokeCodex({ config, stage, prompt, schemaPath, worktree, runDir }) {
  const env = await credentialEnv(config, "openai");
  const outputPath = join(runDir, `${stage}.result.json`);
  const args = [
    "--search", "exec", "--ephemeral", "--json", "--color", "never",
    "--sandbox", stage === "topic" ? "read-only" : "workspace-write",
    "--output-schema", schemaPath,
    "--output-last-message", outputPath,
    "--cd", worktree,
    "--ignore-user-config",
  ];
  if (config.proxyUrl) args.push("-c", `openai_base_url=${JSON.stringify(`${config.proxyUrl}/openai/v1`)}`);
  if (stage === "paper") {
    const paperMcp = join(SOURCE_REPO, "slab", "bin", "paper-mcp.mjs");
    args.push(
      "-c", "mcp_servers.paper.command=\"node\"",
      "-c", `mcp_servers.paper.args=[${JSON.stringify(paperMcp)}]`,
      "-c", "mcp_servers.paper.required=true",
      "-c", `mcp_servers.paper.env.PAPER_REPO=${JSON.stringify(worktree)}`,
      "-c", "mcp_servers.paper.env.PAPER_DISABLE_VAULT=\"1\"",
    );
  }
  args.push(prompt);
  const log = await runLogged(config.codexBin, args, {
    cwd: worktree,
    env,
    timeout: config.stageTimeoutMs,
    stdoutPath: join(runDir, `${stage}.events.jsonl`),
    stderrPath: join(runDir, `${stage}.stderr.log`),
  });
  const value = JSON.parse(await readFile(outputPath, "utf8"));
  let model = null;
  let usage = null;
  for (const line of log.stdout.split("\n").filter(Boolean)) {
    try {
      const event = JSON.parse(line);
      if (event.model) model = event.model;
      if (event.type === "turn.completed") usage = event.usage || null;
    } catch {}
  }
  return { value, model, usage, startedAt: log.startedAt, completedAt: log.completedAt };
}

async function invokeProvider(options) {
  return options.provider === "claude" ? invokeClaude(options) : invokeCodex(options);
}

export function validateTopic(topic) {
  if (!topic || typeof topic !== "object") throw new Error("topic result is not an object");
  for (const key of ["title", "question", "claim", "whyNow"]) {
    if (!String(topic[key] || "").trim()) throw new Error(`topic is missing ${key}`);
  }
  if (!Array.isArray(topic.signals) || topic.signals.length < 3) throw new Error("topic needs at least three signals");
  const hosts = new Set();
  for (const signal of topic.signals) {
    try { hosts.add(new URL(signal.url).hostname.replace(/^www\./, "")); }
    catch { throw new Error(`invalid signal URL: ${signal.url}`); }
    if (!signal.accessedAt || !Number.isFinite(Date.parse(signal.accessedAt))) {
      throw new Error(`signal is missing a valid accessedAt timestamp: ${signal.url}`);
    }
  }
  if (hosts.size < 3) throw new Error("topic signals must span at least three source hosts");
  return topic;
}

export function slugify(value) {
  return String(value).toLowerCase().normalize("NFKD")
    .replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 64) || "untitled";
}

function staysInside(child, parent) {
  const rel = relative(resolve(parent), resolve(child));
  return rel === "" || (rel !== ".." && !rel.startsWith(`..${sep}`) && !isAbsolute(rel));
}

export async function validatePaperResult(result, expectedDir) {
  if (result?.status !== "candidate") throw new Error("paper stage did not return candidate status");
  if (!result.qa?.built || !result.qa?.figureTableCheck || !result.qa?.visualInspection) {
    throw new Error("paper did not complete build and visual QA");
  }
  if (result.qa.remainingFailures?.length) throw new Error("paper still has visual QA failures");
  const paperDir = resolve(expectedDir);
  const reportedPaperDir = isAbsolute(result.paperDir)
    ? resolve(result.paperDir)
    : resolve(paperDir, result.paperDir);
  const texPath = isAbsolute(result.texPath) ? resolve(result.texPath) : resolve(paperDir, result.texPath);
  const pdfPath = isAbsolute(result.pdfPath) ? resolve(result.pdfPath) : resolve(paperDir, result.pdfPath);
  if (reportedPaperDir !== paperDir) {
    throw new Error("paper result does not point to its assigned directory");
  }
  if (!staysInside(texPath, paperDir) || !staysInside(pdfPath, paperDir)) {
    throw new Error("paper artifacts point outside the assigned directory");
  }
  for (const path of [texPath, pdfPath, join(paperDir, "references.bib"), join(paperDir, "botted.json")]) {
    if (!await nonEmptyFile(path)) throw new Error(`missing paper artifact: ${path}`);
  }
  const bib = await readFile(join(paperDir, "references.bib"), "utf8");
  const bibliographyEntries = bib.match(/^@[A-Za-z]+\s*\{/gm)?.length || 0;
  if (bibliographyEntries < 3) throw new Error("paper bibliography has fewer than three entries");
  const bundles = (await readdir(paperDir)).filter((name) => name.endsWith("-source.zip"));
  if (!bundles.length) throw new Error("paper is missing its source bundle");
  if ((await stat(pdfPath)).size < 10_000) throw new Error("paper PDF is unexpectedly small");
  const pdfinfo = await executable("pdfinfo");
  if (!pdfinfo) throw new Error("pdfinfo is unavailable for visual-coverage verification");
  const info = await pexec(pdfinfo, [pdfPath], { timeout: 30_000, maxBuffer: 2 * 1024 * 1024 });
  const pageCount = Number(info.stdout.match(/^Pages:\s+(\d+)/m)?.[1] || 0);
  const inspected = new Set(result.qa.pagesInspected.map(Number));
  const missingPages = Array.from({ length: pageCount }, (_, index) => index + 1)
    .filter((page) => !inspected.has(page));
  if (!pageCount || missingPages.length) {
    throw new Error(`visual inspection did not cover every PDF page${missingPages.length ? `: ${missingPages.join(", ")}` : ""}`);
  }
  return { paperDir, texPath, pdfPath };
}

export async function finalizePaperBundle(validated, runDir) {
  const texBase = basename(validated.texPath, extname(validated.texPath));
  const bundle = await createSourceBundle({ paperDir: validated.paperDir, texBase });
  const xelatex = await executable(process.env.XELATEX || "xelatex");
  const tectonic = xelatex ? null : await executable(process.env.TECTONIC || "tectonic");
  if (!xelatex && !tectonic) throw new Error("no TeX engine is available for the final provenance build");
  let output = "";
  const passes = xelatex ? 2 : 1;
  for (let pass = 0; pass < passes; pass += 1) {
    const command = xelatex || tectonic;
    const args = xelatex
      ? ["-no-shell-escape", "-interaction=nonstopmode", "-halt-on-error", "-file-line-error", basename(validated.texPath)]
      : ["--keep-logs", "--keep-intermediates", "--synctex", basename(validated.texPath)];
    try {
      const result = await pexec(command, args, {
        cwd: validated.paperDir,
        timeout: 180_000,
        maxBuffer: 8 * 1024 * 1024,
      });
      output += `${result.stdout || ""}\n${result.stderr || ""}\n`;
    } catch (error) {
      throw new Error(`final provenance build failed: ${redact(error.stdout || error.stderr || error.message).slice(-1200)}`);
    }
  }
  await writeFile(join(runDir, "final-build.log"), redact(output), { mode: 0o600 });
  const pdfdetach = await executable("pdfdetach");
  if (!pdfdetach) throw new Error("pdfdetach is unavailable for embedded-source verification");
  const { stdout } = await pexec(pdfdetach, ["-list", validated.pdfPath], {
    timeout: 30_000,
    maxBuffer: 2 * 1024 * 1024,
  });
  if (!stdout.includes(bundle.zipName)) throw new Error(`PDF does not embed ${bundle.zipName}`);
  return bundle;
}

async function acquireLock(path) {
  await mkdir(dirname(path), { recursive: true });
  try {
    const handle = await open(path, "wx", 0o600);
    await handle.writeFile(`${JSON.stringify({ pid: process.pid, at: now() })}\n`);
    return handle;
  } catch (error) {
    if (error.code !== "EEXIST") throw error;
    const prior = await readJson(path, {});
    try { process.kill(Number(prior.pid), 0); }
    catch {
      await unlink(path).catch(() => {});
      return acquireLock(path);
    }
    throw new Error(`another Mediascholar run is active (pid ${prior.pid || "unknown"})`);
  }
}

async function releaseLock(handle, path) {
  await handle?.close().catch(() => {});
  await unlink(path).catch(() => {});
}

function runId() {
  return now().replace(/[-:]/g, "").replace("T", "-").replace(/\.(\d{3})Z$/, "-$1Z");
}

async function advertise(config, run, patch) {
  const record = {
    id: `mediascholar-${run.id}`,
    host: hostname().split(".")[0].toLowerCase(),
    name: "mediascholar",
    subject: patch.subject || "Mediascholar",
    status: patch.status || "working",
    kind: "agent",
    seed: createHash("sha256").update(`${run.id}:${patch.subject || ""}`).digest("hex").slice(0, 32),
    cwd: run.worktree || config.repo,
    updated: Date.now(),
    started: Date.parse(run.startedAt),
    agentType: run.provider || "mediascholar",
    artifact: patch.artifact || null,
    runId: run.id,
  };
  await writeJson(config.advertisePath, record);
}

async function cadenceVerdict(config) {
  const last = await readJson(config.lastSuccessPath);
  if (!last?.completedAt || config.minDaysBetweenPapers <= 0) return { accepted: true, last };
  const ageDays = (Date.now() - Date.parse(last.completedAt)) / 86_400_000;
  return ageDays >= config.minDaysBetweenPapers
    ? { accepted: true, last, ageDays }
    : { accepted: false, last, ageDays, reason: `last Botted Paper completed ${ageDays.toFixed(1)} days ago` };
}

export async function capacityVerdict(config) {
  const retained = (await readdir(config.worktreesDir, { withFileTypes: true }).catch(() => []))
    .filter((entry) => entry.isDirectory()).length;
  return retained < config.maxRetainedCandidates
    ? { accepted: true, retained, limit: config.maxRetainedCandidates }
    : {
        accepted: false,
        retained,
        limit: config.maxRetainedCandidates,
        reason: `${retained} retained Mediascholar checkouts require review`,
      };
}

export async function createIsolatedCheckout(config, id) {
  const path = join(config.worktreesDir, id);
  await mkdir(config.worktreesDir, { recursive: true });
  const { stdout } = await pexec("git", ["-C", config.repo, "rev-parse", "HEAD"], {
    maxBuffer: 1024 * 1024,
  });
  const baseCommit = stdout.trim();
  await pexec("git", ["clone", "--shared", "--no-checkout", "--", config.repo, path], {
    maxBuffer: 8 * 1024 * 1024,
  });
  await pexec("git", ["-C", path, "checkout", "--detach", baseCommit], {
    maxBuffer: 8 * 1024 * 1024,
  });
  await pexec("git", ["-C", path, "remote", "remove", "origin"]);
  return { path, baseCommit };
}

export async function validateCandidateBoundary(worktree, paperDir, baseCommit) {
  const { stdout: head } = await pexec("git", ["-C", worktree, "rev-parse", "HEAD"]);
  if (head.trim() !== baseCommit) throw new Error("agent changed the isolated checkout's commit");
  const [tracked, staged, untracked] = await Promise.all([
    pexec("git", ["-C", worktree, "diff", "--name-only", baseCommit]),
    pexec("git", ["-C", worktree, "diff", "--cached", "--name-only", baseCommit]),
    pexec("git", ["-C", worktree, "ls-files", "--others", "--exclude-standard"]),
  ]);
  const changed = new Set(
    `${tracked.stdout}\n${staged.stdout}\n${untracked.stdout}`.split("\n").map((name) => name.trim()).filter(Boolean),
  );
  const outside = [...changed].filter((name) => !staysInside(resolve(worktree, name), paperDir));
  if (outside.length) throw new Error(`agent changed files outside its paper directory: ${outside.slice(0, 8).join(", ")}`);
  if (!changed.size) throw new Error("agent produced no paper files");
  return changed;
}

function stageRecord(result) {
  return {
    startedAt: result.startedAt,
    completedAt: result.completedAt,
    model: result.model || null,
    usage: result.usage || null,
  };
}

async function writeRun(runDir, run) {
  await writeJson(join(runDir, "run.json"), run);
}

export async function doctor(config = configFromEnv()) {
  const snapshot = await resourceSnapshot(config);
  const admission = admissionVerdict(config, snapshot);
  const inventory = await providerInventory(config);
  const cadence = await cadenceVerdict(config);
  const capacity = await capacityVerdict(config);
  return {
    checkedAt: now(),
    enabled: config.enabled,
    repo: config.repo,
    home: config.home,
    admission,
    cadence,
    capacity,
    providers: inventory,
    paperTools: {
      xelatex: await executable(process.env.XELATEX || "xelatex"),
      tectonic: await executable(process.env.TECTONIC || "tectonic"),
      pdftoppm: await executable("pdftoppm"),
      pdfdetach: await executable("pdfdetach"),
      pdfinfo: await executable("pdfinfo"),
      montage: await executable("montage"),
      zip: await executable("zip"),
      bwrap: await executable("bwrap"),
    },
  };
}

async function missingRuntimeDependencies(config, inventory) {
  const engine = await executable(process.env.XELATEX || "xelatex")
    || await executable(process.env.TECTONIC || "tectonic");
  const requirements = {
    provider: Object.values(inventory).some((value) => value.ready),
    texEngine: Boolean(engine),
    pdftoppm: Boolean(await executable("pdftoppm")),
    pdfdetach: Boolean(await executable("pdfdetach")),
    pdfinfo: Boolean(await executable("pdfinfo")),
    montage: Boolean(await executable("montage")),
    zip: Boolean(await executable("zip")),
    bwrap: Boolean(await executable("bwrap")),
  };
  return Object.entries(requirements).filter(([, ready]) => !ready).map(([name]) => name);
}

export async function run(config = configFromEnv(), { dryRun = config.dryRun } = {}) {
  await mkdir(config.runsDir, { recursive: true });
  if (!await exists(join(config.repo, ".git"))) throw new Error(`not a git checkout: ${config.repo}`);
  const cadence = await cadenceVerdict(config);
  const capacity = await capacityVerdict(config);
  const admission = admissionVerdict(config, await resourceSnapshot(config));
  if (!config.enabled && !dryRun) return { status: "skipped", reason: "MEDIASCHOLAR_ENABLED is not set" };
  if (!cadence.accepted) return { status: "skipped", reason: cadence.reason };
  if (!capacity.accepted) return { status: "skipped", reason: capacity.reason, capacity };
  if (!admission.accepted) return { status: "skipped", reason: admission.reasons.join("; "), admission };

  if (!dryRun) {
    const inventory = await providerInventory(config);
    const missing = await missingRuntimeDependencies(config, inventory);
    if (missing.length) return { status: "skipped", reason: `missing runtime dependencies: ${missing.join(", ")}` };
  }

  const lock = await acquireLock(config.lockPath);
  const id = runId();
  const runDir = join(config.runsDir, id);
  await mkdir(runDir, { recursive: true });
  const runState = {
    version: 1,
    id,
    status: "starting",
    startedAt: now(),
    completedAt: null,
    provider: null,
    worktree: null,
    baseCommit: null,
    paperDir: null,
    artifact: null,
    admission,
    stages: {},
  };
  try {
    await writeRun(runDir, runState);
    await advertise(config, runState, { subject: dryRun ? "Mediascholar dry run" : "Mediascholar starting" });
    if (dryRun) {
      runState.status = "dry-run-complete";
      runState.completedAt = now();
      await writeRun(runDir, runState);
      await advertise(config, runState, { status: "complete", subject: "Mediascholar dry run passed" });
      return runState;
    }

    const inventory = await providerInventory(config);
    const provider = chooseProvider(config, inventory);
    runState.provider = provider;
    const workspace = await createIsolatedCheckout(config, id);
    runState.worktree = workspace.path;
    runState.baseCommit = workspace.baseCommit;
    runState.status = "synthesizing-topic";
    await writeRun(runDir, runState);
    await advertise(config, runState, { subject: "Synthesizing an unsupplied new-media topic" });

    const topicPrompt = await readFile(join(MEDIA_DIR, "topic-prompt.md"), "utf8");
    const topicResult = await invokeProvider({
      config, provider, stage: "topic", prompt: topicPrompt,
      schemaPath: join(MEDIA_DIR, "topic.schema.json"),
      worktree: runState.worktree, runDir,
    });
    const topic = validateTopic(topicResult.value);
    runState.stages.topic = stageRecord(topicResult);
    await writeJson(join(runDir, "topic.json"), topic);

    const slug = slugify(topic.title);
    const paperDir = join(runState.worktree, "papers", `arxiv-botted-${slug}`);
    if (await exists(paperDir)) throw new Error(`paper directory already exists: ${relative(runState.worktree, paperDir)}`);
    await mkdir(paperDir, { recursive: true });
    runState.paperDir = paperDir;
    const provenance = {
      version: 1,
      kind: "botted-paper",
      system: "mediascholar",
      runId: id,
      createdAt: now(),
      topicOrigin: "machine-synthesized; no human-supplied topic",
      provider,
      model: topicResult.model || null,
      baseCommit: runState.baseCommit,
      topic,
      stages: { topic: runState.stages.topic },
      publication: { allowed: false, status: "candidate" },
    };
    await writeJson(join(paperDir, "botted.json"), provenance);

    runState.status = "authoring-paper";
    await writeRun(runDir, runState);
    await advertise(config, runState, { subject: `Botted Paper: ${topic.title}` });
    const paperInstructions = await readFile(join(MEDIA_DIR, "paper-prompt.md"), "utf8");
    const paperPrompt = [
      paperInstructions,
      `\nAssigned directory: ${paperDir}`,
      "\nSynthesized topic record:\n",
      JSON.stringify(topic, null, 2),
    ].join("\n");
    const paperResult = await invokeProvider({
      config, provider, stage: "paper", prompt: paperPrompt,
      schemaPath: join(MEDIA_DIR, "paper-result.schema.json"),
      worktree: runState.worktree, runDir,
    });
    const validated = await validatePaperResult(paperResult.value, paperDir);
    runState.stages.paper = stageRecord(paperResult);
    await validateCandidateBoundary(runState.worktree, paperDir, runState.baseCommit);

    const authoredProvenance = await readJson(join(paperDir, "botted.json"), {});
    const completedAt = now();
    await writeJson(join(paperDir, "botted.json"), {
      ...authoredProvenance,
      ...provenance,
      stages: { ...authoredProvenance.stages, topic: runState.stages.topic, paper: runState.stages.paper },
      completedAt,
      sourceBundleFinalizedAt: completedAt,
      qa: paperResult.value.qa,
      publication: { allowed: false, status: "candidate" },
    });
    await finalizePaperBundle(validated, runDir);

    const artifact = join(runDir, basename(validated.pdfPath));
    await copyFile(validated.pdfPath, artifact);
    const relPaper = relative(runState.worktree, paperDir);
    await pexec("git", ["-C", runState.worktree, "add", "-N", "--", relPaper]);
    const patch = await pexec("git", ["-C", runState.worktree, "diff", "--binary", runState.baseCommit, "--", relPaper], {
      maxBuffer: 64 * 1024 * 1024,
    });
    await writeFile(join(runDir, "candidate.patch"), patch.stdout, { mode: 0o600 });

    runState.status = "candidate";
    runState.completedAt = completedAt;
    runState.artifact = artifact;
    await writeRun(runDir, runState);
    await writeJson(config.lastSuccessPath, {
      runId: id, completedAt: runState.completedAt, title: topic.title,
      artifact, worktree: runState.worktree, paperDir,
    });
    await advertise(config, runState, {
      status: "awaiting", subject: `Botted Paper candidate: ${topic.title}`, artifact,
    });
    return runState;
  } catch (error) {
    runState.status = "failed";
    runState.completedAt = now();
    runState.error = redact(error.message);
    await writeRun(runDir, runState).catch(() => {});
    await advertise(config, runState, { status: "complete", subject: `Mediascholar failed: ${runState.error}` }).catch(() => {});
    throw error;
  } finally {
    await releaseLock(lock, config.lockPath);
  }
}

function compactDoctor(report) {
  const p = report.providers;
  return [
    `enabled: ${report.enabled}`,
    `admission: ${report.admission.accepted ? "accept" : `skip — ${report.admission.reasons.join("; ")}`}`,
    `load: ${report.admission.snapshot.load1.toFixed(2)} (${report.admission.snapshot.loadPerCpu.toFixed(2)}/cpu)`,
    `memory available: ${(report.admission.snapshot.availableMemoryBytes / GiB).toFixed(1)} GiB`,
    `disk free: ${(report.admission.snapshot.diskFreeBytes / GiB).toFixed(1)} GiB`,
    `retained checkouts: ${report.capacity.retained}/${report.capacity.limit}${report.capacity.accepted ? "" : " — review required"}`,
    `claude: ${p.claude.ready ? "ready" : "not ready"}${p.claude.viaProxy ? " via proxy" : ""}`,
    `openai: ${p.openai.ready ? "ready" : "not ready"}${p.openai.viaProxy ? " via proxy" : ""}`,
    `paper tools: ${Object.entries(report.paperTools).map(([key, value]) => `${key}=${value ? "yes" : "no"}`).join(" ")}`,
  ].join("\n");
}

async function main() {
  const [command = "doctor", ...args] = process.argv.slice(2);
  const config = configFromEnv();
  const json = args.includes("--json");
  if (command === "doctor" || command === "admit") {
    const report = await doctor(config);
    if (json) console.log(JSON.stringify(report, null, 2));
    else console.log(compactDoctor(report));
    if (command === "admit" && !report.admission.accepted) process.exitCode = 75;
    return;
  }
  if (command === "run") {
    const result = await run(config, { dryRun: args.includes("--dry-run") || config.dryRun });
    console.log(json ? JSON.stringify(result, null, 2) : `${result.status}${result.reason ? `: ${result.reason}` : ""}`);
    return;
  }
  throw new Error("usage: mediascholar.mjs <doctor|admit|run> [--dry-run] [--json]");
}

const isMain = process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) main().catch((error) => {
  console.error(`mediascholar: ${redact(error.message)}`);
  process.exit(1);
});
