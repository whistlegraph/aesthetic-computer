import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { access, mkdir, mkdtemp, readFile, rm, stat, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { createInterface } from "node:readline";
import { fileURLToPath } from "node:url";
import test from "node:test";
import JSZip from "jszip";
import { finalizePaperBundle, validatePaperResult } from "../../papers/bin/mediascholar.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const repo = join(here, "..", "..");
let nextId = 1;

function mcp(script, env = {}) {
  const child = spawn(process.execPath, [script], {
    cwd: repo,
    env: { ...process.env, ...env },
    stdio: ["pipe", "pipe", "pipe"],
  });
  const pending = new Map();
  createInterface({ input: child.stdout }).on("line", (line) => {
    let message;
    try { message = JSON.parse(line); } catch { return; }
    const waiter = pending.get(message.id);
    if (!waiter) return;
    pending.delete(message.id);
    waiter.resolve(message);
  });
  child.on("exit", (code) => {
    for (const waiter of pending.values()) waiter.reject(new Error(`MCP exited ${code}`));
    pending.clear();
  });
  return {
    child,
    call(method, params = {}, timeoutMs = 60_000) {
      const id = nextId++;
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          pending.delete(id);
          reject(new Error(`MCP call timed out: ${method}`));
        }, timeoutMs);
        pending.set(id, {
          resolve: (message) => { clearTimeout(timer); resolve(message); },
          reject: (error) => { clearTimeout(timer); reject(error); },
        });
        child.stdin.write(`${JSON.stringify({ jsonrpc: "2.0", id, method, params })}\n`);
      });
    },
  };
}

test("Prox MCP publishes the fixed Mediascholar job control", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "mediascholar-prox-mcp-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const server = mcp(join(repo, "slab", "bin", "prox-mcp.mjs"), { HOME: root });
  t.after(() => { server.child.kill(); });
  const response = await server.call("tools/list");
  const tool = response.result.tools.find((candidate) => candidate.name === "prox_job");
  assert.ok(tool);
  assert.deepEqual(tool.inputSchema.properties.action.enum, ["start", "status", "cancel"]);
  assert.deepEqual(tool.inputSchema.properties.job.enum, ["mediascholar"]);
});

test("Paper MCP build creates the required deterministic source bundle", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "mediascholar-paper-mcp-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const paperDir = join(root, "papers", "arxiv-botted-fixture");
  await mkdir(paperDir, { recursive: true });
  await writeFile(join(paperDir, "fixture.tex"), [
    "\\documentclass{article}",
    "\\usepackage{attachfile2}",
    "\\title{Botted Paper Fixture}",
    "\\author{Mediascholar}",
    "\\begin{document}",
    "\\maketitle",
    "A build fixture.",
    "\\attachfile{fixture-source.zip}",
    "\\end{document}",
    "",
  ].join("\n"));
  await writeFile(join(paperDir, "botted.json"), '{"kind":"botted-paper"}\n');
  await writeFile(join(paperDir, "references.bib"), [
    "@misc{one, title={One}}",
    "@misc{two, title={Two}}",
    "@misc{three, title={Three}}",
    "",
  ].join("\n"));
  const server = mcp(join(repo, "slab", "bin", "paper-mcp.mjs"), {
    PAPER_REPO: root,
    PAPER_DISABLE_VAULT: "1",
  });
  t.after(() => { server.child.kill(); });
  const response = await server.call("tools/call", {
    name: "paper_build",
    arguments: { paper: "fixture", passes: 1 },
  }, 120_000);
  assert.equal(response.result.isError, undefined);
  assert.match(response.result.content[0].text, /built Botted Paper Fixture/);
  await access(join(paperDir, "fixture-source.zip"));
  assert.ok((await stat(join(paperDir, "fixture-source.zip"))).size > 100);
  assert.ok((await stat(join(paperDir, "fixture.pdf"))).size > 1_000);
  const validated = await validatePaperResult({
    paperDir,
    texPath: "fixture.tex",
    pdfPath: "fixture.pdf",
    status: "candidate",
    qa: {
      built: true,
      figureTableCheck: true,
      visualInspection: true,
      pagesInspected: [1],
      remainingFailures: [],
    },
  }, paperDir);
  await writeFile(join(paperDir, "botted.json"), '{"kind":"botted-paper","status":"final"}\n');
  const finalDir = join(root, "run");
  await mkdir(finalDir);
  await finalizePaperBundle(validated, finalDir);
  const archive = await JSZip.loadAsync(await readFile(join(paperDir, "fixture-source.zip")));
  assert.ok(archive.file("botted.json"));
  assert.match(await archive.file("botted.json").async("string"), /"status":"final"/);
  const detached = await new Promise((resolve, reject) => {
    const child = spawn("pdfdetach", ["-list", join(paperDir, "fixture.pdf")]);
    let output = "";
    child.stdout.on("data", (chunk) => { output += chunk; });
    child.on("error", reject);
    child.on("exit", (code) => code === 0 ? resolve(output) : reject(new Error(`pdfdetach exited ${code}`)));
  });
  assert.match(detached, /fixture-source\.zip/);
});
