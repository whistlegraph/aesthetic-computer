import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const SERVER = resolve(ROOT, "slab/bin/paper-mcp.mjs");

function call(method, params = {}, env = {}) {
  return new Promise((resolveCall, reject) => {
    const child = spawn(process.execPath, [SERVER], {
      cwd: ROOT,
      env: { ...process.env, ...env },
      stdio: ["pipe", "pipe", "pipe"],
    });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk) => { stdout += chunk; });
    child.stderr.on("data", (chunk) => { stderr += chunk; });
    child.on("error", reject);
    child.on("close", (code) => {
      if (code !== 0) return reject(new Error(`paper-mcp exited ${code}: ${stderr}`));
      const lines = stdout.trim().split("\n").filter(Boolean);
      try { resolveCall(JSON.parse(lines.at(-1))); }
      catch { reject(new Error(`invalid paper-mcp response: ${stdout}\n${stderr}`)); }
    });
    child.stdin.end(`${JSON.stringify({ jsonrpc: "2.0", id: 1, method, params })}\n`);
  });
}

test("paper MCP exposes the complete paper loop", async () => {
  const res = await call("tools/list");
  assert.deepEqual(
    res.result.tools.map((tool) => tool.name),
    ["paper_list", "paper_find", "paper_read", "paper_build", "paper_open"],
  );
});

test("paper_find resolves a public scope:basename alias", async () => {
  const res = await call("tools/call", {
    name: "paper_find",
    arguments: { paper: "public:whistlegraph" },
  });
  assert.equal(res.result.isError, undefined);
  assert.match(res.result.content[0].text, /Whistlegraph/i);
  assert.match(res.result.content[0].text, /papers\/arxiv-whistlegraph\/whistlegraph\.tex/);
});

test("paper search discovers the private Fuser draft when the vault is present", async (t) => {
  const res = await call("tools/call", {
    name: "paper_list",
    arguments: { query: "Tokens 2 TLDs", limit: 5 },
  });
  const text = res.result.content[0].text;
  if (/no papers match/.test(text)) return t.skip("private vault is not installed on this machine");
  assert.match(text, /fuser:tokens-2-tlds/);
});

test("paper_find prefers the finished TeX/PDF over same-title working Markdown", async (t) => {
  const res = await call("tools/call", {
    name: "paper_find",
    arguments: { paper: "Tokens 2 TLDs" },
  });
  const text = res.result.content[0].text;
  if (/no paper resolves/.test(text)) return t.skip("private vault is not installed on this machine");
  assert.equal(res.result.isError, undefined);
  assert.match(text, /fuser:arxiv-fuser\/tokens-2-tlds/);
  assert.match(text, /tokens-2-tlds\.pdf/);
});

test("paper tools never resolve arbitrary traversal paths", async () => {
  const res = await call("tools/call", {
    name: "paper_read",
    arguments: { paper: "../../../../etc/passwd" },
  });
  assert.equal(res.result.isError, true);
  assert.match(res.result.content[0].text, /no paper resolves/);
});
