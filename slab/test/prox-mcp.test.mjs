import test from "node:test";
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { mkdir, mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const prox = join(here, "..", "bin", "prox-mcp.mjs");

async function callProx(home, name, args) {
  const child = spawn(process.execPath, [prox], {
    env: { ...process.env, HOME: home },
    stdio: ["pipe", "pipe", "pipe"],
  });
  let stdout = "";
  child.stdout.setEncoding("utf8");
  child.stdout.on("data", (chunk) => { stdout += chunk; });
  child.stdin.end(`${JSON.stringify({
    jsonrpc: "2.0",
    id: 1,
    method: "tools/call",
    params: { name, arguments: args },
  })}\n`);
  const [code] = await once(child, "close");
  assert.equal(code, 0);
  return JSON.parse(stdout.trim()).result.content[0].text;
}

test("prox_find timestamps and labels a subject-only match", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const ledgerDir = join(home, ".config", "slab", "ledger");
  await mkdir(join(ledgerDir, "peers"), { recursive: true });
  const now = Date.now();
  await writeFile(join(ledgerDir, "local.json"), JSON.stringify({
    host: "neo",
    ip: "127.0.0.1",
    updatedAt: now,
    entries: [{
      id: "aaaaaaaa-1111-2222-3333-444444444444",
      host: "neo",
      name: "fotos",
      subject: "working on jastow",
      status: "working",
      kind: "session",
      seed: "1234",
      cwd: home,
      updated: now,
      started: now - 5_000,
    }],
  }));

  const text = await callProx(home, "prox_find", { handle: "jastow" });
  assert.match(text, /^checked_at: \d{4}-\d{2}-\d{2}T/);
  assert.match(text, /by subject-substring/);
  assert.match(text, /discovery-only match/);
  assert.match(text, /neo:fotos#aaaaaaaa/);
  assert.match(text, /duplicate check: ids=0, host:name aliases=0, fleet pet names=0/);
});

test("prox_list includes stale empty ledger snapshots", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const ledgerDir = join(home, ".config", "slab", "ledger");
  const peers = join(ledgerDir, "peers");
  await mkdir(peers, { recursive: true });
  const now = Date.now();
  await writeFile(join(ledgerDir, "local.json"), JSON.stringify({
    host: "neo", ip: "127.0.0.1", updatedAt: now, entries: [],
  }));
  await writeFile(join(peers, "mac.json"), JSON.stringify({
    host: "mac", ip: "127.0.0.2", updatedAt: now - 180_000, entries: [],
  }));

  const text = await callProx(home, "prox_list", {});
  assert.match(text, /^checked_at: \d{4}-\d{2}-\d{2}T/);
  assert.match(text, /ledger snapshots \(2\)/);
  assert.match(text, /mac: .*stale\), 0 rock\(s\)/);
  assert.match(text, /\(no prompt rocks match\)/);
});
