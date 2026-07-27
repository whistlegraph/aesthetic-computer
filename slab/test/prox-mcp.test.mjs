import test from "node:test";
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { mkdir, mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const prox = join(here, "..", "bin", "prox-mcp.mjs");

async function callProx(home, name, args, extraEnv = {}) {
  const child = spawn(process.execPath, [prox], {
    env: { ...process.env, HOME: home, ...extraEnv },
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

test("registry-only routes neither badge nor retrofit an ordinary session", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const slabDir = join(home, ".config", "slab");
  const ledgerDir = join(slabDir, "ledger");
  await mkdir(join(ledgerDir, "peers"), { recursive: true });
  const now = Date.now();
  const id = "aaaaaaaa-1111-2222-3333-444444444444";
  await writeFile(join(ledgerDir, "local.json"), JSON.stringify({
    host: "neo", ip: "127.0.0.1", updatedAt: now,
    entries: [{
      id, host: "neo", name: "fotos", subject: "ordinary prompt",
      status: "working", kind: "session", seed: "1234", cwd: home,
      updated: now, started: now - 5_000,
    }],
  }));
  await writeFile(join(slabDir, "loopboy.json"), JSON.stringify({
    version: 1,
    loops: { alex: { contact: "alex", sessionId: id, host: "neo", name: "fotos" } },
  }));

  const list = await callProx(home, "prox_list", { host: "neo" });
  assert.doesNotMatch(list, /loopboy:alex/);
  const bound = await callProx(home, "prox_bind_notification", {
    handle: "neo:fotos#aaaaaaaa", contact: "alex",
  });
  assert.match(bound, /was not launched as a guarded Loopboy/);
});

test("binding accepts a live marker identity only for its own contact", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const slabDir = join(home, ".config", "slab");
  const ledgerDir = join(slabDir, "ledger");
  await mkdir(join(ledgerDir, "peers"), { recursive: true });
  const now = Date.now();
  const id = "bbbbbbbb-1111-2222-3333-444444444444";
  await writeFile(join(ledgerDir, "local.json"), JSON.stringify({
    host: "neo", ip: "127.0.0.1", updatedAt: now,
    entries: [{
      id, host: "neo", name: "nimef", subject: "guarded prompt",
      status: "working", kind: "session", seed: "5678", cwd: home,
      updated: now, started: now - 5_000, loopboyContact: "alex",
    }],
  }));

  const wrong = await callProx(home, "prox_bind_notification", {
    handle: "neo:nimef#bbbbbbbb", contact: "fia",
  });
  assert.match(wrong, /was launched for alex, not fia/);

  const bound = await callProx(home, "prox_bind_notification", {
    handle: "neo:nimef#bbbbbbbb", contact: "alex",
  });
  assert.match(bound, /Loopboy bound alex/);
  const config = JSON.parse(await readFile(join(slabDir, "loopboy.json"), "utf8"));
  assert.equal(config.loops.alex.sessionId, id);
  assert.equal(config.loops.alex.delivery, "bus");
  assert.equal(config.loops.alex.channel, "imessage");
});

test("wait auto-repairs a stale registry route to the live guarded listener", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const slabDir = join(home, ".config", "slab");
  const ledgerDir = join(slabDir, "ledger");
  await mkdir(join(ledgerDir, "peers"), { recursive: true });
  const now = Date.now();
  const oldId = "aaaaaaaa-1111-2222-3333-444444444444";
  const newId = "bbbbbbbb-1111-2222-3333-444444444444";
  await writeFile(join(ledgerDir, "local.json"), JSON.stringify({
    host: "neo", ip: "127.0.0.1", updatedAt: now,
    entries: [
      {
        id: newId, host: "neo", name: "new", subject: "alex listener",
        status: "working", kind: "session", cwd: home, updated: now,
        started: now, loopboyContact: "alex", agentType: "codex",
      },
      {
        id: oldId, host: "neo", name: "old", subject: "old listener",
        status: "complete", kind: "session", cwd: home, updated: now - 5_000,
        started: now - 10_000, loopboyContact: "alex", agentType: "codex",
      },
    ],
  }));
  await writeFile(join(slabDir, "loopboy.json"), JSON.stringify({
    version: 1,
    loops: {
      alex: {
        contact: "alex", sessionId: oldId, host: "neo", name: "old",
        autoRespond: false, delivery: "inbox",
      },
    },
  }));

  const env = { SLAB_LOOPBOY_CONTACT: "alex", SLAB_PROMPT_SESSION_ID: newId };
  const first = await callProx(home, "prox_loopboy_wait", {
    contact: "alex", timeoutSeconds: 0,
  }, env);
  assert.match(first, /Auto-repaired Loopboy alex.*neo:new#bbbbbbbb/);
  const config = JSON.parse(await readFile(join(slabDir, "loopboy.json"), "utf8"));
  assert.equal(config.loops.alex.sessionId, newId);
  assert.equal(config.loops.alex.delivery, "bus");
  assert.equal(config.loops.alex.channel, "imessage");
  assert.equal(config.loops.alex.autoRespond, false);

  const second = await callProx(home, "prox_loopboy_wait", {
    contact: "alex", timeoutSeconds: 0,
  }, env);
  assert.doesNotMatch(second, /Auto-repaired/);
});

test("a second live Loopboy cannot steal an active contact route", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const slabDir = join(home, ".config", "slab");
  const ledgerDir = join(slabDir, "ledger");
  await mkdir(join(ledgerDir, "peers"), { recursive: true });
  const now = Date.now();
  const ownerId = "aaaaaaaa-1111-2222-3333-444444444444";
  const callerId = "bbbbbbbb-1111-2222-3333-444444444444";
  const entries = [
    { id: ownerId, host: "neo", name: "owner", status: "awaiting", kind: "session",
      updated: now, started: now - 5_000, loopboyContact: "alex" },
    { id: callerId, host: "neo", name: "caller", status: "working", kind: "session",
      updated: now, started: now, loopboyContact: "alex" },
  ];
  await writeFile(join(ledgerDir, "local.json"), JSON.stringify({
    host: "neo", ip: "127.0.0.1", updatedAt: now, entries,
  }));
  await writeFile(join(slabDir, "loopboy.json"), JSON.stringify({
    version: 1,
    loops: { alex: { contact: "alex", sessionId: ownerId, host: "neo", name: "owner" } },
  }));

  const text = await callProx(home, "prox_loopboy_wait", {
    contact: "alex", timeoutSeconds: 0,
  }, { SLAB_LOOPBOY_CONTACT: "alex", SLAB_PROMPT_SESSION_ID: callerId });
  assert.match(text, /not the bound alex listener.*neo:owner#aaaaaaaa/);
});

test("a guarded Loopboy can release its route and schedule its own shutdown", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const slabDir = join(home, ".config", "slab");
  const ledgerDir = join(slabDir, "ledger");
  const markerDir = join(home, ".local", "share", "slab", "state", "active-prompts");
  await mkdir(join(ledgerDir, "peers"), { recursive: true });
  await mkdir(markerDir, { recursive: true });
  const now = Date.now();
  const id = "cccccccc-1111-2222-3333-444444444444";
  await writeFile(join(ledgerDir, "local.json"), JSON.stringify({
    host: "neo", ip: "127.0.0.1", updatedAt: now,
    entries: [{
      id, host: "neo", name: "closer", subject: "alex listener",
      status: "working", kind: "session", cwd: home, updated: now,
      started: now, loopboyContact: "alex", agentType: "codex",
    }],
  }));
  await writeFile(join(markerDir, id), JSON.stringify({
    id, tty: "ttys999", agent_pid: process.pid,
  }));
  await writeFile(join(slabDir, "loopboy.json"), JSON.stringify({
    version: 1,
    loops: { alex: { contact: "alex", sessionId: id, host: "neo", name: "closer" } },
  }));

  const text = await callProx(home, "prox_close", { handle: id }, {
    SLAB_LOOPBOY_CONTACT: "alex",
    SLAB_PROMPT_SESSION_ID: id,
    SLAB_PROX_CLOSE_DRY_RUN: "1",
  });
  assert.match(text, /scheduled guarded Loopboy shutdown/);
  assert.match(text, /released alex route/);
  assert.match(text, /Slab re-tiles/);
  const config = JSON.parse(await readFile(join(slabDir, "loopboy.json"), "utf8"));
  assert.equal(config.loops.alex, undefined);
});
