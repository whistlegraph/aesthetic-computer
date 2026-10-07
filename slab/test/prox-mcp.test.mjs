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

async function ordinaryRock(home, id, extra = {}) {
  const slabDir = join(home, ".config", "slab");
  const ledgerDir = join(slabDir, "ledger");
  const markers = join(home, ".local", "share", "slab", "state", "active-prompts");
  await mkdir(join(ledgerDir, "peers"), { recursive: true });
  await mkdir(markers, { recursive: true });
  const now = Date.now();
  await writeFile(join(ledgerDir, "local.json"), JSON.stringify({
    host: "neo", ip: "127.0.0.1", updatedAt: now,
    entries: [{
      id, host: "neo", name: "surizu", subject: "for her", agentType: "claude",
      status: "complete", kind: "session", seed: "9abc", cwd: home,
      updated: now, started: now - 5_000, ...extra,
    }],
  }));
  await writeFile(join(markers, id), JSON.stringify({
    session_id: id, cwd: home, subject: "for her", summary: "for her", tty: "ttys006",
    claude_pid: process.pid, agent_pid: process.pid, agent_type: "claude",
    updated: new Date(now).toISOString(), state: "complete", nudge_screen: "", loopboy_contact: "",
  }) + "\n");
  return { slabDir, marker: join(markers, id) };
}

test("adopt converts an ordinary Claude rock in place and renames it", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const id = "cccccccc-1111-2222-3333-444444444444";
  const { slabDir, marker } = await ordinaryRock(home, id);
  const env = { SLAB_HOME: join(home, ".local", "share", "slab") };

  const refused = await callProx(home, "prox_bind_notification", {
    handle: "neo:surizu", contact: "fia", adopt: false,
  }, env);
  assert.match(refused, /Pass adopt=true to enter Loopboy mode in place/);
  assert.equal(JSON.parse(await readFile(marker, "utf8")).loopboy_contact, "");

  const bound = await callProx(home, "prox_bind_notification", {
    handle: "neo:surizu", contact: "fia", adopt: true, name: "surizo",
  }, env);
  assert.match(bound, /Loopboy bound fia → neo:surizo/);
  assert.match(bound, /in place.*no automatic typing/);
  const stamped = JSON.parse(await readFile(marker, "utf8"));
  assert.equal(stamped.loopboy_contact, "fia");
  assert.equal(stamped.claude_pid, process.pid);
  const config = JSON.parse(await readFile(join(slabDir, "loopboy.json"), "utf8"));
  assert.equal(config.loops.fia.sessionId, id);
  assert.equal(config.loops.fia.name, "surizo");
  assert.equal(config.loops.fia.agent, "claude");
  assert.equal(config.loops.fia.wake, false);
});

test("prox_send drops a line in a local rock's inbox and prox_inbox reads it", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const id = "eeeeeeee-1111-2222-3333-444444444444";
  await ordinaryRock(home, id);
  const slabHome = join(home, ".local", "share", "slab");
  const env = { SLAB_HOME: slabHome };

  const sent = await callProx(home, "prox_send", { handle: "neo:surizu", text: "look at the diff" }, env);
  assert.match(sent, /^sent to neo:surizu via file as «neo:prox» \(queue, id [0-9a-f-]{36}\)\.\nreceiver sees: \[inbox from neo:prox · [\d: -]{16}\] │ look at the diff\nnote: surizu is complete; a file drop is read at its next prompt — prox_wake to nudge it\.$/);
  const lines = (await readFile(join(slabHome, "inbox", id, "messages.jsonl"), "utf8")).trim().split("\n");
  assert.equal(lines.length, 1);
  const message = JSON.parse(lines[0]);
  assert.equal(message.to_id, id);
  assert.equal(message.to, "neo:surizu");
  assert.equal(message.from, "neo:prox");
  assert.equal(message.text, "look at the diff");
  assert.equal(message.urgency, "queue");

  const tooLong = await callProx(home, "prox_send", { handle: "neo:surizu", text: "x".repeat(8001) }, env);
  assert.match(tooLong, /exceeds 8000/);

  const peeked = await callProx(home, "prox_inbox", { handle: "neo:surizu" }, env);
  assert.match(peeked, /^1 pending message\(s\) for neo:surizu:\n\[inbox from neo:prox · \d{4}-\d{2}-\d{2} \d{2}:\d{2}\]\n  │ look at the diff$/);
  const own = await callProx(home, "prox_inbox", { consume: true }, { ...env, CLAUDE_SESSION_ID: id });
  assert.match(own, /^1 drained message\(s\) for this session \(eeeeeeee\):/);
  assert.match(await callProx(home, "prox_inbox", { handle: "neo:surizu" }, env), /is empty\.$/);
});

test("adopt accepts Codex-backed rocks and refuses a different bound contact", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const id = "dddddddd-1111-2222-3333-444444444444";
  const { marker } = await ordinaryRock(home, id, { agentType: "codex" });
  const env = { SLAB_HOME: join(home, ".local", "share", "slab") };
  const codex = await callProx(home, "prox_bind_notification", {
    handle: "neo:surizu", contact: "fia", adopt: true,
  }, env);
  assert.match(codex, /Loopboy bound fia.*in place/);
  assert.equal(JSON.parse(await readFile(marker, "utf8")).loopboy_contact, "fia");

  const other = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const otherFixture = await ordinaryRock(other, id, { loopboyContact: "alex" });
  const otherMarker = JSON.parse(await readFile(otherFixture.marker, "utf8"));
  await writeFile(otherFixture.marker, JSON.stringify({ ...otherMarker, loopboy_contact: "alex" }));
  const foreign = await callProx(other, "prox_bind_notification", {
    handle: "neo:surizu", contact: "fia", adopt: true,
  }, { SLAB_HOME: join(other, ".local", "share", "slab") });
  assert.match(foreign, /is bound to alex; use prox_unbind_notification/);
});

test("Easel namespace is exact, fleet ambiguity is preserved, and local stays local", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-easel-test-"));
  const dir = join(home, ".config", "slab", "ledger");
  await mkdir(join(dir, "peers"), { recursive: true });
  const entry = (host, agentType, id) => ({ id, host, name: "old-piece", proxName: "bugo", proxNamespace: "easel", agentType, kind: "session", status: "complete", updated: Date.now() });
  await writeFile(join(dir, "local.json"), JSON.stringify({host:"blueberry",entries:[entry("blueberry","easel","one"),entry("blueberry","codex","other")]}));
  await writeFile(join(dir, "peers", "neo.json"), JSON.stringify({host:"neo",entries:[entry("neo","easel","two")]}));
  assert.match(await callProx(home,"prox_find",{handle:"prox:easel:bugo"}),/2 match/);
  const scoped = await callProx(home,"prox_find",{handle:"prox:easel:blueberry:bugo"});
  assert.match(scoped,/1 match/); assert.match(scoped,/id: +one/);
  assert.match(await callProx(home,"prox_find",{handle:"prox:easel:bug"}),/no rock resolves/);
  assert.match(await callProx(home,"prox_find",{handle:"local:one"}),/1 match/);
  assert.match(await callProx(home,"prox_find",{handle:"local:two"}),/no rock resolves/);
  assert.match(await callProx(home,"prox_poke",{handle:"prox:easel:bugo"}),/ambiguous/);
});

test("aesel and easel name the same agent type and namespace", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-aesel-test-"));
  const dir = join(home, ".config", "slab", "ledger");
  await mkdir(join(dir, "peers"), { recursive: true });
  const entry = (host, agentType, id) => ({ id, host, name: "old-piece", proxName: "bugo", proxNamespace: agentType, agentType, kind: "session", status: "complete", updated: Date.now() });
  // One writer that has flipped, one that has not.
  await writeFile(join(dir, "local.json"), JSON.stringify({host:"blueberry",entries:[entry("blueberry","aesel","one")]}));
  await writeFile(join(dir, "peers", "neo.json"), JSON.stringify({host:"neo",entries:[entry("neo","easel","two")]}));
  assert.match(await callProx(home,"prox_find",{handle:"prox:aesel:bugo"}),/2 match/);
  assert.match(await callProx(home,"prox_find",{handle:"prox:easel:bugo"}),/2 match/);
  assert.match(await callProx(home,"prox_find",{handle:"aesel:blueberry:bugo"}),/1 match/);
  const listed = await callProx(home,"prox_list",{agent:"aesel",all:true});
  assert.match(listed,/blueberry/); assert.match(listed,/neo/);
});

test("prox_send signs with the calling session's own host:name", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const target = "eeeeeeee-1111-2222-3333-444444444444";
  const caller = "ffffffff-1111-2222-3333-444444444444";
  const { slabDir } = await ordinaryRock(home, target);
  const ledger = join(slabDir, "ledger", "local.json");
  const local = JSON.parse(await readFile(ledger, "utf8"));
  local.entries.push({ ...local.entries[0], id: caller, name: "nid", subject: "the sender" });
  await writeFile(ledger, JSON.stringify(local));
  const inbox = join(home, ".local", "share", "slab");
  const env = { SLAB_HOME: inbox };

  // a per-session stdio child inherits the session id from the harness
  const viaEnv = await callProx(home, "prox_send", { handle: "neo:surizu", text: "hi" }, { ...env, CLAUDE_SESSION_ID: caller });
  assert.match(viaEnv, /as «neo:nid»/);
  assert.match(viaEnv, /reply: prox_send handle="neo:nid"/);
  // an explicit `by` still wins
  assert.match(await callProx(home, "prox_send", { handle: "neo:surizu", text: "hi", by: "neo:custom" }, { ...env, CLAUDE_SESSION_ID: caller }), /as «neo:custom»/);
  // an id that is no rock falls back to the anonymous sender
  assert.match(await callProx(home, "prox_send", { handle: "neo:surizu", text: "hi" }, { ...env, CLAUDE_SESSION_ID: "no-such-rock" }), /as «neo:prox»/);
});

test("prox_send refuses an ambiguous handle as an error that lists candidates", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const { slabDir } = await ordinaryRock(home, "eeeeeeee-1111-2222-3333-444444444444");
  const ledger = join(slabDir, "ledger", "local.json");
  const local = JSON.parse(await readFile(ledger, "utf8"));
  local.entries.push({ ...local.entries[0], id: "ffffffff-1111-2222-3333-444444444444", name: "surizo" });
  await writeFile(ledger, JSON.stringify(local));
  const env = { SLAB_HOME: join(home, ".local", "share", "slab") };

  const child = spawn(process.execPath, [prox], { env: { ...process.env, HOME: home, ...env }, stdio: ["pipe", "pipe", "pipe"] });
  let stdout = "";
  child.stdout.on("data", (c) => { stdout += c; });
  child.stdin.end(`${JSON.stringify({ jsonrpc: "2.0", id: 1, method: "tools/call", params: { name: "prox_send", arguments: { handle: "neo:suriz", text: "hi" } } })}\n`);
  await once(child, "close");
  const result = JSON.parse(stdout.trim()).result;
  assert.equal(result.isError, true);
  assert.match(result.content[0].text, /ambiguous — nothing sent\. Candidates: neo:surizu \(complete, \d+s\), neo:surizo \(complete, \d+s\)/);
});

// The shared daemon cannot read the caller's env. Claude Code forwards no
// header, so the daemon finds the caller by whoever owns the loopback socket.
async function withDaemon(home, env, run) {
  const port = 20000 + Math.floor(Math.random() * 20000);
  const daemon = spawn(process.execPath, [prox, "--http", String(port)], {
    env: { ...process.env, HOME: home, ...env }, stdio: ["ignore", "ignore", "pipe"],
  });
  let banner = "";
  daemon.stderr.setEncoding("utf8");
  daemon.stderr.on("data", (c) => { banner += c; });
  for (let i = 0; i < 100 && !banner.includes("on http://"); i++) await new Promise((r) => setTimeout(r, 50));
  try {
    return await run(async (args, headers = {}) => {
      const res = await fetch(`http://127.0.0.1:${port}`, {
        method: "POST",
        headers: { "content-type": "application/json", connection: "close", ...headers },
        body: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "tools/call", params: { name: "prox_send", arguments: args } }),
      });
      return (await res.json()).result.content[0].text;
    });
  } finally {
    daemon.kill();
  }
}

test("the shared daemon signs a send by forwarded header or by the connection's owning process", async () => {
  const home = await mkdtemp(join(tmpdir(), "prox-mcp-test-"));
  const target = "eeeeeeee-1111-2222-3333-444444444444";
  const other = "ffffffff-1111-2222-3333-444444444444";
  const { slabDir } = await ordinaryRock(home, target); // marker: claude_pid = this test process
  const ledger = join(slabDir, "ledger", "local.json");
  const local = JSON.parse(await readFile(ledger, "utf8"));
  local.entries.push({ ...local.entries[0], id: other, name: "nid" });
  await writeFile(ledger, JSON.stringify(local));
  // the daemon's own env names an unrelated session; it must never be used
  const env = { SLAB_HOME: join(home, ".local", "share", "slab"), CLAUDE_SESSION_ID: other };

  await withDaemon(home, env, async (send) => {
    // no header: this process owns the socket and is `surizu`'s marker pid
    assert.match(await send({ handle: "neo:nid", text: "a" }), /as «neo:surizu»/);
    // a forwarded header wins over the socket owner
    assert.match(await send({ handle: "neo:surizu", text: "b" }, { "x-slab-prompt-session-id": other }), /as «neo:nid»/);
  });
});
