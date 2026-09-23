import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { copyFile, mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

const here = dirname(fileURLToPath(import.meta.url));
const realBin = join(here, "..", "bin");
const realInbox = join(realBin, "prox-inbox.mjs");

// prox-inbox.mjs lands from another lane. When it's absent, run the drain from
// a temp dir beside a stub that speaks the agreed contract.
const STUB = `
import { appendFileSync, existsSync, mkdirSync, readFileSync, renameSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
const root = process.env.SLAB_HOME || join(homedir(), ".local/share/slab");
const dir = (sid) => join(root, "inbox", sid);
const parse = (text) => text.split("\\n").filter(Boolean).map((l) => JSON.parse(l));
export function peek(sid) {
  const f = join(dir(sid), "messages.jsonl");
  return existsSync(f) ? parse(readFileSync(f, "utf8")) : [];
}
export function drain(sid) {
  const f = join(dir(sid), "messages.jsonl");
  if (!existsSync(f)) return [];
  const tmp = f + ".draining";
  renameSync(f, tmp);
  const msgs = parse(readFileSync(tmp, "utf8"));
  mkdirSync(dir(sid), { recursive: true });
  appendFileSync(join(dir(sid), "log.jsonl"), msgs.map((m) => JSON.stringify(m) + "\\n").join(""));
  renameSync(tmp, f + ".drained");
  return msgs;
}
export function stamp(m) {
  const when = new Date(m.ts).toISOString().slice(0, 16).replace("T", " ");
  return \`[inbox from \${m.from} · \${when}] \${m.text}\`;
}
`;

async function setup(t) {
  const root = await mkdtemp(join(tmpdir(), "inbox-drain-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  let drainPath = join(realBin, "inbox-drain.mjs");
  if (!existsSync(realInbox)) {
    const bin = join(root, "bin");
    await mkdir(bin);
    await copyFile(drainPath, join(bin, "inbox-drain.mjs"));
    await writeFile(join(bin, "prox-inbox.mjs"), STUB);
    drainPath = join(bin, "inbox-drain.mjs");
  }
  return { home: join(root, "slab"), drainPath };
}

async function seed(home, sid, texts) {
  const dir = join(home, "inbox", sid);
  await mkdir(dir, { recursive: true });
  const lines = texts.map((text, i) => JSON.stringify({
    v: 1, id: `m${i}`, ts: Date.UTC(2026, 8, 23, 17, 58), from: "neo:sip", to: "blueberry:claude",
    to_id: sid, text, urgency: "queue", kind: "chat",
  }) + "\n");
  await writeFile(join(dir, "messages.jsonl"), lines.join(""));
}

function run(drainPath, home, args, stdin) {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [drainPath, ...args], { env: { ...process.env, SLAB_HOME: home, TZ: "UTC" } });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c));
    child.stderr.on("data", (c) => (stderr += c));
    child.on("close", (code) => resolve({ code, stdout, stderr }));
    child.stdin.end(stdin);
  });
}

const payload = (extra) => JSON.stringify({ session_id: "sess-1", cwd: "/x", ...extra });

test("prompt: pending messages become UserPromptSubmit additionalContext and are consumed", async (t) => {
  const { home, drainPath } = await setup(t);
  await seed(home, "sess-1", ["ship it", "then nap"]);
  const r = await run(drainPath, home, ["prompt"], payload({ hook_event_name: "UserPromptSubmit", prompt: "hi" }));
  assert.equal(r.code, 0, r.stderr);
  const json = JSON.parse(r.stdout);
  assert.equal(json.hookSpecificOutput.hookEventName, "UserPromptSubmit");
  const ctx = json.hookSpecificOutput.additionalContext;
  assert.ok(ctx.startsWith("Messages from other sessions (via prox inbox):\n"));
  assert.match(ctx, /\[inbox from neo:sip · 2026-09-23 17:58\] ship it\n/);
  assert.match(ctx, /then nap$/);
  const again = await run(drainPath, home, ["prompt"], payload({}));
  assert.equal(again.stdout, "", "second drain finds nothing");
  const log = await readFile(join(home, "inbox", "sess-1", "log.jsonl"), "utf8");
  assert.equal(log.split("\n").filter(Boolean).length, 2);
});

test("prompt: empty inbox is silent", async (t) => {
  const { home, drainPath } = await setup(t);
  const r = await run(drainPath, home, ["prompt"], payload({}));
  assert.equal(r.code, 0);
  assert.equal(r.stdout, "");
});

test("stop: pending messages block the stop with header + stamped lines", async (t) => {
  const { home, drainPath } = await setup(t);
  await seed(home, "sess-1", ["look at the diff"]);
  const r = await run(drainPath, home, ["stop"], payload({ hook_event_name: "Stop", stop_hook_active: false }));
  assert.equal(r.code, 0, r.stderr);
  const json = JSON.parse(r.stdout);
  assert.equal(json.decision, "block");
  assert.equal(json.reason, "Messages from other sessions (via prox inbox):\n[inbox from neo:sip · 2026-09-23 17:58] look at the diff");
});

test("stop: empty inbox is silent, also inside a continuation", async (t) => {
  const { home, drainPath } = await setup(t);
  for (const active of [false, true]) {
    const r = await run(drainPath, home, ["stop"], payload({ stop_hook_active: active }));
    assert.equal(r.code, 0);
    assert.equal(r.stdout, "");
  }
});

test("stop (codex): empty inbox answers {} because Codex wants JSON", async (t) => {
  const { home, drainPath } = await setup(t);
  const r = await run(drainPath, home, ["stop", "codex"], payload({}));
  assert.equal(r.code, 0);
  assert.deepEqual(JSON.parse(r.stdout), {});
});

test("garbage stdin and no stdin both exit 0 without output", async (t) => {
  const { home, drainPath } = await setup(t);
  for (const stdin of ["not json {{{", ""]) {
    const r = await run(drainPath, home, ["prompt"], stdin);
    assert.equal(r.code, 0);
    assert.equal(r.stdout, "");
  }
  const bad = await run(drainPath, home, ["nonsense"], payload({}));
  assert.equal(bad.code, 0);
  assert.match(bad.stderr, /usage/);
});
