import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { ClaudeServer, DEFAULT_CLAUDE_MODEL } from "../src/claude-server.mjs";

const directory = path.dirname(fileURLToPath(import.meta.url));
const fake = path.join(directory, "fake-claude-cli.mjs");

// A flag's value, or undefined when the flag is not there. Reaching for
// `argv.indexOf(name) + 1` instead reads argv[0] on a missing flag and
// compares against "--print", which fails in a way that names the wrong cause.
function flagIn(argv, name) {
  const index = argv.indexOf(name);
  return index < 0 ? undefined : argv[index + 1];
}

// Every spawn that recorded itself, oldest first.
function launches(argvFile) {
  const raw = readFileSync(argvFile, "utf8");
  const lines = raw.split("\n").filter(Boolean).map((line) => JSON.parse(line));
  return { raw, lines, argvs: lines.map((line) => line.argv) };
}

function scratch() {
  const root = mkdtempSync(path.join(tmpdir(), "aesthetic-claude-"));
  return { root, cleanup: () => rmSync(root, { recursive: true, force: true }) };
}

function bridge(t, options = {}) {
  const engine = new ClaudeServer({
    cwd: directory,
    command: process.execPath,
    args: [fake],
    ...options,
  });
  t.after(() => engine.close());
  return engine;
}

test("drives a turn, streams text, and routes the approval to the interface", async (t) => {
  const { root, cleanup } = scratch();
  const decisionFile = path.join(root, "decision.json");
  const engine = bridge(t, { environment: { FAKE_CLAUDE_DECISION: decisionFile } });

  const deltas = [];
  const items = [];
  const completed = new Promise((resolve) => {
    engine.on("notification", ({ method, params }) => {
      if (method === "item/agentMessage/delta") deltas.push(params.delta);
      if (method === "item/started") items.push(params.item);
      if (method === "item/completed") items.push(params.item);
      if (method === "turn/completed") resolve(params.turn.status);
    });
  });
  engine.on("request", (request) => engine.respond(request.id, { decision: "accept" }));

  const connection = await engine.connect();
  // The bridge is ready on the handshake, before the CLI has opened a turn,
  // so the thread is named by the id the launch minted.
  assert.match(connection.thread.id, /^[0-9a-f-]{36}$/);
  assert.equal(connection.thread.id, engine.threadId);
  assert.equal(connection.model, DEFAULT_CLAUDE_MODEL);

  const turn = await engine.startTurn("make a piece");
  assert.equal(turn.turn.status, "inProgress");
  assert.equal(await completed, "completed");
  assert.equal(deltas.join(""), "Writing the piece.");

  // The interface follows the piece through fileChange items.
  const started = items.find((item) => item.type === "fileChange");
  assert.deepEqual(started.changes, [{ path: "/tmp/piece.mjs" }]);
  assert.equal(items.at(-1).status, "completed");

  assert.deepEqual(JSON.parse(readFileSync(decisionFile, "utf8")).behavior, "allow");
  cleanup();
});

test("a declined approval fails the item rather than the turn", async (t) => {
  const engine = bridge(t);
  const items = [];
  const completed = new Promise((resolve) => {
    engine.on("notification", ({ method, params }) => {
      if (method === "item/completed") items.push(params.item);
      if (method === "turn/completed") resolve(params.turn.status);
    });
  });
  engine.on("request", (request) => engine.respond(request.id, { decision: "decline" }));

  await engine.connect();
  await engine.startTurn("make a piece");
  assert.equal(await completed, "completed");
  assert.equal(items.at(-1).status, "failed");
});

test("accept-for-session pins the suggested rules to the session, never to disk", async (t) => {
  const { root, cleanup } = scratch();
  const decisionFile = path.join(root, "decision.json");
  const engine = bridge(t, { environment: { FAKE_CLAUDE_DECISION: decisionFile } });
  const completed = new Promise((resolve) => {
    engine.on("notification", ({ method, params }) => {
      if (method === "turn/completed") resolve(params.turn.status);
    });
  });
  engine.on("request", (request) => engine.respond(request.id, { decision: "acceptForSession" }));

  await engine.connect();
  await engine.startTurn("make a piece");
  await completed;

  const decision = JSON.parse(readFileSync(decisionFile, "utf8"));
  assert.equal(decision.behavior, "allow");
  assert.equal(decision.updatedPermissions.length, 1);
  assert.equal(decision.updatedPermissions[0].destination, "session");
  cleanup();
});

test("the launch carries the approval contract and the workspace", async (t) => {
  const { root, cleanup } = scratch();
  const argvFile = path.join(root, "argv.json");
  const engine = bridge(t, {
    developerInstructions: "piece rules",
    environment: { FAKE_CLAUDE_ARGV: argvFile },
  });
  await engine.connect();
  const record = launches(argvFile);
  assert.equal(record.lines.length, 1, `expected one spawn, got:\n${record.raw}`);
  const argv = record.argvs[0];
  const flag = (name) => flagIn(argv, name);

  assert.equal(flag("--model"), DEFAULT_CLAUDE_MODEL);
  // Without this the CLI has nobody to ask and denies every prompt itself.
  assert.equal(flag("--permission-prompt-tool"), "stdio");
  assert.equal(flag("--permission-prompts"), "host");
  assert.equal(flag("--permission-mode"), "manual");
  // The user's own allow-lists, hooks and MCP servers stay out of a session.
  assert.equal(flag("--setting-sources"), "");
  assert.ok(argv.includes("--strict-mcp-config"));
  for (const tool of ["WebFetch", "WebSearch", "Task"]) assert.ok(argv.includes(tool));
  assert.equal(flag("--add-dir"), directory);
  assert.equal(flag("--append-system-prompt"), "piece rules");
  assert.ok(argv.includes("--session-id"));
  assert.ok(!argv.includes("--resume"));
  cleanup();
});

test("resuming names the thread instead of minting one", async (t) => {
  const { root, cleanup } = scratch();
  const argvFile = path.join(root, "argv.json");
  const threadId = "00000000-0000-0000-0000-000000000001";
  const engine = bridge(t, { resumeThreadId: threadId, environment: { FAKE_CLAUDE_ARGV: argvFile } });
  const completed = new Promise((resolve) => {
    engine.on("notification", ({ method, params }) => {
      if (method === "turn/completed") resolve(params.turn.status);
    });
  });
  engine.on("request", (request) => engine.respond(request.id, { decision: "accept" }));

  await engine.connect();
  const record = launches(argvFile);
  assert.equal(record.lines.length, 1, `expected one spawn, got:\n${record.raw}`);
  const argv = record.argvs[0];
  assert.equal(flagIn(argv, "--resume"), threadId, `argv was:\n${record.raw}`);
  assert.ok(!argv.includes("--session-id"));

  // Take a turn before reading the id back. `system/init` arrives with the
  // turn, and it is what revises the thread id — asserting straight after
  // connect() races that message and can pass without ever seeing it.
  await engine.startTurn("carry on");
  assert.equal(await completed, "completed");
  assert.equal(engine.threadId, threadId, "a resumed thread keeps its id");
  cleanup();
});

test("an interrupted turn reads as interrupted, not as a failure", async (t) => {
  const engine = bridge(t);
  const completed = new Promise((resolve) => {
    engine.on("notification", ({ method, params }) => {
      if (method === "turn/completed") resolve(params.turn);
    });
  });
  await engine.connect();
  await engine.startTurn("count to a thousand");
  await engine.interrupt();
  const turn = await completed;
  assert.equal(turn.status, "interrupted");
  assert.equal(turn.error, undefined);
});

// A turn that the API refuses reports `interrupted`, and an interrupted turn
// carries no error — so if the refusal itself is dropped, the session simply
// stops for no stated reason. That is what two parallel sessions looked like.
test("a refused turn says why, instead of stopping in silence", async (t) => {
  const engine = bridge(t, {
    environment: { FAKE_CLAUDE_API_ERROR: "Usage limit reached. Try again at 6pm." },
  });

  const errors = [];
  const finished = new Promise((resolve) => {
    engine.on("notification", ({ method, params }) => {
      if (method === "error") errors.push(params.error?.message);
      if (method === "turn/completed") resolve(params.turn);
    });
  });

  await engine.connect();
  await engine.startTurn("make a piece");
  const turn = await finished;

  assert.equal(turn.status, "interrupted", "the turn still reports how it ended");
  assert.deepEqual(errors, ["Usage limit reached. Try again at 6pm."],
    "and the reason reaches the interface");
});
