import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import test from "node:test";
import { AcServer } from "../src/ac-server.mjs";
import { ESCALATE_AFTER, askParts, escalate, routeAsk } from "../src/model-router.mjs";
import { HOSTED_MODELS } from "../src/open-models.mjs";

const tier = (text, source = "", images = 0) => routeAsk({ text, source, images }).tier;
const piece = Array.from({ length: 30 }, (_, i) => `// line ${i}`).join("\n");

test("asks that name a hard system go to the hard tier", () => {
  assert.equal(tier("a multiplayer drawing room where other players see your strokes"), "hard");
  assert.equal(tier("a 3D wireframe cube you can rotate by dragging"), "hard");
  assert.equal(tier("a platformer with gravity, three levels, enemies that patrol, coins for score"), "hard");
  assert.equal(tier("make one like this", "", 1), "hard");
  assert.equal(tier("make it blue", Array(500).fill("x").join("\n")), "hard");
});

test("a short tweak to an existing piece goes to the small tier", () => {
  assert.equal(tier("make it blue", piece), "small");
  assert.equal(tier("slower", piece), "small");
  assert.equal(tier("make the text bigger", piece), "small");
});

test("the blank a session opens on is a new piece, not an edit", async () => {
  const { RUNTIMES } = await import("../src/runtimes.mjs");
  for (const runtime of Object.values(RUNTIMES)) {
    assert.equal(tier("a red circle in the middle", runtime.blank("piece")), "normal", runtime.id);
  }
});

test("a tweak that adds behaviour, or any new piece, stays normal", () => {
  assert.equal(tier("make it blue"), "normal");
  assert.equal(tier("now each square plays a note when tapped", piece), "normal");
  assert.equal(tier("add a little spaceship you can steer", piece), "normal");
  assert.equal(tier("a playable snake game with arrow keys and swipes"), "normal");
});

test("every task file routes, and the routing set lands where it is named", () => {
  const root = new URL("../tasks/", import.meta.url).pathname;
  for (const domain of readdirSync(root, { withFileTypes: true }).filter((d) => d.isDirectory())) {
    for (const file of readdirSync(join(root, domain.name)).filter((f) => f.endsWith(".json"))) {
      const task = JSON.parse(readFileSync(join(root, domain.name, file), "utf8"));
      const route = routeAsk({ text: task.requests.at(-1), source: task.requests.length > 1 ? piece : "" });
      assert.ok(HOSTED_MODELS[route.model], `${task.id} routes to a hosted model`);
      if (task.expectTier) assert.equal(route.tier, task.expectTier, `${task.id}: ${route.reasons.join(", ")}`);
    }
  }
});

test("escalation climbs one tier at a time and stops at the top", () => {
  const small = routeAsk({ text: "slower", source: piece });
  const normal = escalate(small), hard = escalate(normal);
  assert.deepEqual([normal.tier, hard.tier], ["normal", "hard"]);
  assert.equal(escalate(hard), hard);
});

test("askParts reads text and counts images in block content", () => {
  assert.deepEqual(askParts([{ type: "text", text: "like this" }, { type: "image", source: {} }]), { text: "like this", images: 1 });
  assert.deepEqual(askParts("hi"), { text: "hi", images: 0 });
});

// The loop, end to end: an "auto" engine names the routed model on the wire,
// says so in a notification, and hands the turn up after failing revisions.
test("auto routes the request and escalates on failing checks", async (t) => {
  const { mkdtemp, writeFile, rm, mkdir } = await import("node:fs/promises");
  const { tmpdir } = await import("node:os");
  const dir = await mkdtemp(join(tmpdir(), "ac-route-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const file = join(dir, "piece.mjs");
  await writeFile(file, piece);

  const { RuntimeFeedback } = await import("../src/runtime-feedback.mjs");
  const { createHash } = await import("node:crypto");
  const feedback = new RuntimeFeedback(dir);
  const fail = () => {
    const revision = createHash("sha256").update(readFileSync(file)).digest("hex");
    feedback.select({ channel: "test", revision, version: 1, piece: "piece.mjs" });
    feedback.log({ level: "error", text: "ReferenceError: circle is not defined", source: "ac check" });
  };

  let n = 0;
  const write = () => [
    { type: "content_block_start", index: 0, content_block: { type: "tool_use", id: `w${++n}`, name: "write_piece" } },
    { type: "content_block_delta", index: 0, delta: { type: "input_json_delta", partial_json: JSON.stringify({ source: `export function paint({wipe}){wipe(${n})}\n` }) } },
    { type: "content_block_stop", index: 0 },
    { type: "message_delta", delta: { stop_reason: "tool_use" } },
  ];
  const done = [{ type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "ok" } }, { type: "message_delta", delta: { stop_reason: "end_turn" } }];
  const scripts = [write(), write(), write(), done];

  const models = [], routed = [];
  const engine = new AcServer({
    cwd: dir, model: "auto", jev: null, frameCapture: false,
    piece: { file, channel: "test", checkpoint: async () => fail() },
    token: async () => "fixture",
    fetch: async (_url, options) => {
      models.push(JSON.parse(options.body).model);
      const events = scripts[Math.min(models.length - 1, scripts.length - 1)];
      const lines = events.map((e) => `data: ${JSON.stringify(e)}\n`);
      let i = 0;
      return { ok: true, body: { getReader: () => ({ read: async () => (i < lines.length ? { done: false, value: new TextEncoder().encode(lines[i++]) } : { done: true }) }) } };
    },
  });
  engine.on("notification", ({ method, params }) => { if (method === "model/routed") routed.push(params); });
  await mkdir(join(dir, ".aesel"), { recursive: true }).catch(() => {});
  await engine.startTurn("slower");

  assert.equal(models[0], HOSTED_MODELS.qwen, "a small edit starts on the small model");
  assert.equal(routed[0].tier, "small");
  assert.ok(routed.some((r) => r.escalated && r.tier === "normal"), `escalated after ${ESCALATE_AFTER} failing revisions`);
  assert.ok(models.includes(HOSTED_MODELS.flash));
});
