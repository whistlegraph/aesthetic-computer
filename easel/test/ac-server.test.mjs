import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { AcServer } from "../src/ac-server.mjs";

// A stand-in for the endpoint: hands back whatever SSE the test wants, so the
// loop can be driven through paths a live server would be slow or costly to
// reproduce.
function serving(...turns) {
  let call = 0;
  return async () => {
    const events = turns[Math.min(call++, turns.length - 1)];
    return {
      ok: true,
      body: {
        getReader() {
          const lines = events.map((e) => `data: ${JSON.stringify(e)}\n`);
          let i = 0;
          return {
            read: async () =>
              i < lines.length
                ? { done: false, value: new TextEncoder().encode(lines[i++]) }
                : { done: true },
          };
        },
      },
    };
  };
}

const say = (text) => [
  { type: "content_block_delta", index: 0, delta: { type: "text_delta", text } },
  { type: "message_delta", delta: { stop_reason: "end_turn" } },
];

const writes = (source) => [
  { type: "content_block_start", index: 0, content_block: { type: "tool_use", id: "t1", name: "write_piece" } },
  {
    type: "content_block_delta",
    index: 0,
    delta: { type: "input_json_delta", partial_json: JSON.stringify({ source, note: "first draft" }) },
  },
  { type: "content_block_stop", index: 0 },
  { type: "message_delta", delta: { stop_reason: "tool_use" } },
];

test("streamed text reaches the interface as deltas and one completed message", async () => {
  const engine = new AcServer({ fetch: serving(say("hello there")), token: async () => "tok" });
  const deltas = [];
  let completed = null;
  engine.on("notification", ({ method, params }) => {
    if (method === "item/agentMessage/delta") deltas.push(params.delta);
    if (method === "item/completed") completed = params.item;
  });
  await engine.connect();
  await engine.startTurn("hi");
  assert.deepEqual(deltas, ["hello there"]);
  assert.equal(completed.text, "hello there");
});

test("a tool call writes the piece and the loop continues to a real answer", async () => {
  const dir = await mkdtemp(join(tmpdir(), "ac-server-"));
  const file = join(dir, "murafi.mjs");
  await writeFile(file, "// blank\n");

  const engine = new AcServer({
    fetch: serving(writes("// painted\nfunction paint({ wipe }) { wipe(0); }"), say("done")),
    token: async () => "tok",
    piece: { file },
  });
  await engine.connect();
  await engine.startTurn("paint it black");

  const written = await readFile(file, "utf8");
  assert.match(written, /function paint/, "the tool wrote the piece");
  assert.ok(written.endsWith("\n"), "a trailing newline is added when missing");
  await rm(dir, { recursive: true, force: true });
});

// The loop must end. A model that keeps calling tools is a model spending
// someone's daily allowance on a loop, and the bound is what stops that.
test("an endless tool caller is cut off rather than run forever", async () => {
  const engine = new AcServer({
    fetch: serving(writes("// again")),
    token: async () => "tok",
    piece: { file: join(tmpdir(), `loop-${Date.now()}.mjs`) },
  });
  let turn = null;
  engine.on("notification", ({ method, params }) => {
    if (method === "turn/completed") turn = params.turn;
  });
  await engine.connect();
  await engine.startTurn("loop");
  assert.equal(turn.status, "failed");
  assert.match(turn.error.message, /12 tool rounds/);
});

test("no handle means a clear refusal, not a confusing failure", async () => {
  const engine = new AcServer({ fetch: serving(say("x")), token: async () => null });
  let turn = null;
  engine.on("notification", ({ method, params }) => {
    if (method === "turn/completed") turn = params.turn;
  });
  await engine.connect();
  await engine.startTurn("hi");
  assert.equal(turn.status, "failed");
  assert.match(turn.error.message, /\/login/);
});

test("the guides travel in the prompt, because this bridge has no file tools", async () => {
  const engine = new AcServer({ developerInstructions: "INSTRUCTIONS" });
  let sent = null;
  engine.fetch = async (_url, options) => {
    sent = JSON.parse(options.body);
    return serving(say("ok"))();
  };
  engine.token = async () => "tok";
  await engine.connect();
  await engine.startTurn("hi");
  assert.match(sent.system, /INSTRUCTIONS/);
  assert.match(sent.system, /pieces\.md/, "the piece guide is inlined, not named");
  assert.ok(sent.system.length > 5000, "the bundle actually travels");
  assert.equal(sent.tools[0].name, "write_piece");
});
