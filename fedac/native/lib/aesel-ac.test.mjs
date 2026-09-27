// node --test fedac/native/lib/aesel-ac.test.mjs
//
// Drives the hosted bridge with a fake transport that hands the stream out
// in pieces, the way a file re-read once a frame does.
import { test } from "node:test";
import assert from "node:assert/strict";

import { AcBridge, acModelLabel, errorMessage, parseSse, resolveAcModel } from "./aesel-ac.mjs";

function sse(events) {
  return events.map((e) => `data: ${JSON.stringify(e)}\n`).join("");
}

// A transport whose stream the test feeds by hand.
function fakeTransport() {
  const t = {
    started: [],
    text: "",
    done: false,
    error: "",
    cancelled: 0,
    accept: true,
    start(body, headers) {
      t.started.push({ body: JSON.parse(body), headers });
      t.text = "";
      t.done = false;
      t.error = "";
      return t.accept;
    },
    poll() {
      return { text: t.text, done: t.done, error: t.error };
    },
    cancel() {
      t.cancelled += 1;
    },
  };
  return t;
}

function make({ token = "tok", piece = "// blank\n" } = {}) {
  const transport = fakeTransport();
  const writes = [];
  const bridge = new AcBridge({
    transport,
    token,
    model: "glm",
    instructions: "native rules",
    file: "/pieces/lumo.mjs",
    readPiece: () => piece,
    writePiece: (source) => { writes.push(source); return true; },
    uuid: () => "s-1",
  });
  return { bridge, transport, writes };
}

test("parseSse takes whole data lines only and leaves a torn tail for later", () => {
  const text = 'data: {"a":1}\n: comment\ndata: [DONE]\ndata: {"b":2}\ndata: {"c":';
  const first = parseSse(text, 0);
  assert.deepEqual(first.events, [{ a: 1 }, { b: 2 }]);
  const second = parseSse(text + "3}\n", first.offset);
  assert.deepEqual(second.events, [{ c: 3 }]);
});

test("models resolve by alias and label back", () => {
  assert.equal(resolveAcModel("glm"), "z-ai/glm-4.6");
  assert.equal(resolveAcModel(""), "z-ai/glm-4.6");
  assert.equal(resolveAcModel("some/other"), "some/other");
  assert.equal(acModelLabel("qwen/qwen3-coder"), "qwen");
  assert.equal(acModelLabel("x/y-1"), "y-1");
  assert.equal(errorMessage('{"error":{"message":"used today\'s allowance"}}', "fallback"), "used today's allowance");
  assert.equal(errorMessage("<html>", "fallback"), "fallback");
});

test("no token: the handshake fails with a pointer to link", () => {
  const { bridge } = make({ token: "" });
  bridge.launch();
  bridge.handshake();
  const [event] = bridge.take();
  assert.equal(event.type, "error");
  assert.equal(event.fatal, true);
  assert.match(event.message, /link/);
  assert.equal(bridge.ready, false);
});

test("a turn streams text, writes the piece through the tool, and goes round again", () => {
  const { bridge, transport, writes } = make();
  bridge.launch();
  bridge.handshake();
  assert.equal(bridge.take()[0].type, "ready");

  bridge.say("make it blue");
  assert.equal(transport.started.length, 1);
  const body = transport.started[0].body;
  assert.equal(body.model, "z-ai/glm-4.6");
  assert.equal(body.tools[0].name, "write_piece");
  assert.equal(body.system[0].text, "native rules");
  assert.match(body.system[1].text, /Current source of \/pieces\/lumo\.mjs/);
  assert.deepEqual(body.messages, [{ role: "user", content: "make it blue" }]);
  assert.equal(transport.started[0].headers.Authorization, "Bearer tok");

  // Frame 1: half the stream, a torn line at the end.
  const full = sse([
    { type: "content_block_start", index: 0, content_block: { type: "text" } },
    { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "Blue " } },
    { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "it is." } },
    { type: "content_block_start", index: 1, content_block: { type: "tool_use", id: "t1", name: "write_piece" } },
    { type: "content_block_delta", index: 1, delta: { type: "input_json_delta", partial_json: '{"source":"function paint({ wipe }) { wipe(0,0,255); }' } },
    { type: "content_block_delta", index: 1, delta: { type: "input_json_delta", partial_json: '\\nexport { paint };","note":"blue"}' } },
    { type: "content_block_stop", index: 1 },
    { type: "message_delta", delta: { stop_reason: "tool_use" } },
  ]);
  // Cut just after the first delta's line, so frame one carries exactly one delta.
  transport.text = full.slice(0, full.indexOf("\n", full.indexOf("Blue ")) + 1);
  bridge.tick();
  let events = bridge.take();
  assert.deepEqual(events.map((e) => e.type), ["turn", "delta"]);
  assert.equal(events[1].text, "Blue ");

  // Frame 2: the rest, and the request ends.
  transport.text = full;
  transport.done = true;
  bridge.tick();
  events = bridge.take();
  assert.deepEqual(events.map((e) => e.type), ["delta", "tool", "tool"]);
  assert.equal(events[1].status, "started");
  assert.equal(events[2].status, "completed");
  assert.equal(events[2].path, "/pieces/lumo.mjs");
  assert.equal(events[2].label, "/pieces/lumo.mjs · blue");
  assert.equal(writes.length, 1);
  assert.match(writes[0], /wipe\(0,0,255\)/);
  assert.ok(writes[0].endsWith("\n"));

  // The second round carries the assistant blocks and the tool result.
  assert.equal(transport.started.length, 2);
  const again = transport.started[1].body.messages;
  assert.equal(again.length, 3);
  assert.equal(again[1].role, "assistant");
  assert.equal(again[1].content[0].text, "Blue it is.");
  assert.equal(again[1].content[1].type, "tool_use");
  assert.equal(again[2].content[0].type, "tool_result");
  assert.equal(again[2].content[0].tool_use_id, "t1");
  assert.equal(bridge.busy, true);

  transport.text = sse([
    { type: "content_block_start", index: 0, content_block: { type: "text" } },
    { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "Done." } },
    { type: "message_delta", delta: { stop_reason: "end_turn" } },
  ]);
  transport.done = true;
  bridge.tick();
  events = bridge.take();
  assert.deepEqual(events.map((e) => e.type), ["delta", "turn"]);
  assert.equal(events[1].status, "completed");
  assert.equal(bridge.busy, false);
  assert.equal(bridge.messages.length, 4);
});

test("an HTTP refusal ends the turn with the server's message", () => {
  const { bridge, transport } = make();
  bridge.launch();
  bridge.handshake();
  bridge.take();
  bridge.say("go");
  transport.text = '{"error":{"message":"@jeffrey has used today\'s allowance"}}';
  transport.error = "request failed (22): curl: (22) The requested URL returned error: 429";
  transport.done = true;
  bridge.tick();
  const events = bridge.take();
  assert.equal(events.at(-1).type, "turn");
  assert.equal(events.at(-1).status, "failed");
  assert.match(events.at(-1).error, /allowance/);
  assert.equal(bridge.busy, false);
});

test("a busy device, an unknown tool, an interrupt", () => {
  const { bridge, transport, writes } = make();
  bridge.launch();
  bridge.handshake();
  bridge.take();

  transport.accept = false;
  bridge.say("go");
  let events = bridge.take();
  assert.equal(events.at(-1).status, "failed");
  assert.match(events.at(-1).error, /in flight/);
  transport.accept = true;

  bridge.say("again");
  bridge.take();
  transport.text = sse([
    { type: "content_block_start", index: 0, content_block: { type: "tool_use", id: "t9", name: "read_file" } },
    { type: "content_block_delta", index: 0, delta: { type: "input_json_delta", partial_json: "{}" } },
    { type: "content_block_stop", index: 0 },
    { type: "message_delta", delta: { stop_reason: "tool_use" } },
  ]);
  transport.done = true;
  bridge.tick();
  events = bridge.take();
  assert.equal(events[0].type, "tool");
  assert.equal(events[0].status, "failed");
  assert.equal(writes.length, 0);
  assert.equal(transport.started.at(-1).body.messages.at(-1).content[0].is_error, true);

  assert.equal(bridge.interrupt(), true);
  assert.equal(transport.cancelled, 1);
  assert.equal(bridge.take().at(-1).status, "interrupted");
  assert.equal(bridge.busy, false);
  assert.equal(bridge.interrupt(), false);
});
