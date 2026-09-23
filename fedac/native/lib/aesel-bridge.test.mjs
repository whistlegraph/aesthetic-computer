// node --test fedac/native/lib/aesel-bridge.test.mjs
//
// Drives the bridge with a scripted transcript of Claude's stream-json
// protocol and checks what the piece would be told and what the child would
// be sent. No runtime, no process: the same file the device loads.
import { test } from "node:test";
import assert from "node:assert/strict";

import { Bridge, blankPiece, instructionsFor, launchArguments, randomSlug, uuid } from "./aesel-bridge.mjs";

function make() {
  const sent = [];
  const bridge = new Bridge({
    send: (line) => sent.push(JSON.parse(line)),
    cwd: "/pieces",
    instructions: "be brief",
    uuid: () => "00000000-0000-4000-8000-000000000000",
  });
  return { bridge, sent };
}

test("launch names the session, withholds the tools, and points at the workspace", () => {
  const { bridge } = make();
  const args = bridge.launch();
  assert.equal(args[args.indexOf("--session-id") + 1], "00000000-0000-4000-8000-000000000000");
  assert.equal(args[args.indexOf("--permission-prompt-tool") + 1], "stdio");
  assert.equal(args[args.indexOf("--setting-sources") + 1], "");
  assert.deepEqual(args.slice(args.indexOf("--disallowed-tools") + 1, args.indexOf("--add-dir")), ["WebFetch", "WebSearch", "Task"]);
  assert.equal(args[args.indexOf("--add-dir") + 1], "/pieces");
  assert.equal(args[args.indexOf("--append-system-prompt") + 1], "be brief");
  assert.ok(!args.includes("--resume"));
  const resumed = launchArguments({ cwd: "/pieces", sessionId: "x", resume: "abc" });
  assert.equal(resumed[resumed.indexOf("--resume") + 1], "abc");
  assert.ok(!resumed.includes("--session-id"));
});

test("the handshake answer makes the bridge ready", () => {
  const { bridge, sent } = make();
  bridge.launch();
  bridge.handshake();
  assert.equal(sent[0].type, "control_request");
  assert.equal(sent[0].request.subtype, "initialize");
  assert.equal(bridge.ready, false);
  bridge.feed(JSON.stringify({ type: "control_response", response: { subtype: "success", request_id: sent[0].request_id } }));
  assert.equal(bridge.ready, true);
  assert.deepEqual(bridge.take().map((e) => e.type), ["ready"]);
});

test("a turn streams text, reports tools, and completes", () => {
  const { bridge, sent } = make();
  bridge.launch();
  bridge.say("make it blue");
  assert.equal(sent[0].type, "user");
  assert.equal(sent[0].message.content[0].text, "make it blue");
  assert.equal(bridge.busy, true);

  const feed = (m) => bridge.feed(JSON.stringify(m));
  feed({ type: "system", subtype: "init", model: "claude-opus-5", session_id: "s1" });
  feed({ type: "stream_event", event: { type: "message_start", message: { id: "m1" } } });
  feed({ type: "stream_event", event: { type: "content_block_start", index: 0, content_block: { type: "text" } } });
  feed({ type: "stream_event", event: { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "Sure" } } });
  feed({ type: "stream_event", event: { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: ", blue." } } });
  feed({ type: "assistant", message: { id: "m1", content: [
    { type: "text", text: "Sure, blue." },
    { type: "tool_use", id: "t1", name: "Edit", input: { file_path: "/pieces/lumo.mjs" } },
  ] } });
  feed({ type: "user", message: { content: [{ type: "tool_result", tool_use_id: "t1" }] } });
  feed({ type: "result", is_error: false });

  const events = bridge.take();
  assert.deepEqual(events.map((e) => e.type), ["turn", "delta", "delta", "tool", "tool", "turn"]);
  assert.equal(events[1].text + events[2].text, "Sure, blue.");
  assert.equal(events[3].kind, "file");
  assert.equal(events[3].path, "/pieces/lumo.mjs");
  assert.equal(events[4].status, "completed");
  assert.equal(events[5].status, "completed");
  assert.equal(bridge.busy, false);
  assert.equal(bridge.sessionId, "s1");
});

test("an approval is asked once and answered with the caller's decision", () => {
  const { bridge, sent } = make();
  bridge.launch();
  bridge.feed(JSON.stringify({ type: "control_request", request_id: "r1", request: {
    subtype: "can_use_tool", tool_name: "Bash", input: { command: "ls /pieces" },
    permission_suggestions: [{ type: "addRules", rules: [{ toolName: "Bash", ruleContent: "ls:*" }], behavior: "allow" }],
  } }));
  const [approval] = bridge.take();
  assert.equal(approval.type, "approval");
  assert.equal(approval.kind, "command");
  assert.equal(approval.label, "ls /pieces");
  assert.equal(bridge.pendingApproval.id, "r1");

  bridge.approve("r1", "acceptForSession");
  const reply = sent.at(-1);
  assert.equal(reply.type, "control_response");
  assert.equal(reply.response.request_id, "r1");
  assert.equal(reply.response.response.behavior, "allow");
  assert.deepEqual(reply.response.response.updatedInput, { command: "ls /pieces" });
  assert.equal(reply.response.response.updatedPermissions[0].destination, "session");
  assert.equal(bridge.pendingApproval, null);
  assert.equal(bridge.approve("r1", "accept"), false);

  bridge.feed(JSON.stringify({ type: "control_request", request_id: "r2", request: {
    subtype: "can_use_tool", tool_name: "Write", input: { file_path: "/pieces/lumo.mjs", content: "x" },
  } }));
  const [resolved, file] = bridge.take();
  assert.equal(resolved.type, "approval-resolved");
  assert.equal(file.kind, "file");
  assert.equal(file.label, "/pieces/lumo.mjs");
  bridge.approve("r2", "decline");
  assert.equal(sent.at(-1).response.response.behavior, "deny");
});

test("chatter ahead of a message is logged, chatter alone is logged, bad JSON is an error", () => {
  const { bridge } = make();
  bridge.launch();
  bridge.feed("\u001b[2K⠋ thinking{\"type\":\"result\",\"is_error\":false}");
  bridge.feed("Connector notice: nothing to see");
  bridge.feed("{not json");
  bridge.feed("   ");
  const events = bridge.take();
  assert.deepEqual(events.map((e) => e.type), ["log", "turn", "log", "error"]);
  assert.match(events[3].message, /invalid engine message/);
});

test("an api error message and an aborted result read as error and interruption", () => {
  const { bridge } = make();
  bridge.launch();
  bridge.say("go");
  bridge.take();
  bridge.feed(JSON.stringify({ type: "assistant", is_api_error_message: true, message: { content: [{ type: "text", text: "out of credits" }] } }));
  bridge.feed(JSON.stringify({ type: "result", terminal_reason: "aborted_by_user" }));
  const events = bridge.take();
  assert.equal(events[0].type, "error");
  assert.equal(events[0].message, "out of credits");
  assert.equal(events[1].status, "interrupted");
  assert.equal(bridge.interrupt(), false);
});

test("helpers: slug, uuid, blank and instructions", () => {
  let n = 0;
  const seq = [0.1, 0.3, 0.6, 0.9, 0.2, 0.5, 0.7];
  const slug = randomSlug(() => seq[n++ % seq.length]);
  assert.match(slug, /^([bdfgklmnprstvz][aeiou]){2,3}$/);
  const id = uuid(() => 0.5);
  assert.match(id, /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/);
  const blank = blankPiece("lumo", "2026-09-23");
  assert.match(blank, /^\/\/ lumo, 2026-09-23\n/);
  assert.match(blank, /export \{ paint \};/);
  const text = instructionsFor({ handle: "jeffrey", slug: "lumo", file: "/pieces/lumo.mjs" });
  assert.match(text, /@jeffrey/);
  assert.match(text, /\/pieces\/lumo\.mjs/);
  assert.match(text, /no browser/);
});
