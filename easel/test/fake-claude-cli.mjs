// A stand-in for `claude` in headless stream-json mode: enough of the
// protocol to drive one turn that writes a file and asks to do it.
import { appendFileSync, writeFileSync } from "node:fs";
import { createInterface } from "node:readline";

const argv = process.argv.slice(2);
// One line per spawn, appended: a second launch must show up rather than
// overwrite the first.
if (process.env.FAKE_CLAUDE_ARGV) {
  appendFileSync(
    process.env.FAKE_CLAUDE_ARGV,
    `${JSON.stringify({ pid: process.pid, ppid: process.ppid, at: Date.now(), argv })}\n`,
  );
}

// A missing flag has no value. `argv.indexOf(name) + 1` reads argv[0] when the
// flag is absent, which quietly hands back "--print" instead of nothing.
function flag(name) {
  const index = argv.indexOf(name);
  return index < 0 ? "" : argv[index + 1] || "";
}

function send(message) {
  process.stdout.write(`${JSON.stringify(message)}\n`);
}

const file = "/tmp/piece.mjs";
const toolUseId = "toolu_1";
// The real CLI honours --session-id and keeps a resumed id, and reports it
// back on every turn's init.
const sessionId = flag("--session-id") || flag("--resume") || "session-1";

createInterface({ input: process.stdin }).on("line", (line) => {
  const message = JSON.parse(line);

  if (message.type === "control_request" && message.request?.subtype === "initialize") {
    send({
      type: "control_response",
      response: { subtype: "success", request_id: message.request_id, response: { commands: [] } },
    });
    send({
      type: "system",
      subtype: "init",
      session_id: sessionId,
      model: "claude-fable-5-1",
      cwd: process.cwd(),
    });
    return;
  }

  if (message.type === "control_request" && message.request?.subtype === "interrupt") {
    send({
      type: "control_response",
      response: { subtype: "success", request_id: message.request_id, response: {} },
    });
    send({ type: "result", subtype: "error_during_execution", is_error: true, terminal_reason: "aborted_streaming" });
    return;
  }

  // A turn the API refuses outright: a usage cap or a rate limit arrives as a
  // synthetic assistant message with no stream behind it, then an aborted
  // result. This is the shape two parallel sessions hit.
  if (message.type === "user" && process.env.FAKE_CLAUDE_API_ERROR) {
    send({
      type: "assistant",
      is_api_error_message: true,
      message: { id: "msg_err", content: [{ type: "text", text: process.env.FAKE_CLAUDE_API_ERROR }] },
    });
    send({ type: "result", subtype: "error_during_execution", terminal_reason: "aborted_by_api" });
    return;
  }

  if (message.type === "user") {
    send({ type: "stream_event", event: { type: "message_start", message: { id: "msg_1" } } });
    send({ type: "stream_event", event: { type: "content_block_start", index: 0, content_block: { type: "text", text: "" } } });
    for (const delta of ["Writing ", "the piece."]) {
      send({ type: "stream_event", event: { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: delta } } });
    }
    send({
      type: "assistant",
      message: {
        id: "msg_1",
        content: [{ type: "tool_use", id: toolUseId, name: "Write", input: { file_path: file, content: "// hi\n" } }],
      },
    });
    send({
      type: "control_request",
      request_id: "perm-1",
      request: {
        subtype: "can_use_tool",
        tool_name: "Write",
        input: { file_path: file, content: "// hi\n" },
        description: "piece.mjs",
        permission_suggestions: [
          { type: "addRules", rules: [{ toolName: "Write" }], behavior: "allow", destination: "localSettings" },
        ],
        tool_use_id: toolUseId,
      },
    });
    return;
  }

  if (message.type === "control_response" && message.response?.request_id === "perm-1") {
    if (process.env.FAKE_CLAUDE_DECISION) {
      writeFileSync(process.env.FAKE_CLAUDE_DECISION, JSON.stringify(message.response.response));
    }
    const allowed = message.response.response?.behavior === "allow";
    send({
      type: "user",
      message: {
        role: "user",
        content: [
          {
            type: "tool_result",
            tool_use_id: toolUseId,
            content: allowed ? `File created at ${file}` : "Denied.",
            is_error: !allowed,
          },
        ],
      },
    });
    send({ type: "result", subtype: "success", is_error: false, terminal_reason: "completed", result: "done" });
  }
});
