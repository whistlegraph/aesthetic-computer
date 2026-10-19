import { test } from "node:test";
import assert from "node:assert/strict";
import { braincellText } from "../lib/braincell-writer.mjs";

const events = [
  { type: "content_block_start", content_block: { type: "text", text: "" } },
  { type: "content_block_delta", delta: { type: "text_delta", text: "A tiny café." } },
  { type: "message_delta", delta: { stop_reason: "end_turn" } },
  { type: "message_stop" },
];
function stream(rows) {
  const bytes = new TextEncoder().encode(rows.map(r => `data: ${JSON.stringify(r)}\r\n\r\n`).join(""));
  return new Response(new ReadableStream({ start(controller) {
    for (let i = 0; i < bytes.length; i += 3) controller.enqueue(bytes.slice(i, i + 3));
    controller.close();
  } }));
}
function setup({ handle = "@radio", remaining = 10, purchased = 0, rows = events, status = 200 } = {}) {
  const calls = [];
  return { calls, options: { file: "/test/radio-token", handle: "radio",
    session: async ({ file }) => { assert.equal(file, "/test/radio-token"); return { access_token: "test-token" }; },
    fetch: async (url, options) => {
      calls.push({ url, options });
      assert.equal(options.headers.Authorization, "Bearer test-token");
      if (url.endsWith("easel-credits")) return Response.json({ handle, remaining, purchased });
      return status === 200 ? stream(rows) : new Response("provider failure", { status });
    },
  } };
}
test("uses the selected account and Aesel's automatic model with bounded output", async () => {
  const { calls, options } = setup();
  assert.equal(await braincellText("Filtered public subjects", options), "A tiny café.");
  const request = JSON.parse(calls[1].options.body);
  assert.equal(request.max_tokens, 2048);
  assert.equal(request.messages[0].content, "Filtered public subjects");
  assert.equal(request.model, undefined);
  assert.equal(request.tools, undefined);
});
test("the operator may explicitly select @jeffrey's existing session", async () => {
  const { options } = setup({ handle: "@jeffrey" });
  options.handle = "@jeffrey";
  assert.equal(await braincellText("prompt", options), "A tiny café.");
});
test("never bills another account or generates on an unverified balance", async () => {
  for (const config of [{ handle: "@jeffrey" }, { remaining: 0 }, { remaining: null }]) {
    const { calls, options } = setup(config);
    await assert.rejects(braincellText("prompt", options));
    assert.equal(calls.length, 1);
  }
  await assert.rejects(braincellText("prompt"), /DAILY_BRAIN_TOKEN_FILE/);
});
test("purchased braincells work after the free allowance is spent", async () => {
  const { options } = setup({ remaining: 0, purchased: 100 });
  assert.equal(await braincellText("prompt", options), "A tiny café.");
});
test("rejects partial, capped and failed streams without retrying paid generation", async () => {
  for (const config of [
    { rows: events.slice(0, 2) },
    { rows: [...events.slice(0, 2), { type: "message_delta", delta: { stop_reason: "max_tokens" } }, events[3]] },
    { rows: [...events.slice(0, 2), { type: "error", error: { message: "test" } }] },
    { status: 402 },
  ]) {
    const { calls, options } = setup(config);
    await assert.rejects(braincellText("prompt", options));
    assert.equal(calls.length, 2);
  }
});
