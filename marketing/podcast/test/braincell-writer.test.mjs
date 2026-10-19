import { test } from "node:test";
import assert from "node:assert/strict";
import { braincellText, DAILY_BRAIN_MODEL } from "../lib/braincell-writer.mjs";

const session = async () => ({ access_token: "test-only" });
const sse = (events) => new Response(events.map((e) => `data: ${JSON.stringify(e)}\n\n`).join(""), { status: 200 });
const credits = (over = {}) => new Response(JSON.stringify({ handle: "@jeffrey", remaining: 40, purchased: 0, ...over }), { status: 200 });
const script = [
  { type: "message_start", message: { usage: { input_tokens: 10 } } },
  { type: "content_block_start", index: 0, content_block: { type: "text", text: "---\n" } },
  { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "title: a quiet night\n---\n\n" } },
  { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "The oven hummed." } },
  { type: "message_delta", delta: { stop_reason: "end_turn" }, usage: { output_tokens: 20 } },
  { type: "message_stop" },
];

function fetcher({ credit = credits(), stream = sse(script), calls = [] } = {}) {
  return async (url, init) => {
    calls.push({ url, init });
    if (url.endsWith("/easel-credits")) return credit;
    if (url.endsWith("/easel-inference")) return stream;
    throw new Error(`unexpected ${url}`);
  };
}

test("writes the script through the daily's own braincells on Sonnet", async () => {
  const calls = [];
  const text = await braincellText("write it", { file: "/x", handle: "@jeffrey", session, fetch: fetcher({ calls }) });
  assert.equal(text, "---\ntitle: a quiet night\n---\n\nThe oven hummed.");
  const inference = calls.find((c) => c.url.endsWith("/easel-inference"));
  const body = JSON.parse(inference.init.body);
  assert.equal(body.model, DAILY_BRAIN_MODEL);
  assert.deepEqual(body.thinking, { type: "disabled" });
  assert.equal(inference.init.headers.Authorization, "Bearer test-only");
  assert.equal(body.messages[0].content, "write it");
});

test("a chosen model rides along", async () => {
  const calls = [];
  await braincellText("write it", { file: "/x", handle: "jeffrey", model: "anthropic/claude-opus-5.5", session, fetch: fetcher({ calls }) });
  assert.equal(JSON.parse(calls[1].init.body).model, "anthropic/claude-opus-5.5");
});

test("refuses to bill a different handle than the one configured", async () => {
  await assert.rejects(
    braincellText("write it", { file: "/x", handle: "jeffrey", session, fetch: fetcher({ credit: credits({ handle: "@someone" }) }) }),
    /Daily account mismatch/,
  );
});

test("refuses when the account has no braincells", async () => {
  await assert.rejects(
    braincellText("write it", { file: "/x", handle: "jeffrey", session, fetch: fetcher({ credit: credits({ remaining: 0, purchased: 0 }) }) }),
    /no braincells/,
  );
});

test("an incomplete stream saves nothing", async () => {
  const cut = sse(script.slice(0, 3));
  await assert.rejects(braincellText("write it", { file: "/x", handle: "jeffrey", session, fetch: fetcher({ stream: cut }) }), /incomplete script/);
});

test("a tool request is refused — the writer has no tools", async () => {
  const tool = sse([{ type: "content_block_start", index: 0, content_block: { type: "tool_use", name: "write_file" } }]);
  await assert.rejects(braincellText("write it", { file: "/x", handle: "jeffrey", session, fetch: fetcher({ stream: tool }) }), /requested a tool/);
});

test("missing configuration is named", async () => {
  await assert.rejects(braincellText("write it", { session, fetch: fetcher() }), /DAILY_BRAIN_TOKEN_FILE and DAILY_BRAIN_HANDLE/);
});
