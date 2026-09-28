// The hosted provider as a braincell-paid twin of the OpenRouter bridge: the
// same open models in the picker, the same pro loop, and plain words when the
// relay refuses for braincells. Plus the honesty rules for a piece session
// with no preview running.
import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { AcServer } from "../src/ac-server.mjs";
import { BACKENDS, hostedModel } from "../src/backends.mjs";
import { PIECE_INSTRUCTIONS } from "../src/harness-contract.mjs";
import { McpTools } from "../src/mcp-client.mjs";
import { OPEN_MODEL_INFO } from "../src/open-models.mjs";
import { normalizeSettings, pickerModels } from "../src/provider-picker.mjs";

const sse = (events) => new Response(events.map((e) => `data: ${JSON.stringify(e)}\n\n`).join(""));
const say = (text) => [
  { type: "content_block_delta", index: 0, delta: { type: "text_delta", text } },
  { type: "message_delta", delta: { stop_reason: "end_turn" } },
];
const writes = (source) => [
  { type: "content_block_start", index: 0, content_block: { type: "tool_use", id: "w1", name: "write_piece" } },
  { type: "content_block_delta", index: 0, delta: { type: "input_json_delta", partial_json: JSON.stringify({ source }) } },
  { type: "content_block_stop", index: 0 },
  { type: "message_delta", delta: { stop_reason: "tool_use" } },
];
async function tempPiece(t) {
  const dir = await mkdtemp(join(tmpdir(), "hosted-braincells-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const file = join(dir, "piece.mjs");
  await writeFile(file, "// blank\n");
  return { dir, file };
}
function completion(engine) {
  let turn = null;
  engine.on("notification", ({ method, params }) => { if (method === "turn/completed") turn = params.turn; });
  return () => turn;
}

test("the aesthetic picker offers Automatic and every open model, marked, and nothing custom", () => {
  for (const model of ["", "opus", "custom", "deepseek/deepseek-v4.1-flash"]) {
    const rows = pickerModels({ backend: "ac", model, row: 1 });
    assert.deepEqual(rows[0], { id: "", label: "Automatic", detail: "aesthetic.computer default" });
    assert.deepEqual(rows.slice(1).map((r) => r.id), Object.values(OPEN_MODEL_INFO).map((m) => m.id));
    assert.equal(rows.find((r) => r.id === "moonshotai/kimi-k3").detail, "●●●●○  $$$$");
  }
  assert.equal(BACKENDS.ac.defaultModel, "", "hosted opens on Automatic");
});

test("settings may choose a model for the aesthetic provider but never its effort", () => {
  const previous = { provider: "claude", model: "opus", effort: "high", autopublish: false };
  assert.equal(normalizeSettings({ provider: "ac" }, previous).model, "");
  assert.equal(normalizeSettings({ provider: "ac", model: "moonshotai/kimi-k3" }, previous).model, "moonshotai/kimi-k3");
  assert.equal(normalizeSettings({ provider: "ac", model: "qwen" }, previous).model, "qwen/qwen3.7-plus");
  assert.throws(() => normalizeSettings({ provider: "ac", model: "anthropic/claude-opus-5" }, previous), /AC hosted runs/);
  assert.throws(() => normalizeSettings({ provider: "ac", effort: "high" }, previous), /effort/);
  // A remembered older hosted model falls back to Automatic rather than sticking.
  assert.equal(normalizeSettings({}, { provider: "ac", model: "openai/gpt-5.6-luna", effort: "" }).model, "");
  assert.equal(hostedModel("flash"), "deepseek/deepseek-v4.1-flash");
});

test("Automatic names no model, so the relay runs its default; a chosen one is sent", async () => {
  for (const [model, expected] of [["", undefined], ["kimi", "moonshotai/kimi-k3"]]) {
    let sent;
    const engine = new BACKENDS.ac.Engine({ model, jev: null, token: async () => "tok", fetch: async (_url, o) => { sent = JSON.parse(o.body); return sse(say("ok")); } });
    await engine.startTurn("hi");
    assert.equal(sent.model, expected);
  }
});

test("pro on the aesthetic provider carries the OpenRouter bridge's workspace loop", async (t) => {
  const { dir } = await tempPiece(t);
  const urls = [];
  let sent;
  const engine = new BACKENDS.ac.Engine({ cwd: dir, pro: true, token: async () => "tok", site: "https://relay.test",
    fetch: async (url, o) => { urls.push(url); sent = JSON.parse(o.body); return sse(say("ok")); } });
  assert.equal(engine.workspace, true);
  assert.equal(engine.rounds, 80);
  assert.ok(engine.extensions instanceof McpTools, "the person's MCP servers load in pro");
  engine.extensions = { tools: async () => [{ name: "mcp__demo__ping", description: "ping", input_schema: { type: "object", properties: {} } }], has: () => false, close() {} };
  await engine.startTurn("make a sheep png");
  const names = sent.tools.map((tool) => tool.name);
  for (const name of ["read_file", "search", "edit_file", "write_file", "bash", "close_session", "mcp__demo__ping"]) assert.ok(names.includes(name), name);
  assert.ok(!names.includes("write_piece"), "a repository session does not rewrite a piece");
  assert.equal(sent.max_tokens, 32000);
  assert.deepEqual(urls, ["https://relay.test/api/easel-inference"], "only messages go to the relay");

  const piece = new BACKENDS.ac.Engine({ cwd: dir, token: async () => "tok" });
  assert.equal(piece.workspace, false);
  assert.equal(piece.rounds, 12);
  assert.equal(piece.extensions, null);
});

test("a braincell refusal reaches the interface whole and flagged", async () => {
  for (const status of [402, 429]) {
    const message = status === 402 ? "Out of braincells — buy more from the braincell meter in Aesel, or switch provider with /provider." : "That is today's limit.";
    const engine = new AcServer({ token: async () => "tok", jev: null, fetch: async () => Response.json({ error: { message } }, { status }) });
    const turn = completion(engine);
    await engine.startTurn("hi");
    assert.equal(turn().status, "failed");
    assert.equal(turn().error.message, message);
    assert.equal(turn().error.billing, true);
  }
  const other = new AcServer({ token: async () => "tok", jev: null, fetch: async () => Response.json({ error: { message: "bad" } }, { status: 400 }) });
  const turn = completion(other);
  await other.startTurn("hi");
  assert.equal(turn().error.billing, undefined);
});

test("with no preview connected, write_piece says only that the file is on disk", async (t) => {
  const { file } = await tempPiece(t);
  const sent = [];
  let call = 0;
  const engine = new AcServer({ piece: { file, channel: "chan" }, preview: () => false, jev: null, token: async () => "tok",
    fetch: async (_u, o) => { sent.push(JSON.parse(o.body)); return sse(call++ === 0 ? writes("export function paint() {}") : say("done")); } });
  await engine.startTurn("draw");
  const result = sent[1].messages.at(-1).content[0];
  assert.equal(result.content, `Saved to ${file}. No preview is connected in this session.`);
  assert.match(await readFile(file, "utf8"), /paint/);
});

test("with no preview connected, ac_preview and ac_frame are not offered and the prompt does not promise them", async (t) => {
  const { file } = await tempPiece(t);
  for (const preview of [true, false]) {
    let sent;
    const engine = new AcServer({ piece: { file, channel: "chan" }, preview, jev: null, token: async () => "tok",
      fetch: async (_u, o) => { sent = JSON.parse(o.body); return sse(say("ok")); } });
    await engine.startTurn("draw");
    const names = sent.tools.map((tool) => tool.name);
    const system = sent.system.map((block) => block.text).join("\n");
    assert.equal(names.includes("ac_preview"), preview);
    assert.equal(names.includes("ac_frame"), preview);
    assert.ok(names.includes("write_piece"));
    assert.equal(/inspect ac_preview runtime feedback/.test(system), preview);
    assert.equal(/save the smallest useful working piece promptly with write_piece/.test(system), preview);
  }
});

test("developer instructions never drop the piece contract from a piece session", async (t) => {
  const { file } = await tempPiece(t);
  for (const developerInstructions of ["Recent conversation: keep the dots purple", `Aesel.\n${PIECE_INSTRUCTIONS}`]) {
    let sent;
    const engine = new AcServer({ piece: { file }, developerInstructions, jev: null, token: async () => "tok",
      fetch: async (_u, o) => { sent = JSON.parse(o.body); return sse(say("ok")); } });
    await engine.startTurn("3+3 = ?");
    const system = sent.system.map((block) => block.text).join("\n");
    assert.equal(system.split(PIECE_INSTRUCTIONS).length - 1, 1, "present exactly once");
  }
});
