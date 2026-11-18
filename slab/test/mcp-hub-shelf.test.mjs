import test, { after } from "node:test";
import assert from "node:assert/strict";
import { SHARED, SHELF_TOOLS, rest, shelf, shelved } from "../bin/mcp-hub.mjs";

after(rest);

const call = async (name, args = {}) => (await shelf({ jsonrpc: "2.0", id: 1, method: "tools/call", params: { name, arguments: args } })).result;

test("a shelved server is not also served by the hub, and says how to come back", () => {
  for (const [name, entry] of Object.entries(shelved())) {
    assert.ok(!SHARED.includes(name), `${name} is on the shelf and in hub-servers.json`);
    assert.ok(entry.about && entry.why && entry.restore, `${name} lacks about, why or restore`);
    assert.ok(entry.command || entry.url, `${name} has neither a command nor a url`);
  }
});

test("the shelf answers as an MCP server with three tools", async () => {
  const hello = await shelf({ jsonrpc: "2.0", id: 1, method: "initialize", params: {} });
  assert.equal(hello.result.serverInfo.name, "shelf");
  const listed = await shelf({ jsonrpc: "2.0", id: 2, method: "tools/list" });
  assert.deepEqual(listed.result.tools.map((t) => t.name), SHELF_TOOLS.map((t) => t.name));
  assert.equal(await shelf({ jsonrpc: "2.0", method: "notifications/initialized" }), null);
});

test("shelf_list names every shelved server and whether it can be called here", async () => {
  const list = JSON.parse((await call("shelf_list")).content[0].text);
  assert.deepEqual(Object.keys(list), Object.keys(shelved()));
  assert.equal(list.dmgify.callable, true);
  assert.equal(list.asana.callable, false);
});

test("a remote or unknown server is refused with the way back, not a crash", async () => {
  const remote = await call("shelf_tools", { server: "asana" });
  assert.equal(remote.isError, true);
  assert.match(remote.content[0].text, /claude mcp add/);
  assert.equal((await call("shelf_call", { server: "nope", tool: "x" })).isError, true);
});

test("a shelved stdio server starts on demand and lists its tools", async () => {
  const tools = JSON.parse((await call("shelf_tools", { server: "dmgify" })).content[0].text);
  assert.ok(tools.some((t) => t.name === "dmgify_plan"));
});
