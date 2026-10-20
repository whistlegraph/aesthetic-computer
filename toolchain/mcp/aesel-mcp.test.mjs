import test from "node:test";
import assert from "node:assert/strict";
import { deviceCommand, handleMessage, since } from "./aesel-mcp.mjs";

test("the device command splices only an IPv4 address and fixed commands", () => {
  for (const ip of ["ac11.local", "1.2.3", "1.2.3.4; reboot", "$(id)", "256.1.1.1"]) assert.throws(() => deviceCommand(ip, "cat /tmp/aesel-state.json"));
  for (const cmd of ["cat /tmp/x; reboot", "cat $(id)", "rm -rf /'", "echo `id`"]) assert.throws(() => deviceCommand("192.168.1.251", cmd));
  assert.match(deviceCommand("192.168.1.251", "cat > /tmp/aesel-say"), /root@192\.168\.1\.251 'cat > \/tmp\/aesel-say'$/);
});

test("state can be read from an entry id on", () => {
  const s = since({ status: "ready", entries: [{ id: 1, kind: "user", text: "a" }, { id: 2, kind: "agent", text: "b" }] }, 1);
  assert.equal(s.lastId, 2);
  assert.deepEqual(s.entries.map((e) => e.id), [2]);
});

test("tools are listed and unknown ones refused", async () => {
  const list = await handleMessage({ id: 1, method: "tools/list" });
  assert.deepEqual(list.result.tools.map((t) => t.name), ["aesel_devices", "aesel_state", "aesel_say", "aesel_wait", "aesel_piece"]);
  const bad = await handleMessage({ id: 2, method: "tools/call", params: { name: "reboot" } });
  assert.equal(bad.result.isError, true);
  assert.equal(await handleMessage({ method: "notifications/initialized" }), null);
});
