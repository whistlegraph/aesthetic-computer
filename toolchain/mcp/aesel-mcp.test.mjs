import test from "node:test";
import assert from "node:assert/strict";
import { deviceCommand, handleMessage, since, stickOf } from "./aesel-mcp.mjs";

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
  assert.deepEqual(list.result.tools.map((t) => t.name), ["aesel_devices", "aesel_state", "aesel_say", "aesel_wait", "aesel_piece", "aesel_sticks", "aesel_flash"]);
  const bad = await handleMessage({ id: 2, method: "tools/call", params: { name: "reboot" } });
  assert.equal(bad.result.isError, true);
  assert.equal(await handleMessage({ method: "notifications/initialized" }), null);
});

test("only a whole external USB stick can be flashed", () => {
  const stick = { DeviceIdentifier: "disk6", Internal: false, WholeDisk: true, BusProtocol: "USB", TotalSize: 123009761280, MediaName: "SanDisk 3.2Gen1" };
  assert.deepEqual(stickOf(stick, ["ACBOOT", null, "ACEFI"]), { disk: "disk6", name: "SanDisk 3.2Gen1", bytes: 123009761280, ac: true });
  assert.equal(stickOf(stick).ac, false);
  // frisbee's own SSD carries an ACBOOT partition; the label never qualifies it.
  assert.equal(stickOf({ DeviceIdentifier: "disk0", Internal: true, WholeDisk: true, BusProtocol: "Apple Fabric", TotalSize: 500277792768 }, ["ACBOOT"]), null);
  assert.equal(stickOf({ ...stick, DeviceIdentifier: "disk0" }), null);
  assert.equal(stickOf({ ...stick, DeviceIdentifier: "disk6s1", WholeDisk: false }), null);
  assert.equal(stickOf({ ...stick, BusProtocol: "Thunderbolt" }), null);
  assert.equal(stickOf({ ...stick, TotalSize: 2e12 }), null);
  assert.equal(stickOf({ ...stick, Internal: undefined }), null);
});

test("a flash names a disk by its identifier only", async () => {
  for (const disk of ["disk0", "/dev/disk6; reboot", "disk6s1", "../disk6"]) {
    const r = await handleMessage({ id: 3, method: "tools/call", params: { name: "aesel_flash", arguments: { disk } } });
    assert.equal(r.result.isError, true);
  }
});
