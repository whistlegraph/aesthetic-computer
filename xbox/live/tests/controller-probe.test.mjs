import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { Script, createContext } from "node:vm";

const source = readFileSync(new URL("../controller-probe.js", import.meta.url), "utf8");

function harness({ online = true, pads = [] } = {}) {
  const calls = { boxes: [], writes: [], synths: [], telemetry: [], wipes: [] };
  let monotonicUs = 2_000_000;
  let input = { leftX: 0, leftY: 0, rightX: 0, rightY: 0,
    leftTrigger: 0, rightTrigger: 0, down: [] };
  const caps = { online, networkLevel: online ? "internet" : "none",
    networkName: online ? "TEST LAN" : "", version: "1.0.0.10",
    width: 1920, height: 1080 };
  const globals = {
    Math, JSON,
    wipe: (...args) => calls.wipes.push(args),
    box: (...args) => calls.boxes.push(args),
    write: (...args) => calls.writes.push(args),
    synth: (...args) => calls.synths.push(args),
    telemetry: (...args) => calls.telemetry.push(args),
    capabilities: () => ({ ...caps }),
    controllers: () => pads.map((pad) => ({ ...pad })),
    gamepad: () => ({ ...input, down: [...input.down] }),
    runtime: () => ({ width: 1920, height: 1080, sampleRate: 48000,
      simCount: 12, paintCount: 11, monotonicUs: monotonicUs += 16_667,
      unixMs: 1_784_587_200_000 }),
  };
  const context = createContext(globals);
  new Script(source, { filename: "controller-probe.js" }).runInContext(context);
  return { context, calls, caps,
    setInput(next) { input = { ...input, ...next }; },
    advance(seconds) { monotonicUs += seconds * 1_000_000; } };
}

const controller = { id: "test-pad", name: "XBOX TEST PAD", vendorId: 1118,
  productId: 2834, axes: 6, buttons: 14, switches: 0, gamepad: true };

{
  const test = harness({ pads: [controller] });
  test.context.boot();
  test.context.sim();
  test.context.paint();
  assert.equal(test.calls.wipes.length, 1);
  assert.ok(test.calls.boxes.length >= 15, "dashboard should draw panels and meters");
  assert.ok(test.calls.writes.length >= 18, "dashboard should draw all status sections");
  assert.ok(test.calls.writes.some(([text]) => text.includes("RAW CONTROLLERS 1")));
  assert.ok(test.calls.writes.some(([text]) => text.includes("XBOX TEST PAD")));
  assert.ok(test.calls.telemetry.some(([event]) => event === "DASHBOARD_INVENTORY"));
  assert.ok(test.calls.telemetry.some(([event]) => event === "DASHBOARD_BOOT"));

  test.setInput({ leftX: -0.75, rightY: 0.5, leftTrigger: 0.25,
    rightTrigger: 1, down: ["A", "RightShoulder"] });
  test.context.act("A");
  test.context.paint();
  assert.ok(test.calls.telemetry.some(([event, detail]) =>
    event === "DASHBOARD_BUTTON" && JSON.parse(detail).button === "A"));
  assert.ok(test.calls.synths.some(([frequency]) => frequency === 440));
  assert.ok(test.calls.writes.some(([text]) => text.includes("A RightShoulder")));
}

{
  const test = harness({ online: false, pads: [] });
  test.context.boot();
  test.context.paint();
  assert.ok(test.calls.writes.some(([text]) => text.includes("NETWORK NONE")));
  assert.ok(test.calls.writes.some(([text]) => text.includes("NO CONTROLLERS DETECTED")));
  assert.equal(test.calls.synths[0][0], 220);
}

console.log("controller-probe: online/offline, inventory, input, drawing, audio, telemetry OK");
