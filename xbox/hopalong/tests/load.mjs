// Load hopalong.js the way the console does: one script, the host's bindings
// its only way out. A binding the host leaves out is simply not there.

import { readFileSync } from "node:fs";

export const source = readFileSync(new URL("../hopalong.js", import.meta.url), "utf8");
export const bindings = ["wipe", "synth", "drum", "triangle3d", "triangles3d", "runtime", "gamepad", "telemetry"];

export function load(host = {}) {
  const make = new Function(...bindings, `${source}\nreturn { boot, sim, paint, act, leave, hopalong };`);
  return make(...bindings.map((name) => host[name]));
}

// A host that keeps the last frame's triangles and plays nothing.
export function quietHost(width = 1920, height = 1080) {
  const frame = { faces: new Float32Array(0), count: 0, wipe: [0, 0, 0] };
  const host = {
    wipe: (r, g, b) => { frame.wipe = [r, g, b]; },
    triangles3d: (buffer, count) => { frame.faces = buffer; frame.count = count; },
    runtime: () => ({ width, height }),
    gamepad: () => ({ down: [], leftX: 0, leftY: 0 }),
    synth: () => {}, drum: () => {}, telemetry: () => {},
  };
  return { host, frame };
}
