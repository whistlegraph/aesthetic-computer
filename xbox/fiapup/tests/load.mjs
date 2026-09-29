// Load fiapup.js the way the console does: one script, the host's bindings as
// its only way out. `host` names the bindings; anything it leaves out is
// simply not there, as on a host that lacks it.

import { readFileSync } from "node:fs";

export const source = readFileSync(new URL("../fiapup.js", import.meta.url), "utf8");

// The bindings the Xbox host (xbox/native-bios/QuickJsEngine.cpp) gives a
// piece that fiapup could reach for. Nothing else exists there.
export const xboxBindings = ["wipe", "synth", "drum", "oscillator", "oscillatorStop", "write",
  "box", "line", "triangle", "triangle3d", "triangles3d", "runtime", "gamepad", "controllers",
  "capabilities", "telemetry"];

export function load(host = {}) {
  const names = [...new Set([...xboxBindings, "frame", ...Object.keys(host)])];
  const make = new Function(...names, `${source}\nreturn { boot, sim, paint, act, leave, fiapup };`);
  return make(...names.map((name) => host[name]));
}

// A host that counts what it is asked to draw, and plays nothing.
export function countingHost(extra = {}) {
  const drawn = { faces: 0, texts: [], wipes: 0 };
  let us = 0;
  const host = {
    triangle3d: () => { drawn.faces++; },
    write: (s) => { drawn.texts.push(s); },
    wipe: () => { drawn.wipes++; },
    box: () => {}, line: () => {}, synth: () => {},
    runtime: () => ({ width: 1920, height: 1080, monotonicUs: (us += 16667) }),
    gamepad: () => ({ down: [], leftX: 0, leftY: 0 }),
    ...extra,
  };
  return { host, drawn };
}
