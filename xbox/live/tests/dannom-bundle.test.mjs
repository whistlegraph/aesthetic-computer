import assert from "node:assert/strict";
import test from "node:test";

import { buildNomBundle } from "../../tools/bundle-nom.mjs";

function nativeProbe(source) {
  const commands = [];
  const pad = { down: [] };
  let now = 1_786_089_600_000;
  const globals = {
    wipe: (...args) => commands.push(["wipe", ...args]),
    box: (...args) => commands.push(["box", ...args]),
    line: (...args) => commands.push(["line", ...args]),
    write: (...args) => commands.push(["write", ...args]),
    comicWrite: (...args) => commands.push(["comicWrite", ...args]),
    synth: () => {}, oscillator: () => {}, oscillatorStop: () => {},
    gameView: () => ({ width: 1280, height: 720 }),
    gamepad: () => ({ connected: true, down: pad.down.slice() }),
    runtime: () => ({ unixMs: now, monotonicUs: now * 1000 }),
    capabilities: () => ({ platform: "xbox-uwp", inputFamily: "xbox" }),
    telemetry: (...args) => commands.push(["telemetry", ...args]),
  };
  const names = Object.keys(globals);
  const values = Object.values(globals);
  const lifecycle = new Function(...names,
    `${source}\nreturn { boot: globalThis.boot, sim: globalThis.sim, paint: globalThis.paint, act: globalThis.act, leave: globalThis.leave };`)(...values);
  return { lifecycle, commands, pad, tick(ms = 17) { now += ms; lifecycle.sim(); } };
}

test("the native Dannom bundle contains the shared engine and Xbox lifecycle", async () => {
  const source = await buildNomBundle();
  assert.match(source, /source: system\/public\/aesthetic\.computer\/lib\/nom\.mjs/);
  assert.match(source, /const COLS = 5/);
  assert.match(source, /if \(state === "over"\) \{[\s\S]*?paintLeaderboard/);
  assert.match(source, /leaderboard\.slice\(0, screen\.height < 430 \? 3 : 5\)/);
  assert.doesNotMatch(source, /^import\s/m);
  const probe = nativeProbe(source);
  probe.lifecycle.boot();
  probe.tick();
  probe.lifecycle.paint();
  assert.ok(probe.commands.some(([name]) => name === "wipe"));
  assert.ok(probe.commands.some(([name]) => name === "box"));
  assert.ok(probe.commands.some(([name]) => name === "comicWrite"));
});

test("Xbox A and d-pad edges enter the shared Nom act path once", async () => {
  const probe = nativeProbe(await buildNomBundle());
  probe.lifecycle.boot();
  probe.pad.down = ["ArrowRight", "A"];
  probe.tick();
  probe.tick(); // held buttons must not create another edge
  probe.pad.down = [];
  probe.tick();
  probe.lifecycle.paint();
  assert.ok(probe.commands.filter(([name]) => name === "comicWrite").length > 10);
});
