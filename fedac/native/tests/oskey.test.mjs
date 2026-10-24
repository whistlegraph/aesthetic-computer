// Smoke test for pieces/oskey.mjs against a mocked AC Native sound API:
// plays every layer, records a loop, replays it, and paints a frame.
import { test } from "node:test";
import assert from "node:assert/strict";
import { register } from "node:module";
import { pathToFileURL } from "node:url";

// The piece imports "/lib/percussion.mjs" — the on-device path. Map it to
// the web lib the initramfs copies it from.
const LIB = pathToFileURL(new URL("../../../system/public/aesthetic.computer/lib/", import.meta.url).pathname).href;
register(`data:text/javascript,export function resolve(s,c,n){if(s.startsWith("/lib/"))return n(${JSON.stringify(LIB)}+s.slice(5),c);return n(s,c);}`, import.meta.url);

async function rig() {
  const piece = await import(new URL("../pieces/oskey.mjs", import.meta.url).href + "?" + Math.random());
  const log = { synth: [], kill: [], zoo: [], sample: [], perc: 0 };
  let id = 0;
  const sound = {
    bpm() {}, allNotesOff() {}, microphone: { sampleLength: 0, sampleRate: 48000, rec() { return true; }, cut() { return 4800; } },
    synth(o) { log.synth.push(o); return { id: ++id }; }, kill(v, f) { log.kill.push([v.id, f]); },
    zoo: { play(n, o) { log.zoo.push([n, o]); return { id: ++id }; } },
    sample: { play(o) { log.sample.push(o); return { id: ++id }; } },
    room: { setMix() {}, toggle() {} }, echo: { setMix() {} }, glitch: { setMix() {}, toggle() {} },
  };
  const draws = { box: 0, write: 0, line: 0 };
  const paintApi = { wipe() {}, ink() {}, box() { draws.box++; }, line() { draws.line++; }, write() { draws.write++; }, screen: { width: 1366, height: 768 } };
  const system = { jump(t) { system.jumped = t; } };
  const ev = (name, extra = {}) => ({ is: (p) => name === p || name.startsWith(p + ":") || (p === "keyboard:down" && name.startsWith("keyboard:down")), ...extra });
  const key = (k, down = true) => piece.act({ event: ev(`keyboard:${down ? "down" : "up"}:${k}`, { key: k }), sound, system, screen: paintApi.screen });
  piece.boot({ sound });
  return { piece, sound, log, draws, paintApi, system, key, ev };
}

test("instrument keys sound and release", async () => {
  const r = await rig();
  r.key("c"); assert.equal(r.log.synth.length, 1);
  r.key("v"); r.key("b"); assert.equal(r.log.synth.length, 3); // diagonal neighbours
  r.key("c", false); assert.equal(r.log.kill.length, 1);
  r.key("f6"); r.key("z"); assert.equal(r.log.synth.length, 6); // chord = 3 voices
});

test("drums: left hand kit, right hand zoo", async () => {
  const r = await rig();
  r.key("f2");
  const before = r.log.synth.length;
  r.key("z"); assert.ok(r.log.synth.length > before, "kick is layered synth voices");
  r.key("n"); assert.equal(r.log.zoo.length, 1); assert.equal(r.log.zoo[0][0], "bird");
  r.key("b"); assert.equal(r.log.zoo.length, 1, "col 4 is free");
});

test("sampler needs a recording, then plays by degree", async () => {
  const r = await rig();
  r.key("f3"); r.key("c"); assert.equal(r.log.sample.length, 0);
  r.key("f9"); r.key("f9"); r.key("c"); assert.equal(r.log.sample.length, 1);
  assert.ok(r.log.sample[0].tone > 0);
});

test("rec arms play, hits land on the track, loop replays them", async () => {
  const r = await rig();
  r.key("f12"); r.key("c"); r.key("c", false);
  const synthBefore = r.log.synth.length;
  const t0 = Date.now();
  while (Date.now() - t0 < 60000 / 120 / 4 * 17) { r.piece.sim({ sound: r.sound }); await new Promise((res) => setTimeout(res, 5)); }
  assert.ok(r.log.synth.length > synthBefore, "the loop fired the recorded note");
  r.key("insert"); r.key("delete");
});

test("seq view toggles steps and paints", async () => {
  const r = await rig();
  r.key("f4"); r.key("arrowright"); r.key("enter");
  r.key("2"); r.key("enter");
  r.piece.paint(r.paintApi);
  assert.ok(r.draws.box > 100 && r.draws.write > 20 && r.draws.line > 10);
});

test("escape three times jumps to the prompt", async () => {
  const r = await rig();
  r.key("escape"); r.key("escape"); r.key("escape");
  assert.equal(r.system.jumped, "prompt");
  r.piece.leave({ sound: r.sound });
});
