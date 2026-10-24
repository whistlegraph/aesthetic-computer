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
  // Patches may layer several voices per key (hoover, shepard), so count
  // growth rather than exact voices.
  const n0 = r.log.synth.length;
  r.key("c"); assert.ok(r.log.synth.length > n0);
  const n1 = r.log.synth.length;
  r.key("v"); r.key("b"); assert.ok(r.log.synth.length > n1); // diagonal neighbours
  r.key("c", false); assert.ok(r.log.kill.length >= 1);
  const n2 = r.log.synth.length;
  r.key("f6"); r.key("z"); assert.ok(r.log.synth.length - n2 >= 3); // chord = at least 3 voices
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
  const run = async (ms) => { const t0 = Date.now(); while (Date.now() - t0 < ms) { r.piece.sim({ sound: r.sound }); await new Promise((res) => setTimeout(res, 5)); } };
  const notes = () => r.log.synth.filter((o) => !(o.type === "square" && o.duration === 0.02)).length; // ignore the click
  r.key("f12");                       // arms: one bar of count-in at 120 bpm = 2 s
  r.key("c"); r.key("c", false);      // during the count-in: heard, not recorded
  await run(2200);
  const before = notes();
  r.key("c"); r.key("c", false);      // now it records
  const after = notes();
  await run(60000 / 120 / 4 * 17);    // one full loop later it plays back
  assert.ok(notes() > after, "the loop fired the recorded note");
  assert.ok(after > before);
  r.key("insert"); r.key("delete");
});

test("seq view toggles steps and paints", async () => {
  const r = await rig();
  r.key("f4"); r.key("arrowright"); r.key("enter");
  r.key("2"); r.key("enter");
  r.piece.paint(r.paintApi);
  assert.ok(r.draws.box > 50 && r.draws.write > 10);
});

test("escape three times jumps to the prompt", async () => {
  const r = await rig();
  r.key("escape"); r.key("escape"); r.key("escape");
  assert.equal(r.system.jumped, "prompt");
  r.piece.leave({ sound: r.sound });
});
