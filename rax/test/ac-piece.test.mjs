import test from 'node:test';
import assert from 'node:assert/strict';
import { pathToFileURL, fileURLToPath } from 'node:url';
import { acPiece, freq } from '../ac-piece.mjs';
import { createPanel } from '../panel.mjs';

// AC's own disks and libraries, where they are on disk (a page is served them at ./ac/).
const base = pathToFileURL(fileURLToPath(new URL('../../system/public/aesthetic.computer/', import.meta.url))).href;
// A host's voice that keeps what is sounding: [{ type, hz, on }].
const voices = () => {
  const all = [];
  const synth = (o) => { const v = { type: o.type, hz: o.tone, on: true }; all.push(v); return { kill() { v.on = false; }, update(to) { if (to.tone) v.hz = to.tone; } }; };
  return { all, sound: { synth, sampleContext: () => null }, on: () => all.filter((v) => v.on).map((v) => Math.round(v.hz)) };
};
const quiet = async (f) => { const log = console.log; console.log = () => {}; try { return await f(); } finally { console.log = log; } }; // (notepat says a good deal as it boots)
const later = (ms) => new Promise((r) => setTimeout(r, ms));
const hz = (name) => Math.round(freq(name));
const mount = async (slug, cols, rows, kept, sampler) => {
  const v = voices(), { piece, host, ready } = await acPiece(slug, { base, seed: 'test', sound: { ...v.sound, ...sampler }, kept });
  const panel = createPanel({ id: 't', piece, host, cols, rows });
  await ready();
  for (let i = 0; i < 3; i++) panel.step();
  return { panel, v, colours: () => { const s = new Set(); for (let k = 0; k < panel.pixels.length; k += 4) s.add((panel.pixels[k] << 16) | (panel.pixels[k + 1] << 8) | panel.pixels[k + 2]); return s.size; } };
};

test('the real notepat on a panel: pads drawn, a press a note, a slide the next, a lift its end, two hands two notes', () => quiet(async () => {
  const { panel, v, colours } = await mount('notepat:sine', 320, 160, { 'notepat:layout': 'grid' });
  assert.ok(colours() > 24, 'a bar and two grids of pads, each note in its hue');
  const rest = panel.pixels.slice();
  panel.act('touch', 30, 50); panel.step();
  assert.deepEqual([v.on(), v.all.at(-1).type], [[hz('4c')], 'sine'], 'the first pad at its first octave is middle C');
  assert.ok(rest.some((p, k) => p !== panel.pixels[k]), 'the pad held is lit');
  panel.act('draw', 100, 90); panel.step();
  assert.deepEqual(v.on(), [hz('4f#')], 'slid to another pad: the note moves');
  panel.hear('touch', 220, 60, 3); panel.step(); // someone else, from seat 1
  assert.deepEqual(v.on(), [hz('4f#'), hz('5c#')]);
  panel.act('lift', 100, 90); panel.step();
  assert.deepEqual(v.on(), [hz('5c#')], 'one hand\'s lift leaves the other\'s note');
  panel.hear('lift', 220, 60, 3); panel.step();
  assert.deepEqual(v.on(), []);
  panel.act('move', -1, -1); panel.step();
  // Released notes leave a trail that fades by 0.0065 per simulation step.
  for (let i = 0; i < 160; i++) panel.step();
  assert.ok(panel.pixels.every((v, i) => v === rest[i]), 'the board returns to rest after the release trails fade');
  // Its mode bar: the octave's button takes the pads up one.
  panel.act('touch', 292, 10); panel.act('lift', 292, 10); panel.act('touch', 30, 50);
  assert.deepEqual(v.on(), [hz('5c')]);
  panel.leave();
}));

test('the kiosk picker, mute and numeric shortcuts play the selected GM sample', t => quiet(async () => {
  const samples = [], stopped = [], output = {};
  const param = () => ({setValueAtTime() {}, setTargetAtTime() {}, linearRampToValueAtTime() {}, cancelAndHoldAtTime() {}});
  const ctx = {
    currentTime: 0,
    createGain: () => ({ gain: param(), connect(target) { assert.equal(target, output); }, disconnect() {} }),
    createBufferSource: () => ({ playbackRate: param(), connect() {}, disconnect() {},
      start() { samples.push(this.buffer.program); }, stop() { stopped.push(this.buffer.program); } }),
    decodeAudioData: async bytes => ({program:new Uint8Array(bytes)[0]}),
  };
  t.mock.method(globalThis, 'fetch', async url => {
    if (url.endsWith('/manifest.json')) return Response.json({patches: Array.from({length:128}, (_,id) => ({id,notes:[60]}))});
    const match = /\/(\d{3})\/C4\.mp3$/.exec(url);
    assert.ok(match, url);
    return new Response(Uint8Array.of(Number(match[1])));
  });
  const sampler = { sampleContext: () => ctx, sampleOutput: () => output, holdSample: start => start() };
  const { panel } = await mount('notepat', 320, 160, {}, sampler);
  const tap = (x, y) => { panel.act('touch', x, y); panel.act('lift', x, y); panel.step(); };
  await later(0);
  tap(60, 140);
  assert.deepEqual(samples, [0]);
  assert.deepEqual(stopped, [0]);
  tap(262, 10); tap(100, 69); tap(285, 33); // local sound off, close
  tap(60, 140);
  assert.deepEqual(samples, [0]);
  tap(262, 10); tap(100, 69); tap(100, 52); tap(100, 120); tap(285, 33);
  await later(0);
  tap(60, 140);
  assert.deepEqual(samples, [0, 4], 'selecting the fifth row plays electric piano');
  for (const digit of '128') {
    const key = {key:digit,code:'Digit' + digit};
    panel.key('down', key); panel.key('up', key);
  }
  await later(0);
  tap(60, 140);
  assert.deepEqual(samples, [0, 4, 127]);
  assert.deepEqual(stopped, samples);
  panel.leave();
}));

test('bleep and song are hosted as they were', () => quiet(async () => {
  const bleep = await mount('bleep:sine 4x4', 64, 64);
  assert.ok(bleep.colours() > 8);
  bleep.panel.act('touch', 8, 8); bleep.panel.step();
  assert.equal(bleep.v.on().length, 1);
  bleep.panel.act('lift', 8, 8); bleep.panel.step();
  assert.equal(bleep.v.on().length, 0);
  const song = await mount('song row', 128, 128);
  assert.ok(song.colours() > 2);
  song.panel.act('touch', 64, 90); song.panel.step();
  assert.ok(song.v.all.length >= 1, 'a syllable sung');
  song.panel.act('lift', 64, 90); song.panel.step();
  song.panel.leave(); bleep.panel.leave();
}));
