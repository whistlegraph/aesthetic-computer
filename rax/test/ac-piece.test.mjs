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
  return { all, sound: { synth }, on: () => all.filter((v) => v.on).map((v) => Math.round(v.hz)) };
};
const quiet = async (f) => { const log = console.log; console.log = () => {}; try { return await f(); } finally { console.log = log; } }; // (notepat says a good deal as it boots)
const later = (ms) => new Promise((r) => setTimeout(r, ms));
const hz = (name) => Math.round(freq(name));
const mount = async (slug, cols, rows, kept) => {
  const v = voices(), { piece, host, ready } = await acPiece(slug, { base, seed: 'test', sound: v.sound, kept });
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
  assert.deepEqual([...panel.pixels], [...rest], 'and the board is as it was');
  // Its mode bar: the octave's button takes the pads up one.
  panel.act('touch', 292, 10); panel.act('lift', 292, 10); panel.act('touch', 30, 50);
  assert.deepEqual(v.on(), [hz('5c')]);
  panel.leave();
}));

test('notepat as it opens (its piano, on General MIDI) is played by the one voice a kiosk has, and its sound panel chooses another', () => quiet(async () => {
  const { panel, v } = await mount('notepat', 320, 160);
  panel.act('touch', 60, 140); panel.step();
  await later(80); // (its services are loaded at the first note)
  assert.deepEqual([v.on(), v.all.at(-1).type], [[hz('4e')], 'triangle']);
  panel.act('lift', 60, 140); panel.step();
  assert.deepEqual(v.on(), []);
  const tap = (x, y) => { panel.act('touch', x, y); panel.act('lift', x, y); panel.step(); };
  const play = () => { panel.act('touch', 60, 140); panel.step(); const on = v.on(); panel.act('lift', 60, 140); panel.step(); return on; };
  tap(262, 10); tap(100, 69); tap(285, 33); // its sound button; "Local sound: on" → off; the panel shut
  assert.deepEqual(play(), [], 'local sound off: the pads make none');
  tap(262, 10); tap(100, 69); tap(100, 52); tap(100, 120); tap(285, 33); // on again; the list of instruments; the fifth program; shut
  assert.deepEqual(play(), [hz('4e')], 'another program is still the one voice');
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
