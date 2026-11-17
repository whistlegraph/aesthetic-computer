import test from 'node:test';
import assert from 'node:assert/strict';
import {KeystrokeClock,summarizeCadence,cadenceContext,withCadence,cadenceEnabled,PAUSE_MS} from '../src/keystroke-timing.mjs';

const clockAt = () => { let t = 0; const clock = new KeystrokeClock({now: () => t}); return {clock, at: (ms) => { t = ms; }}; };

test('intervals, pauses and bursts come from press times alone', () => {
  const {clock, at} = clockAt();
  clock.ready(0);
  at(1500); clock.type();
  for (const ms of [1620, 1730, 1850, 1960]) { at(ms); clock.type(); }
  at(1960 + PAUSE_MS + 500); clock.type();
  at(4600); clock.erase();
  at(4700); clock.type();
  const s = clock.take();
  assert.equal(s.keys, 7);
  assert.equal(s.typedChars, 7);
  assert.equal(s.erases, 1);
  assert.equal(s.firstKeyMs, 1500);
  assert.equal(s.pauses, 1);
  assert.deepEqual(s.longestPauses, [{afterChars: 5, ms: 2500}]);
  assert.equal(s.bursts, 2);
  assert.deepEqual(s.gapsMs, [120, 110, 120, 110]);
  assert.equal(s.ikiMs.median, 120);
  assert.equal(clock.take(), null, 'take() starts fresh');
});

test('characters arriving in one read count as a paste, not as keystrokes', () => {
  const {clock, at} = clockAt();
  at(100); clock.type(1);
  at(400); clock.type(1); clock.type(1); clock.type(1);
  at(900); clock.type(40);
  const s = clock.take();
  assert.equal(s.keys, 1);
  assert.equal(s.pastedChars, 43);
  assert.equal(s.typedChars, 44);
});

test('a first key typed before the reply finished reports no first-key latency', () => {
  const {clock, at} = clockAt();
  at(50); clock.type();
  clock.ready(200);
  at(300); clock.type();
  assert.equal(clock.take().firstKeyMs, undefined);
});

test('the context names what it is and carries no key identities', () => {
  const s = summarizeCadence([{at: 0, chars: 1, kind: 'key'}, {at: 140, chars: 1, kind: 'key'}]);
  const text = withCadence('make it blue', s);
  assert.match(text, /^make it blue\n\n\[Typing cadence/);
  assert.ok(!/"key"\s*:/.test(text));
  assert.equal(cadenceContext(null), '');
  assert.equal(cadenceContext([]), '');
  assert.match(cadenceContext([s, s]), /"messages":\[/);
});

test('AESEL_CADENCE=off turns it off', () => {
  assert.equal(cadenceEnabled({}), true);
  assert.equal(cadenceEnabled({AESEL_CADENCE: 'off'}), false);
  assert.equal(cadenceEnabled({AESEL_CADENCE: '0'}), false);
});
