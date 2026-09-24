import test from 'node:test';
import assert from 'node:assert/strict';
import { batteryWarningInterval, createBatteryWatch } from '../lib/battery-watch.mjs';

test('warning cadence speeds up from 30 seconds at 10% to 1 second at 0%', () => {
  assert.deepEqual([11, 10, 5, 1, 0, -1, null, NaN].map(batteryWarningInterval), [null, 30, 15, 3, 1, null, null, null]);
});

test('threshold, exact timing, falling charge, recovery, and clock reset', () => {
  let notes = 0;
  const watch = createBatteryWatch();
  const api = { system: { battery: { percent: 11 } }, sound: { time: 0, synth() { notes++; } } };
  const tick = (time, percent) => { api.sound.time = time; api.system.battery.percent = percent; watch.update(api); return notes; };
  assert.equal(tick(0, 11), 0);
  assert.equal(tick(1, 10), 2);
  assert.equal(tick(30.99, 10), 2);
  assert.equal(tick(31, 10), 4);
  assert.equal(tick(45.99, 5), 4);
  assert.equal(tick(46, 5), 6);
  assert.equal(tick(49, 1), 8);
  assert.equal(tick(50, 11), 8);
  assert.equal(tick(51, 10), 10);
  assert.equal(tick(0, 10), 12);
  assert.equal(tick(100, -1), 12);
});

test('charging does not hide the requested warning while still at or below 10%', () => {
  let notes = 0;
  const watch = createBatteryWatch();
  const result = watch.update({ system: { battery: { percent: 5, charging: true } }, sound: { time: 0, synth() { notes++; } } });
  assert.equal(result.interval, 15);
  assert.equal(result.charging, true);
  assert.equal(notes, 2);
});

test('percentage and flashing bell draw without depending on the performance text layer', () => {
  const watch = createBatteryWatch();
  const labels = [], colors = [];
  const api = { system: { battery: { percent: 10 } }, sound: { time: 0, synth() {} }, screen: { width: 455, height: 256 }, ink(...c) { colors.push(c); }, box() {}, write(label) { labels.push(label); } };
  watch.update(api); watch.paint(api);
  api.sound.time = .5; watch.update(api); watch.paint(api);
  assert.deepEqual(labels, ['10%', '10%']);
  assert.notDeepEqual(colors[0], colors[2]);
});

test('lightning follows external power even when full, and disappears when unplugged', () => {
  let online='1';let rectangles=0;
  const watch=createBatteryWatch();
  const api={system:{battery:{percent:100,charging:false,status:'Full'},listDir:()=>[{name:'AC'}],readFile:()=>online},sound:{time:0},screen:{width:455,height:256},ink(){},write(){},box(){rectangles++;}};
  assert.equal(watch.update(api).pluggedIn,true);watch.paint(api);const pluggedRects=rectangles;
  online='0';api.sound.time=1.1;rectangles=0;
  assert.equal(watch.update(api).pluggedIn,false);watch.paint(api);
  assert(pluggedRects>rectangles);
});
