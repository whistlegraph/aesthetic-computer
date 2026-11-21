import test from 'node:test';
import assert from 'node:assert/strict';
import { gyroLook } from '../gyro.mjs';

function phone(angle = 0, permission = 'granted') {
  let read, requests = 0, active = true, timeStamp = 0;
  const turns = [];
  const host = {
    isSecureContext: true, screen: { orientation: { angle } },
    DeviceOrientationEvent: { async requestPermission() { requests++; return permission; } },
    addEventListener(type, fn) { assert.equal(type, 'deviceorientation'); read = fn; },
  };
  const gyro = gyroLook(host, () => active, (yaw, pitch) => turns.push([yaw, pitch]));
  return { gyro, host, turns, requests: () => requests, active(v) { active = v; },
    pose(alpha, beta, gamma, ms = 16) { timeStamp += ms; read?.({ alpha, beta, gamma, timeStamp }); } };
}
const close = (actual, expected) => assert.ok(Math.abs(actual - expected) < 1e-6, actual + ' vs ' + expected);

test('gyro starts at the current grip and measures only relative look in screen axes', async () => {
  for (const [angle, yaw, pitch] of [[0, 15, 0], [90, 0, -15], [270, 0, 15], [180, -15, 0]]) {
    const p = phone(angle);
    await p.gyro.enable();
    p.pose(70, 43, 0);
    assert.deepEqual(p.turns, []);
    p.pose(70, 43, 15);
    close(p.turns[0][0], yaw); close(p.turns[0][1], pitch);
  }
  const p = phone();
  await p.gyro.enable();
  p.pose(355, 90, 0); p.pose(5, 90, 0); p.pose(5, 95, 0);
  close(p.turns[0][0], 10); close(p.turns[1][1], 5);
});

test('a pause, rotation or reset establishes a fresh neutral sample', async () => {
  const p = phone();
  await Promise.all([p.gyro.enable(), p.gyro.enable()]);
  assert.equal(p.requests(), 1);
  p.pose(0, 45, 0);
  p.active(false); p.pose(50, 50, 50); p.active(true); p.pose(100, 20, 0);
  p.host.screen.orientation.angle = 90; p.pose(0, 80, 0);
  p.pose(0, 0, 0, 1000);
  p.gyro.reset(); p.pose(0, 45, 0);
  p.pose(null, null, null); p.pose(0, 20, 0);
  assert.deepEqual(p.turns, []);
  p.pose(0, 20, 0);
  close(p.turns[0][0], 0); close(p.turns[0][1], 0);
});

test('declining access keeps look still and does not prompt again', async () => {
  const p = phone(0, 'denied');
  await p.gyro.enable(); await p.gyro.enable();
  p.pose(0, 0, 0); p.pose(0, 10, 10);
  assert.equal(p.requests(), 1);
  assert.deepEqual(p.turns, []);
});
