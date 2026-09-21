import test from 'node:test';
import assert from 'node:assert/strict';
import { summarizeBenchmark } from '../lib/computer-use-bench-report.mjs';

test('failed drags remain visible and do not improve successful latency', () => {
  const row = { holdMs: 0, durationMs: 32, releaseMs: 32, ms: 120,
    receipt: { verification: { ok: true }, releasePosted: true },
    drops: 'Drops: 1', released: 'Released: 1', rejected: 'Rejected: 0' };
  const report = summarizeBenchmark({ ok: false, error: 'Drag failed', dragSamples: [row,
    { ...row, ms: 700, rejected: 'Rejected: 1' },
    { ...row, ms: 600, released: 'Released: 0' },
    { ...row, ms: 650, verified: false }], dragBatches: 5 }, '/tmp/result.json');
  const group = report.drags['hold=0,path=32,release=32'];
  assert.equal(report.ok, false);
  assert.equal(report.error, 'Drag failed');
  assert.equal(report.reportPath, '/tmp/result.json');
  assert.equal(report.dragBatches, 5);
  assert.equal(group.attempted, 4);
  assert.equal(group.verified, 1);
  assert.equal(group.successful.medianMs, 120);
  assert.equal(group.failed.medianMs, 650);
});

test('timing profiles stay separate and raw rounds stay in the file', () => {
  const report = summarizeBenchmark({ ok: true, game: { correct: 30, rounds: [{ secretFixtureDetail: 'unused' }] },
    nativeClickLatency: [10, 20, 40, 50].map(ms => ({ ms, settleMs: 0, holdMs: 0, verifiedCount: 1 }))
      .concat([{ ms: 500, settleMs: 180, holdMs: 40, verifiedCount: 2 }]) }, '/tmp/result.json');
  assert.equal(report.clicks['hold=0,settle=0'].successful.medianMs, 30);
  assert.equal(report.clicks['hold=40,settle=180'].attempted, 1);
  assert.deepEqual(report.game, { correct: 30 });
  assert.equal(JSON.stringify(report).includes('secretFixtureDetail'), false);
});
