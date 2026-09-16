import assert from "node:assert/strict";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { benchmarkPiece } from "../src/perf.mjs";

async function piece(t, source) {
  const root = await mkdtemp(join(tmpdir(), "easel-perf-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const file = join(root, "piece.mjs");
  await writeFile(file, source);
  return file;
}

test("counts measured drawing work, excluding warmup, with seeded top-level randomness", async (t) => {
  const file = await piece(t, `
    const count = 1 + Math.floor(Math.random() * 100);
    export function boot({ circle }) { circle(); }
    export function sim({ line }) { line(); }
    export function paint({ ink, circle }) { ink(); for (let i=0; i<count; i++) circle(); }
  `);
  const first = await benchmarkPiece({ file, frames: 10, warmup: 5, seed: 123 });
  const again = await benchmarkPiece({ file, frames: 10, warmup: 5, seed: 123 });
  assert.equal(first.measurement, "headless-logic");
  assert.equal(first.drawCalls.line, 1);
  assert.equal(first.drawCalls.ink, 1);
  assert.equal(first.totalCalls.line, 10);
  assert.deepEqual(first.totalCalls, again.totalCalls);
  assert.equal(first.msPerFrame, first.elapsedMs / 10);
  assert.ok(first.elapsedMs >= 0);
  assert.equal(first.fps, undefined, "logic timing is not rendering FPS");
});

test("a looping piece times out without blocking the parent", async (t) => {
  const file = await piece(t, "export function paint() { while(true) {} }");
  let ticked = false;
  const tick = setTimeout(() => { ticked = true; }, 30);
  await assert.rejects(benchmarkPiece({ file, timeoutMs: 150 }), /exceeded|timed out/);
  clearTimeout(tick);
  assert.ok(ticked);
});

test("filesystem imports and host globals are unavailable", async (t) => {
  const imported = await piece(t, 'import fs from "node:fs"; export function paint() { fs.readFileSync("/etc/passwd"); }');
  await assert.rejects(benchmarkPiece({ file: imported }), /Imports are unavailable/);
  const processPiece = await piece(t, 'export function paint() { process.env; }');
  await assert.rejects(benchmarkPiece({ file: processPiece }), /process is not defined/);
  const constructorEscape = await piece(t, 'export function paint({wipe}) { wipe.constructor("return process")(); }');
  await assert.rejects(benchmarkPiece({ file: constructorEscape }), /Code generation from strings disallowed/);
});

test("bounded options and cancellation refuse work cleanly", async (t) => {
  const file = await piece(t, "export function paint() {}");
  await assert.rejects(benchmarkPiece({ file, frames: 100000 }), /frames must be/);
  const controller = new AbortController();
  controller.abort(new Error("cancelled"));
  await assert.rejects(benchmarkPiece({ file, signal: controller.signal }), /cancelled/);
});
