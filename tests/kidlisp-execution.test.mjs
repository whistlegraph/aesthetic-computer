import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { KidLisp } from "../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { KidLispExecution } from "../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import { replay } from "../kidlisp/conformance/replay.mjs";

const fixture = JSON.parse(readFileSync(new URL("../kidlisp/conformance/seeded-input.json", import.meta.url)));
const api = { screen: { width: 128, height: 128 }, clock: { time() { throw new Error("Live clock read during replay"); } }, write() {} };

test("fixed seed, clock and pointer inputs reproduce the same command trace", () => {
  const a = replay(fixture), b = replay(fixture);
  assert.deepEqual(a, b);
  assert.notDeepEqual(a.frames, replay({ ...fixture, seed: fixture.seed + 1 }).frames);
  assert.ok(a.frames[1].calls.some(call => call[0] === "write" && call[1] === "pressed"));
  assert.ok(a.frames[3].calls.some(call => call[0] === "write" && call[1] === "released"));
  assert.deepEqual(a.frames.filter(f => f.calls.some(call => call[0] === "write" && call[1] === "tick")).map(f => f.frame), [4, 8]);
  assert.ok(a.frames[2].calls.some(call => call[0] === "line" && call[1] === 24 && call[2] === 36));
});

test("performance monitoring does not change controlled execution", () => {
  assert.deepEqual(replay(fixture), replay({ ...fixture, monitor: true }));
});

test("trace transport preserves non-finite numbers and negative zero", () => {
  const huge = "1" + "0".repeat(200);
  const result = replay({ source: `(ink (round -0.5) (* ${huge} ${huge}) 0)`, frames: 1 });
  assert.deepEqual(JSON.parse(JSON.stringify(result)), result);
  const ink = result.frames[0].calls.find(call => call[0] === "ink");
  assert.deepEqual(ink.slice(1, 3), [{ $number: "-0" }, { $number: "+Infinity" }]);
});

test("live clock and concurrent execution contexts cannot affect each other", () => {
  const a = new KidLispExecution({ seed: 5, epochMs: 1000, stepMs: 25 });
  const b = new KidLispExecution({ seed: 5, epochMs: 2000, stepMs: 40 });
  const first = new KidLisp({ execution: a }), second = new KidLisp({ execution: b });
  a.beginFrame(3); b.beginFrame(3);
  assert.equal(first.evaluate(["clock"], api), 1075);
  assert.equal(second.evaluate(["clock"], api), 2120);
  assert.equal(first.evaluate(["clock"], api), 1075);
  assert.equal(first.seededRandom(), second.seededRandom());
  first.reset(); second.reset();
  assert.equal(first.seededRandom(), second.seededRandom());
});

test("nested repeat work is bounded and failure unwinds scope and survives catches", () => {
  const execution = new KidLispExecution({ maxSteps: 1000 });
  const lisp = new KidLisp({ execution });
  const ast = lisp.parse('(repeat 100 (repeat 100 (write 1 0 0)))');
  let writes = 0;
  execution.beginFrame(0);
  assert.throws(() => lisp.evaluate(ast, { ...api, write() { writes++; } }), e => e.code === "STEP_BUDGET" && e.frame === 0 && e.operation !== null);
  assert.ok(writes < 10000);
  assert.equal(execution.state.depth, 0);
  assert.equal(lisp.localEnvLevel, 0);
  assert.throws(() => lisp.evaluate(1, api), e => e.code === "STEP_BUDGET");
  execution.beginFrame(1);
  assert.equal(lisp.evaluate(["+", 1, 2], api), 3);
});

test("children have stable distinct seeds and share the parent frame budget", () => {
  const execution = new KidLispExecution({ seed: 42, maxSteps: 10 });
  const left = execution.fork("left"), right = execution.fork("right");
  assert.equal(left.seed, execution.fork("left").seed);
  assert.notEqual(left.seed, right.seed);
  execution.beginFrame(5);
  left.consume("left", 6);
  assert.throws(() => right.consume("right", 5), e => e.code === "STEP_BUDGET");
  assert.equal(left.timeMs, execution.timeMs);
  assert.equal(execution.state.steps, 11);
});

test("depth, source size and invalid execution controls fail explicitly", () => {
  const execution = new KidLispExecution({ maxDepth: 8 });
  const lisp = new KidLisp({ execution });
  let ast = 1;
  for (let i = 0; i < 20; i++) ast = ["+", 1, ast];
  assert.throws(() => lisp.evaluate(ast, api), e => e.code === "DEPTH_BUDGET");
  assert.equal(execution.state.depth, 0);
  execution.beginFrame(1);
  assert.throws(() => lisp.parse("(".repeat(9) + "1" + ")".repeat(9)), e => e.code === "PARSE_DEPTH");
  execution.beginFrame(2);
  assert.throws(() => lisp.parse(" ".repeat(1000001)), e => e.code === "SOURCE_BUDGET");
  execution.beginFrame(3);
  assert.throws(() => lisp.parse(")" + "(".repeat(9)), e => e.code === "PARSE_SYNTAX");
  for (const options of [{ seed: -1 }, { seed: 1.2 }, { stepMs: Infinity }, { maxSteps: 0 }, { maxDepth: NaN }, { maxDepth: 257 }]) assert.throws(() => new KidLispExecution(options));
});

test("replay rejects live resources and unsupported media before execution", () => {
  for (const source of ['(fetch "https://example.com")', '(clock "cdefg")', '(ink rainbow)', '($cow)']) {
    assert.throws(() => replay({ source, frames: 1 }), e => e.code === "REPLAY_CAPABILITY");
  }
  assert.throws(() => replay({ ...fixture, events: [{ frame: 500, type: "touch", x: 0, y: 0 }] }), /input event/);
});
