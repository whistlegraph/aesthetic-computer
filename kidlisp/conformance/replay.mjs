#!/usr/bin/env node
// Deterministic reference-evaluator command replay. This profile deliberately
// records host calls; the browser pixel oracle remains a separate renderer gate.
import { KidLisp } from "../../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { KidLispExecution, KidLispExecutionError } from "../../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import { numericOperationNames } from "../../system/public/aesthetic.computer/lib/kidlisp-ops.mjs";

const heads = new Set([...numericOperationNames(), "abs", "sqrt", "tan", "exp", "sign", "pow", "atan2", "hypot", "clamp", "pool", "spawn", "each", "kill", "alive", "empty", "rank", "shape", "key", "pad", "wipe", "ink", "line", "box", "circle", "point", "write", "clock", "random", "?", "repeat", "def", "tap", "draw", "lift", "if", "once", "frame", "width", "height", "w", "h", "no", "yes", "=", ">", "<", "..."]);
const traceValue = value => {
  if (typeof value === "number") {
    if (Number.isNaN(value)) return { $number: "NaN" };
    if (!Number.isFinite(value)) return { $number: value > 0 ? "+Infinity" : "-Infinity" };
    if (Object.is(value, -0)) return { $number: "-0" };
  }
  if (value === undefined) return { $undefined: true };
  if (Array.isArray(value)) return value.map(traceValue);
  if (value && typeof value === "object") return Object.fromEntries(Object.entries(value).map(([key, v]) => [key, traceValue(v)]));
  return value;
};
export function validateReplayForms(forms, { operations = heads, profile = "commands-v1" } = {}) {
  const visit = form => {
    if (!Array.isArray(form)) {
      if (typeof form === "string" && (/^[$#!]/.test(form) || ["rainbow", "zebra", "mic", "amp", "speaker"].includes(form))) throw new KidLispExecutionError("REPLAY_CAPABILITY", `Unavailable replay value: ${form}`);
      return;
    }
    const [head, ...args] = form;
    if (!(typeof head === "number" || operations.has(head) || (typeof head === "string" && /^\d*\.?\d+s(?:!|\.{2,3})?$/.test(head)))) throw new KidLispExecutionError("REPLAY_CAPABILITY", `Unavailable ${profile} operation: ${String(head)}`);
    if (head === "clock" && args.length) throw new KidLispExecutionError("REPLAY_CAPABILITY", "Replay clock is numeric; audio needs a recorded media host");
    if (head === "def" && (args.length !== 2 || typeof args[0] !== "string")) throw new KidLispExecutionError("REPLAY_CAPABILITY", "commands-v1 supports value definitions");
    args.forEach(visit);
  };
  forms.forEach(visit);
}

export function replay({ source, seed = 0, epochMs = 0, stepMs = 1000 / 60, frames = 60, width = 128, height = 128, events = [], maxSteps = 100000, maxDepth = 128, monitor = false }) {
  if (!Number.isInteger(frames) || frames < 1 || frames > 10000) throw new RangeError("Replay frames must be 1..10000");
  if (![width, height].every(n => Number.isInteger(n) && n > 0 && n <= 4096)) throw new RangeError("Invalid replay viewport");
  if (!Array.isArray(events) || events.length > 10000) throw new RangeError("Invalid replay input count");
  const byFrame = new Map();
  for (const event of events) {
    if (!Number.isInteger(event.frame) || event.frame < 0 || event.frame >= frames || !["touch", "draw", "lift"].includes(event.type) || ![event.x, event.y, event.dx ?? 0, event.dy ?? 0].every(Number.isFinite)) throw new TypeError("Invalid replay input event");
    if (!byFrame.has(event.frame)) byFrame.set(event.frame, []);
    byFrame.get(event.frame).push({ ...event });
  }
  const execution = new KidLispExecution({ seed, epochMs, stepMs, maxSteps, maxDepth });
  const lisp = new KidLisp({ execution });
  // Validate before module construction: unsupported source must not resolve
  // cached programs or initiate any host/network work.
  const parsed = lisp.parse(source);
  validateReplayForms(parsed);
  const piece = lisp.module(source, true);
  if (!piece || !lisp.ast || lisp.lastValidationErrors?.length) throw new KidLispExecutionError("REPLAY_SOURCE", "Replay needs valid source in commands-v1");
  lisp.perf.enabled = monitor;
  const output = [];
  let calls;
  const api = {
    screen: { width, height }, clock: execution.clock, needsPaint() {}, toggleHUD() {},
    send() { throw new KidLispExecutionError("REPLAY_CAPABILITY", "Replay has no external messaging host"); },
  };
  for (const name of ["wipe", "ink", "line", "box", "circle", "point", "plot", "write"]) {
    api[name] = (...args) => { calls.push([name, ...args.map(traceValue)]); return api; };
  }
  try {
    for (let frame = 0; frame < frames; frame++) {
      execution.beginFrame(frame);
      lisp.frameCount = frame;
      api.paintCount = frame;
      calls = [];
      const bound = execution.bindApi(api);
      for (const event of byFrame.get(frame) || []) {
        piece.act({ api: bound, event: { ...event, delta: { x: event.dx ?? 0, y: event.dy ?? 0 }, is: type => event.type === type } });
      }
      lisp.evaluate(lisp.ast, bound, undefined, undefined, true);
      output.push({ frame, timeMs: execution.timeMs, calls });
    }
  } finally { piece.leave?.(); }
  return { version: 1, profile: "commands-v1", seed, epochMs, stepMs, width, height, frames: output };
}

// JSON fixture in; a stable command trace and SHA-256 out. No browser or service.
if (typeof process !== "undefined" && process.argv[1] && import.meta.url === (await import("node:url")).pathToFileURL(process.argv[1]).href) {
  const { readFileSync } = await import("node:fs");
  const { createHash } = await import("node:crypto");
  if (!process.argv[2]) throw new Error("usage: node kidlisp/conformance/replay.mjs fixture.json");
  const result = replay(JSON.parse(readFileSync(process.argv[2], "utf8")));
  const hash = createHash("sha256").update(JSON.stringify(result)).digest("hex");
  process.stdout.write(JSON.stringify({ ...result, sha256: hash }, null, 2) + "\n");
}
