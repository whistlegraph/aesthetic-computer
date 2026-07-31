import assert from "node:assert/strict";
import test from "node:test";
import { IRIS_ADDRESS, aggregateMachineHeartbeat, boundedNudge,
  makeIrisContact, parseAgentAddress, resolveFleetMachine } from "../lib/loopboy-family.mjs";

test("Iris is an agent contact with the locked Joeydon recovery guardrails", () => {
  const iris = makeIrisContact();
  assert.equal(iris.address, IRIS_ADDRESS);
  assert.deepEqual(parseAgentAddress(iris.address), { kind: "agent", name: "iris", machine: "panda" });
  assert.equal(iris.responsibility.executionHost, "chicken");
  assert.equal(iris.responsibility.preserveOriginal, true);
  assert.match(iris.responsibility.verification, /chunked-resource/);
  assert.match(iris.responsibility.guardrail, /must not outrank/);
});

test("agent nudges are bounded and cannot be confused with iMessage routes", () => {
  const nudge = boundedNudge(makeIrisContact(), "  report recovery evidence  ");
  assert.equal(nudge.to, IRIS_ADDRESS);
  assert.equal(nudge.text, "report recovery evidence");
  assert.throws(() => boundedNudge({ kind: "human" }, "hello"), /agent contact/);
  assert.throws(() => boundedNudge(makeIrisContact(), "x".repeat(501)), /500/);
});

test("fleet addressing uses registry identities and aliases, not a host list", () => {
  const registry = { machines: { "office-panda": { tailscale: { name: "panda" }, hostname: "panda.local" } } };
  assert.equal(resolveFleetMachine("PANDA", registry)?.id, "office-panda");
  assert.equal(resolveFleetMachine("missing", registry), null);
});

test("machine heartbeat distinguishes active, quiet, stalled, offline, and real progress", () => {
  const now = 1_000_000;
  assert.equal(aggregateMachineHeartbeat({ known: false }).state, "unknown");
  assert.equal(aggregateMachineHeartbeat({ online: false }).state, "offline");
  assert.equal(aggregateMachineHeartbeat({ online: true, now, machineUpdated: now - 50_000,
    rocks: [{ status: "working", updated: now - 10_000 }] }).state, "active");
  assert.equal(aggregateMachineHeartbeat({ online: true, now, machineUpdated: now - 1_000_000 }).state, "quiet");
  assert.equal(aggregateMachineHeartbeat({ online: true, now, mission: { blocked: true, updatedAt: now } }).state, "stalled");
  assert.equal(aggregateMachineHeartbeat({ online: true, now, mission: { bounded: false, progress: .9, updatedAt: now } }).boundedProgress, null);
  assert.equal(aggregateMachineHeartbeat({ online: true, now, mission: { bounded: true, progress: .4, updatedAt: now } }).boundedProgress, .4);
});
