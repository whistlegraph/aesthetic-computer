import assert from "node:assert/strict";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { Terrarium } from "../src/core.mjs";
import { parseReflectionOutput, reflectionPrompt, ReflectionOrgan } from "../src/reflection.mjs";
import { StateRepository, verifyRepository } from "../src/repository.mjs";

async function repository(prefix) {
  const root = await mkdtemp(join(tmpdir(), prefix));
  const state = await StateRepository.create(root, { seed: "reflection-test", profile: "1gb" });
  state.segmentPath = join(root, "journal", "segments", "reflection.ndjson");
  return { root, state };
}

test("bounded reflection accepts a schema proposal without exposing selected-memory secrets", async () => {
  const { root, state } = await repository("terrarium-reflect-accept-");
  const secret = "private-token-and-email@example.test";
  await state.transact("visitor-enter", { handle: "@alex", position: {} });
  await state.transact("visitor-signal", { handle: "@alex", signal: secret });
  let captured;
  const reflection = new ReflectionOrgan(state, {
    contextTokens: 99_999,
    maxOutputTokens: 999,
    timeoutMs: 99_999,
    engine: "qwen3-test",
    infer: async (request) => {
      captured = request;
      return {
        text: '<think>not persisted</think>\n{"schema":1,"action":"attune","target":"sensory","intensity":0.125}',
        metrics: { latencyMs: 12, outputTokensPerSecond: 34 },
      };
    },
  });
  const result = await reflection.reflect();
  assert.equal(result.decision, "accepted");
  assert.equal(result.contextTokens, 2048);
  assert.equal(result.maxOutputTokens, 128);
  assert.equal(result.timeoutMs, 30_000);
  assert.ok(result.promptChars < 6000);
  assert.equal(captured.contextTokens, 2048);
  assert.doesNotMatch(captured.prompt, /@alex|private-token|example\.test/);
  const journal = await readFile(state.segmentPath, "utf8");
  assert.match(journal, /"kind":"reflection-decision"/);
  assert.match(journal, /"decision":"accepted"/);
  const reflectionRecord = journal.trim().split("\n").at(-1);
  assert.doesNotMatch(reflectionRecord, /private-token|example\.test|not persisted/);
  const replay = await verifyRepository(root);
  assert.equal(replay.stateHash, state.stateHash());
  assert.equal(replay.headRecordHash, state.headRecordHash);
});

test("authority deterministically rejects an unsafe but schema-valid proposal", async () => {
  const { root, state } = await repository("terrarium-reflect-reject-");
  const result = await new ReflectionOrgan(state, {
    infer: async () => '{"schema":1,"action":"broadcast","target":"voice","intensity":0.1}',
  }).reflect();
  assert.equal(result.decision, "rejected");
  assert.equal(result.reason, "action-not-authorized");
  assert.equal((await verifyRepository(root)).stateHash, state.stateHash());

  const candidate = Terrarium.fromSnapshot(state.terrarium.snapshot());
  assert.throws(() => candidate.apply({
    seq: candidate.state.lastSeq + 1,
    kind: "reflection-decision",
    payload: {
      schema: 1,
      requestId: "a".repeat(24),
      engine: "test",
      outputDigest: "b".repeat(64),
      proposal: { schema: 1, action: "broadcast", target: "voice", intensity: 0.1 },
      decision: "accepted",
      reason: "bounded-attunement",
    },
  }), /does not match deterministic policy/);
});

test("parser selects the final proposal when llama-cli echoes prompt JSON", async () => {
  const { state } = await repository("terrarium-reflect-echo-");
  assert.throws(() => parseReflectionOutput(reflectionPrompt(state)), /no valid proposal/);
  const result = await new ReflectionOrgan(state, {
    infer: async () => [
      '> Schema: {"schema":1,"action":"attune","target":"sensory","intensity":0.2}',
      '{"schema":1,"action":"attune","target":"voice","intensity":0.05}',
      "[ Prompt: 205.2 t/s | Generation: 24.9 t/s ]",
    ].join("\n"),
  }).reflect();
  assert.equal(result.decision, "accepted");
  assert.equal(result.record.payload.proposal.target, "voice");
});

test("malformed, timeout, and disabled inference unconditionally fall back", async () => {
  const malformed = await repository("terrarium-reflect-malformed-");
  const malformedResult = await new ReflectionOrgan(malformed.state, {
    infer: async () => '{"schema":1,"action":"attune","target":"memory","intensity":0.1,"secret":"no"}',
  }).reflect();
  assert.deepEqual([malformedResult.decision, malformedResult.reason], ["fallback", "malformed"]);

  const timeout = await repository("terrarium-reflect-timeout-");
  const timeoutResult = await new ReflectionOrgan(timeout.state, {
    timeoutMs: 100,
    infer: ({ signal }) => new Promise((resolve, reject) => {
      signal.addEventListener("abort", () => {
        const error = new Error("aborted test inference");
        error.name = "AbortError";
        reject(error);
      }, { once: true });
    }),
  }).reflect();
  assert.deepEqual([timeoutResult.decision, timeoutResult.reason], ["fallback", "timeout"]);

  const disabled = await repository("terrarium-reflect-disabled-");
  const disabledResult = await new ReflectionOrgan(disabled.state).reflect();
  assert.deepEqual([disabledResult.decision, disabledResult.reason], ["fallback", "disabled"]);
  await disabled.state.transact("advance", { ticks: 1 });
  assert.equal((await verifyRepository(disabled.root)).stateHash, disabled.state.stateHash());
});

test("reflection inference admits only one request at a time", async () => {
  const { state } = await repository("terrarium-reflect-queue-");
  let active = 0;
  let peak = 0;
  const reflection = new ReflectionOrgan(state, {
    infer: async () => {
      active += 1;
      peak = Math.max(peak, active);
      await new Promise((resolve) => setTimeout(resolve, 10));
      active -= 1;
      return '{"schema":1,"action":"attune","target":"drive","intensity":0.01}';
    },
  });
  const results = await Promise.all([reflection.reflect(), reflection.reflect(), reflection.reflect()]);
  assert.equal(peak, 1);
  assert.deepEqual(results.map(({ decision }) => decision), ["accepted", "accepted", "accepted"]);
  assert.deepEqual(state.records.map(({ seq }) => seq), [1, 2, 3]);
});
