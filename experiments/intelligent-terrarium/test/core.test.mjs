import assert from "node:assert/strict";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { LIMITS, outputHash, Terrarium } from "../src/core.mjs";
import { StateRepository } from "../src/repository.mjs";

async function scripted(root) {
  const repository = await StateRepository.create(root, { seed: "same-seed" });
  await repository.transact("visitor-enter", { handle: "@alex", position: { x: -3, y: 1.6, z: 2 } });
  const signal = await repository.transact("visitor-signal", { handle: "@alex", signal: "listen" });
  const advance = await repository.transact("advance", { ticks: 240 });
  await repository.transact("visitor-move", { handle: "@alex", position: { x: 4, y: 1.6, z: -1 } });
  return { repository, outputs: [...signal.outputs, ...advance.outputs] };
}

test("same seed and events reproduce state and semantic sound", async () => {
  const parent = await mkdtemp(join(tmpdir(), "terrarium-core-"));
  const first = await scripted(join(parent, "a"));
  const second = await scripted(join(parent, "b"));
  assert.equal(first.repository.stateHash(), second.repository.stateHash());
  assert.equal(first.repository.headRecordHash, second.repository.headRecordHash);
  assert.equal(outputHash(first.outputs), outputHash(second.outputs));
  assert.ok(first.outputs.length > 1);
  for (const event of first.outputs) {
    assert.equal(event.kind, "sonic");
    assert.equal(event.source.length, 3);
    assert.ok(event.intensity >= 0 && event.intensity <= 1);
    assert.ok(event.radius > 0);
  }
});

test("active episodic memory stays bounded", () => {
  const terrarium = new Terrarium("bounded");
  let seq = 1;
  terrarium.apply({ seq: seq++, kind: "visitor-enter", payload: { handle: "@alex" } });
  for (let index = 0; index < LIMITS.episodes + 80; index += 1) {
    terrarium.apply({ seq: seq++, kind: "visitor-signal", payload: { handle: "@alex", signal: `signal-${index}` } });
  }
  assert.equal(terrarium.state.mind.episodes.length, LIMITS.episodes);
  assert.equal(Object.keys(terrarium.state.visitors).length, 1);
});

test("unverified-looking handles and oversized advances are rejected", () => {
  const terrarium = new Terrarium("validation");
  assert.throws(() => terrarium.apply({ seq: 1, kind: "visitor-enter", payload: { handle: "alex/../../token" } }), /invalid verified handle/);
  assert.throws(() => terrarium.apply({ seq: 1, kind: "advance", payload: { ticks: LIMITS.advanceTicks + 1 } }), /advance ticks/);
});
