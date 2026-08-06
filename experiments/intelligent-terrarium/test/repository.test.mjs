import assert from "node:assert/strict";
import { appendFile, mkdtemp, readFile, readdir, writeFile } from "node:fs/promises";
import { execFileSync } from "node:child_process";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { StateRepository, verifyRepository } from "../src/repository.mjs";
import { sleepCommit } from "../src/sleep.mjs";

test("journal replays and quarantines a truncated final record", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-recovery-"));
  const repository = await StateRepository.create(root, { seed: "recover-me" });
  await repository.transact("advance", { ticks: 90 });
  const expected = repository.stateHash();
  await appendFile(repository.segmentPath, '{"schema":1,"seq":2');

  const reopened = await StateRepository.open(root);
  assert.equal(reopened.stateHash(), expected);
  assert.equal(reopened.recoveries.length, 1);
  assert.ok(reopened.recoveries[0].recoveredBytes > 0);
  const quarantine = await readdir(join(root, "quarantine"));
  assert.equal(quarantine.length, 1);
  assert.doesNotMatch(await readFile(repository.segmentPath, "utf8"), /"seq":2$/);
});

test("tampered journal content fails hash verification", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-tamper-"));
  const repository = await StateRepository.create(root, { seed: "tamper-proof" });
  await repository.transact("advance", { ticks: 25 });
  const original = await readFile(repository.segmentPath, "utf8");
  await writeFile(repository.segmentPath, original.replace('"ticks":25', '"ticks":26'));
  await assert.rejects(StateRepository.open(root), /record hash mismatch|state hash mismatch/);
});

test("journal replay follows sequence rather than segment filename order", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-segment-order-"));
  const repository = await StateRepository.create(root, { seed: "segment-order" });
  repository.segmentPath = join(root, "journal", "segments", "zzz-first.ndjson");
  await repository.transact("advance", { ticks: 3 });
  repository.segmentPath = join(root, "journal", "segments", "aaa-second.ndjson");
  await repository.transact("advance", { ticks: 4 });
  const replay = await verifyRepository(root);
  assert.equal(replay.lastSeq, 2);
  assert.equal(replay.stateHash, repository.stateHash());
  assert.equal(replay.headRecordHash, repository.headRecordHash);
});

test("sleep commits only allowlisted state and no-op sleep makes no commit", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-sleep-"));
  const repository = await StateRepository.create(root, { seed: "sleep-cycle" });
  await writeFile(join(root, "do-not-stage.txt"), "unrelated\n");
  await repository.transact("visitor-enter", { handle: "@alex", position: { x: 1, y: 1, z: 1 } });
  await repository.transact("advance", { ticks: 120 });

  const first = await sleepCommit(repository, { day: "2026-07-23" });
  assert.equal(first.status, "committed");
  assert.equal(first.stateHash, repository.stateHash());
  const committed = execFileSync("git", ["show", "--pretty=", "--name-only", "HEAD"], { cwd: root, encoding: "utf8" });
  assert.doesNotMatch(committed, /do-not-stage/);
  assert.match(execFileSync("git", ["status", "--short"], { cwd: root, encoding: "utf8" }), /\?\? do-not-stage\.txt/);

  const countBefore = execFileSync("git", ["rev-list", "--count", "HEAD"], { cwd: root, encoding: "utf8" }).trim();
  const second = await sleepCommit(repository, { day: "2026-07-23" });
  const countAfter = execFileSync("git", ["rev-list", "--count", "HEAD"], { cwd: root, encoding: "utf8" }).trim();
  assert.equal(second.status, "no-change");
  assert.equal(countBefore, countAfter);

  await repository.transact("advance", { ticks: 10 });
  const third = await sleepCommit(repository, { day: "2026-07-23" });
  assert.equal(third.status, "committed");
  assert.notEqual(third.commit, first.commit);
  assert.deepEqual(await verifyRepository(root), {
    stateHash: repository.stateHash(),
    lastSeq: repository.terrarium.state.lastSeq,
    headRecordHash: repository.headRecordHash,
    recoveries: [],
  });
});
