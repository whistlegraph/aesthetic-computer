import assert from "node:assert/strict";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";
import { execFile } from "node:child_process";
import test from "node:test";
import { SoupHistory } from "../src/soup-history.mjs";

const run = promisify(execFile);

test("soup history restores behavior and commits compact local editions", async () => {
  const root = await mkdtemp(join(tmpdir(), "sort-soup-history-"));
  const opened = await SoupHistory.open(root, { seed: "history-seed" });
  assert.deepEqual(opened.history.snapshot(), {
    head: null, shortHead: null, editions: 0, lastEdition: null,
  });
  opened.archive.seedClassics();
  for (let i = 0; i < 12; i += 1) opened.archive.mutate();
  const expected = opened.archive.snapshot();
  const commit = await opened.history.save(opened.archive, { commit: true, reason: "test" });
  assert.match(commit, /^[a-f0-9]{40}$/);
  assert.deepEqual(opened.history.snapshot(), {
    head: commit, shortHead: commit.slice(0, 8), editions: 1,
    lastEdition: {
      schema: 1, reason: "test", iteration: expected.iteration,
      accepted: expected.accepted, rejected: expected.rejected,
      coverage: expected.coverage, capacity: expected.capacity,
      selected: expected.selected,
      pieceVm: null,
    },
  });
  const restored = await SoupHistory.open(root, { seed: "ignored-after-restore" });
  assert.equal(restored.restored, true);
  assert.deepEqual(restored.archive.snapshot(), expected);
  assert.deepEqual(restored.history.snapshot(), opened.history.snapshot());
  assert.deepEqual(await restored.history.pieceVmLineage(), []);
  const files = (await run("git", ["-C", root, "show", "--name-only", "--format="])).stdout.trim().split("\n");
  assert.deepEqual(files, ["archive.json", "edition.json"]);
});
