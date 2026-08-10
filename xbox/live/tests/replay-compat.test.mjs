import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import {
  inputTimeline, migrateReplay, playbackPlan, replayDigest,
  simulationContracts, timelineDigest, validateCompatibleReplay,
  validateFixtureManifest,
} from "../replay/compat.mjs";

const fixture = async (name) => JSON.parse(await readFile(new URL(
  `../replay/fixtures/${name}.json`, import.meta.url), "utf8"));

test("an old build migrates without changing its deterministic input ticks", async () => {
  const old = await fixture("legacy-v0");
  const migrated = migrateReplay(old);
  assert.equal(migrated.version, 1);
  assert.equal(migrated.simulation, "oskiewar-physics-1");
  assert.deepEqual(inputTimeline(old), [
    [0, 2, 0], [1, 2, 0], [2, 2, 16], [3, 2, 16], [4, 0, 16],
    [5, 0, 16], [6, 0, 16], [7, 0, 0], [8, 0, 0],
  ]);
  assert.equal(timelineDigest(old),
    "a6d46e00073439d314fce7d278a2827078a9fcedc6c1eb3eb22c01e34aea330b");
});

test("playback is pinned to an archived simulation, not the current build", async () => {
  const old = await fixture("legacy-v0");
  const plan = playbackPlan(old, 900);
  assert.equal(plan.recordingBuild, 37);
  assert.equal(plan.playerBuild, 900);
  assert.equal(plan.simulation, "oskiewar-physics-1");
  assert.equal(plan.clock.tickUs, 16667);
  assert.equal(plan.timeline.length, 9);
});

test("unknown old physics fail loudly instead of playing inaccurately", async () => {
  const demo = await fixture("v1");
  demo.simulation = "oskiewar-physics-0-lost";
  assert.deepEqual(validateCompatibleReplay(demo), {
    ok: false, error: "Archived simulation unavailable: oskiewar-physics-0-lost",
  });
  assert.throws(() => playbackPlan(demo, 500), /Archived simulation unavailable/);
});

test("current envelope has a stable canonical and timeline digest", async () => {
  const demo = await fixture("v1");
  assert.equal(simulationContracts[demo.simulation].inputEncoding,
    "oskiewar-buttons-v1");
  assert.equal(replayDigest(demo),
    "ca36d27675af75123121de9b3a72af1af2a7bb21e6ade38798ce2ce470022437");
  assert.equal(timelineDigest(demo),
    "865d88c02a15a9fb9328f674e2811cee08f7971edb724fbb16df2ad2760b98c5");
});

test("wall-clock order cannot alter deterministic command order", async () => {
  const demo = await fixture("v1");
  demo.commands = [[2, 0, 1], [1, 0, 0]];
  assert.equal(validateCompatibleReplay(demo).error,
    "Command timeline is not monotonic");
});

test("versioned fixture ledger pins old recordings and compatibility builds", async () => {
  const manifest = await fixture("manifest");
  assert.equal(validateFixtureManifest(manifest).ok, true);
  for (const item of manifest.fixtures) {
    const demo = await fixture(item.file.replace(/\.json$/, ""));
    assert.equal(demo.build, item.recordingBuild);
    assert.equal(replayDigest(demo), item.replayDigest);
    assert.equal(timelineDigest(demo), item.timelineDigest);
    assert.doesNotThrow(() => playbackPlan(demo, item.minimumPlayerBuild));
  }
});
