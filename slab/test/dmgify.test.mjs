import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { planDmg } from "../bin/dmgify.mjs";

test("dmgify plans a local HTML directory without mutating it", async () => {
  const dir = await mkdtemp(join(homedir(), ".dmgify-test-"));
  await writeFile(join(dir, "index.html"), "<!doctype html><title>Test</title>");
  await mkdir(join(dir, "media"));
  await writeFile(join(dir, "media/example.jpg"), "image");
  const plan = await planDmg({ source: dir, name: "Test Archive", bundleId: "computer.aesthetic.testarchive" });
  assert.equal(plan.entry, "index.html");
  assert.equal(plan.name, "Test Archive");
  assert.equal(plan.bundleId, "computer.aesthetic.testarchive");
  assert.equal(plan.payload.files, 2);
  assert.ok(plan.developerId?.name.startsWith("Developer ID Application:"));
});

test("dmgify rejects entry traversal", async () => {
  await assert.rejects(
    () => planDmg({ source: homedir(), entry: "../outside.html", bundleId: "computer.aesthetic.bad" }),
    /entry may not traverse/,
  );
});

test("dmgify plans a native Swift gallery from manifest.json", async () => {
  const dir = await mkdtemp(join(homedir(), ".dmgify-native-test-"));
  await writeFile(join(dir, "manifest.json"), JSON.stringify({
    title: "Native Test", account: "test", counts: { posts: 0, stills: 0 }, posts: [],
  }));
  const plan = await planDmg({
    source: dir, runtime: "swift-gallery", name: "Native Test", bundleId: "computer.aesthetic.nativetest",
  });
  assert.equal(plan.runtime, "swift-gallery");
  assert.equal(plan.entryPath, join(dir, "manifest.json"));
});
