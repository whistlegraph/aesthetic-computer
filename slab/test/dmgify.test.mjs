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
