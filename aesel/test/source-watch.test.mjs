import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { isSourceChange, watchSource } from "../src/source-watch.mjs";

test("source files count, build outputs and other files do not", () => {
  for (const name of ["tui.mjs", "media/sound.mjs", "terminal-help.txt", "model-catalog.json"]) assert.ok(isSourceChange(name), name);
  for (const name of [".tui-built.mjs", ".tui-built.json", ".tui-bun.cjs", "notes.md", "", "out.wav"]) assert.ok(!isSourceChange(name), name);
});

test("a burst of saves settles into one change, nested files included", async () => {
  const dir = mkdtempSync(path.join(tmpdir(), "aesel-src-"));
  mkdirSync(path.join(dir, "media"));
  const calls = [];
  const stop = watchSource(dir, (files) => calls.push(files.sort()), { settleMs: 60 });
  assert.equal(typeof stop, "function");
  await new Promise((resolve) => setTimeout(resolve, 100));
  writeFileSync(path.join(dir, ".tui-built.mjs"), "ignored");
  writeFileSync(path.join(dir, "tui.mjs"), "1");
  writeFileSync(path.join(dir, "tui.mjs"), "2");
  writeFileSync(path.join(dir, "media", "sound.mjs"), "3");
  await new Promise((resolve) => setTimeout(resolve, 400));
  stop();
  writeFileSync(path.join(dir, "tui.mjs"), "after");
  await new Promise((resolve) => setTimeout(resolve, 200));
  assert.equal(calls.length, 1);
  assert.deepEqual(calls[0], [path.join("media", "sound.mjs"), "tui.mjs"]);
});
