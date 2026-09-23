import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { Layout, normalize } from "../src/layout.mjs";

function scratch(context) {
  const root = mkdtempSync(join(tmpdir(), "layout-"));
  context.after(() => rmSync(root, { recursive: true, force: true }));
  mkdirSync(join(root, "pkg", "layouts"), { recursive: true });
  writeFileSync(join(root, "pkg", "layouts", "pro.json"), JSON.stringify({ bottom: ["bar", "status"], status: ["model"], prompt: ">" }));
  return { root: join(root, "pkg"), file: join(root, "cfg", "layout.json") };
}

test("the baked default is read, the override merged, and nonsense dropped", (context) => {
  const { root, file } = scratch(context);
  const layout = new Layout({ root, file });
  assert.deepEqual(layout.spec.bottom, ["bar", "status"]);
  assert.deepEqual(layout.spec.status, ["model"]);
  assert.equal(layout.spec.prompt, ">");
  assert.equal(layout.spec.separator, " · ", "unset keys keep the built-in value");
  mkdirSync(join(root, "..", "cfg"), { recursive: true });
  writeFileSync(file, JSON.stringify({ status: ["handle", "bogus", "model"], bar: [1, 2], prompt: "way too long" }));
  layout.load();
  assert.deepEqual(layout.spec.status, ["handle", "model"], "an unknown fact is skipped, not fatal");
  assert.deepEqual(layout.spec.bar, [95, 70, 135], "a colour that is not three channels is ignored");
  assert.equal(layout.spec.prompt, ">", "a glyph longer than two cells is ignored");
  assert.deepEqual(normalize({ bottom: "gap" }), {}, "a list that is not a list is nothing");
});

test("a saved edit reaches the running session as a change", async (context) => {
  const { root, file } = scratch(context);
  const layout = new Layout({ root, file, interval: 50 });
  context.after(() => layout.close());
  layout.watch();
  const changed = new Promise((resolve) => layout.once("change", resolve));
  // The poller's first look is its baseline; a file written before that look
  // is part of the baseline and never a change. Let it look first.
  await new Promise((resolve) => setTimeout(resolve, 150));
  mkdirSync(join(root, "..", "cfg"), { recursive: true });
  writeFileSync(file, JSON.stringify({ bottom: ["gap", "status", "gap", "bar"] }));
  const spec = await changed;
  assert.deepEqual(spec.bottom, ["gap", "status", "gap", "bar"]);
});

test("set writes one key to the override and reset takes it back", (context) => {
  const { root, file } = scratch(context);
  const layout = new Layout({ root, file });
  const seen = [];
  layout.on("change", (spec) => seen.push(spec.status));
  assert.deepEqual(layout.set("status", "handle, model, mode"), ["handle", "model", "mode"]);
  assert.deepEqual(JSON.parse(readFileSync(file, "utf8")), { status: ["handle", "model", "mode"] });
  assert.throws(() => layout.set("bar", "purple"), /does not take/);
  layout.reset();
  assert.deepEqual(layout.spec.status, ["model"]);
  assert.equal(seen.length, 2);
});

test("bake writes the shape into the package and commits that one file", async (context) => {
  const { root, file } = scratch(context);
  // The scratch repo commits unsigned: a signing key, or a gpg agent another
  // process is holding, is not what this test is about.
  const unsigned = { GIT_CONFIG_COUNT: "1", GIT_CONFIG_KEY_0: "commit.gpgsign", GIT_CONFIG_VALUE_0: "false" };
  for (const [key, value] of Object.entries(unsigned)) process.env[key] = value;
  context.after(() => { for (const key of Object.keys(unsigned)) delete process.env[key]; });
  execFileSync("git", ["-C", root, "init", "-q"]);
  execFileSync("git", ["-C", root, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "--allow-empty", "-m", "start"]);
  writeFileSync(join(root, "stray.txt"), "not mine to commit");
  const layout = new Layout({ root, file });
  layout.set("status", "handle,model");
  const result = await layout.bake();
  assert.match(result.commit, /^[0-9a-f]{7,}$/);
  assert.deepEqual(JSON.parse(readFileSync(join(root, "layouts", "pro.json"), "utf8")).status, ["handle", "model"]);
  assert.equal(execFileSync("git", ["-C", root, "status", "--porcelain"], { encoding: "utf8" }).trim(), "?? stray.txt", "only the layout was committed");
  assert.ok(!layout.override.status, "the override is gone once it is the default");
  assert.deepEqual(layout.spec.status, ["handle", "model"], "and the session looks the same");
});
