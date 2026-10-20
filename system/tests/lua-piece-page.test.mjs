// A Lua piece's page is found, like a JavaScript or KidLisp piece's.
// Run from anywhere: node --test system/tests/lua-piece-page.test.mjs
import test from "node:test";
import assert from "node:assert/strict";
import { fileURLToPath } from "node:url";

// The index reads disks from ./public in dev, so stand in system/ first.
process.env.CONTEXT = "dev";
process.chdir(fileURLToPath(new URL("../", import.meta.url)));
const { handler } = await import("../netlify/functions/index.mjs");
const { inferTitleDesc } = await import("../public/aesthetic.computer/lib/parse.mjs");

const page = (path) => handler({ path, headers: { host: "aesthetic.computer" }, queryStringParameters: {} });

test("a Lua piece answers 200 whether or not the URL names its extension", async () => {
  for (const path of ["/l5-hello.lua", "/l5-hello"]) {
    const response = await page(path);
    assert.equal(response.statusCode, 200, path);
    assert.match(response.body, /boot\.mjs/);
  }
});

test("a piece that exists in no language is still a 404", async () => {
  const response = await page("/zzz-no-such-piece");
  assert.equal(response.statusCode, 404);
});

test("a first-line comment titles a file in any of the three languages", () => {
  assert.equal(inferTitleDesc("// Prompt, 2023\n// Type things.").title, "Prompt");
  assert.equal(inferTitleDesc("// Prompt, 2023\n// Type things.").desc, "Type things.");
  assert.deepEqual(inferTitleDesc("; Clock\n; Tells time."), { title: "Clock", desc: "Tells time.", standaloneTitle: true });
  assert.deepEqual(inferTitleDesc(";; Clock\n;; Tells time."), { title: "Clock", desc: "Tells time.", standaloneTitle: true });
  assert.deepEqual(inferTitleDesc("-- Hello\n-- A Lua pulse."), { title: "Hello", desc: "A Lua pulse.", standaloneTitle: true });
  assert.equal(inferTitleDesc("function setup()\nend").title, undefined);
});
