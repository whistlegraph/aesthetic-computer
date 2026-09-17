import assert from "node:assert/strict";
import test from "node:test";
import { serveShell } from "./shell.mjs";

test("the render server serves the game shell's complete static import graph", async () => {
  const shell = await serveShell();
  try {
    const pending = [new URL("/", shell.origin)];
    const seen = new Set();
    while (pending.length) {
      const url = pending.pop();
      if (seen.has(url.href)) continue;
      seen.add(url.href);
      const response = await fetch(url);
      assert.equal(response.status, 200, `missing shell dependency: ${url.pathname}`);
      assert.match(response.headers.get("content-type"), /(?:html|javascript)/);
      const source = await response.text();
      for (const match of source.matchAll(/\b(?:from\s*|import\s*)["']([^"']+)["']/g)) {
        const specifier = match[1];
        if (!specifier.startsWith("/") && !specifier.startsWith(".")) continue;
        const dependency = new URL(specifier, url);
        if (dependency.origin === shell.origin) pending.push(dependency);
      }
    }
    assert.ok(seen.size > 5, "the test must traverse the module graph");
  } finally {
    await shell.close();
  }
});
