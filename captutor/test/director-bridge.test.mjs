import assert from "node:assert/strict";
import test from "node:test";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

test("director bridge yields to a fresh Captutor recording heartbeat", () => {
  const source = readFileSync(join(dirname(fileURLToPath(import.meta.url)), "../ops/director-bridge.mjs"), "utf8");
  assert.match(source, /current\?\.source === "captutor"/);
  assert.match(source, /current\.status === "recording"/);
  assert.match(source, /age < 7_000/);
  assert.match(source, /setInterval\(publish, 2_500\)/);
});
