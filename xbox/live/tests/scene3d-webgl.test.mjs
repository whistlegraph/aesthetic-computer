import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../scene3d-webgl.mjs", import.meta.url),
  "utf8");

test("web scene renderer requests and clears a real depth buffer", () => {
  assert.match(source, /getContext\("webgl2"/);
  assert.match(source, /depth:\s*true/);
  assert.match(source, /enable\(gl\.DEPTH_TEST\)/);
  assert.match(source, /depthFunc\(gl\.LEQUAL\)/);
  assert.match(source, /COLOR_BUFFER_BIT\s*\|\s*gl\.DEPTH_BUFFER_BIT/);
});
test("web scene renderer streams one interleaved triangle buffer", () => {
  assert.match(source, /bufferSubData\(gl\.ARRAY_BUFFER/);
  assert.match(source, /drawArrays\(gl\.TRIANGLES/);
});
