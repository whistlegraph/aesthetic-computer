import test from "node:test";
import assert from "node:assert/strict";
import { boot, target } from "../public/aesthetic.computer/disks/oskiewar.mjs";

test("the AC piece jumps out to the standalone OSKIEWAR suite", () => {
  const destinations = [];
  boot({ jump: (url) => destinations.push(url) });
  assert.deepEqual(destinations, ["out:https://oskiewar.com/"]);
});

test("a colon round opens the same raw room URL", () => {
  const name = "bafegu-dorimi-kunapo";
  assert.equal(target({ colon: [name] }), `https://oskiewar.com/${name}`);
  assert.equal(target({ params: [`ow-${name}`] }), `https://oskiewar.com/${name}`);
  assert.equal(target({ colon: ["not-a-round"] }), "https://oskiewar.com/");
});
