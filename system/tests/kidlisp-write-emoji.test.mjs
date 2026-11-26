import assert from "node:assert/strict";
import test from "node:test";
import { KidLisp } from "../public/aesthetic.computer/lib/kidlisp.mjs";

function evaluate(source) {
  const draws = [];
  const api = {
    screen: { width: 100, height: 80 },
    text: { width: value => value.length * 6, height: () => 10 },
    write(value, pos) { draws.push(["text", value, pos]); },
    paste(sprite, x, y, scale) { draws.push(["sprite", sprite.width, sprite.height, x, y, scale]); },
    box(...args) { draws.push(["box", ...args]); },
    ink(...args) { draws.push(["ink", ...args]); },
    inkrn: () => [1, 2, 3, 255],
  };
  const kidlisp = new KidLisp();
  kidlisp.evaluate(kidlisp.parse(source), api);
  return draws;
}

test("write centers scaled letters and color emoji as one transparent line", () => {
  assert.deepEqual(evaluate('(write "A🦃B" "center" "center" "transparent" 2)'), [
    ["text", "A", { x: 18, y: 29, size: 2 }],
    ["sprite", 19, 16, 30, 23, 2],
    ["text", "B", { x: 70, y: 29, size: 2 }],
  ]);
});

test("write paints the background across the entire mixed row and restores ink", () => {
  assert.deepEqual(evaluate('(write "🦃!" 4 5 "red" 1)'), [
    ["ink", "red"], ["box", 4, 5, 26, 17], ["ink", 1, 2, 3, 255],
    ["sprite", 19, 16, 4, 5, 1], ["text", "!", { x: 24, y: 8, size: 1 }],
  ]);
});

test("write keeps plain text on the native drawing path", () => {
  assert.deepEqual(evaluate('(write "hello" 2 3 "transparent" 4)'), [
    ["text", "hello", { x: 2, y: 3, size: 4 }],
  ]);
});
