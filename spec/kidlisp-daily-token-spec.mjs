// KidLisp regressions found while building the nightly daily token
// (marketing/podcast/bin/daily-token.mjs), which sets text in a piece.

import { KidLisp } from "../system/public/aesthetic.computer/lib/kidlisp.mjs";

// Run a source once against a recording API and return the calls it made.
function run(source) {
  const lisp = new KidLisp();
  const calls = [];
  const known = {
    clock: { time: () => new Date() },
    screen: { width: 256, height: 256 },
  };
  const api = new Proxy(known, {
    get: (target, key) =>
      key in target ? target[key] : (...args) => calls.push([key, ...args]),
  });
  lisp.evaluate(lisp.parse(source), api);
  return calls;
}

const writes = (calls) => calls.filter(([name]) => name === "write");

describe("🪙 KidLisp daily token", () => {
  describe("punctuation in strings", () => {
    it("keeps commas, semicolons, parens, and escaped quotes", () => {
      const [[, text, pos]] = writes(run('(write "hi, there; (ok) \\"yes\\"" 10 10)'));
      expect(text).toBe('hi, there; (ok) "yes"');
      expect(pos).toEqual({ x: 10, y: 10 });
    });

    it("keeps them on multi-line sources with trailing comments", () => {
      const calls = run('(wipe black)\n(write "a, b; (c)" 1 2) ; note, with (parens)\n(ink red)');
      expect(writes(calls).map(([, text]) => text)).toEqual(["a, b; (c)"]);
      expect(calls.some(([name]) => name === "ink")).toBe(true);
    });

    it("keeps apostrophes in double-quoted strings without validation errors", () => {
      const lisp = new KidLisp();
      lisp.parse(`(write "don't, won't" 1 1)`);
      expect(lisp.lastValidationErrors).toBeNull();
    });

    it("still splits comma one-liners outside strings", () => {
      const lisp = new KidLisp();
      expect(lisp.parse('wipe blue, ink red, write "a, b; c" 5 5')).toEqual([
        ["wipe", "blue"],
        ["ink", "red"],
        ["write", '"a, b; c"', 5, 5],
      ]);
      expect(lisp.parse("wipe blue, ink rainbow, repeat 100 line")).toEqual([
        ["wipe", "blue"],
        ["ink", "rainbow"],
        ["repeat", 100, "line"],
      ]);
    });
  });

  describe("if", () => {
    it("runs its body when a comparison is true", () => {
      const calls = run('(wipe black)(ink yellow)(if (< 1 2) (write "HELLO" 20 60))');
      expect(writes(calls)).toEqual([["write", "HELLO", { x: 20, y: 60 }]]);
      // A bare comparison used to evaluate an empty body: checkerboard + falsy.
      expect(calls.some(([name]) => name === "box")).toBe(false);
    });

    it("runs every body form, with no else", () => {
      const calls = run('(if (> 2 1) (write "A" 1 1) (write "B" 2 2))');
      expect(writes(calls).map(([, text]) => text)).toEqual(["A", "B"]);
    });

    it("skips its body when false", () => {
      expect(writes(run('(if (> 1 2) (write "no" 1 1))'))).toEqual([]);
    });

    it("answers true from bare comparisons", () => {
      const calls = run("(write (< 1 2) 1 1) (write (> 1 2) 1 1)");
      expect(writes(calls).map(([, text]) => text)).toEqual(["true", "false"]);
    });
  });

  describe("write with no background", () => {
    for (const word of ["nil", "false", "transparent", "clear", '"transparent"']) {
      it(`treats ${word} as no box while still passing a size`, () => {
        const [call] = writes(run(`(write "HI" 10 10 ${word} 2)`));
        expect(call).toEqual(["write", "HI", { x: 10, y: 10, size: 2 }]);
      });
    }

    it("still paints a named background", () => {
      const [call] = writes(run('(write "HI" 10 10 red 2)'));
      expect(call[3]).toEqual({ bg: [255, 0, 0] });
    });
  });
});
