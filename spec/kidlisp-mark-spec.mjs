// `mark`: a pressure stroke. A smooth curve through x y thickness triplets.

import { KidLisp } from "../system/public/aesthetic.computer/lib/kidlisp.mjs";

// Run a source once against a recording API and return the calls it made.
function run(source, frame = 0) {
  const lisp = new KidLisp();
  lisp.frameCount = frame;
  const calls = [];
  const known = { clock: { time: () => new Date() }, screen: { width: 400, height: 400 }, paintCount: frame };
  const api = new Proxy(known, {
    get: (target, key) => (key in target ? target[key] : (...args) => calls.push([key, ...args])),
  });
  lisp.evaluate(lisp.parse(source), api);
  return calls;
}

const quads = (calls) => calls.filter(([name]) => name === "shape").map(([, s]) => s);
const caps = (calls) => calls.filter(([name]) => name === "circle");
const width = (q) => Math.hypot(q.points[0] - q.points[6], q.points[1] - q.points[7]);

describe("🖌️ KidLisp mark", () => {
  it("draws a ribbon of filled quads with round ends", () => {
    const calls = run("(mark 50 200 10 200 200 10 350 200 10)");
    expect(quads(calls).length).toBeGreaterThan(7);
    expect(quads(calls).every((q) => q.filled && q.points.length === 8)).toBe(true);
    expect(caps(calls).map(([, x, y, r]) => [x, y, r])).toEqual([[50, 200, 5], [350, 200, 5]]);
  });

  it("carries the thickness of each point, eased between them", () => {
    const q = quads(run("(mark 50 200 4 200 200 40 350 200 4)"));
    expect(width(q[0])).toBeCloseTo(4, 0);
    const widest = Math.max(...q.map(width));
    expect(widest).toBeGreaterThan(36);
    expect(widest).toBeLessThanOrEqual(40.01);
    expect(width(q[q.length - 1])).toBeLessThan(12);
  });

  it("bends: a point off the line pulls the ribbon to it", () => {
    const q = quads(run("(mark 50 300 6 200 100 6 350 300 6)"));
    const top = Math.min(...q.flatMap((s) => s.points.filter((_, i) => i % 2 === 1)));
    expect(top).toBeLessThan(105);
    expect(top).toBeGreaterThan(90);
  });

  it("takes the same expressions the other shapes take", () => {
    const calls = run("(mark w/2 (- h/2 100) 8 (+ w/2 (* 20 (sin frame))) h/2 20 w/2 (+ h/2 100) 8)", 30);
    expect(quads(calls).length).toBeGreaterThan(7);
    expect(caps(calls)[0].slice(1, 4)).toEqual([200, 100, 4]);
  });

  it("draws nothing from a malformed list", () => {
    expect(quads(run("(mark 10 10 4 20 20)")).length).toBe(0);
    expect(quads(run("(mark 10 10 4)")).length).toBe(0);
  });
});

// Thick outlines: the graphics library fills the whole shape (or draws nothing
// for a box) when asked for an outline wider than one pixel, so the evaluator
// draws them itself, as quads along the edge.
describe("🖊️ KidLisp thick outlines", () => {
  const edgeOnly = (source, inside) => {
    const q = quads(run(source));
    expect(q.length).toBeGreaterThan(2);
    expect(q.every((s) => s.filled && s.points.length === 8)).toBe(true);
    // No quad covers the middle of the shape.
    const [mx, my] = inside;
    const covers = (p) => { let c = false; for (let i = 0, j = 3; i < 4; j = i++) { const [xi, yi, xj, yj] = [p[i * 2], p[i * 2 + 1], p[j * 2], p[j * 2 + 1]]; if (yi > my !== yj > my && mx < ((xj - xi) * (my - yi)) / (yj - yi) + xi) c = !c; } return c; };
    expect(q.some((s) => covers(s.points))).toBe(false);
  };
  it("circle", () => edgeOnly('(circle 200 200 80 "outline:4")', [200, 200]));
  it("box", () => edgeOnly('(box 100 100 200 120 "outline:3")', [200, 160]));
  it("tri", () => edgeOnly('(tri 100 300 200 100 300 300 "outline:3")', [200, 240]));
  it("shape", () => edgeOnly('(shape 100 100 300 100 320 280 80 300 "outline:3")', [200, 200]));
  it("leaves a one pixel outline to the graphics library", () => {
    const calls = run('(circle 200 200 80 "outline")');
    expect(quads(calls).length).toBe(0);
    expect(calls.some(([name]) => name === "circle")).toBe(true);
  });
});
