// Triangle and Ellipse, recovered from the Construct sheets of the same names.
// They are one module because they are the same brush wearing two shapes: both
// take their geometry from numeric parameters, both mix the same dark
// translucent shadow, and both run a one second "Shake" timer that nudges every
// coordinate by choose(-1, 0, 1) and plays `common - jitter`. Constants read out
// of the compiled expression table (toolchain/nopaint/expressions.mjs).

const frozen = (value) => Object.freeze(value);
const choose = (random, values) => values[Math.floor(random() * values.length)];

// rgba(random(0,45), random(0,45), random(0,45), random(10,60)) — the same
// expression in both sheets.
export const SHAPE = frozen({
  canvas: 256,
  shadow: frozen({ channel: frozen([0, 45]), alpha: frozen([10, 60]) }),
  shakeSeconds: 1,
  jitter: frozen([-1, 0, 1]),
  jitterCue: "common - jitter",
  // Ellipse's width and height are ProcessNumericParameter(n, 3, 255) — three
  // is the floor, not zero.
  minimumSize: 3,
});

const shadowOf = (random) => frozen([
  Math.floor(random() * (SHAPE.shadow.channel[1] + 1)),
  Math.floor(random() * (SHAPE.shadow.channel[1] + 1)),
  Math.floor(random() * (SHAPE.shadow.channel[1] + 1)),
  Math.round(SHAPE.shadow.alpha[0]
    + random() * (SHAPE.shadow.alpha[1] - SHAPE.shadow.alpha[0])),
]);

// Every Shake tick moves each coordinate by one of choose(-1, 0, 1). The walk
// is derived from the score's seed so a frame can be drawn without replaying
// the ones before it.
function shakeOf(seed, count, shakes) {
  let state = seed >>> 0 || 1;
  const random = () => {
    state += 0x6d2b79f5;
    let value = state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
  const drift = new Array(count).fill(0);
  const history = [drift.slice()];
  for (let shake = 0; shake < shakes; shake += 1) {
    for (let index = 0; index < count; index += 1) {
      drift[index] += choose(random, SHAPE.jitter);
    }
    history.push(drift.slice());
  }
  return frozen(history.map(frozen));
}

// A proposal only ever shows a couple of minutes; keep the shake table bounded.
const SHAKES = 240;
const shakes = new WeakMap();
function shakeAt(score, count, tick) {
  let history = shakes.get(score);
  if (!history) shakes.set(score, history = shakeOf(score.seed, count, SHAKES));
  return history[Math.min(history.length - 1,
    Math.floor(tick / 60 / SHAPE.shakeSeconds))];
}

export const triangleProposal = frozen({
  version: 1,
  slug: "triangle",
  label: "Triangle",
  compatible: true,
  source: frozen({ ...SHAPE, actionSheet: "Triangle",
    parameters: frozen(["x1", "y1", "x2", "y2", "x3", "y3", "colour"]),
    cue: "triangle - start",
    reconstructed: frozen(["the shadow offset"]) }),
  generate({ random, width, height, base }) {
    const points = frozen(Array.from({ length: 3 }, () => frozen({
      x: Math.floor(random() * width), y: Math.floor(random() * height),
    })));
    return frozen({ ...base, kind: "triangle", points,
      seed: Math.floor(random() * 0xffffffff),
      shadow: shadowOf(random), width, height,
      brush: frozen({ slug: "triangle",
        params: frozen(points.flatMap(({ x, y }) => [String(x), String(y)])),
        colon: frozen([]),
        parameters: frozen({ shakeSeconds: SHAPE.shakeSeconds, cue: "triangle - start" }) }) });
  },
  render({ ink }, score, tick) {
    const drift = shakeAt(score, 6, tick);
    const corners = score.points.map(({ x, y }, index) =>
      [x + drift[index * 2], y + drift[index * 2 + 1]]);
    ink(score.shadow).poly(corners.map(([x, y]) => [x + 2, y + 2]));
    ink(score.color).poly(corners);
  },
});

export const ellipseProposal = frozen({
  version: 1,
  slug: "ellipse",
  label: "Ellipse",
  compatible: true,
  source: frozen({ ...SHAPE, actionSheet: "Ellipse",
    parameters: frozen(["x", "y", "width", "height", "colour"]),
    cue: "elipse - start", // The original file name is misspelled; keep it.
    reconstructed: frozen(["the shadow offset"]) }),
  generate({ random, width, height, base }) {
    const size = (extent) => Math.max(SHAPE.minimumSize,
      Math.floor(SHAPE.minimumSize + random() * (extent - SHAPE.minimumSize)));
    return frozen({ ...base, kind: "ellipse",
      seed: Math.floor(random() * 0xffffffff),
      cx: Math.floor(random() * width), cy: Math.floor(random() * height),
      rx: size(width) / 2, ry: size(height) / 2,
      shadow: shadowOf(random), width, height,
      brush: frozen({ slug: "ellipse", params: frozen([]), colon: frozen([]),
        parameters: frozen({ shakeSeconds: SHAPE.shakeSeconds, cue: "elipse - start" }) }) });
  },
  render({ ink }, score, tick) {
    // x, y, and w all shake; h is the one coordinate the sheet leaves alone.
    const [driftX, driftY, driftW] = shakeAt(score, 3, tick);
    const rx = Math.max(SHAPE.minimumSize / 2, score.rx + driftW / 2);
    ink(score.shadow).oval(score.cx + driftX + 2, score.cy + driftY + 2,
      rx * 2, score.ry * 2, true);
    ink(score.color).oval(score.cx + driftX, score.cy + driftY,
      rx * 2, score.ry * 2, true);
  },
});
