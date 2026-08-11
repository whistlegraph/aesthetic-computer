// Softy, recovered from the Construct "Softy" event sheet. Every constant here
// was read out of the compiled expression table (toolchain/nopaint/
// expressions.mjs): the weighted size tiers, the three radius bands and their
// turn levels, the hardness and step formulas, the four timers, the hue drift,
// and the blurp's radius-pitched playback rate.
//
// Construct built a 2r × 2r buffer whose alpha is
//   255 - (distance(centre) - hardness) / (radius - hardness) * 255
// and stamped it into a layer on every Move tick. This does the same: one mask
// per score, composited into a layer that only ever grows by the stamps the
// clock has actually reached. Redrawing the whole stroke each frame instead
// costs ~1.7s a frame at the largest radius; pasting the layer costs ~0.2ms.

import { canvasFor, seededStream, softMask } from "./nopaint-canvas.mjs";

const frozen = (value) => Object.freeze(value);

export const SOFTY = frozen({
  canvas: 256,
  // The picker weights S:5 M:1 L:2, so most Softies are small.
  tiers: frozen([
    frozen({ name: "S", weight: 5, radius: frozen([6, 16]), turn: frozen([1, 2]) }),
    frozen({ name: "M", weight: 1, radius: frozen([16, 48]), turn: frozen([2, 3]) }),
    frozen({ name: "L", weight: 2, radius: frozen([48, 64]), turn: frozen([4, 5, 6]) }),
  ]),
  minHardnessGap: 6,          // hardness = round(random(0, radius - 6))
  stepDivisor: 4,             // step = max(1, (radius - hardness) / 4)
  speeds: frozen([.5, 1, 1.5]),
  turnSpeeds: frozen([16, 30, 45]),
  hueDirections: frozen([-1, 1]),
  colorSeconds: .1,           // Timer "Color"
  moveSeconds: .1,            // Timer "Move", × speed
  turnSeconds: .2,            // Timer "Turn", × turnSpeed
  reheadOdds: 10,             // one turn in ten picks a whole new heading
  hueStep: .001,
  wander: .002,               // s and l each drift ±.002 per colour tick
  cue: "softy - landed",      // played on the "Blurp" tag every Move tick
  // A Construct stroke ran until you pressed Paint. A proposal has to end, so
  // the walk is capped — this length is ours, everything above is not.
  stamps: 96,
});

const choose = (random, values) => values[Math.floor(random() * values.length)];

export function softyCueRate(radius, jitter = 0) {
  return 1.8 - radius / 64 + jitter; // Construct adds random(-.1, .1).
}

function hslaToRgba(hue, saturation, lightness) {
  const chroma = (1 - Math.abs(2 * lightness - 1)) * saturation;
  const sector = ((hue % 1) + 1) % 1 * 6;
  const second = chroma * (1 - Math.abs(sector % 2 - 1));
  const [r, g, b] = [[chroma, second, 0], [second, chroma, 0], [0, chroma, second],
    [0, second, chroma], [second, 0, chroma], [chroma, 0, second]][Math.floor(sector) % 6];
  const base = lightness - chroma / 2;
  return [r, g, b].map((channel) => Math.round((channel + base) * 255));
}

// Colour, turning, and movement each run on their own Construct timer, so the
// walk is generated once at whatever rates the score drew rather than re-derived
// per frame. Capping it at SOFTY.stamps keeps a redraw bounded.
function walkOf(score) {
  const random = seededStream(score.seed);
  const turnEvery = score.turnSeconds / score.moveSeconds;
  const colorEvery = SOFTY.colorSeconds / score.moveSeconds;
  let angle = score.startAngle - 90;
  let [x, y] = [score.x, score.y];
  let [hue, saturation, lightness] = [score.hue, score.saturation, score.lightness];
  const stamps = [];
  for (let move = 0; move < SOFTY.stamps; move += 1) {
    if (move && move % turnEvery < 1) {
      angle = random() * SOFTY.reheadOdds < 1
        ? random() * 360
        : angle + random() * score.turnLevel * 2 - score.turnLevel;
    }
    if (move && move % colorEvery < 1) {
      hue = (hue + SOFTY.hueStep * score.hueDirection) % 1;
      saturation = Math.min(1, saturation + random() * SOFTY.wander * 2 - SOFTY.wander);
      lightness = Math.min(1, lightness + random() * SOFTY.wander * 2 - SOFTY.wander);
    }
    stamps.push(frozen({ x, y, color: frozen(hslaToRgba(hue, saturation, lightness)) }));
    const radians = angle * Math.PI / 180;
    x += Math.cos(radians) * score.step;
    y += Math.sin(radians) * score.step;
  }
  return frozen(stamps);
}

export const softyProposal = frozen({
  version: 1,
  slug: "softy",
  label: "Softy",
  compatible: true,
  source: frozen({ ...SOFTY, actionSheet: "Softy", object: "SoftCircleBuffer",
    falloff: "255 - (distance - hardness) / (radius - hardness) * 255",
    reconstructed: frozen(["stroke length"]) }),
  generate({ random, width, height, base }) {
    const weights = SOFTY.tiers.reduce((sum, tier) => sum + tier.weight, 0);
    let cursor = random() * weights;
    const tier = SOFTY.tiers.find((entry) => (cursor -= entry.weight) < 0) || SOFTY.tiers[0];
    const [low, high] = tier.radius;
    const radius = Math.round(low + random() * (high - low));
    const hardness = Math.round(random() * Math.max(0, radius - SOFTY.minHardnessGap));
    const speed = choose(random, SOFTY.speeds);
    const turnSpeed = choose(random, SOFTY.turnSpeeds);
    // Construct laid Softy out on a 256 painting; carry its radii and step
    // onto whatever this canvas actually is.
    const scale = Math.min(width, height) / SOFTY.canvas;
    return frozen({
      ...base,
      kind: "softy",
      seed: Math.floor(random() * 0xffffffff),
      tier: tier.name,
      radius, hardness, speed, turnSpeed, scale,
      turnLevel: choose(random, tier.turn),
      hueDirection: choose(random, SOFTY.hueDirections),
      startAngle: Math.floor(random() * 360),
      step: Math.max(1, (radius - hardness) / SOFTY.stepDivisor),
      moveSeconds: SOFTY.moveSeconds * speed,
      turnSeconds: SOFTY.turnSeconds * turnSpeed,
      // ProcessColorParameter's unspecified case is the shared random colour.
      hue: random(),
      saturation: .2 + random() * .8,
      lightness: .4 + random() * .2,
      x: random() * width,
      y: random() * height,
      width, height,
      brush: frozen({
        slug: "softy",
        params: frozen([String(Math.round(radius * scale)), tier.name]),
        colon: frozen([]),
        parameters: frozen({ tier: tier.name, radius, hardness, speed,
          cue: SOFTY.cue, cueRate: softyCueRate(radius) }),
      }),
    });
  },
  render({ paste }, score, tick) {
    const held = canvasFor(score, (canvas, state) => {
      state.mask = softMask(Math.max(1, score.radius * score.scale),
        score.hardness * score.scale);
      state.stamps = walkOf(score);
    });
    const due = Math.min(held.stamps.length,
      1 + Math.floor(tick / 60 / score.moveSeconds));
    while (held.placed < due) {
      const { x, y, color } = held.stamps[held.placed];
      held.canvas.stamp(held.mask, x, y, color);
      held.placed += 1;
    }
    paste(held.canvas, 0, 0);
  },
});
