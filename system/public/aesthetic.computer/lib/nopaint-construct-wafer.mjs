// Wafer, recovered from the Construct "Wafer" event sheet and its Bite /
// CircularBite / BiscuitEnlarge / BiscuitRender functions. Read out of the
// compiled expression table (toolchain/nopaint/expressions.mjs): the weighted
// radius list, the twelve bite positions 30° apart, the three erasing ellipses
// per bite and their jitter, the half-second Draw timer, the thirteen bites
// before the biscuit grows by 1.3, and all five cues.
//
// The bite positions are not walked in order. Construct builds an
// AdvancedRandom permutation of twelve and reads biteAngle as
// `30 * permutation[biscuitLocationIndex]`, reshuffling on every enlarge, so
// the biscuit is nibbled around its rim in a different order each generation.
//
// A bite erases — Construct switches to blend mode 7 (destination-out), cuts an
// ellipse, and switches back. Like Softy, the biscuit is therefore a layer that
// accumulates rather than a shape that can be redrawn from the frame number.

import { canvasFor, seededStream } from "./nopaint-canvas.mjs";

const frozen = (value) => Object.freeze(value);

export const WAFER = frozen({
  canvas: 256,
  // choose(16,16,32,32,32,32,48,48,48,64,64,96): 32 is four times as likely as
  // 96, so most wafers are middling.
  radii: frozen([16, 16, 32, 32, 32, 32, 48, 48, 48, 64, 64, 96]),
  positions: 12,
  arc: 30,                        // 360 / 12, inlined in the biteAngle expression
  bitesPerVisit: 3,               // Bite cuts three ellipses
  biteRadius: frozen([.2, .5]),   // random(.2 * radius, .5 * radius)
  biteAngleJitter: 5,             // biteAngle + random(-5, 5)
  biteDistanceJitter: 4,          // radius + random(-4, 4)
  drawSeconds: .5,                // Timer "Draw"
  bitesBeforeEnlarge: 13,
  enlarge: 1.3,
  particleDistance: .65,          // .65 * radius out along biteAngle
  colorJitter: frozen([-5, 0, 5]),
  cues: frozen({
    appear: "wafer - nibble appear",
    bites: frozen(["wafer - nibble bite 1", "wafer - nibble bite 2", "wafer - nibble bite 3"]),
    enlarge: "wafer - enlarge",
  }),
});

const choose = (random, values) => values[Math.floor(random() * values.length)];
const clamp = (value) => Math.max(0, Math.min(255, Math.round(value)));

// AdvancedRandom's "create permutation of 12 from 0", as a Fisher-Yates the
// score can reproduce from its seed.
export function permutation(random, count) {
  const order = Array.from({ length: count }, (_, index) => index);
  for (let index = count - 1; index > 0; index -= 1) {
    const swap = Math.floor(random() * (index + 1));
    [order[index], order[swap]] = [order[swap], order[index]];
  }
  return frozen(order);
}

function biteInto(canvas, state, score) {
  const angle = WAFER.arc * state.order[state.index % WAFER.positions];
  for (let cut = 0; cut < WAFER.bitesPerVisit; cut += 1) {
    const { random } = state;
    const [low, high] = WAFER.biteRadius;
    const biteRadius = (low + random() * (high - low)) * state.radius;
    const adjusted = angle + random() * WAFER.biteAngleJitter * 2 - WAFER.biteAngleJitter;
    const reach = state.radius
      + random() * WAFER.biteDistanceJitter * 2 - WAFER.biteDistanceJitter;
    const radians = adjusted * Math.PI / 180;
    canvas.erase(score.x + Math.cos(radians) * reach,
      score.y + Math.sin(radians) * reach, biteRadius);
  }
  state.index += 1;
  return angle;
}

// BiscuitRender redraws the biscuit whole, then CircularBite takes twelve bites
// out of its rim before the first frame is ever shown.
function reset(canvas, state, score) {
  canvas.wipe().disc(score.x, score.y, state.radius, score.color);
  state.index = 0;
  for (let bite = 0; bite < WAFER.positions; bite += 1) biteInto(canvas, state, score);
  state.index = 0;
}

function biscuitFor(score) {
  return canvasFor(score, (canvas, state) => {
    state.random = seededStream(score.seed);
    state.index = 0;
    state.drawn = 0;
    state.generation = 0;
    state.radius = score.radius;
    state.order = permutation(state.random, WAFER.positions);
    reset(canvas, state, score);
  });
}

export const waferProposal = frozen({
  version: 1,
  slug: "wafer",
  label: "Wafer",
  compatible: true,
  source: frozen({ ...WAFER, actionSheet: "Wafer",
    objects: frozen(["NibbleBiscuit", "NibbleParticles", "NibblePalette"]),
    biteAngle: "30 * AdvancedRandom.permutation[biscuitLocationIndex]",
    // NibblePalette reads its colours out of a sprite; only the ±5 per-channel
    // jitter around them is recovered, so the proposal's own colour stands in.
    palette: "not recovered",
    reconstructed: frozen(["the palette colour", "the nibble particles"]) }),
  generate({ random, width, height, base }) {
    const radius = choose(random, WAFER.radii);
    const scale = Math.min(width, height) / WAFER.canvas;
    const color = frozen(base.color.slice(0, 3)
      .map((channel) => clamp(channel + choose(random, WAFER.colorJitter))));
    return frozen({
      ...base,
      kind: "wafer",
      seed: Math.floor(random() * 0xffffffff),
      radius: Math.max(1, radius * scale),
      sourceRadius: radius,
      color,
      scale,
      x: random() * width,
      y: random() * height,
      width, height,
      brush: frozen({
        slug: "wafer",
        params: frozen([String(radius)]),
        colon: frozen([]),
        parameters: frozen({ radius, positions: WAFER.positions,
          bitesBeforeEnlarge: WAFER.bitesBeforeEnlarge, cue: WAFER.cues.appear }),
      }),
    });
  },
  render({ paste }, score, tick) {
    const state = biscuitFor(score);
    const canvas = state.canvas;
    const due = Math.floor(tick / 60 / WAFER.drawSeconds);
    while (state.drawn < due) {
      state.drawn += 1;
      if (state.index < WAFER.bitesBeforeEnlarge) {
        biteInto(canvas, state, score);
        continue;
      }
      // BiscuitEnlarge: grow, reshuffle, and start nibbling again. Stop once
      // the biscuit has outgrown the painting rather than growing forever.
      const grown = state.radius * WAFER.enlarge;
      if (grown > Math.max(score.width, score.height)) { state.drawn = due; break; }
      state.radius = grown;
      state.generation += 1;
      state.order = permutation(state.random, WAFER.positions);
      reset(canvas, state, score);
    }
    paste(canvas, 0, 0);
  },
});
