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

function seededWalk(seed) {
  let state = seed >>> 0 || 1;
  return () => {
    state += 0x6d2b79f5;
    let value = state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
}

function disc(layer, centerX, centerY, radius, color) {
  const top = Math.max(0, Math.floor(centerY - radius));
  const bottom = Math.min(layer.height - 1, Math.ceil(centerY + radius));
  for (let y = top; y <= bottom; y += 1) {
    const span = Math.sqrt(Math.max(0, radius * radius - (y - centerY) ** 2));
    const left = Math.max(0, Math.floor(centerX - span));
    const right = Math.min(layer.width - 1, Math.ceil(centerX + span));
    for (let x = left; x <= right; x += 1) {
      const at = (y * layer.width + x) * 4;
      layer.pixels[at] = color[0];
      layer.pixels[at + 1] = color[1];
      layer.pixels[at + 2] = color[2];
      layer.pixels[at + 3] = 255;
    }
  }
}

// Construct's blend mode 7 on an ellipse: the bite takes the biscuit away.
function erase(layer, centerX, centerY, radius) {
  const top = Math.max(0, Math.floor(centerY - radius));
  const bottom = Math.min(layer.height - 1, Math.ceil(centerY + radius));
  for (let y = top; y <= bottom; y += 1) {
    const span = Math.sqrt(Math.max(0, radius * radius - (y - centerY) ** 2));
    const left = Math.max(0, Math.floor(centerX - span));
    const right = Math.min(layer.width - 1, Math.ceil(centerX + span));
    for (let x = left; x <= right; x += 1) layer.pixels[(y * layer.width + x) * 4 + 3] = 0;
  }
}

function biteInto(layer, state, score) {
  const angle = WAFER.arc * state.order[state.index % WAFER.positions];
  for (let cut = 0; cut < WAFER.bitesPerVisit; cut += 1) {
    const { random } = state;
    const [low, high] = WAFER.biteRadius;
    const biteRadius = (low + random() * (high - low)) * state.radius;
    const adjusted = angle + random() * WAFER.biteAngleJitter * 2 - WAFER.biteAngleJitter;
    const reach = state.radius
      + random() * WAFER.biteDistanceJitter * 2 - WAFER.biteDistanceJitter;
    const radians = adjusted * Math.PI / 180;
    erase(layer, score.x + Math.cos(radians) * reach,
      score.y + Math.sin(radians) * reach, biteRadius);
  }
  state.index += 1;
  return angle;
}

const layers = new WeakMap();
function layerFor(score) {
  let state = layers.get(score);
  if (!state) {
    const width = Math.max(1, Math.round(score.width));
    const height = Math.max(1, Math.round(score.height));
    const random = seededWalk(score.seed);
    state = {
      random, index: 0, drawn: 0, generation: 0,
      radius: score.radius,
      order: permutation(random, WAFER.positions),
      layer: { width, height, pixels: new Uint8ClampedArray(width * height * 4) },
    };
    layers.set(score, state);
    reset(state, score);
  }
  return state;
}

// BiscuitRender: the biscuit is redrawn whole, then nibbled. CircularBite runs
// twelve bites before the first frame is ever shown.
function reset(state, score) {
  state.layer.pixels.fill(0);
  disc(state.layer, score.x, score.y, state.radius, score.color);
  state.index = 0;
  for (let bite = 0; bite < WAFER.positions; bite += 1) biteInto(state.layer, state, score);
  state.index = 0;
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
    const state = layerFor(score);
    const due = Math.floor(tick / 60 / WAFER.drawSeconds);
    while (state.drawn < due) {
      state.drawn += 1;
      if (state.index < WAFER.bitesBeforeEnlarge) {
        biteInto(state.layer, state, score);
        continue;
      }
      // BiscuitEnlarge: grow, reshuffle, and start nibbling again. Stop once
      // the biscuit has outgrown the painting rather than growing forever.
      const grown = state.radius * WAFER.enlarge;
      if (grown > Math.max(score.width, score.height)) { state.drawn = due; break; }
      state.radius = grown;
      state.generation += 1;
      state.order = permutation(state.random, WAFER.positions);
      reset(state, score);
    }
    paste(state.layer, 0, 0);
  },
});
