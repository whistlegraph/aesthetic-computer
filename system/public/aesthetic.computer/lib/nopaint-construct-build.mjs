// Build and Banner, recovered from the Construct sheets of the same names.
// They are together because they are the same idea twice: something walks and
// leaves a trail behind it, so both accumulate into a layer rather than being
// redrawn from the frame number. Constants read out of the compiled expression
// table (toolchain/nopaint/expressions.mjs).

const frozen = (value) => Object.freeze(value);
const choose = (random, values) => values[Math.floor(random() * values.length)];
const between = (random, [low, high]) => low + random() * (high - low);

export const BUILD = frozen({
  canvas: 256,
  blockSizes: frozen([4, 8, 16, 32, 64, 128]),
  opacity: frozen([50, 100]),          // random(50, 100)
  stepSeconds: .1,                     // the builder's beat
  cues: frozen({
    theme: "build - builder's beat",
    scrape: "build - brick scrape",
    click: "build - brick click",
    clop: "build - brick clop",
    blowDown: "build - blow down",
  }),
  cueVolume: frozen([-20, -10]),       // -20 + random(0,10) and -10 + random(0,10)
  cueRate: frozen([.9, 1.1]),          // 1 + random(-.1, .1)
});

export const BANNER = frozen({
  canvas: 256,
  sizes: frozen([4, 8, 16]),           // choose(4, 8, 16)
  speeds: frozen([1, 2, 3, 4]),        // choose(1, 2, 3, 4)
  depths: frozen([1, 2, 5]),           // choose(1, 2, 5)
  turns: frozen([-45, 45, -15, 15]),   // choose(-45, 45, -15, 15)
  drawSeconds: .1,                     // Timer "Draw"
  turnSeconds: .1,                     // Timer "Turn"
  // hslaToRgba(random(1), random(.2,.6), random(.1,.5), 1) and
  // hslaToRgba(random(1), random(.7,1), random(.5,.95), 1)
  dark: frozen({ saturation: frozen([.2, .6]), lightness: frozen([.1, .5]) }),
  light: frozen({ saturation: frozen([.7, 1]), lightness: frozen([.5, .95]) }),
  cue: "banner - theme",
  cueTag: "Zipper",
  cueRate: (speed) => .9 + speed / 4 * .2,
  cueVolume: (size) => -5 - (10 - size / 16 * 10),
});

function hslaToRgba(hue, saturation, lightness) {
  const chroma = (1 - Math.abs(2 * lightness - 1)) * saturation;
  const sector = ((hue % 1) + 1) % 1 * 6;
  const second = chroma * (1 - Math.abs(sector % 2 - 1));
  const [r, g, b] = [[chroma, second, 0], [second, chroma, 0], [0, chroma, second],
    [0, second, chroma], [second, 0, chroma], [chroma, 0, second]][Math.floor(sector) % 6];
  const base = lightness - chroma / 2;
  return frozen([r, g, b].map((channel) => Math.round((channel + base) * 255)));
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

function fill(layer, x, y, w, h, color, alpha) {
  const left = Math.max(0, Math.floor(x));
  const top = Math.max(0, Math.floor(y));
  const right = Math.min(layer.width, Math.ceil(x + w));
  const bottom = Math.min(layer.height, Math.ceil(y + h));
  for (let py = top; py < bottom; py += 1) {
    for (let px = left; px < right; px += 1) {
      const at = (py * layer.width + px) * 4;
      const source = alpha;
      const under = layer.pixels[at + 3] * (255 - source) / 255;
      const total = source + under || 1;
      layer.pixels[at] = (color[0] * source + layer.pixels[at] * under) / total;
      layer.pixels[at + 1] = (color[1] * source + layer.pixels[at + 1] * under) / total;
      layer.pixels[at + 2] = (color[2] * source + layer.pixels[at + 2] * under) / total;
      layer.pixels[at + 3] = Math.min(255, total);
    }
  }
}

// Scanline fill of a convex quad — the banner's ribbon segment.
function quad(layer, points, color) {
  const top = Math.max(0, Math.floor(Math.min(...points.map((p) => p[1]))));
  const bottom = Math.min(layer.height - 1, Math.ceil(Math.max(...points.map((p) => p[1]))));
  for (let y = top; y <= bottom; y += 1) {
    let left = Infinity;
    let right = -Infinity;
    for (let index = 0; index < points.length; index += 1) {
      const [ax, ay] = points[index];
      const [bx, by] = points[(index + 1) % points.length];
      if ((ay <= y && by > y) || (by <= y && ay > y)) {
        const x = ax + (y - ay) / (by - ay) * (bx - ax);
        left = Math.min(left, x);
        right = Math.max(right, x);
      }
    }
    if (left > right) continue;
    const start = Math.max(0, Math.round(left));
    const end = Math.min(layer.width - 1, Math.round(right));
    for (let x = start; x <= end; x += 1) {
      const at = (y * layer.width + x) * 4;
      layer.pixels[at] = color[0];
      layer.pixels[at + 1] = color[1];
      layer.pixels[at + 2] = color[2];
      layer.pixels[at + 3] = 255;
    }
  }
}

const layers = new WeakMap();
function layerFor(score) {
  let state = layers.get(score);
  if (!state) {
    const width = Math.max(1, Math.round(score.width));
    const height = Math.max(1, Math.round(score.height));
    state = {
      random: seededWalk(score.seed), placed: 0,
      layer: { width, height, pixels: new Uint8ClampedArray(width * height * 4) },
    };
    layers.set(score, state);
  }
  return state;
}

export const buildProposal = frozen({
  version: 1,
  slug: "build",
  label: "Build",
  compatible: true,
  source: frozen({ ...BUILD, actionSheet: "Build",
    grid: "blockIndexMax = 256 / blockSize - 1; the builder steps ±1 and clamps",
    // The builder's own sprite and the blow-down lifecycle are not modeled;
    // what is here is where it walks and what it leaves.
    reconstructed: frozen(["the builder sprite", "the blow-down lifecycle"]) }),
  generate({ random, width, height, base }) {
    const blockSize = choose(random, BUILD.blockSizes);
    const scale = Math.min(width, height) / BUILD.canvas;
    const opacity = Math.round(between(random, BUILD.opacity));
    return frozen({ ...base, kind: "build",
      seed: Math.floor(random() * 0xffffffff),
      blockSize, opacity, scale,
      block: Math.max(1, blockSize * scale),
      columns: Math.max(1, Math.ceil(width / Math.max(1, blockSize * scale))),
      rows: Math.max(1, Math.ceil(height / Math.max(1, blockSize * scale))),
      color: hslaToRgba(random(), random(), random()),
      width, height,
      brush: frozen({ slug: "build", params: frozen([String(blockSize)]),
        colon: frozen([]),
        parameters: frozen({ blockSize, opacity,
          blockIndexMax: BUILD.canvas / blockSize - 1, cue: BUILD.cues.theme }) }) });
  },
  render({ paste }, score, tick) {
    const state = layerFor(score);
    // The builder lays its first brick on the first frame, not a beat later.
    const due = 1 + Math.floor(tick / 60 / BUILD.stepSeconds);
    if (state.placed === 0) {
      state.column = Math.floor(state.random() * score.columns);
      state.row = Math.floor(state.random() * score.rows);
    }
    // The builder walks ±1 on one axis at a time, clamped to the grid, laying
    // a brick wherever it lands.
    const bricks = Math.min(due, score.columns * score.rows * 4);
    while (state.placed < bricks) {
      state.placed += 1;
      fill(state.layer, state.column * score.block, state.row * score.block,
        score.block, score.block, score.color,
        Math.round(score.opacity / 100 * 255));
      const axis = state.random() < .5;
      const step = state.random() < .5 ? -1 : 1;
      if (axis) state.column = Math.max(0, Math.min(score.columns - 1, state.column + step));
      else state.row = Math.max(0, Math.min(score.rows - 1, state.row + step));
    }
    paste(state.layer, 0, 0);
  },
});

export const bannerProposal = frozen({
  version: 1,
  slug: "banner",
  label: "Banner",
  compatible: true,
  source: frozen({ ...BANNER, actionSheet: "Banner",
    imagePoints: frozen(["BaseLeft", "BaseRight", "TopLeft", "TopRight",
      "BottomLeft", "BottomRight"]),
    reconstructed: frozen(["the ribbon's quad geometry", "the advance per step"]) }),
  generate({ random, width, height, base }) {
    const size = choose(random, BANNER.sizes);
    const speed = choose(random, BANNER.speeds);
    const depth = choose(random, BANNER.depths);
    const scale = Math.min(width, height) / BANNER.canvas;
    return frozen({ ...base, kind: "banner",
      seed: Math.floor(random() * 0xffffffff),
      size, speed, depth, scale,
      band: Math.max(2, size * 2 * scale),
      startAngle: Math.floor(random() * 360),
      dark: hslaToRgba(random(), between(random, BANNER.dark.saturation),
        between(random, BANNER.dark.lightness)),
      light: hslaToRgba(random(), between(random, BANNER.light.saturation),
        between(random, BANNER.light.lightness)),
      x: Math.floor(random() * width), y: Math.floor(random() * height),
      width, height,
      brush: frozen({ slug: "banner",
        params: frozen([String(size), String(speed)]),
        colon: frozen([]),
        parameters: frozen({ size, speed, depth, cue: BANNER.cue,
          cueRate: BANNER.cueRate(speed), cueVolume: BANNER.cueVolume(size) }) }) });
  },
  render({ paste }, score, tick) {
    const state = layerFor(score);
    const due = 1 + Math.floor(tick / 60 / BANNER.drawSeconds);
    if (state.placed === 0) {
      state.angle = score.startAngle;
      state.target = score.startAngle;
      state.x = score.x;
      state.y = score.y;
    }
    const steps = Math.min(due, 600);
    while (state.placed < steps) {
      state.placed += 1;
      // Draw and Turn share a tenth-second beat, so every laid segment also
      // turns by one of choose(-45, 45, -15, 15). The banner is a ribbon of
      // width `band` between successive cross-sections — its six image points
      // are BaseLeft/BaseRight and the two corners at each end — so lay a quad
      // rather than a loose square, or it reads as confetti.
      const radians = state.angle * Math.PI / 180;
      const from = { x: state.x, y: state.y };
      // The sheet gives speed and depth but not a distance — speed feeds the
      // theme's playback rate, and depth reads as a layer count. Advancing by
      // the band width is what keeps the ribbon a ribbon instead of a row of
      // loose squares, so that is the reconstructed part.
      const advance = score.band;
      state.x += Math.cos(radians) * advance;
      state.y += Math.sin(radians) * advance;
      const across = radians + Math.PI / 2;
      const half = score.band / 2;
      const dx = Math.cos(across) * half;
      const dy = Math.sin(across) * half;
      quad(state.layer, [
        [from.x + dx, from.y + dy], [from.x - dx, from.y - dy],
        [state.x - dx, state.y - dy], [state.x + dx, state.y + dy],
      ], state.placed % 2 ? score.dark : score.light);
      // turnAngle is a target the banner rotates toward, not a per-step snap:
      // snapping ±45° every tenth of a second makes a scribble, not a banner.
      if (state.placed % 8 === 0) state.target = state.angle + choose(state.random, BANNER.turns);
      const toward = ((state.target - state.angle + 540) % 360) - 180;
      state.angle += Math.sign(toward) * Math.min(Math.abs(toward), 6);
    }
    paste(state.layer, 0, 0);
  },
});
