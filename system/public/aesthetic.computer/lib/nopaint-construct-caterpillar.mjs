// Caterpillar, recovered from the Construct "Caterpillar" event sheet and its
// four sprite objects. Every constant below was read out of the compiled
// expression table (see toolchain/nopaint/expressions.mjs) rather than guessed:
// the wander target, the two timer rates, the turn limits, the four entry
// sides, the 9px start margin, and the hue/saturation ramps are exact. Only the
// pixel look of Construct's pinned sprite chain is an AC reading.
//
// The rainbow road is the original easter egg: ask for seven segments
// (`Caterpillar: 7`, so length 6) and the body cycles hue instead of fading.

import { spritePaste } from "./nopaint-construct-sprites.mjs";

const frozen = (value) => Object.freeze(value);
const S4 = "/nopaint.art/images/shared-2-sheet4.png";
const S5 = "/nopaint.art/images/shared-2-sheet5.png";
const sprite = (sheet, x, y, w, h, ox, oy) => frozen({ sheet, x, y, w, h, ox, oy });

// Exact crops and origins from the head, CaterpillarTailpiece, and
// CaterpillarLead objects.
export const caterpillarSprites = frozen({
  head: frozen({ fps: 2, frames: frozen([
    sprite(S4, 22, 65, 19, 16, 0.43243243243243246, .5),
    sprite(S4, 1, 65, 19, 16, 0.43243243243243246, .5),
  ]) }),
  tail: frozen({ fps: 10, frames: frozen([
    sprite(S4, 36, 33, 16, 20, .53125, .525),
    sprite(S4, 36, 1, 16, 20, .53125, .525),
  ]) }),
  lead: frozen({ fps: 15, frames: frozen([
    sprite(S5, 9, 49, 3, 3, 1 / 3, 1 / 3),
    sprite(S5, 19, 41, 5, 5, .4, .4),
  ]) }),
});

// Construct's own numbers. CANVAS is the 256×256 painting every No Paint
// layout used, which is what the wander target and margins are stated against.
export const CATERPILLAR = frozen({
  canvas: 256,
  startMargin: 9,            // StartMargin
  segments: frozen([3, 32]), // ProcessNumericParameter(1, 3, 32), then minus 1
  rainbowLength: 6,          // "Caterpillar: 7" turns on the rainbow road
  saturation: frozen([.2, .7]),
  squirmHz: 35,              // Timer "Squirm" every 1/35s
  turnHz: 15,                // Timer "Turn" every 1/15s
  turnToward: 10,            // rotate up to 10° toward the target
  turnJitter: 10,            // then ±10° of its own
  retarget: 16,              // pick a new target within 16px
  spacing: 12,               // |xOffset| / |yOffset| between segments
  cues: frozen({ rainbow: "caterpillar - rain bow road", walk: "caterpillar - trotting along" }),
});

// startingSide = choose(0, 1, 2, 3). Each side sets one coordinate to the
// margin and offsets the body by ±12 along that axis, which is also the
// direction the lead sets off in.
const MARGIN = CATERPILLAR.startMargin;
const EDGE = CATERPILLAR.canvas + MARGIN;
const SIDES = frozen([
  frozen({ axis: "y", at: -MARGIN, heading: 90 }),
  frozen({ axis: "y", at: EDGE, heading: -90 }),
  frozen({ axis: "x", at: -MARGIN, heading: 0 }),
  frozen({ axis: "x", at: EDGE, heading: 180 }),
]);

function hslaToRgba(hue, saturation, lightness, alpha) {
  const chroma = (1 - Math.abs(2 * lightness - 1)) * saturation;
  const sector = ((hue % 1) + 1) % 1 * 6;
  const second = chroma * (1 - Math.abs(sector % 2 - 1));
  const [r, g, b] = [[chroma, second, 0], [second, chroma, 0], [0, chroma, second],
    [0, second, chroma], [second, 0, chroma], [chroma, 0, second]][Math.floor(sector) % 6];
  const base = lightness - chroma / 2;
  return frozen([r, g, b].map((channel) => Math.round((channel + base) * 255))
    .concat(Math.round(alpha * 255)));
}

// The lead squirms one pixel at a time toward a wandering target, so its path
// has to be walked rather than solved. Each score keeps its own walk, extended
// on demand and never recomputed from the start. Only the stretch the body
// still covers is retained, so a brush left running does not grow.
const walks = new WeakMap();

function walkTo(score, squirms) {
  let walk = walks.get(score);
  if (!walk) {
    const random = seededWalk(score.seed);
    walk = { random, step: 0, angle: score.angle, x: score.x, y: score.y,
      body: score.length * CATERPILLAR.spacing + 1,
      target: { x: random() * CATERPILLAR.canvas, y: random() * CATERPILLAR.canvas },
      path: [{ x: score.x, y: score.y }] };
    walks.set(score, walk);
  }
  const turnEvery = CATERPILLAR.squirmHz / CATERPILLAR.turnHz;
  while (walk.step < squirms) {
    walk.step += 1;
    const { random } = walk;
    const reach = Math.hypot(walk.target.x - walk.x, walk.target.y - walk.y);
    if (reach < CATERPILLAR.retarget) {
      walk.target = { x: random() * CATERPILLAR.canvas, y: random() * CATERPILLAR.canvas };
    }
    const toward = Math.atan2(walk.target.y - walk.y, walk.target.x - walk.x) * 180 / Math.PI;
    walk.angle = rotateToward(walk.angle, toward, random() * CATERPILLAR.turnToward);
    // The slower "Turn" timer adds its own jitter on top of the squirm's steer.
    if (walk.step % turnEvery < 1) {
      walk.angle += random() * CATERPILLAR.turnJitter * 2 - CATERPILLAR.turnJitter;
    }
    const radians = walk.angle * Math.PI / 180;
    walk.x += Math.cos(radians);
    walk.y += Math.sin(radians);
    walk.path.push({ x: walk.x, y: walk.y });
    if (walk.path.length > walk.body) walk.path.shift();
  }
  return walk.path;
}

function rotateToward(from, to, degrees) {
  let delta = ((to - from + 540) % 360) - 180;
  if (Math.abs(delta) > degrees) delta = Math.sign(delta) * degrees;
  return from + delta;
}

// A tiny generator of the score's own, so a walk stays reproducible from the
// seed the score carries rather than from the proposal's shared stream.
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

export const caterpillarProposal = frozen({
  version: 1,
  slug: "caterpillar",
  label: "Caterpillar",
  compatible: true,
  assets: frozen([S4, S5]),
  source: frozen({ ...CATERPILLAR, actionSheet: "Caterpillar",
    objects: frozen(["head", "CaterpillarTailpiece", "CaterpillarLead"]),
    reconstructed: "the pinned sprite chain is drawn as a trailing path",
    // Construct tinted each segment with Set color; AC's paste cannot tint, so
    // the recovered ramp rides in the score and only shows on the fallback.
    tintRendering: "not modeled" }),
  generate({ random, width, height, base }) {
    const [low, high] = CATERPILLAR.segments;
    const length = Math.floor(low + random() * (high - low + 1)) - 1;
    const side = Math.floor(random() * SIDES.length);
    const { axis, at, heading } = SIDES[side];
    const along = random() * CATERPILLAR.canvas;
    const rainbow = length === CATERPILLAR.rainbowLength;
    const hue = random();
    const saturation = CATERPILLAR.saturation[0] +
      random() * (CATERPILLAR.saturation[1] - CATERPILLAR.saturation[0]);
    // Construct laid the caterpillar out on a 256 painting; scale its margins
    // and spacing onto whatever this canvas actually is.
    const scale = Math.min(width, height) / CATERPILLAR.canvas;
    const colors = frozen(Array.from({ length: length + 1 }, (_, index) => rainbow
      ? hslaToRgba(hue + index / (length + 1), 1, .5, 1)
      : hslaToRgba(hue, saturation, .5 - index / Math.max(1, length) / 2 * .5, 1)));
    return frozen({
      ...base,
      kind: "caterpillar",
      seed: Math.floor(random() * 0xffffffff),
      length, side, rainbow, hue, saturation, scale, colors,
      angle: heading,
      x: axis === "y" ? along : at,
      y: axis === "y" ? at : along,
      width, height,
      brush: frozen({
        slug: "caterpillar",
        params: frozen([String(length + 1)]),
        colon: frozen([]),
        parameters: frozen({ segments: length + 1, side, rainbow, hue, saturation }),
      }),
    });
  },
  render(api, score, tick) {
    const squirms = Math.floor(tick * CATERPILLAR.squirmHz / 60);
    const path = walkTo(score, squirms);
    const place = (index) => path[Math.max(0, path.length - 1 - index * CATERPILLAR.spacing)];
    for (let index = score.length; index >= 0; index -= 1) {
      const at = place(index);
      const x = at.x * score.scale;
      const y = at.y * score.scale;
      const part = index === 0 ? caterpillarSprites.head : caterpillarSprites.tail;
      const frame = part.frames[Math.floor(tick * part.fps / 60) % part.frames.length];
      if (!spritePaste(api, frame, x, y, Math.max(1, Math.round(score.scale)))) {
        api.ink(score.colors[index]).oval(x, y, frame.w, frame.h, true);
      }
    }
  },
});
