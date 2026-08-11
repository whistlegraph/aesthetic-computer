// Sprite-backed No Paint contracts recovered from Construct's object and
// action-sheet data. Frame rectangles, origins, speeds, and animation names
// are exact; movement marked reconstructed is an AC interpretation.

const frozen = (value) => Object.freeze(value);
const frame = (sheet, x, y, w, h, ox = .5, oy = .5) => frozen({ sheet, x, y, w, h, ox, oy });
const animation = (fps, frames) => frozen({ fps, loop: true, frames: frozen(frames) });

const B0 = "/nopaint.art/images/bubbles-sheet0.png";
const B1 = "/nopaint.art/images/bubbles-sheet1.png";
const W0 = "/nopaint.art/images/walkerella-sheet0.png";
const W1 = "/nopaint.art/images/walkerella-sheet1.png";
const F0 = "/nopaint.art/images/frames-sheet0.png";

export const bubblesAnimations = frozen({
  XS: animation(4, [frame(B1, 19, 49, 8, 8), frame(B1, 19, 33, 8, 8)]),
  S: animation(2, [frame(B1, 1, 33, 16, 16), frame(B1, 1, 1, 16, 16)]),
  M: animation(5, [frame(B0, 1, 193, 32, 32), frame(B0, 65, 193, 32, 32)]),
  L: animation(1, [frame(B0, 67, 65, 48, 48), frame(B0, 67, 129, 48, 48)]),
  XL: animation(1, [frame(B0, 1, 67, 64, 64), frame(B0, 1, 1, 64, 64)]),
});

// The Frames object holds one still animation of eleven 256×256 borders drawn
// from the top-left. Construct's CycleFrame walks them in this exact order.
export const frameFrames = frozen([
  [1, 1], [513, 1], [1, 513], [517, 513], [259, 513], [517, 1025],
  [1, 1025], [259, 1025], [517, 1537], [1, 1537], [259, 1537],
].map(([x, y]) => frame(F0, x, y, 256, 256, 0, 0)));

// Compact transcription of all nine WalkerElla animations. Each row is
// [sheet, x, y, width, height, originX, originY].
const walkerRows = frozen({
  "1": [[0,260,769,65,56,.184615,.464286],[0,449,833,61,56,.163934,.464286],[0,417,769,65,56,.184615,.428571],[0,1,833,64,56,.203125,.428571],[0,67,833,57,56,.140351,.410714],[0,260,833,60,56,.183333,.482143],[0,1,769,64,56,.15625,.464286],[0,110,641,65,70,.184615,.457143]],
  "2": [[0,129,385,66,98,.69697,.44898],[0,449,513,54,98,.537037,.438776],[0,321,385,62,94,.564516,.62766],[0,460,257,50,90,.62,.611111],[0,65,641,42,98,.654762,.479592],[0,220,769,38,98,.710526,.489796]],
  "3": [[0,356,961,32,59,.375,.516949],[1,0,0,32,35,.40625,.1]],
  "4": [[0,214,513,98,63,.428571,.857143],[0,129,129,115,75,.478261,.8],[0,130,1,120,92,.391667,.869565],[0,252,1,127,81,.385827,.851852],[0,1,129,120,75,.383333,.84],[0,1,1,127,92,.362205,.847826],[0,385,1,110,91,.372727,.846154],[0,385,129,116,80,.37069,.875],[0,257,129,117,80,.324786,.8625]],
  "5": [[0,1,513,53,98,.471698,.489796],[0,321,513,53,98,.377358,.510204],[0,246,641,50,95,.5,.505263],[0,65,513,52,98,.403846,.530612]],
  "6": [[0,432,641,68,68,.514706,.382353],[0,182,641,62,80,.483871,.5],[0,126,897,35,89,.457143,.662921],[0,321,897,32,86,.5,.534884],[0,1,897,32,86,.53125,.465116],[0,298,641,68,65,.514706,.4]],
  "7": [[0,220,385,68,97,.455882,.123711],[0,386,257,72,92,.5,.130435],[0,317,257,67,102,.567164,.186275],[0,146,257,72,97,.388889,.134021],[0,385,385,67,97,.567164,.134021],[0,129,513,52,102,.480769,.176471],[0,385,513,52,102,.423077,.196078],[0,1,385,62,102,.467742,.166667],[0,240,257,75,97,.533333,.113402]],
  "8": [[0,257,897,27,95,.518519,.463158],[0,481,897,27,95,.592593,.452632],[0,129,769,41,100,.707317,.5],[0,455,385,56,100,.535714,.49],[0,1,641,43,100,.465116,.49],[0,369,641,46,95,.456522,.473684],[0,327,769,36,100,.388889,.5],[0,65,385,61,102,.557377,.490196],[0,1,257,71,100,.577465,.5],[0,74,257,70,100,.585714,.51],[0,402,897,35,100,.542857,.51],[0,365,769,35,100,.571429,.48]],
  "9": [[0,439,897,35,78,.557143,.128205],[0,163,897,38,80,.381579,.11875],[0,203,897,35,86,.357143,.168605],[0,65,897,30,86,.366667,.180233],[0,289,897,30,81,.4,.123457],[0,97,897,25,81,.38,.123457]],
});

export const walkerAnimations = frozen(Object.fromEntries(Object.entries(walkerRows)
  .map(([name, rows]) => [name, animation(5, rows.map(([sheet, ...values]) => frame(sheet ? W1 : W0, ...values)))])));

// Paste one recovered frame with its Construct origin honoured. Returns false
// when the sheet has not loaded yet so callers can fall back to a primitive.
//
// Mirroring is Construct's negative X scale, which `paste` reaches by handing
// grid a per-axis {x, y}. The origin has to flip with it: a mirrored frame is
// anchored from its right edge, so the offset becomes 1 - ox. An unmirrored
// paste keeps its plain number scale and its fast path.
export function spritePaste(
  { paste, nopaintAssets }, sprite, x, y, scale = 1, { angle = 0, mirrored = false } = {},
) {
  const painting = nopaintAssets?.get(sprite.sheet);
  if (!painting) return false;
  const ox = mirrored ? 1 - sprite.ox : sprite.ox;
  paste(painting,
    Math.round(x - sprite.w * ox * scale),
    Math.round(y - sprite.h * sprite.oy * scale), {
      scale: mirrored ? { x: -scale, y: scale } : scale,
      angle,
      crop: { x: sprite.x, y: sprite.y, w: sprite.w, h: sprite.h },
    });
  return true;
}

export const bubblesProposal = frozen({
  version: 1, slug: "bubbles", label: "Bubbles", compatible: true,
  assets: frozen([B0, B1]),
  source: frozen({ actionSheet: "Bubbles", animations: bubblesAnimations,
    physics: "Construct Physics", movement: "reconstructed from MakeBubble timer events" }),
  generate({ random, width, height, base }) {
    const names = Object.keys(bubblesAnimations);
    const bubbles = Array.from({ length: 18 }, () => frozen({
      x: Math.floor(random() * width), y: Math.floor(random() * height),
      animation: names[Math.floor(random() * names.length)],
      speed: 3 + Math.floor(random() * 3), phase: Math.floor(random() * 120),
    }));
    return frozen({ ...base, kind: "bubbles", bubbles: frozen(bubbles), height,
      brush: frozen({ slug: "bubbles", params: frozen([]), colon: frozen([]),
        parameters: frozen({ count: bubbles.length, animations: frozen(names) }) }) });
  },
  render(api, score, tick) {
    for (const bubble of score.bubbles) {
      const animation = bubblesAnimations[bubble.animation];
      const sprite = animation.frames[Math.floor((tick + bubble.phase) * animation.fps / 60) % animation.frames.length];
      const y = (bubble.y - tick * bubble.speed / 10 + score.height) % score.height;
      if (!spritePaste(api, sprite, bubble.x, y))
        api.ink(score.color).oval(bubble.x, y, sprite.w, sprite.h, false, 1);
    }
  },
});

export const walkerProposal = frozen({
  version: 1, slug: "walker", label: "Walker", compatible: true,
  assets: frozen([W0, W1]),
  source: frozen({ actionSheet: "Walker", object: "WalkerElla",
    animations: walkerAnimations, movement: "reconstructed from fromTop/fromRight/step" }),
  generate({ random, width, height, base }) {
    const name = String(1 + Math.floor(random() * 9));
    const fromTop = random() < .5;
    const fromRight = random() < .5;
    const scale = .35 + random() * .65;
    return frozen({ ...base, kind: "walker", animation: name, fromTop, fromRight,
      start: frozen({ x: fromRight ? width : 0, y: fromTop ? 0 : height }),
      step: 1 + random() * 2, scale, width, height,
      brush: frozen({ slug: "walker", params: frozen([name]), colon: frozen([]),
        parameters: frozen({ animation: name, fromTop, fromRight, step: true, scale }) }) });
  },
  render(api, score, tick) {
    const animation = walkerAnimations[score.animation];
    const sprite = animation.frames[Math.floor(tick * animation.fps / 60) % animation.frames.length];
    const travel = tick * score.step;
    const x = score.fromRight ? score.width - travel : travel;
    const y = score.fromTop ? travel : score.height - travel;
    if (!spritePaste(api, sprite, x, y, score.scale))
      api.ink(score.color).box(x - 3, y - 3, 6, 6);
  },
});

// Construct starts frameIndex at 1 and runs a repeating one second "CycleFrame"
// timer: knock, then frameIndex = (frameIndex + 1) % AnimationFrameCount. At
// 60hz that is 60 ticks a border.
export const FRAME_CYCLE_TICKS = 60;
export const FRAME_START_INDEX = 1;

export const frameProposal = frozen({
  version: 1, slug: "frame", label: "Frame", compatible: true,
  assets: frozen([F0]),
  source: frozen({ actionSheet: "Frame", object: "Frames", cycle: "CycleFrame",
    frames: frameFrames.length, cycleSeconds: 1, start: FRAME_START_INDEX,
    cue: "frame - knock", knockPlaybackRate: frozen([.25, 2]),
    reconstructed: "the border stretches to fill the painting" }),
  generate({ base, width, height }) {
    // The original never randomised its border: it always opened on index 1
    // and let the second-by-second cycle decide what you kept.
    const start = FRAME_START_INDEX;
    return frozen({ ...base, kind: "frame", start, width, height,
      // The operation is `frame`; the piece that owns it is `frames`, the way
      // Box owns `rect`.
      brush: frozen({ slug: "frames", params: frozen([String(start)]), colon: frozen([]),
        parameters: frozen({ start, frames: frameFrames.length, cycleSeconds: 1 }) }) });
  },
  render(api, score, tick) {
    const index = (score.start + Math.floor(tick / FRAME_CYCLE_TICKS)) % frameFrames.length;
    const sprite = frameFrames[index];
    const sheet = api.nopaintAssets?.get(sprite.sheet);
    if (sheet) {
      api.paste(sheet, 0, 0, {
        width: score.width, height: score.height,
        crop: { x: sprite.x, y: sprite.y, w: sprite.w, h: sprite.h },
      });
      return;
    }
    const inset = Math.round(Math.min(score.width, score.height) * .04);
    api.ink(score.color).box(inset, inset,
      score.width - inset * 2, score.height - inset * 2, "outline:" + inset);
  },
});
