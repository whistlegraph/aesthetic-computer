// Stamp, 24.02.13.15.23
// A basic stamp brush that imports a user's painting.

import { spritePaste } from "../lib/nopaint-construct-sprites.mjs";
import {
  STAMP_SHEETS,
  STAMP_USERS,
  stampAnimations,
} from "../lib/nopaint-stamp-sprites.mjs";

// 🖌️ Brush
function brush({ clear, pen }) {
  clear().stamp("@jeffrey/2024.2.13.15.31.05.635", pen.x, pen.y);
}

// Machine-readable proposal contract beside the real Stamp brush. No Paint 3
// imports this module rather than keeping a second stamp imitation, exactly as
// Box and Line do.
//
// Recovered whole from Construct: the nineteen handles, which of them carry
// looping animations, every crop and origin in `nopaint-stamp-sprites.mjs`, the
// `size` / `direction` / `mirror` parameter names, and the two cues. Where the
// stamp lands, how big it gets, and how far it turns are this contract's own
// reading of a gesture-driven brush — see `source.reconstructed`.
const nopaintProposal = Object.freeze({
  version: 1,
  slug: "stamp",
  label: "Stamp",
  compatible: true,
  assets: STAMP_SHEETS,
  source: Object.freeze({
    actionSheet: "Stamp",
    object: "Stamp",
    users: STAMP_USERS,
    parameters: Object.freeze(["size", "direction", "mirror"]),
    cues: Object.freeze({ place: "stamp - stick", animated: "common - jitter" }),
    // Construct kept StampLastChosenUserNumber so a handle never repeats
    // back-to-back. A seeded, single-shot generate cannot see the last
    // proposal, so the rule is recorded but not enforced.
    lastChosenUserRule: "recovered, not modeled",
    reconstructed: Object.freeze(["placement", "size range", "angle range"]),
    // Construct mirrored on a negative X scale, which paste now reaches by
    // handing grid a per-axis {x, y}.
    mirrorRendering: "negative x scale",
  }),
  generate({ random, width, height, base }) {
    const user = STAMP_USERS[Math.floor(random() * STAMP_USERS.length)];
    const loops = Object.keys(stampAnimations)
      .filter((name) => name.startsWith(`@${user}-a-`));
    const animation = loops.length && random() < .5
      ? loops[Math.floor(random() * loops.length)]
      : `@${user}`;
    const frames = stampAnimations[animation].frames.length;
    // Integer scales up to 8 take paste's nearest-neighbour fast path.
    const scale = 1 + Math.floor(random() * 4);
    return Object.freeze({
      ...base,
      kind: "stamp",
      user,
      animation,
      index: Math.floor(random() * frames),
      x: Math.floor(random() * width),
      y: Math.floor(random() * height),
      scale,
      angle: Math.floor(random() * 8) * 45,
      mirrored: random() < .5,
      brush: Object.freeze({
        slug: "stamp",
        params: Object.freeze([user]),
        colon: Object.freeze([]),
        parameters: Object.freeze({ animation, scale, frames }),
      }),
    });
  },
  render(api, score, frame) {
    const animation = stampAnimations[score.animation];
    // Stills hold the chosen index; the "-a-N" collections run at their own fps.
    const index = animation.loop
      ? Math.floor(frame * animation.fps / 60) % animation.frames.length
      : score.index;
    const sprite = animation.frames[index];
    if (spritePaste(api, sprite, score.x, score.y, score.scale,
      { angle: score.angle, mirrored: score.mirrored })) return;
    api.ink(score.color).box(
      score.x - sprite.w * sprite.ox * score.scale,
      score.y - sprite.h * sprite.oy * score.scale,
      sprite.w * score.scale,
      sprite.h * score.scale,
    );
  },
});

export { brush, nopaintProposal };
