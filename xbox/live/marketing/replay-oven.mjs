// Oskiewar Replay Oven — deterministic, game-native, high-quality demo burns.
//
// Keep this seam between simulation capture and distribution. Offline-only
// lighting, grading, ray/depth passes, and supersampling belong behind this
// API; the live game must never pay their frame cost.

import { renderReel } from "./render.mjs";

export const replayOvenProfile = Object.freeze({
  fps: 60,
  sourceQuality: 92,
  videoCodec: "h264",
  crf: 14,
  hud: true,
  offlinePasses: Object.freeze([
    "simulation", "audio", "fixed-step", "contrast", "color", "detail",
  ]),
});

export function bakeReplay(spec, options) {
  return renderReel({ ovenStyle: "cinematic", hud: replayOvenProfile.hud,
    ...spec }, options);
}
