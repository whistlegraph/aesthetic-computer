import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));

// Fuser owns these values; Captutor only consumes the generic contract.
// The mark and Marund word are deliberately separate, compact signatures.
export const fuserEffectTheme = Object.freeze({
  color: "#111111",
  ringColor: "#111111",
  glowColor: "#ffffff",
  labelColor: "#090909",
  labelShadow: "-1px 0 0 rgba(255,255,255,.98), 1px 0 0 rgba(255,255,255,.98), 0 -1px 0 rgba(255,255,255,.98), 0 1px 0 rgba(255,255,255,.98), 0 3px 2px rgba(0,0,0,.62)",
  ringBlur: 0.12,
  ringShadowBlur: 1.35,
  ringShadowColor: "rgba(255,255,255,.98)",
  glowBlur: 1.75,
  glowShadowBlur: 1.6,
  glowShadowColor: "rgba(0,0,0,.46)",
  ringOpacity: 0.98,
  glowOpacity: 0.76,
  scrollIntoView: false,
});

export default Object.freeze({
  id: "fuser",
  markAsset: join(HERE, "..", "assets", "fuser-mark.svg"),
  label: "Fuser",
  labelAsset: "/Users/jas/Developer/fuser/apps/app/public/fuser-thumbnail-logo.svg",
  labelAssetCrop: { x:86 / 202, y:69 / 161, width:65 / 202, height:21 / 161, aspect:202 / 161 },
  font: join(HERE, "..", "assets", "Marund.ttf"),
  labelWeight: 700,
  labelStrokeFraction: 0.018,
  periodSec: 8,
  driftFraction: 0.0035,
  bobFraction: 0.002,
  opacity: 0.84,
  markColor: "#17151a",
  labelColor: "#17151a",
  shadow: { color:"#a58cbc", opacity:92, blur:0.55, x:1, y:2 },
  leftMarkCenterY: 0.748,
  leftLabelCenterY: 0.8,
  rightMarkCenterY: 0.252,
  rightLabelCenterY: 0.2,
  labelCharacterCuts: [[0, 0.225], [0.225, 0.423], [0.423, 0.626], [0.626, 0.843], [0.843, 1]],
  characterMotion: {
    driftFraction: 0.0014,
    periodSec: 3.2,
    shimmerPeriodSec: 2.4,
    shimmerAmount: 0.18,
  },
  formats: {
    docs: { edgeFraction: 0.018, markSideFraction:0.017, labelPxFraction:0.024 },
    youtube: { edgeFraction: 0.02, markSideFraction:0.019, labelPxFraction:0.026 },
    reel: { edgeFraction: 0.034, markSideFraction:0.022, labelPxFraction:0.03 },
    vertical: { edgeFraction: 0.032, markSideFraction:0.021, labelPxFraction:0.029 },
  },
});
