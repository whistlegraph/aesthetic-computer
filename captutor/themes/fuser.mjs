import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));

// Fuser owns these values; Captutor only consumes the generic contract.
// The supplied asset is already the Fuser glyph + the word “Fuser”, so each
// inward-facing side mark remains a single responsive lockup.
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
  font: join(HERE, "..", "assets", "Marund.ttf"),
  periodSec: 20,
  driftFraction: 0.0032,
  bobFraction: 0.0018,
  opacity: 0.97,
  shadow: { opacity: 92, blur: 1.2, x: 2, y: 3 },
  formats: {
    docs: { edgeFraction: 0.038, markSideFraction:0.105, labelPxFraction:0.075 },
    youtube: { edgeFraction: 0.042, markSideFraction:0.11, labelPxFraction:0.078 },
    reel: { edgeFraction: 0.072, markSideFraction:0.12, labelPxFraction:0.084 },
    vertical: { edgeFraction: 0.068, markSideFraction:0.115, labelPxFraction:0.082 },
  },
});
