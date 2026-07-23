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
  asset: join(HERE, "..", "assets", "fuser-thumbnail-logo.svg"),
  periodSec: 20,
  driftFraction: 0.0032,
  bobFraction: 0.0018,
  opacity: 0.97,
  transparentColors: [{ color:"#171717", fuzz:12 }],
  shadow: { opacity: 86, blur: 1.6, x: 2, y: 3 },
  formats: {
    docs: { longSideFraction: 0.16, edgeFraction: 0.038, leftCenterY: 0.79, rightCenterY: 0.21 },
    youtube: { longSideFraction: 0.17, edgeFraction: 0.042, leftCenterY: 0.78, rightCenterY: 0.22 },
    reel: { longSideFraction: 0.18, edgeFraction: 0.072, leftCenterY: 0.81, rightCenterY: 0.19 },
    vertical: { longSideFraction: 0.17, edgeFraction: 0.068, leftCenterY: 0.82, rightCenterY: 0.18 },
  },
});
