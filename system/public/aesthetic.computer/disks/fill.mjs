// Fill, 2024.4.11.07.08.03.042
// Fill pixels in with a particular color.

import { isFadeColor } from "../lib/num.mjs";
import { setFadeAlpha } from "../lib/fade-state.mjs";
import { generateNopaintHUDLabel } from "../lib/color-highlighting.mjs";

let color;

// 🥤️ Filter
function filter({ params, num, pen, flood, hud }) {
  // Parse color with fade support
  color ||= num.parseColor(params);
  
  // Generate colored HUD label for color highlighting
  if (hud?.label && params?.length > 0) {
    const coloredLabel = generateNopaintHUDLabel("fill", params[0], params.slice(1), "", { hud });
    hud.label(coloredLabel);
  }
  
  // Handle fade colors with alpha
  let floodColor = color;
  if (isFadeColor(color)) {
    if (color.alpha !== undefined) {
      setFadeAlpha(color.alpha);
    }
    floodColor = color.fadeString;
  }
  
  flood(pen.x, pen.y, floodColor);
}

export { filter };