// KidLisp Mini Transforms
// Fade gradient background, timing forms
// ~100 lines, produces ~2-3 KB minified

import { resolveColor, rainbowColor, hslToRgb } from './color.mjs';

class TransformState {
  constructor(width, height) {
    this.width = width;
    this.height = height;
    this.frame = 0;
    this.fadeColors = null;
    this.pendingTransforms = [];
  }

  tick() {
    this.frame++;
  }

  setFade(colors) {
    // Parse color names: "red-blue-black-blue-red" → resolve to RGB
    this.fadeColors = colors.map(name => resolveColor(name));
  }

  applyFade(renderer) {
    if (!this.fadeColors) return;

    // Cycle through fade colors based on frame
    const idx = Math.floor(this.frame / 20) % this.fadeColors.length;
    const color = this.fadeColors[idx];
    renderer.wipe(color[0], color[1], color[2], color[3]);
  }

  evaluateTimingExpr(expr, startVal, endVal) {
    if (typeof expr === 'object' && expr.type === 'timing-repeating') {
      const ms = expr.ms;
      const elapsed = (this.frame * 16) % ms; // assume ~16ms per frame
      const progress = elapsed / ms;
      return startVal + (endVal - startVal) * progress;
    }

    if (typeof expr === 'object' && expr.type === 'timing-once') {
      const ms = expr.ms;
      const elapsed = this.frame * 16;
      const progress = Math.min(1, elapsed / ms);
      return startVal + (endVal - startVal) * progress;
    }

    // Not a timing expression, return as-is
    return expr;
  }

  interpolate(value, startVal, endVal) {
    // If value is between 0 and 1, it's a progress value
    if (typeof value === 'number' && value >= 0 && value <= 1) {
      return startVal + (endVal - startVal) * value;
    }
    return value;
  }
}

// Helper: parse and apply an interpolated timing form
// e.g., (1s... 24 64) → current alpha value interpolated between 24-64
function evalTimingForm(timingExpr, frame, ms, repeating) {
  if (!timingExpr) return 0;

  let elapsed;
  if (repeating) {
    elapsed = (frame * 16) % ms; // repeat
  } else {
    elapsed = frame * 16; // one-shot
    elapsed = Math.min(elapsed, ms); // clamp
  }

  const progress = elapsed / ms;
  return progress;
}

function applyInterpolation(progress, startVal, endVal) {
  if (typeof startVal !== 'number' || typeof endVal !== 'number') {
    return startVal;
  }
  return startVal + (endVal - startVal) * progress;
}

export { TransformState, evalTimingForm, applyInterpolation };
