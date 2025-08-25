// Shared fade state for cross-module communication
// Used to pass fadeAlpha from kidlisp coat function to graphics rendering

let fadeAlpha = null;

export function setFadeAlpha(alpha) {
  fadeAlpha = alpha;
}

export function getFadeAlpha() {
  return fadeAlpha;
}

export function clearFadeAlpha() {
  fadeAlpha = null;
}
