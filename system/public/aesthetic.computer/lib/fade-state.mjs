// Shared fade state for cross-module communication
// Used to pass fadeAlpha from kidlisp coat function to graphics rendering

let fadeAlpha = null;
let preserveFadeAlpha = false; // Flag to preserve fadeAlpha through graphics operations

export function setFadeAlpha(alpha) {
  // Reduced logging frequency
  if (fadeAlpha !== alpha) {
    // console.log("📝 FADE-STATE: Setting fadeAlpha to", alpha, "previous was:", fadeAlpha);
  }
  fadeAlpha = alpha;
}

export function getFadeAlpha() {
  // console.log("📖 FADE-STATE: Getting fadeAlpha:", fadeAlpha);
  return fadeAlpha;
}

export function clearFadeAlpha() {
  if (preserveFadeAlpha) {
    // console.log("🛡️ FADE-STATE: Preserving fadeAlpha from clear, value:", fadeAlpha);
    return;
  }
  // Only log when we actually have a value to clear
  if (fadeAlpha !== null) {
    // console.log("🗑️ FADE-STATE: Clearing fadeAlpha, was:", fadeAlpha);
    // console.trace("🗑️ FADE-STATE: clearFadeAlpha called from:");
  }
  fadeAlpha = null;
}

export function setPreserveFadeAlpha(preserve) {
  // Only log when value actually changes
  if (preserveFadeAlpha !== preserve) {
    // console.log("🔒 FADE-STATE: Setting preserve flag to", preserve, "was:", preserveFadeAlpha);
  }
  preserveFadeAlpha = preserve;
}

export function getPreserveFadeAlpha() {
  return preserveFadeAlpha;
}