import "./profile-card.mjs";

// The canvas piece only hosts this component; the DOM owns layout and input.
const host = document.getElementById("ac-profile-page");
if (host) {
  const stop = (event) => event.stopPropagation();
  for (const type of ["keydown", "keyup", "pointerdown", "pointerup", "touchstart", "touchmove", "wheel"]) {
    host.addEventListener(type, stop);
  }
}
