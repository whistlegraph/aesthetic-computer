// Menu Fighter, 26.07.20
// Two pals enter. The menu decides what happens next.

// Keep `fight` as the implementation while Menu Fighter's product shell takes
// shape. Re-exporting it means both routes exercise the exact same deterministic
// sim and rollback code; there must never be a second gameplay implementation.
export { boot, paint, act, sim, leave, system } from "./fight.mjs";

export function meta() {
  return {
    title: "Menu Fighter",
    desc: "A rollback-ready two-player fighter starring Pals.",
    icon_url: "/menu-fighter-icon.png",
    image_url: "/menu-fighter-icon.png",
  };
}
