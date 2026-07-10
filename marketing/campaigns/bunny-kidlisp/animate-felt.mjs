// animate-felt.mjs — felt bunny still → Seedance 2.0 motion clip (silent-ish),
// motion-only prompt (the felt still carries the look). → ~/Desktop.
import { generateShot } from "../../../pop/lib/fal.mjs";
import { homedir } from "node:os";

const r = await generateShot({
  image: new URL("./gens/felt-v1.png", import.meta.url).pathname,
  prompt:
    "stop-motion needle-felt diorama coming gently to life: the wool bunnies' " +
    "long felt ears twitch and tip, one bunny slowly turns its head toward the " +
    "screen, tiny breathing wobble in the woolly bodies; the colorful KidLisp " +
    "animation on the little laptop screen softly pulses and morphs through its " +
    "bands of color; very slow, gentle push-in; warm daylight steady. cozy, " +
    "tender, locked camera, subtle handmade stop-motion motion only.",
  duration: "5",
  ratio: "9:16",
  resolution: "720p",
  tier: "fast",
  outPath: `${homedir()}/Desktop/bunny-kidlisp-felt.mp4`,
  label: "bunny-felt",
});
console.log(JSON.stringify(r, null, 2));
