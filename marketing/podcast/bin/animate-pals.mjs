// animate-pals.mjs — turn a still "pals" logo tile into a seamless LOOPING
// turnaround clip with Seedance 2.0. The trick: pass the same still as both
// the start frame (image) and the end frame (endImage) so the last frame
// snaps back to the first → the object spins 360° and loops cleanly.
//
// Usage: node bin/animate-pals.mjs [slug]   (default: nat-amethyst)
//   → ~/Desktop/pals-turnaround-<slug>.mp4
import { generateShot } from "../../../pop/lib/fal.mjs";
import { homedir } from "node:os";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const slug = process.argv[2] || "nat-amethyst";
const still = resolve(HERE, "..", "out", "pals", `${slug}.png`);

const r = await generateShot({
  image: still,
  endImage: still, // same first & last frame → seamless loop
  prompt:
    "product turntable: the sculpted logo object rotates smoothly and " +
    "continuously through a full 360-degree spin on an invisible turntable and " +
    "returns to the exact starting angle, a seamless loop. as it turns, its " +
    "polished crystal facets catch and refract soft studio light with gentle " +
    "moving glints and highlights. the flat pastel background stays perfectly " +
    "still and unchanged. locked, centered camera with NO camera movement — " +
    "only the object rotates in place. crisp, high detail, even studio light, " +
    "seamless loopable turntable, no text, no extra objects.",
  duration: "5",
  ratio: "1:1",
  resolution: "720p",
  tier: "standard", // nicer facets/light than fast for a hero turnaround
  outPath: `${homedir()}/Desktop/pals-turnaround-${slug}.mp4`,
  label: `pals-turn-${slug}`,
});
console.log(JSON.stringify(r, null, 2));
