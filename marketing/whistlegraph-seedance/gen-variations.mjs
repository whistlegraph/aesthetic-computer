// One-off: Seedance 2.0 reference-to-video variations of jeffrey's
// 7s whistlegraph chalkboard recording, with generated audio.
import { generateReferenceShot } from "/Users/jas/aesthetic-computer/pop/lib/fal-seedance.mjs";

const SRC = "/Users/jas/Downloads/v12044gd0000ca7e5trc77udclceer40.MP4";
const OUT = "/Users/jas/Desktop/seedance-variations";

const BASE =
  "@Video1 shows a hand drawing white chalk marks on a green chalkboard " +
  "while a person whistles a simple melody; chalk taps and scratches are audible. " +
  "Create a new variation in exactly the same style: same green chalkboard, same " +
  "natural handheld phone framing, same lighting, same kind of hand, same chalk " +
  "sound and casual whistled tune. ";

const VARIATIONS = [
  {
    name: "var1-spiral",
    motion: "The hand draws one continuous chalk spiral that grows outward from the center, finishing with a confident dot.",
  },
  {
    name: "var2-zigzag",
    motion: "The hand draws a row of tall chalk zigzag waves left to right, then underlines them with one long sweeping stroke.",
  },
  {
    name: "var3-stars",
    motion: "The hand draws three small five-pointed chalk stars in a diagonal line, each with quick decisive strokes.",
  },
];

for (const v of VARIATIONS) {
  console.log(`\n=== ${v.name} ===`);
  const r = await generateReferenceShot({
    videos: [SRC],
    prompt: BASE + v.motion,
    duration: "8",
    ratio: "9:16",
    resolution: "720p",
    tier: "fast",
    audio: true,
    outPath: `${OUT}/${v.name}.mp4`,
    label: v.name,
  });
  console.log(r.ok ? `✓ ${v.name}: ${(r.bytes / 1e6).toFixed(1)}MB in ${r.seconds.toFixed(0)}s (seed ${r.seed})` : `✗ ${v.name}: ${r.error}`);
}
console.log("\ndone");
