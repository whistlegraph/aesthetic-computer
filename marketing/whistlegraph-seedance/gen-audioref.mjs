// Companion batch: same whistlegraph variations but with the ACTUAL audio
// track passed as @Audio1 alongside the @Video1 ref, so the output whistling
// follows jeffrey's real melody and chalk sounds.
import { generateReferenceShot } from "/Users/jas/aesthetic-computer/pop/lib/fal-seedance.mjs";

const SRC = "/Users/jas/Downloads/v12044gd0000ca7e5trc77udclceer40.MP4";
const AUD = "/Users/jas/Desktop/seedance-variations/src-audio.mp3";
const OUT = "/Users/jas/Desktop/seedance-variations";

const BASE =
  "@Video1 shows a hand drawing white chalk marks on a green chalkboard " +
  "while a person whistles a simple melody. @Audio1 is the real soundtrack: " +
  "that exact whistled melody with chalk taps and scratches. Create a new " +
  "variation in exactly the same style: same green chalkboard, same natural " +
  "handheld phone framing, same lighting, same kind of hand, and the whistling " +
  "and chalk sounds should match @Audio1. ";

const VARIATIONS = [
  {
    name: "aud1-spiral",
    motion: "The hand draws one continuous chalk spiral that grows outward from the center, finishing with a confident dot.",
  },
  {
    name: "aud2-zigzag",
    motion: "The hand draws a row of tall chalk zigzag waves left to right, then underlines them with one long sweeping stroke.",
  },
  {
    name: "aud3-stars",
    motion: "The hand draws three small five-pointed chalk stars in a diagonal line, each with quick decisive strokes.",
  },
];

for (const v of VARIATIONS) {
  console.log(`\n=== ${v.name} ===`);
  const r = await generateReferenceShot({
    videos: [SRC],
    audios: [AUD],
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
