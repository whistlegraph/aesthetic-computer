// Storyboard animate: Seedance image-to-video pinned between the REAL start
// frame and the validated gpt-image-2 end keyframe. The end state is known
// good, so stray marks can't survive into the final frame.
import { generateShot } from "/Users/jas/aesthetic-computer/pop/lib/fal-seedance.mjs";

const HERE = "/Users/jas/Desktop/seedance-variations/storyboard";

const PROMPT =
  "A real phone video of a hand drawing on a green chalkboard while a person " +
  "casually whistles. Starting from the first frame, the hand draws the " +
  "smiling chalk fish seen in the last frame, stroke by stroke: first the " +
  "almond body outline in one long curve, then the triangle tail, then the " +
  "eye, the smile, and three short gill lines. THE ONE RULE: the graphics " +
  "align with the sounds — the whistle traces each stroke as it is drawn, " +
  "rising and falling with the line, and the chalk scratch is heard exactly " +
  "and only while the chalk touches the board. Every mark grows only from " +
  "the chalk tip; no line appears by itself. Natural handheld framing, " +
  "dry chalk taps, soft casual whistling.";

const r = await generateShot({
  image: `${HERE}/start-frame.png`,
  endImage: `${HERE}/end-fish.png`,
  prompt: PROMPT,
  duration: "8",
  ratio: "9:16",
  resolution: "720p",
  tier: "fast",
  audio: true,
  outPath: `${HERE}/board-fish-t2.mp4`,
  label: "board-fish-t2",
});
console.log(r.ok ? `✓ board-fish: ${(r.bytes / 1e6).toFixed(1)}MB in ${r.seconds.toFixed(0)}s (seed ${r.seed})` : `✗ ${r.error}`);
