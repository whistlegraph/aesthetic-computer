// Singing test: voice SINGS (not whistles) and sings actual WORDS as the
// marks are drawn, the way whistlegraphs chant the graph. Same one rule:
// the sung syllables align with the strokes.
import { generateReferenceShot } from "/Users/jas/aesthetic-computer/pop/lib/fal-seedance.mjs";

const SRC1 = "/Users/jas/Downloads/v12044gd0000ca7e5trc77udclceer40.MP4";
const SRC2 = "/Users/jas/Desktop/seedance-variations/src2-trim.mp4";
const OUT = "/Users/jas/Desktop/seedance-variations";

const AESTHETIC =
  "@Video1 and @Video2 show a hand drawing white chalk on a green chalkboard. " +
  "Match their exact visual aesthetic: same green chalkboard, natural handheld " +
  "phone framing, daylight, real hand, dry chalk taps and scratches. " +
  "IMPORTANT: instead of whistling, a warm casual human voice SINGS along — " +
  "unpolished, playful, like someone singing to themselves while drawing. " +
  "THE ONE RULE: the graphics align with the sounds — each sung syllable lands " +
  "exactly on the stroke being drawn, the melody rises and falls with the line, " +
  "and the chalk scratch is heard only while the chalk touches the board. " +
  "Every mark grows only from the chalk tip; no line appears by itself. ";

console.log("\n=== sing-hello ===");
let r = await generateReferenceShot({
  videos: [SRC1, SRC2],
  prompt: AESTHETIC +
    "New drawing: the hand writes the single lowercase word \"hello\" in large " +
    "clear chalk handwriting, one letter at a time, left to right — h, e, l, l, o — " +
    "spelled correctly and legible. The voice SINGS the word slowly as it is " +
    "written: \"hehhh\" while the h and e are drawn, \"lll\" stretched over the " +
    "two l strokes, and a bright held \"...looooh\" as the o closes.",
  duration: "8", ratio: "9:16", resolution: "720p", tier: "fast", audio: true,
  outPath: `${OUT}/sing-hello.mp4`, label: "sing-hello",
});
console.log(r.ok ? `✓ sing-hello: ${(r.bytes / 1e6).toFixed(1)}MB (seed ${r.seed})` : `✗ sing-hello: ${r.error}`);

console.log("\ndone");
