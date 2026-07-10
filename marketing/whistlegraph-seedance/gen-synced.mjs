// Whistlegraph-rule batch: NEW drawings, and the graphics must align with
// the sounds — whistle pitch follows the drawn line, chalk scratch only
// while chalk touches the board. Aesthetics ride on two actual source
// recordings as @Video1/@Video2 (combined <15s). Audio is generated.
import { generateReferenceShot } from "/Users/jas/aesthetic-computer/pop/lib/fal.mjs";

const SRC1 = "/Users/jas/Downloads/v12044gd0000ca7e5trc77udclceer40.MP4";
const SRC2 = "/Users/jas/Desktop/seedance-variations/src2-trim.mp4";
const OUT = "/Users/jas/Desktop/seedance-variations";

const BASE =
  "@Video1 and @Video2 show a hand drawing white chalk on a green chalkboard " +
  "while a person casually whistles. Match their exact visual and sonic " +
  "aesthetic: same green chalkboard, natural handheld phone framing, daylight, " +
  "real hand, soft casual whistling, dry chalk taps and scratches. " +
  "THE ONE RULE: the graphics must align with the sounds. The whistled melody " +
  "and the drawing are the same gesture — when the drawn line rises, the " +
  "whistle rises in pitch; when the line falls, the whistle falls; fast strokes " +
  "get fast notes, long strokes get held notes. The chalk scratch is heard " +
  "exactly and only while the chalk touches the board, silent when the hand " +
  "lifts. New drawing: ";

const VARIATIONS = [
  {
    name: "sync1-mountains",
    motion: "the hand draws a jagged mountain range in one continuous line, each peak a rising-then-falling whistle note, ending on a long flat valley with one long low held note.",
  },
  {
    name: "sync2-stairs",
    motion: "the hand draws an ascending staircase step by step, the whistle climbing one scale note per step, then one fast diagonal line back down with a quick descending slide-whistle fall.",
  },
  {
    name: "sync3-bounces",
    motion: "the hand draws a series of bouncing arcs that get smaller left to right, each arc a whistled note that leaps up and settles down, the last tiny arc a short high chirp.",
  },
];

for (const v of VARIATIONS) {
  console.log(`\n=== ${v.name} ===`);
  const r = await generateReferenceShot({
    videos: [SRC1, SRC2],
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
