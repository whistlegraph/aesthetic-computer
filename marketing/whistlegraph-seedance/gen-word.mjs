// Word test: the hand WRITES a word in chalk, whistle articulating each
// letter's strokes (same one-rule sync). Two takes — model text is flaky.
import { generateReferenceShot } from "/Users/jas/aesthetic-computer/pop/lib/fal-seedance.mjs";

const SRC1 = "/Users/jas/Downloads/v12044gd0000ca7e5trc77udclceer40.MP4";
const SRC2 = "/Users/jas/Desktop/seedance-variations/src2-trim.mp4";
const OUT = "/Users/jas/Desktop/seedance-variations";

const BASE =
  "@Video1 and @Video2 show a hand drawing white chalk on a green chalkboard " +
  "while a person casually whistles. Match their exact visual and sonic " +
  "aesthetic: same green chalkboard, natural handheld phone framing, daylight, " +
  "real hand, soft casual whistling, dry chalk taps and scratches. " +
  "THE ONE RULE: the graphics must align with the sounds — the chalk scratch " +
  "is heard exactly and only while the chalk touches the board, silent when " +
  "the hand lifts, and the whistle traces each stroke as it is drawn. " +
  "New drawing: ";

const SHOTS = [
  {
    name: "word-hello-t1",
    motion: "the hand writes the single lowercase word \"hello\" in large clear chalk handwriting, one letter at a time, left to right — h, e, l, l, o — each letter getting its own short whistled phrase that rises and falls with the pen strokes, ending with a bright held note as the o closes. The word must be spelled correctly and stay legible.",
  },
  {
    name: "word-hello-t2",
    motion: "the hand writes the single lowercase word \"hello\" in large clear chalk handwriting, one letter at a time, left to right — h, e, l, l, o — each letter getting its own short whistled phrase that rises and falls with the pen strokes, ending with a bright held note as the o closes. The word must be spelled correctly and stay legible.",
  },
  {
    name: "pic-sun",
    motion: "the hand draws a recognizable smiling sun: first a big circle with one long circular whistled note, then short rays around it one by one, each ray a quick bright staccato whistle chirp, finishing with two dot eyes and a curved smile on three little notes.",
  },
  {
    name: "pic-house",
    motion: "the hand draws a recognizable simple house: a square with four steady notes one per side, a triangle roof with two rising-then-falling notes, a small door with a quick low phrase, and a chimney with one short high toot.",
  },
];

for (const s of SHOTS) {
  console.log(`\n=== ${s.name} ===`);
  const r = await generateReferenceShot({
    videos: [SRC1, SRC2],
    prompt: BASE + s.motion,
    duration: "8",
    ratio: "9:16",
    resolution: "720p",
    tier: "fast",
    audio: true,
    outPath: `${OUT}/${s.name}.mp4`,
    label: s.name,
  });
  console.log(r.ok ? `✓ ${s.name}: ${(r.bytes / 1e6).toFixed(1)}MB in ${r.seconds.toFixed(0)}s (seed ${r.seed})` : `✗ ${s.name}: ${r.error}`);
}
console.log("\ndone");
