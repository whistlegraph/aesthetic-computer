// Artifact fix test: chalk marks must grow ONLY from the chalk tip.
// Same sun drawing, simplified choreography (fits 8s without rushing),
// plus a physical-contact constraint — A/B fast tier vs standard tier.
import { generateReferenceShot } from "/Users/jas/aesthetic-computer/pop/lib/fal.mjs";

const SRC1 = "/Users/jas/Downloads/v12044gd0000ca7e5trc77udclceer40.MP4";
const SRC2 = "/Users/jas/Desktop/seedance-variations/src2-trim.mp4";
const OUT = "/Users/jas/Desktop/seedance-variations";

const PROMPT =
  "@Video1 and @Video2 show a hand drawing white chalk on a green chalkboard " +
  "while a person casually whistles. Match their exact visual and sonic " +
  "aesthetic: same green chalkboard, natural handheld phone framing, daylight, " +
  "real hand, soft casual whistling, dry chalk taps and scratches. " +
  "THE ONE RULE: the graphics must align with the sounds — the chalk scratch " +
  "is heard exactly and only while the chalk touches the board, silent when " +
  "the hand lifts, and the whistle traces each stroke as it is drawn. " +
  "PHYSICAL REALISM: every chalk mark grows only from the exact point where " +
  "the chalk tip touches the board, following the hand's motion; no line ever " +
  "appears ahead of the chalk, away from the hand, or by itself; marks already " +
  "drawn stay exactly where they are and never shift or redraw themselves. " +
  "New drawing: the hand draws a simple sun — one big circle in a single slow " +
  "stroke with one long circular whistled note, then just five short rays " +
  "around it, one at a time, each ray one quick staccato whistle chirp.";

for (const tier of ["fast", "standard"]) {
  const name = `sunfix-${tier}`;
  console.log(`\n=== ${name} ===`);
  const r = await generateReferenceShot({
    videos: [SRC1, SRC2],
    prompt: PROMPT,
    duration: "8",
    ratio: "9:16",
    resolution: "720p",
    tier,
    audio: true,
    outPath: `${OUT}/${name}.mp4`,
    label: name,
  });
  console.log(r.ok ? `✓ ${name}: ${(r.bytes / 1e6).toFixed(1)}MB in ${r.seconds.toFixed(0)}s (seed ${r.seed})` : `✗ ${name}: ${r.error}`);
}
console.log("\ndone");
