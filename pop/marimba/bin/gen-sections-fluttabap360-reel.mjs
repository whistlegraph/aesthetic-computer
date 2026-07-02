#!/usr/bin/env node
// marimba/bin/gen-sections-fluttabap360-reel.mjs — generate the
// fluttabap360 METAMORPHOSIS REEL panel set: 8 portrait (9:16)
// colored-pencil + gouache illustrations tracing the keeper's origin
// myth — chrysalis crack → crawl out → unfurl → first dance → the
// laptop marimba → the molt → the robot-drone hardening → outer space.
//
// Concept + story: pop/marimba/fluttabap360-reel.story.txt (read first).
// Medium + character inherited from the locked fluttabap360 album cover
// (fluttabap360.illy.txt) and the visualizer set — with the story's two
// SANCTIONED exceptions: laptops exist in the `laptops`/`molt` beats
// (screens pale cream, glow-less, never UI) and the `drone`/`space`
// beats trade painted paper for hand-drawn brushed metal + heavy indigo.
//
// Human-less beats (cocoon/drone/space) drop the jeffrey face refs and
// keep only the cover ref (medium anchor) so gpt-image-2 doesn't invent
// a bystander.
//
// Output: pop/marimba/out/_fluttabap360-reel-beat-<name>.png  (8 masters, cached)
//
// Usage:
//   node pop/marimba/bin/gen-sections-fluttabap360-reel.mjs               # cached
//   node pop/marimba/bin/gen-sections-fluttabap360-reel.mjs --force       # regen all
//   node pop/marimba/bin/gen-sections-fluttabap360-reel.mjs --only laptops,molt
//   node pop/marimba/bin/gen-sections-fluttabap360-reel.mjs --proof       # cocoon+laptops+space

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import * as progress from "../../lib/render-progress.mjs";
import { COLORED_PENCIL_TOOTH } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const SLUG = "fluttabap360-reel";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}
const FORCE = flags.force === true;
const PROOF = flags.proof === true;
const SIZE  = "1024x1536"; // portrait 9:16-ish
mkdirSync(`${LANE}/out`, { recursive: true });

// ── identity refs — same set as gen-sections-fluttabap360.mjs ────────
const SHOOT_DIR   = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const COVER_REF   = `${LANE}/out/fluttabap360-cover.png`;
const FACE_REFS = [
  COVER_REF, // costume + medium anchor (first)
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
].filter((p) => {
  if (existsSync(p)) return true;
  console.warn(`  ⚠ ref missing, dropping: ${p}`);
  return false;
});
const MEDIUM_REFS = FACE_REFS.slice(0, 1); // cover only — human-less beats
// the notepat screenshot, pre-rotated UPRIGHT (the keymaps-cover original
// is deliberately upside down for its flipped-lid gag — using it raw made
// the laptop screens render upside down), plus the keymaps-paper keyboard
// figure (which QWERTY keys get which note colors) — both appended as
// refs on screens:true beats.
const NOTEPAT_REF = `${LANE}/out/notepat-upright.png`;
const KEYMAP_REF  = `${REPO}/papers/arxiv-keymaps/notepat-keymap.png`;

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) {
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("OPENAI_API_KEY not set and not found in vault devcontainer.env");
}

// ── shared scene law (inherited from the fluttabap360 set) ───────────
// shot-aware framing à la amaythingra: the SHOT directive in each beat
// wins — close-ups crop IN, POVs see through jeffrey's eyes, wides breathe.
const PORTRAIT_NOTE =
`FRAME — compose for a TALL vertical 9:16 frame. HONOR THE SHOT DIRECTIVE in this beat exactly: a close-up / extreme close-up FILLS the frame with the detail (crop in tight — do NOT pull back); a first-person POV shows the scene through jeffrey's eyes; a wide shot lets the whole park breathe with the figure comfortably within it. keep the colored-pencil + gouache medium on warm cream paper, the confident hatching + striping, diegetic light only.`;

const POV_NOTE =
`POV INTEGRITY — this is the keeper's OWN first-person viewpoint (his eyes ARE the camera). The only part of him in frame is his own hands / forearms / knees in the near foreground. ABSOLUTELY NO second figure resembling him anywhere — no mirror, no reflection showing his face.`;

const MEDIUM =
`${COLORED_PENCIL_TOOTH}

confident hatching and striping build tone — tapered pencil edges, visible paper grain, optical mixing in the knit textures. diegetic light only — NO overlay shine, NO glow filter, NO neon. NOT a photograph, NOT cinematic. this is a hand-drawn drawing. any human figure NEVER looks at the camera.`;

const KEEPER =
`THE KEEPER — the butterfly cosplayer, recognizable from the jeffrey reference photographs AND the album-cover ref: a gently-grinning adult about 30, soft brown curly hair, light beard. he wears his REAL clothes — a pale-yellow short-sleeve button-down with a small enamel butterfly pin on the collar, and blue jeans; NO tutu, NO leotard, NO costume beyond the wings. strapped to his back are HAND-MADE monarch wings — orange + black markings on paper-mache panels, edges visibly hand-cut, two slim wooden dowels along the bone of each wing. a thin black antenna headband arcs above his hair, springs tipped with two tiny felt balls.`;

const MONARCHS =
`THE MONARCHS — real monarch butterflies (orange + black, painted-paper edged, drawn at different scales for depth) that are ALSO the living data of this near-future AESTHETIC COMPUTER nature-park. they drift on their OWN little arcs — a current of grace, NEVER a swarm. the park's quiet computation reads ONLY as: a faint hand-ruled ground-grid in the lawn (almost invisible, pencil) and soft hand-drawn DOTTED signal-arcs trailing the butterflies' paths.`;

const PALETTE =
`PALETTE — warm cream paper ground; pale-sky-blue overhead; sap-green park grass; pale-yellow button-down; soft brown hair + warm grey; orange + black monarch wings; jade-green + gold-dot chrysalis. hand-drawn, hatched, print-tech-aware. no haze, no neon, no glow filters.`;

const AVOID_BASE =
`AVOID — any photographic / cinematic / neon look (this is a colored-pencil + gouache drawing); any glowing UI; any swarm of butterflies; any motion-blur / speed-line / glow-filter language; any readable text / wordmark / logo; modern flat tech-illustration style; any figure looking at or acknowledging the camera; a tutu or ballet costume.`;
const AVOID_NO_SCREENS =
`AVOID also — ANY screen, monitor, laptop, phone, tablet or device of any kind.`;
const AVOID_LAPTOP_LAW =
`LAPTOP LAW — the laptop computers are hand-drawn honored guests, and there are FIVE of them, EACH CLAMSHELL A DIFFERENT COLOR (five distinct candy-pastel bodies — e.g. butter yellow, sky blue, mint green, rose pink, tangerine), drawn with the same pencil hatching as everything else, screens lit like paper and completely GLOW-LESS. every visible screen shows the interface from the tile-interface reference screenshot, reproduced faithfully and RIGHT-SIDE UP (transport bar at the top of the screen, tile banks at the bottom — NEVER upside down, NEVER mirrored): the NOTEPAT instrument — TWO side-by-side banks of note tiles, each bank EXACTLY 4 TILES WIDE and 3 TILES TALL (a full chromatic octave of 12 candy-colored flat rectangular tiles per bank, packed edge to edge like a sheet of postage stamps), a thin vertical slider strip hugging the OUTER edge of each bank, the thin pale waveform strip. never cut off, crop, or merge a column or row. the KEYBOARDS wear the notepat KEYMAP from the keyboard-figure reference: the note keys individually COLORED in the same candy palette, each key its own color, exactly as the keymap reference paints them — colored keycaps on an otherwise plain keyboard. KEYBOARD GEOMETRY is real-laptop accurate, exactly as the keyboard-figure reference shows: STAGGERED rows (each letter row offset sideways from the row above, like every real QWERTY laptop), a wide SPACEBAR centered on the bottom row, a top row of slim function keys — NEVER an ortholinear grid of aligned square keys, NEVER a calculator pad, NEVER keys stacked in perfect columns. render it all as flat hand-hatched color fields matching the references' layout and palette; small labels may stay illegible. on each laptop a few DIFFERENT tiles are pressed mid-chord — bright saturated against their pastel neighbors — so the five screens read as five different moments of the same song. NO drumsticks, NO mallets — he plays with his BARE FINGERS.`;

// ── the beats — "the metamorphosis" ──────────────────────────────────
// 8 WIDE establishing masters + 6 INSERT masters (extreme close-ups +
// first-person POVs, amaythingra-style). human: include KEEPER block +
// full face refs. screens: laptop law + notepat ref. pov: POV_NOTE.
const BEAT_NAMES = [
  "cocoon", "crack", "hatch", "hatchpov", "grow", "dance",
  "laptops", "mallets", "laptopspov", "molt", "wing", "drone",
  "spacepov", "space",
];
const PROOF_NAMES = new Set(["crack", "laptopspov", "spacepov"]);

const BEATS = {
  cocoon: {
    human: false,
    text:
`BEAT — COCOON (the crack) · MOOD: dawn hush, held breath. a HUMAN-SIZED paper-mache chrysalis hangs from a thick oak branch over the park lawn at first light — jade-green gouache with a hand-drawn crown of gold dots near the top, seams visibly hand-cut, clearly hand-MADE like a big costume prop. it has just CRACKED: one bright seam splitting down its side, a thin sliver of pale-yellow fabric showing through the split. TWO monarchs wait on the branch beside it like midwives. cool pale pre-sun sky, long soft shadows, the faint hand-ruled ground-grid in the lawn far below. no human figure visible — only the sliver of shirt inside the crack.`,
  },
  crack: {
    human: false,
    text:
`BEAT — CRACK (insert) · SHOT — EXTREME CLOSE-UP filling the frame: the splitting seam of the jade-green paper-mache chrysalis, inches away — the hand-cut paper edge tearing fiber by fiber, gold dots huge and soft at the frame edge, and through the widening split a sliver of pale-yellow fabric and the warm shadow of something waking inside. one dew-drop clinging beside the seam. EMOTION — held breath, the instant before a life. no figure visible, only the seam and the light inside.`,
  },
  hatch: {
    human: true,
    text:
`BEAT — HATCH (crawling out, GOOEY) · MOOD: brand-new, delighted-confused, wet. the keeper crawls OUT of the split paper-mache chrysalis along the thick oak branch, on hands and knees, gripping the bark — and he is GOOEY with birth: glistening strands of translucent pale-jade chrysalis fluid stretch from his shoulders and wings back to the husk, sagging and about to snap; fat drops of it bead on his forearms and drip off his elbows toward the lawn far below; his curls are slicked wet and matted to his head, the antenna headband bent askew, the paper-mache monarch wings soaked and folded FLAT against his back, his shirt wet-dark and clinging. the goo is drawn as glossy gouache highlights over the pencil hatching — wet shine by PAINT, never a glow filter. his head is DOWN, eyes on his own hands, blinking at his first morning — face mostly hidden by the angle and his curls. the cracked jade-green husk swings gently behind him, gold dots catching the light, more fluid stringing from its torn lip. morning sun warming the lawn far below the branch.`,
  },
  hatchpov: {
    human: true,
    pov: true,
    text:
`BEAT — HATCHPOV (insert) · SHOT — FIRST-PERSON POV, literally through the keeper's brand-new eyes (his FACE IS NOT SHOWN): his own two hands in the near foreground gripping the rough oak bark, knuckles pale, a torn flake of jade-green paper-mache still stuck to one wrist — and beyond his hands, far below and swimming into focus, the dawn park lawn with its faint hand-ruled ground-grid and one monarch drifting up toward the camera-as-his-eyes. EMOTION — first sight, disoriented wonder, "where am I".`,
  },
  grow: {
    human: true,
    text:
`BEAT — GROW (the unfurl) · MOOD: one long waking stretch. down on the park lawn now, the keeper UNFURLS — seen from behind, rising from a crouch to full height in one stretch, arms spreading wide, the paper-mache monarch wings drying and creaking OPEN to full span behind him, antenna springs popping upright with their felt balls bobbing. two or three monarchs circle in close to inspect the new one, on their own little arcs. morning sun, long fresh shadows on the sap-green grass, the oak with the empty cracked chrysalis small in the background.`,
  },
  dance: {
    human: true,
    text:
`BEAT — DANCE (first steps) · MOOD: joy arriving all at once. the keeper's first steps go straight into ballet — caught at the lift: rising onto the ball of one foot (relevé), the other leg extended back in a low arabesque, both arms curved overhead in a soft port de bras — the pose learned in one go, like it was folded into him all along. seen three-quarter FROM BEHIND, chin lifted away from camera, lost in it. three or four monarchs falling in beside him on their arcs, sharing the upward turning motion. a soft ground-shadow anchors his standing foot to the lawn. late-morning sun.`,
  },
  laptops: {
    human: true,
    screens: true,
    text:
`BEAT — LAPTOPS (the laptop marimba — the gag) · MOOD: gleeful recital. the keeper KNEELS behind FIVE laptop computers — each clamshell a DIFFERENT candy color — laid open in a gentle arc on the park lawn like the bars of a marimba, PLAYING the keyboards with his BARE FINGERS striking like marimba mallets: fingertips caught mid-bounce off the colored keys, wrists loose and percussive, one hand rebounding high, the other landing. seen from behind / over his shoulder so the arc of laptops faces him, his head in three-quarter PROFILE. and he is SINGING like a pop star: ONE antenna of his headband has stretched and bent DOWN around the side of his head, its little felt bobble hovering right in front of his lips like a headset microphone — chin lifted, mouth open mid-note, singing his heart out into the antenna-mic while his fingers keep drumming the keys (the other antenna still springs upright). the screens tilt up toward him, EACH showing the notepat tile interface from the reference screenshot RIGHT-SIDE UP — the candy-colored 4×3 twin tile banks — lit like paper, glow-less, a different chord pressed on each machine; the keyboards wear the colored keymap keycaps. monarchs perched along the tops of the screens like an audience on a fence. his wings tipped open with the playing motion, head bobbing, delighted. bright late-morning sun — the brightest beat.`,
  },
  mallets: {
    human: true,
    screens: true,
    text:
`BEAT — MALLETS (insert) · SHOT — EXTREME CLOSE-UP filling the frame: ONE open laptop seen close from just above its screen-top — the keeper's BARE HAND caught mid-strike, two fingertips bouncing off the colored keymap keycaps like marimba mallets, wrist loose (face never shown), and the notepat tile interface from the reference screenshot filling the tilted screen RIGHT-SIDE UP — twin 4×3 candy-color tile banks, one tile pressed bright mid-chord matching the struck key's color. a monarch perched on the screen-top lip, wings half-open, looking down at the keys. EMOTION — the strike, the note sounding. detail-tight on fingers + colored keys + tiles.`,
  },
  laptopspov: {
    human: true,
    screens: true,
    pov: true,
    text:
`BEAT — LAPTOPSPOV (insert) · SHOT — FIRST-PERSON POV, through the keeper's eyes mid-performance (his FACE IS NOT SHOWN): his own two BARE forearms and hands in the near foreground, fingers spread mid-strike like a pair of marimba mallets, one hand descending toward a colored keycap — and fanned out below the camera-as-his-eyes, the arc of FIVE open laptops, each clamshell a DIFFERENT candy color, on the lawn, each screen showing the notepat tile interface from the reference screenshot RIGHT-SIDE UP with a DIFFERENT chord pressed, the keyboards wearing the colored keymap keycaps, monarchs perched along the screen-tops like an audience. sap-green grass between the machines. EMOTION — flow state, the whole instrument his. EXACT tile-grid law on every visible screen.`,
  },
  molt: {
    human: true,
    screens: true,
    text:
`BEAT — MOLT (the body goes back) · MOOD: solemn wonder, the turn of the story. the five laptops now CLOSED and packed in a neat little stack on the grass beside him. the keeper stands FACING AWAY from the camera, arms rising into one last port de bras — and the transformation is TAKING: the upper half of his outline dissolving into a swirl of orange-and-black painted-paper wing fragments, a single LARGE monarch already lifting away above where his head and shoulders blur, while the pale-yellow button-down begins settling soft and empty toward the lawn. half-figure, half-butterfly — the wings claiming him. warm gold light turning rose.`,
  },
  wing: {
    human: false,
    text:
`BEAT — WING (insert) · SHOT — EXTREME CLOSE-UP filling the frame: one monarch wing mid-transformation, inches away — the lower half still painted-paper orange + black with hand-cut edges, the upper half already thin brushed-metal plates with tiny rivet dots along the wing bone, the change sweeping across the surface like frost. the small round lens-eye at the wing root catching one glint of sun. EMOTION — quiet awe, the hardening. no figure, only the wing.`,
  },
  drone: {
    human: false,
    text:
`BEAT — DRONE (the hardening) · MOOD: quiet sci-fi lift. mid-air over the park, no human figure: ONE large monarch climbing up through the frame — and its painted-paper wings are HARDENING panel by panel into thin brushed-metal plates: tiny rivet dots along the wing bones, one small round lens-eye glinting on its head, still perfectly orange + black. ANATOMY LAW — real monarch butterfly anatomy, exact: six slender legs, all attached at the THORAX, tucked neatly BENEATH the body, knees bending downward-backward with the feet trailing back toward the abdomen tip — never splayed forward, never reversed, never insect legs pointing toward the head. the abdomen points down-and-back, antennae forward. a hand-drawn dotted signal-arc trails behind it, crisper now. below, the park shrinks away — the lawn, the oak, the faint ground-grid seen from above. pale-blue sky deepening upward toward indigo at the top of the frame.`,
  },
  spacepov: {
    human: false,
    text:
`BEAT — SPACEPOV (insert) · SHOT — FIRST-PERSON POV through the drone-monarch's own lens-eye (no human anywhere): the tips of its OWN two brushed-metal wings frame the left and right edges of the view like outstretched arms, rivet dots catching sun — and between them, far below, Earth as a small blue-green hand-hatched marble against heavy deep-indigo pencil space, stars picked out as bare cream paper, the faint hand-ruled orbit-lines curving away. EMOTION — leaving home, vast calm. the machine's first person.`,
  },
  space: {
    human: false,
    text:
`BEAT — SPACE (outer space) · MOOD: a held last breath, vast + calm. OUTER SPACE drawn in heavy deep-indigo colored pencil, the stars picked out as bare cream paper sparkling through. the brushed-metal monarch drone drifts up-frame among the stars, wings open, sunlight raking the metal panels warm orange. Earth hangs below as a small blue-green marble, hand-hatched. the park's ground-grid has become faint hand-ruled ORBIT-LINES curving through the dark. no human figure. the keeper is everywhere now.`,
  },
};

function build(beat) {
  const parts = [MEDIUM];
  if (beat.human) parts.push(KEEPER);
  parts.push(MONARCHS, beat.text, PALETTE, AVOID_BASE);
  parts.push(beat.screens ? AVOID_LAPTOP_LAW : AVOID_NO_SCREENS);
  if (beat.pov) parts.push(POV_NOTE);
  return parts.join("\n\n") + `\n\n${PORTRAIT_NOTE}\n`;
}

const apiKey = loadOpenAIKey();
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function buildForm(promptText, refs) {
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", promptText);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of refs) {
    const buf = readFileSync(ref);
    const lower = ref.toLowerCase();
    const ext = lower.endsWith(".png") ? "png" : lower.endsWith(".webp") ? "webp" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  return fd;
}

// One gpt-image-2 edit call → write outPath. Per-file cached; --force
// regens. Transient 429/5xx + network blips retried; never throws.
async function generate(promptText, refs, outPath, label) {
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) { console.log(`✓ cached → ${rel}`); return true; }
  console.log(`▸ ${label} · ${SIZE} · ${refs.length} refs`);
  const MAX_TRIES = 4;
  for (let attempt = 1; attempt <= MAX_TRIES; attempt++) {
    const t0 = Date.now();
    try {
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST",
        headers: { Authorization: `Bearer ${apiKey}` },
        body: buildForm(promptText, refs),
      });
      if (!res.ok) {
        const err = await res.text();
        const transient = res.status === 429 || res.status >= 500;
        if (transient && attempt < MAX_TRIES) {
          const wait = 4000 * attempt;
          console.warn(`  ⚠ OpenAI ${res.status} (${label}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
          await sleep(wait); continue;
        }
        console.error(`✗ OpenAI ${res.status} (${label}): ${err.slice(0, 600)}`);
        return false;
      }
      const json = await res.json();
      const b64 = json.data?.[0]?.b64_json;
      if (!b64) { console.error(`✗ no image (${label}): ${JSON.stringify(json).slice(0, 280)}`); return false; }
      writeFileSync(outPath, Buffer.from(b64, "base64"));
      const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
      const u = json.usage || {};
      const tok = u.input_tokens ? ` · tok in=${u.input_tokens} out=${u.output_tokens}` : "";
      console.log(`✓ ${elapsed}s${tok} → ${rel}`);
      return true;
    } catch (e) {
      const cause = e?.cause?.code || e?.cause?.message || e?.message || "unknown";
      if (attempt < MAX_TRIES) {
        const wait = 4000 * attempt;
        console.warn(`  ⚠ network fail (${label}: ${cause}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
        await sleep(wait); continue;
      }
      console.error(`✗ network fail (${label}): ${cause} — gave up after ${MAX_TRIES} tries`);
      return false;
    }
  }
}

const onlySet = typeof flags.only === "string"
  ? new Set(flags.only.split(",").map((x) => x.trim().toLowerCase()))
  : null;
const wantBeat = (name) => {
  if (PROOF) return PROOF_NAMES.has(name);
  if (onlySet) return onlySet.has(name);
  return true;
};

const jobs = [];
for (const name of BEAT_NAMES) {
  if (!wantBeat(name)) continue;
  const beat = BEATS[name];
  const refs = [...(beat.human ? FACE_REFS : MEDIUM_REFS)];
  if (beat.screens) for (const r of [NOTEPAT_REF, KEYMAP_REF]) if (existsSync(r)) refs.push(r);
  jobs.push({
    prompt: build(beat),
    refs,
    out: `${LANE}/out/_${SLUG}-beat-${name}.png`,
    label: `${SLUG} beat · ${name}`,
  });
}

progress.begin({ type: "illy", label: `${SLUG} · ${jobs.length} panels${PROOF ? " (PROOF)" : ""}` });
let done = 0, ok = 0;
for (const job of jobs) {
  if (await generate(job.prompt, job.refs, job.out, job.label)) ok++;
  progress.update((++done / jobs.length) * 100, { done, total: jobs.length });
}
progress.end();

console.log(`\n✓ ${SLUG} panel set — ${ok}/${jobs.length} panels · "the metamorphosis"`);
process.exit(ok === jobs.length ? 0 : 1);
