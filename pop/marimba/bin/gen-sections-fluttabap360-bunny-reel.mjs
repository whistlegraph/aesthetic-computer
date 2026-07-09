#!/usr/bin/env node
// marimba/bin/gen-sections-fluttabap360-bunny-reel.mjs — the BUNNY cut of
// the fluttabap360 METAMORPHOSIS REEL. Same 8-beat origin myth (14 panels:
// 8 wide + 6 inserts) as gen-sections-fluttabap360-reel.mjs, but the keeper
// is replaced by the recurring AESTHETIC COMPUTER felt-bunny character —
// rendered in THIS lane's drawn medium (colored-pencil + gouache on warm
// cream paper, NOT needle-felt; felt is momboba's lane). The bunny wears
// the SAME costume the keeper wore (paper-mache monarch wings on dowels +
// antenna headband) so the story reads identically: he hatches, learns to
// stand, dances, plays the laptop-marimba with his paws, molts, hardens
// into the monarch-drone, and flies to space.
//
// Concept + story: pop/marimba/fluttabap360-reel.story.txt (read first).
// ALL shared scene law (medium, monarchs, palette, LAPTOP LAW, POV
// integrity, AVOID rules) is carried over VERBATIM from the jeffrey
// template — only the character block is swapped (KEEPER → BUNNY). Bunny
// character rules (glasses ride HIGH + ring both eyes, rims never empty,
// TWO long ears always) are inherited from pop/momboba/bunny/PREAMBLE.txt.
//
// NO jeffrey refs anywhere. Anchor pattern (from gen-reel-bunny.mjs): the
// first bunny-present beat, `hatch`, generates with NO refs (pure
// images/generations); every later bunny-present beat then passes the
// finished `hatch` PNG as the character-consistency ref (images/edits) so
// the same bunny walks through every scene. Human-less beats use no refs.
//
// QA (from gen-reel-bunny.mjs): every bunny-FACE-VISIBLE beat is gated by
// gpt-5.5 vision — eyes-inside-red-rims (rims never empty), TWO long ears,
// TWO wings — re-rolled up to 3× on fail, rejects archived to out/rejected/.
// POV / back / human-less beats skip QA (geometry not shown).
//
// Output: pop/marimba/out/_fluttabap360-bunny-reel-beat-<name>.png (cached)
//         + ~/Desktop/fluttabap360-bunny-reel-beat-<name>.png mirror
//
// Usage:
//   node pop/marimba/bin/gen-sections-fluttabap360-bunny-reel.mjs           # cached
//   node pop/marimba/bin/gen-sections-fluttabap360-bunny-reel.mjs --force   # regen all
//   node pop/marimba/bin/gen-sections-fluttabap360-bunny-reel.mjs --only hatch,dance
//   node pop/marimba/bin/gen-sections-fluttabap360-bunny-reel.mjs --proof   # hatch+laptops+space

import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync, copyFileSync, renameSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import * as progress from "../../lib/render-progress.mjs";
import { COLORED_PENCIL_TOOTH } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const SLUG = "fluttabap360-bunny-reel";
const OUT = `${LANE}/out`;
const REJECTED = `${OUT}/rejected`;

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
const QA_MODEL = "gpt-5.5";
const QA_ATTEMPTS = 3;
mkdirSync(OUT, { recursive: true });

// ── refs — NO jeffrey. bunny consistency is anchored on the `hatch` PNG ─
// (first bunny beat, generated with no refs). notepat + keymap refs stay
// exactly as the template used them, appended on screens:true beats.
const ANCHOR = `${OUT}/_${SLUG}-beat-hatch.png`; // char ref for later bunny beats
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

// ── shared scene law (inherited from the fluttabap360 set — VERBATIM) ────
const PORTRAIT_NOTE =
`FRAME — compose for a TALL vertical 9:16 frame. HONOR THE SHOT DIRECTIVE in this beat exactly: a close-up / extreme close-up FILLS the frame with the detail (crop in tight — do NOT pull back); a first-person POV shows the scene through the bunny's eyes; a wide shot lets the whole park breathe with the figure comfortably within it. keep the colored-pencil + gouache medium on warm cream paper, the confident hatching + striping, diegetic light only.`;

const POV_NOTE =
`POV INTEGRITY — this is the bunny's OWN first-person viewpoint (his eyes ARE the camera). The only part of him in frame is his own paws / forelegs / knees in the near foreground. ABSOLUTELY NO second figure resembling him anywhere — no mirror, no reflection showing his face.`;

const MEDIUM =
`${COLORED_PENCIL_TOOTH}

confident hatching and striping build tone — tapered pencil edges, visible paper grain, optical mixing in the wool textures. diegetic light only — NO overlay shine, NO glow filter, NO neon. NOT a photograph, NOT cinematic. this is a hand-drawn drawing. the bunny character NEVER looks at the camera.`;

// ── the character swap: KEEPER → BUNNY (drawn in this lane's medium) ─────
// the recurring AESTHETIC COMPUTER felt-bunny character, rendered here as a
// colored-pencil + gouache DRAWING (not felt). glasses rules carried over
// from momboba's PREAMBLE.txt. wears the keeper's costume (wings + antenna).
const BUNNY =
`THE BUNNY — the recurring AESTHETIC COMPUTER rabbit, drawn here in colored pencil + gouache on warm cream paper (a DRAWING, not a felt doll, not a photo): a wonderfully woolly little rabbit with an off-white body and soft-grey ear tips and paws, TWO long ears (BOTH always present — never draw only one ear; even if one flops the second stays visible), a little pink nose, tiny dark bead eyes, and round soft RED glasses — his cross-track signature. GLASSES GEOMETRY (CRITICAL): a rabbit's eyes sit HIGH and WIDE on the head, so the round red glasses ride HIGH over the eyes, up near the brow — NEVER slid down low on the muzzle or snout; each eye is centered INSIDE its own round red lens, one eye per lens, the rim ringing the eye like real worn glasses; the red rims are NEVER empty (an eye above an empty lower rim reads as extra eyes — the worst failure). he stands upright on his hind legs and plays with his front PAWS. he wears the keeper's hand-made costume: strapped to his back are HAND-MADE monarch wings — orange + black markings on paper-mache panels, edges visibly hand-cut, two slim wooden dowels along the bone of each wing; and a thin black antenna headband arcs above his ears, springs tipped with two tiny felt balls. NO clothing beyond the wings + antenna.`;

const MONARCHS =
`THE MONARCHS — real monarch butterflies (orange + black, painted-paper edged, drawn at different scales for depth) that are ALSO the living data of this near-future AESTHETIC COMPUTER nature-park. they drift on their OWN little arcs — a current of grace, NEVER a swarm. the park's quiet computation reads ONLY as: a faint hand-ruled ground-grid in the lawn (almost invisible, pencil) and soft hand-drawn DOTTED signal-arcs trailing the butterflies' paths.`;

const PALETTE =
`PALETTE — warm cream paper ground; pale-sky-blue overhead; sap-green park grass; off-white + soft-grey woolly bunny; round red glasses; soft pink nose; orange + black monarch wings; jade-green + gold-dot chrysalis. hand-drawn, hatched, print-tech-aware. no haze, no neon, no glow filters.`;

const AVOID_BASE =
`AVOID — any photographic / cinematic / neon look (this is a colored-pencil + gouache drawing); any glowing UI; any swarm of butterflies; any motion-blur / speed-line / glow-filter language; any readable text / wordmark / logo; modern flat tech-illustration style; any figure looking at or acknowledging the camera; a tutu or ballet costume; ANY human person (the character is the bunny only); only one ear on the bunny; empty red glasses rims.`;
const AVOID_NO_SCREENS =
`AVOID also — ANY screen, monitor, laptop, phone, tablet or device of any kind.`;
const AVOID_LAPTOP_LAW =
`LAPTOP LAW — the laptop computers are hand-drawn honored guests, and there are FIVE of them, EACH CLAMSHELL A DIFFERENT COLOR (five distinct candy-pastel bodies — e.g. butter yellow, sky blue, mint green, rose pink, tangerine), drawn with the same pencil hatching as everything else, screens lit like paper and completely GLOW-LESS. every visible screen shows the interface from the tile-interface reference screenshot, reproduced faithfully and RIGHT-SIDE UP (transport bar at the top of the screen, tile banks at the bottom — NEVER upside down, NEVER mirrored): the NOTEPAT instrument — TWO side-by-side banks of note tiles, each bank EXACTLY 4 TILES WIDE and 3 TILES TALL (a full chromatic octave of 12 candy-colored flat rectangular tiles per bank, packed edge to edge like a sheet of postage stamps), a thin vertical slider strip hugging the OUTER edge of each bank, the thin pale waveform strip. never cut off, crop, or merge a column or row. the KEYBOARDS wear the notepat KEYMAP from the keyboard-figure reference: the note keys individually COLORED in the same candy palette, each key its own color, exactly as the keymap reference paints them — colored keycaps on an otherwise plain keyboard. KEYBOARD GEOMETRY is real-laptop accurate, exactly as the keyboard-figure reference shows: STAGGERED rows (each letter row offset sideways from the row above, like every real QWERTY laptop), a wide SPACEBAR centered on the bottom row, a top row of slim function keys — NEVER an ortholinear grid of aligned square keys, NEVER a calculator pad, NEVER keys stacked in perfect columns. render it all as flat hand-hatched color fields matching the references' layout and palette; small labels may stay illegible. on each laptop a few DIFFERENT tiles are pressed mid-chord — bright saturated against their pastel neighbors — so the five screens read as five different moments of the same song. NO drumsticks, NO mallets — he plays with his BARE PAWS.`;

// ── the beats — "the metamorphosis" (bunny cut) ──────────────────────────
// 8 WIDE establishing masters + 6 INSERT masters. bunny: include BUNNY
// block, anchor on hatch PNG. screens: laptop law + notepat ref. pov:
// POV_NOTE. faceVisible flags which beats get gated by the QA vision guard.
const BEAT_NAMES = [
  "cocoon", "crack", "hatch", "hatchpov", "grow", "dance",
  "laptops", "mallets", "laptopspov", "molt", "wing", "drone",
  "spacepov", "space",
];
const PROOF_NAMES = new Set(["hatch", "laptops", "space"]);

const BEATS = {
  cocoon: {
    bunny: false,
    text:
`BEAT — COCOON (the crack) · MOOD: dawn hush, held breath. a small paper-mache chrysalis (sized for a little rabbit) hangs from a thick oak branch over the park lawn at first light — jade-green gouache with a hand-drawn crown of gold dots near the top, seams visibly hand-cut, clearly hand-MADE like a big costume prop. it has just CRACKED: one bright seam splitting down its side, a thin sliver of off-white wool showing through the split. TWO monarchs wait on the branch beside it like midwives. cool pale pre-sun sky, long soft shadows, the faint hand-ruled ground-grid in the lawn far below. no figure visible — only the sliver of wool inside the crack.`,
  },
  crack: {
    bunny: false,
    text:
`BEAT — CRACK (insert) · SHOT — EXTREME CLOSE-UP filling the frame: the splitting seam of the jade-green paper-mache chrysalis, inches away — the hand-cut paper edge tearing fiber by fiber, gold dots huge and soft at the frame edge, and through the widening split a sliver of off-white wool and the warm shadow of something waking inside. one dew-drop clinging beside the seam. EMOTION — held breath, the instant before a life. no figure visible, only the seam and the light inside.`,
  },
  hatch: {
    bunny: true,
    faceVisible: true, // anchor beat — face + geometry must be clean
    text:
`BEAT — HATCH (crawling out, GOOEY) · MOOD: brand-new, delighted-confused, wet. the woolly bunny crawls OUT of the split paper-mache chrysalis along the thick oak branch, on all fours, gripping the bark with his front paws — and he is GOOEY with birth: glistening strands of translucent pale-jade chrysalis fluid stretch from his shoulders and wings back to the husk, sagging and about to snap; fat drops of it bead on his forelegs and drip off toward the lawn far below; his wool is slicked wet and matted, the antenna headband bent askew over his TWO long ears, the paper-mache monarch wings soaked and folded FLAT against his back. the goo is drawn as glossy gouache highlights over the pencil hatching — wet shine by PAINT, never a glow filter. his round red glasses ride HIGH over his eyes (each dark bead eye centered inside its red lens, rims never empty), head tilted DOWN, blinking at his first morning. the cracked jade-green husk swings gently behind him, gold dots catching the light, more fluid stringing from its torn lip. morning sun warming the lawn far below the branch.`,
  },
  hatchpov: {
    bunny: true,
    pov: true,
    text:
`BEAT — HATCHPOV (insert) · SHOT — FIRST-PERSON POV, literally through the bunny's brand-new eyes (his FACE IS NOT SHOWN): his own two front PAWS in the near foreground gripping the rough oak bark, off-white wool with soft-grey pads, a torn flake of jade-green paper-mache still stuck to one paw — and beyond his paws, far below and swimming into focus, the dawn park lawn with its faint hand-ruled ground-grid and one monarch drifting up toward the camera-as-his-eyes. EMOTION — first sight, disoriented wonder, "where am I".`,
  },
  grow: {
    bunny: true,
    text:
`BEAT — GROW (the unfurl) · MOOD: one long waking stretch. down on the park lawn now, the woolly bunny UNFURLS — seen from BEHIND (his face away from camera), rising up onto his hind legs to full height in one stretch, front paws spreading wide, the paper-mache monarch wings drying and creaking OPEN to full span behind him, antenna springs popping upright with their felt balls bobbing above his TWO long ears. two or three monarchs circle in close to inspect the new one, on their own little arcs. morning sun, long fresh shadows on the sap-green grass, the oak with the empty cracked chrysalis small in the background.`,
  },
  dance: {
    bunny: true,
    faceVisible: true,
    text:
`BEAT — DANCE (first steps) · MOOD: joy arriving all at once. the bunny's first steps go straight into ballet — caught at the lift: risen onto the ball of one hind foot (relevé), the other leg extended back in a low arabesque, both front paws curved overhead in a soft port de bras — the pose learned in one go, like it was folded into him all along. seen three-quarter FROM BEHIND, muzzle lifted away from camera but his round red glasses and TWO long ears visible in profile (each eye inside its red lens, rims never empty). three or four monarchs falling in beside him on their arcs, sharing the upward turning motion. a soft ground-shadow anchors his standing foot to the lawn. late-morning sun.`,
  },
  laptops: {
    bunny: true,
    screens: true,
    faceVisible: true,
    text:
`BEAT — LAPTOPS (the laptop marimba — the gag) · MOOD: gleeful recital. the woolly bunny KNEELS behind FIVE laptop computers — each clamshell a DIFFERENT candy color — laid open in a gentle arc on the park lawn like the bars of a marimba, PLAYING the keyboards with his BARE PAWS striking like marimba mallets: paw-tips caught mid-bounce off the colored keys, one paw rebounding high, the other landing. seen from behind / over his shoulder so the arc of laptops faces him, his head in three-quarter PROFILE with his round red glasses and TWO long ears clearly visible (each eye inside its red lens, rims never empty). and he SINGS like a pop star: ONE antenna of his headband has stretched and bent DOWN around the side of his head, its little felt bobble hovering right in front of his pink nose/mouth like a headset microphone — muzzle lifted, singing his heart out into the antenna-mic while his paws keep drumming the keys (the other antenna still springs upright above his ears). the screens tilt up toward him, EACH showing the notepat tile interface from the reference screenshot RIGHT-SIDE UP — the candy-colored 4×3 twin tile banks — lit like paper, glow-less, a different chord pressed on each machine; the keyboards wear the colored keymap keycaps. monarchs perched along the tops of the screens like an audience on a fence. his wings tipped open with the playing motion, delighted. bright late-morning sun — the brightest beat.`,
  },
  mallets: {
    bunny: true,
    screens: true,
    faceVisible: false, // face never shown — close on paw + keys
    text:
`BEAT — MALLETS (insert) · SHOT — EXTREME CLOSE-UP filling the frame: ONE open laptop seen close from just above its screen-top — the bunny's off-white front PAW caught mid-strike, paw-tips bouncing off the colored keymap keycaps like marimba mallets (face never shown), and the notepat tile interface from the reference screenshot filling the tilted screen RIGHT-SIDE UP — twin 4×3 candy-color tile banks, one tile pressed bright mid-chord matching the struck key's color. a monarch perched on the screen-top lip, wings half-open, looking down at the keys. EMOTION — the strike, the note sounding. detail-tight on paw + colored keys + tiles.`,
  },
  laptopspov: {
    bunny: true,
    screens: true,
    pov: true,
    text:
`BEAT — LAPTOPSPOV (insert) · SHOT — FIRST-PERSON POV, through the bunny's eyes mid-performance (his FACE IS NOT SHOWN): his own two off-white front PAWS in the near foreground, paw-tips spread mid-strike like a pair of marimba mallets, one paw descending toward a colored keycap — and fanned out below the camera-as-his-eyes, the arc of FIVE open laptops, each clamshell a DIFFERENT candy color, on the lawn, each screen showing the notepat tile interface from the reference screenshot RIGHT-SIDE UP with a DIFFERENT chord pressed, the keyboards wearing the colored keymap keycaps, monarchs perched along the screen-tops like an audience. sap-green grass between the machines. EMOTION — flow state, the whole instrument his. EXACT tile-grid law on every visible screen.`,
  },
  molt: {
    bunny: true,
    screens: true,
    faceVisible: false, // facing away, dissolving into wings
    text:
`BEAT — MOLT (the body goes back) · MOOD: solemn wonder, the turn of the story. the five laptops now CLOSED and packed in a neat little stack on the grass beside him. the woolly bunny stands FACING AWAY from the camera on his hind legs, front paws rising into one last port de bras — and the transformation is TAKING: the upper half of his outline dissolving into a swirl of orange-and-black painted-paper wing fragments, a single LARGE monarch already lifting away above where his head and TWO ears blur, while tufts of off-white wool drift soft and empty toward the lawn. half-bunny, half-butterfly — the wings claiming him. warm gold light turning rose.`,
  },
  wing: {
    bunny: false,
    text:
`BEAT — WING (insert) · SHOT — EXTREME CLOSE-UP filling the frame: one monarch wing mid-transformation, inches away — the lower half still painted-paper orange + black with hand-cut edges, the upper half already thin brushed-metal plates with tiny rivet dots along the wing bone, the change sweeping across the surface like frost. the small round lens-eye at the wing root catching one glint of sun. EMOTION — quiet awe, the hardening. no figure, only the wing.`,
  },
  drone: {
    bunny: false,
    text:
`BEAT — DRONE (the hardening) · MOOD: quiet sci-fi lift. mid-air over the park, no figure: ONE large monarch climbing up through the frame — and its painted-paper wings are HARDENING panel by panel into thin brushed-metal plates: tiny rivet dots along the wing bones, one small round lens-eye glinting on its head, still perfectly orange + black. ANATOMY LAW — real monarch butterfly anatomy, exact: six slender legs, all attached at the THORAX, tucked neatly BENEATH the body, knees bending downward-backward with the feet trailing back toward the abdomen tip — never splayed forward, never reversed, never insect legs pointing toward the head. the abdomen points down-and-back, antennae forward. a hand-drawn dotted signal-arc trails behind it, crisper now. below, the park shrinks away — the lawn, the oak, the faint ground-grid seen from above. pale-blue sky deepening upward toward indigo at the top of the frame.`,
  },
  spacepov: {
    bunny: false,
    text:
`BEAT — SPACEPOV (insert) · SHOT — FIRST-PERSON POV through the drone-monarch's own lens-eye (no figure anywhere): the tips of its OWN two brushed-metal wings frame the left and right edges of the view like outstretched arms, rivet dots catching sun — and between them, far below, Earth as a small blue-green hand-hatched marble against heavy deep-indigo pencil space, stars picked out as bare cream paper, the faint hand-ruled orbit-lines curving away. EMOTION — leaving home, vast calm. the machine's first person.`,
  },
  space: {
    bunny: false,
    text:
`BEAT — SPACE (outer space) · MOOD: a held last breath, vast + calm. OUTER SPACE drawn in heavy deep-indigo colored pencil, the stars picked out as bare cream paper sparkling through. the brushed-metal monarch drone drifts up-frame among the stars, wings open, sunlight raking the metal panels warm orange. Earth hangs below as a small blue-green marble, hand-hatched. the park's ground-grid has become faint hand-ruled ORBIT-LINES curving through the dark. no figure. the bunny is everywhere now.`,
  },
};

function build(beat) {
  const parts = [MEDIUM];
  if (beat.bunny) parts.push(BUNNY);
  parts.push(MONARCHS, beat.text, PALETTE, AVOID_BASE);
  parts.push(beat.screens ? AVOID_LAPTOP_LAW : AVOID_NO_SCREENS);
  if (beat.pov) parts.push(POV_NOTE);
  return parts.join("\n\n") + `\n\n${PORTRAIT_NOTE}\n`;
}

const apiKey = loadOpenAIKey();
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// ── QA: catch the empty-rims-as-eyes / one-ear / missing-wing artifacts ──
// adapted from gen-reel-bunny.mjs qaPanel — interrogate geometry, compute
// verdict locally. adds a TWO-WINGS check (the costume must survive).
async function qaPanel(b64) {
  const body = {
    model: QA_MODEL,
    max_completion_tokens: 3000,
    messages: [{
      role: "user",
      content: [
        { type: "text", text:
          "A hand-drawn colored-pencil illustration of a woolly rabbit " +
          "character who wears small round RED glasses and hand-made monarch " +
          "butterfly wings strapped to his back. Zoom in and answer as a " +
          "strict geometry inspector, ignoring cuteness. Works even in " +
          "three-quarter or side profile where only ONE eye is visible. " +
          "Step 1: find every EYE FEATURE — an open dark bead eye, or a " +
          "closed-eye stitch/lash line. Step 2: find the red lens rims if " +
          "glasses are on the face. Step 3: THE KEY TEST — does each visible " +
          "eye sit centered INSIDE a red rim (the rim encircles the eye), or " +
          "does the eye sit HIGHER on the head with the red rims resting " +
          "LOWER on the muzzle/snout, leaving the rims EMPTY (plain wool " +
          "inside, no eye)? An eye above an empty rim is the failure we hunt. " +
          "If the rabbit is a back or far-away view where no eye+rim geometry " +
          "is clearly visible, treat eyes as PASS. " +
          "Count eyes truly encircled by a rim (eyes_inside_rims) vs eyes " +
          "sitting outside/above/below the rims (eyes_outside_rims). Set " +
          "rims_empty true only if a red rim on the face CLEARLY has no eye " +
          "inside it. " +
          "Step 4: EARS — a rabbit has TWO long ears. Count how many distinct " +
          "long ears he clearly has (ear_count); one ear flopped over still " +
          "counts as one, the second ear should also be present. If a " +
          "distant/back view where ears can't be counted, set ear_count to 2. " +
          "Step 5: WINGS — he wears TWO monarch wings (a left and a right). " +
          "Count clearly-present wings (wing_count); if wings are out of frame " +
          "or the view can't show them, set wing_count to 2. " +
          'Reply ONLY compact JSON: {"glasses_on_face": true|false, ' +
          '"eyes_inside_rims": <int>, "eyes_outside_rims": <int>, ' +
          '"rims_empty": true|false, "ear_count": <int>, "wing_count": <int>, ' +
          '"note": "short"}' },
        { type: "image_url", image_url: { url: `data:image/png;base64,${b64}`, detail: "high" } },
      ],
    }],
  };
  try {
    const r = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
    if (!r.ok) { console.warn(`  ⚠ QA skipped (HTTP ${r.status})`); return { pass: true, reason: "qa-unavailable" }; }
    const txt = (await r.json()).choices?.[0]?.message?.content || "{}";
    const m = txt.match(/\{[\s\S]*\}/);
    const v = m ? JSON.parse(m[0]) : {};
    const eyesOk = v.glasses_on_face
      ? (v.eyes_outside_rims === 0 && !v.rims_empty)
      : ((v.eyes_inside_rims ?? 0) + (v.eyes_outside_rims ?? 0)) <= 2;
    const earsOk = (v.ear_count ?? 2) >= 2; // bunny keeps both long ears
    const wingsOk = (v.wing_count ?? 2) >= 2; // costume keeps both wings
    const pass = eyesOk && earsOk && wingsOk;
    const reason = [
      !eyesOk && "eyes",
      !earsOk && `ears=${v.ear_count}`,
      !wingsOk && `wings=${v.wing_count}`,
    ].filter(Boolean).join("+") + (v.note ? ` (${v.note})` : "");
    return { pass, reason };
  } catch (e) {
    console.warn(`  ⚠ QA error (${e.message}) — passing`);
    return { pass: true, reason: "qa-error" };
  }
}

// One image-API call (3 network retries) → b64 or null. useAnchor picks
// images/edits (anchor + notepat/keymap refs) vs images/generations.
async function genOnce(promptText, refs, useEdit, label) {
  const MAX_TRIES = 4;
  for (let attempt = 1; attempt <= MAX_TRIES; attempt++) {
    const t0 = Date.now();
    try {
      let res;
      if (useEdit) {
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
        res = await fetch("https://api.openai.com/v1/images/edits", {
          method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
        });
      } else {
        res = await fetch("https://api.openai.com/v1/images/generations", {
          method: "POST",
          headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
          body: JSON.stringify({ model: "gpt-image-2", prompt: promptText, size: SIZE, quality: "high", n: 1 }),
        });
      }
      if (!res.ok) {
        const err = await res.text();
        const transient = res.status === 429 || res.status >= 500;
        if (transient && attempt < MAX_TRIES) {
          const wait = 4000 * attempt;
          console.warn(`  ⚠ OpenAI ${res.status} (${label}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
          await sleep(wait); continue;
        }
        console.error(`✗ OpenAI ${res.status} (${label}): ${err.slice(0, 400)}`);
        return null;
      }
      const json = await res.json();
      const b64 = json.data?.[0]?.b64_json;
      if (!b64) { console.error(`✗ no image (${label}): ${JSON.stringify(json).slice(0, 280)}`); return null; }
      const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
      const u = json.usage || {};
      const tok = u.input_tokens ? ` · tok in=${u.input_tokens} out=${u.output_tokens}` : "";
      console.log(`  · gen ${elapsed}s${tok}`);
      return b64;
    } catch (e) {
      const cause = e?.cause?.code || e?.cause?.message || e?.message || "unknown";
      if (attempt < MAX_TRIES) {
        const wait = 4000 * attempt;
        console.warn(`  ⚠ network fail (${label}: ${cause}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
        await sleep(wait); continue;
      }
      console.error(`✗ network fail (${label}): ${cause} — gave up after ${MAX_TRIES} tries`);
      return null;
    }
  }
  return null;
}

function archiveRejected(png, reason) {
  mkdirSync(REJECTED, { recursive: true });
  const base = basename(png).replace(/\.png$/, "");
  const n = readdirSync(REJECTED).filter((f) => f.startsWith(base + ".rej")).length + 1;
  const dest = join(REJECTED, `${base}.rej${n}.png`);
  renameSync(png, dest);
  console.log(`  ⌂ rejected (${reason}) → rejected/${basename(dest)}`);
}

// full generate: gen (+QA re-roll on faceVisible beats) → write + mirror.
// cached unless --force. never throws; returns true on a written panel.
async function generate(beat, refs, useEdit, outPath, label) {
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) { console.log(`✓ cached → ${rel}`); return true; }
  console.log(`▸ ${label} · ${SIZE} · ${useEdit ? refs.length + " refs" : "no refs"}${beat.faceVisible ? " · QA" : ""}`);
  const attempts = beat.faceVisible ? QA_ATTEMPTS : 1;
  for (let qa = 1; qa <= attempts; qa++) {
    const b64 = await genOnce(build(beat), refs, useEdit, label);
    if (!b64) return false; // network exhausted
    if (beat.faceVisible) {
      const verdict = await qaPanel(b64);
      if (!verdict.pass) {
        writeFileSync(outPath, Buffer.from(b64, "base64"));
        archiveRejected(outPath, `QA ${qa}/${attempts}: ${verdict.reason}`);
        continue;
      }
      console.log(`  ✓ QA pass (${verdict.reason || "clean"})`);
    }
    writeFileSync(outPath, Buffer.from(b64, "base64"));
    try {
      copyFileSync(outPath, join(homedir(), "Desktop", `${SLUG}-beat-${basename(outPath).replace(`_${SLUG}-beat-`, "")}`));
    } catch {}
    console.log(`✓ → ${rel}`);
    return true;
  }
  console.error(`✗ ${label} — no QA-passing panel after ${attempts} attempts (rejects kept)`);
  return false;
}

const onlySet = typeof flags.only === "string"
  ? new Set(flags.only.split(",").map((x) => x.trim().toLowerCase()))
  : null;
const wantBeat = (name) => {
  if (PROOF) return PROOF_NAMES.has(name);
  if (onlySet) return onlySet.has(name);
  return true;
};

// ── run · SEQUENTIAL (8 GB machine). hatch first (it's the anchor), then
// the rest in beat order. bunny beats after hatch pass the hatch PNG as
// the character ref; screens beats append notepat + keymap; human-less
// beats use no refs (images/generations). ─────────────────────────────
function refsFor(name, beat) {
  const refs = [];
  if (beat.bunny && name !== "hatch" && existsSync(ANCHOR)) refs.push(ANCHOR);
  if (beat.screens) for (const r of [NOTEPAT_REF, KEYMAP_REF]) if (existsSync(r)) refs.push(r);
  return refs;
}

// order: hatch first so its PNG exists to anchor the others
const order = [...BEAT_NAMES].filter(wantBeat).sort((a, b) =>
  (a === "hatch" ? -1 : 0) - (b === "hatch" ? -1 : 0));

progress.begin({ type: "illy", label: `${SLUG} · ${order.length} panels${PROOF ? " (PROOF)" : ""}` });
let done = 0, ok = 0;
const failed = [];
for (const name of order) {
  const beat = BEATS[name];
  const refs = refsFor(name, beat);
  const useEdit = refs.length > 0;
  const out = `${OUT}/_${SLUG}-beat-${name}.png`;
  const okd = await generate(beat, refs, useEdit, out, `${SLUG} beat · ${name}`);
  if (okd) ok++; else failed.push(name);
  progress.update((++done / order.length) * 100, { done, total: order.length });
}
progress.end();

console.log(`\n✓ ${SLUG} panel set — ${ok}/${order.length} panels · "the metamorphosis" (bunny cut)`);
if (failed.length) console.log(`✗ failed: ${failed.join(", ")}`);
process.exit(ok === order.length ? 0 : 1);
