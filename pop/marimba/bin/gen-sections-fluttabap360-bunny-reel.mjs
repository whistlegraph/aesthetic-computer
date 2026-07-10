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
const KB_ATTEMPTS = 4; // best-of-N for the keyboard OCR check on screen beats
// the ONLY letter keys that should be DARK on the notepat MacBook keymap
const KB_DARK_TARGET = ["V", "Z", "S", "W", "Q", "R", "T", "Y", "U", "O", "P"];
mkdirSync(OUT, { recursive: true });

// ── refs — NO jeffrey. TWO character anchors: the LARVAL bunnypillar
// (hatch PNG, generated with no refs) anchors the other larval/transform
// beats; the WINGED bunnyfly (dance PNG, generated with no refs) anchors
// the laptop + molt beats so the same hybrid recurs. The metal beats carry
// no character ref (the metal bunnyfly is described inline). Screens beats
// append the ACCURATE notepat refs: the crisp real interface capture for
// the SCREENS + the canonical staggered-QWERTY colored keymap (cropped to
// keyboard-only) for the physical KEYBOARDS.
const ANCHOR_LARVAL = `${OUT}/_${SLUG}-beat-hatch.png`; // bunnypillar ref
const ANCHOR_FLY    = `${OUT}/_${SLUG}-beat-dance.png`;  // bunnyfly ref
const NOTEPAT_REF = `${OUT}/notepat-screen-ref.png`;      // crisp real interface
const NEO_REF     = `${OUT}/neo-keyboard-ref.jpg`;        // real MacBook Neo geometry (uncolored)

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

// ── the character: a BUNNYFLY and its larval BUNNYPILLAR (drawn) ─────────
// The hero is a BUNNYFLY — a single bunny × monarch-butterfly hybrid, NOT a
// bunny wearing a wing costume. Its larval form is the BUNNYPILLAR. Both
// keep the recurring AESTHETIC COMPUTER rabbit head + the round-red-glasses
// law. Rendered as a colored-pencil + gouache DRAWING (not felt).
const BUNNYFLY =
`THE BUNNYFLY — the hero is a BUNNYFLY: ONE creature, a bunny × monarch-butterfly hybrid (NOT a rabbit wearing a costume, NOT wings strapped on). Drawn in colored pencil + gouache on warm cream paper (a DRAWING, not a felt doll, not a photo). He has the HEAD and soul of the recurring AESTHETIC COMPUTER rabbit — an off-white woolly head with soft-grey ear tips, TWO long rabbit ears (BOTH always present — never only one; even if one flops the other stays visible), a little pink nose, tiny dark bead eyes, and round soft RED glasses (his signature) — set on a small soft velvety bunny-furred butterfly BODY from which grow HIS OWN real monarch WINGS: orange + black, veined and hand-drawn, PART OF HIS BODY (never paper-mache, never dowels, never props), and two slender butterfly ANTENNAE tipped with tiny club-knobs curving up from between his ears. GLASSES GEOMETRY (CRITICAL): a rabbit's eyes sit HIGH and WIDE, so the round red glasses ride HIGH over the eyes, up near the brow — NEVER slid down the muzzle/snout; each eye centered INSIDE its own round red lens, one eye per lens, the rim ringing the eye like real worn glasses; the red rims are NEVER empty (an eye above an empty lower rim reads as extra eyes — the worst failure). he moves on his little bunny forelegs and plays with his front PAWS.`;
const BUNNYPILLAR =
`THE BUNNYPILLAR — at the start of the story the creature is a BUNNYPILLAR: the LARVAL, caterpillar form of the bunnyfly. Drawn in colored pencil + gouache on warm cream paper. A plump, soft, gently SEGMENTED caterpillar body (off-white wool in soft ringed segments, a row of tiny stubby legs gripping along underneath) — but the head is unmistakably the AESTHETIC COMPUTER rabbit: TWO long rabbit ears (BOTH present), a little pink nose, tiny dark bead eyes, round soft RED glasses, and two short nubby proto-antennae. On his back only tiny CRUMPLED wing-buds — the monarch wings NOT yet grown. Newborn, soft, a touch clumsy. GLASSES GEOMETRY (CRITICAL): the round red glasses ride HIGH over the eyes, each eye centered INSIDE its own round red lens, rims NEVER empty (an eye above an empty rim reads as extra eyes — the worst failure).`;

const MONARCHS =
`THE MONARCHS — real monarch butterflies (orange + black, painted-paper edged, drawn at different scales for depth) that are ALSO the living data of this near-future AESTHETIC COMPUTER nature-park. they drift on their OWN little arcs — a current of grace, NEVER a swarm. the park's quiet computation reads ONLY as: a faint hand-ruled ground-grid in the lawn (almost invisible, pencil) and soft hand-drawn DOTTED signal-arcs trailing the butterflies' paths.`;

const PALETTE =
`PALETTE — warm cream paper ground; pale-sky-blue overhead; sap-green park grass; off-white + soft-grey woolly bunny; round red glasses; soft pink nose; orange + black monarch wings; jade-green + gold-dot chrysalis. hand-drawn, hatched, print-tech-aware. no haze, no neon, no glow filters.`;

const AVOID_BASE =
`AVOID — any photographic / cinematic / neon look (this is a colored-pencil + gouache drawing); any glowing UI; any swarm of butterflies; any motion-blur / speed-line / glow-filter language; any readable text / wordmark / logo; modern flat tech-illustration style; any figure looking at or acknowledging the camera; a tutu or ballet costume; ANY human person (the character is the bunny only); only one ear on the bunny; empty red glasses rims.`;
const AVOID_NO_SCREENS =
`AVOID also — ANY screen, monitor, laptop, phone, tablet or device of any kind.`;
const AVOID_LAPTOP_LAW =
`LAPTOP LAW — the laptop computers are hand-drawn honored guests, and there are FIVE of them, EACH CLAMSHELL A DIFFERENT COLOR (five distinct candy-pastel bodies — e.g. butter yellow, sky blue, mint green, rose pink, tangerine), drawn with the same pencil hatching as everything else, screens lit like paper and completely GLOW-LESS. each laptop has TWO clearly DIFFERENT parts, and they must NOT be confused with each other:
(A) THE SCREEN (upper half of the clamshell) shows the NOTEPAT interface exactly as the SCREEN reference capture, reproduced faithfully and RIGHT-SIDE UP: a thin transport/status bar across the TOP, then TWO side-by-side banks of note tiles, each bank EXACTLY 4 TILES WIDE and 3 TILES TALL (12 candy-colored flat rectangular tiles per bank with a small letter in each, packed edge to edge like postage stamps), a thin pale waveform strip. never cut off, crop, merge, or duplicate a column or row. a few DIFFERENT tiles are pressed bright mid-chord on each machine.
(B) THE KEYBOARD is a NORMAL, uncolorful MacBook keyboard — its GEOMETRY exactly like the MacBook-Neo geometry reference: STAGGERED rows offset sideways (slim function/number row, then QWERTYUIOP, then ASDFGHJKL, then ZXCVBNM), a wide SPACEBAR centered on the bottom row flanked by command/option, arrow keys bottom-right. KEY COLORS — the keyboard is NOT colorful and carries NO candy colors at all: every key is the ordinary PALE / light silver keycap color of a normal MacBook, EXCEPT for a handful of DARK near-black keycaps that stand out exactly like the BLACK KEYS of a piano. the DARK (black) keys are ONLY these ELEVEN letter keys and no others: V, Z, S, W, Q, R, T, Y, U, O, P. that is — the whole top letter row EXCEPT E and I (so Q, W, R, T, Y, U, O, P dark; E and I stay pale), plus S on the home row, plus Z and V on the bottom row. every other key — E, I, D, F, G, H, J, K, L, A, B, C, N, M, X, the number row, the function row, all modifiers and the spacebar — stays plain pale keyboard color. so the deck reads as a normal pale MacBook keyboard with exactly those eleven BLACK keys, like piano sharps. ABSOLUTELY NO orange/green/red/yellow/cyan/purple/magenta keys, NO candy keys, NO rainbow, NO every-key-a-different-color; NOT an ortholinear grid, NOT a copy of the on-screen tile banks, NOT a calculator pad, NOT keys in perfect columns. render both parts as flat hand-hatched color fields matching the references; small labels may stay illegible. NO drumsticks, NO mallets — he plays with his BARE PAWS.`;

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
    larval: true, // the BUNNYPILLAR — wing-buds only, skip the wing check
    faceVisible: true, // larval anchor — face + glasses geometry must be clean
    text:
`BEAT — HATCH (the bunnypillar crawls out, GOOEY) · MOOD: brand-new, delighted-confused, wet. the BUNNYPILLAR — the caterpillar-form of the bunnyfly, a plump softly-segmented off-white wool body with a row of tiny stubby legs and the rabbit head (TWO long ears, pink nose, round red glasses, short nubby proto-antennae) — inches OUT of the split jade-green paper-mache chrysalis along the thick oak branch, gripping the bark with his little legs, and he is GOOEY with birth: glistening strands of translucent pale-jade chrysalis fluid stretch from his body back to the husk, sagging and about to snap; fat drops bead along his segments and drip off toward the lawn far below; his wool is slicked wet and matted, the tiny CRUMPLED wing-buds on his back soaked and folded flat. the goo is drawn as glossy gouache highlights over the pencil hatching — wet shine by PAINT, never a glow filter. his round red glasses ride HIGH over his eyes (each dark bead eye centered inside its red lens, rims never empty), head tilted DOWN, blinking at his first morning. the cracked jade-green husk swings gently behind him, gold dots catching the light, more fluid stringing from its torn lip. morning sun warming the lawn far below the branch.`,
  },
  hatchpov: {
    bunny: true,
    larval: true,
    pov: true,
    text:
`BEAT — HATCHPOV (insert) · SHOT — FIRST-PERSON POV, literally through the bunnypillar's brand-new eyes (his FACE IS NOT SHOWN): in the near foreground his own front stubby legs and the first ringed segment of his own off-white wool caterpillar body gripping the rough oak bark, a torn flake of jade-green paper-mache still stuck to one leg — and beyond, far below and swimming into focus, the dawn park lawn with its faint hand-ruled ground-grid and one monarch drifting up toward the camera-as-his-eyes. EMOTION — first sight, disoriented wonder, "where am I".`,
  },
  grow: {
    bunny: true,
    transform: true, // the metamorphosis: bunnypillar → bunnyfly (both blocks)
    text:
`BEAT — GROW (the metamorphosis) · MOOD: one long transforming stretch — the caterpillar becomes the winged one. down on the park lawn now, seen from BEHIND (his face away from camera): the BUNNYPILLAR rears UP out of his segmented caterpillar body and TRANSFORMS into the BUNNYFLY in one unfurling motion — the ringed larval segments drawing together into a small velvety butterfly body, and the tiny crumpled wing-buds on his back SWELLING and creaking OPEN into full-span real monarch WINGS (orange + black, veined, his own — never paper-mache, never props), the two slender butterfly antennae extending tall above his TWO long ears. mid-transformation: still a little larval below, already winged above. two or three monarchs circle in close to inspect the new one, on their own little arcs. morning sun, long fresh shadows on the sap-green grass, the oak with the empty cracked chrysalis small in the background.`,
  },
  dance: {
    bunny: true,
    faceVisible: true, // establishes the BUNNYFLY (generated with no char anchor)
    text:
`BEAT — DANCE (first flight-steps) · MOOD: joy arriving all at once. now fully the BUNNYFLY, his first steps go straight into ballet — caught at the lift: risen onto the ball of one hind foot (relevé), the other leg extended back in a low arabesque, both front paws curved overhead in a soft port de bras, his own monarch wings fanned wide behind him. seen three-quarter FROM BEHIND, muzzle lifted away from camera but his round red glasses and TWO long ears visible in profile (each eye inside its red lens, rims never empty), the two butterfly antennae curving above. three or four monarchs falling in beside him on their arcs, sharing the upward turning motion. a soft ground-shadow anchors his standing foot to the lawn. late-morning sun.`,
  },
  laptops: {
    bunny: true,
    screens: true,
    faceVisible: true,
    text:
`BEAT — LAPTOPS (the laptop marimba — the gag) · MOOD: gleeful recital. the woolly bunny KNEELS behind FIVE laptop computers — each clamshell a DIFFERENT candy color — laid open in a gentle arc on the park lawn like the bars of a marimba, PLAYING the keyboards with his BARE PAWS striking like marimba mallets: paw-tips caught mid-bounce off the colored keys, one paw rebounding high, the other landing. seen from behind / over his shoulder so the arc of laptops faces him, his head in three-quarter PROFILE with his round red glasses and TWO long ears clearly visible (each eye inside its red lens, rims never empty). and he SINGS like a pop star: ONE of his slender butterfly antennae has curved DOWN around the side of his head, its little club-knob tip hovering right in front of his pink nose/mouth like a headset microphone — muzzle lifted, singing his heart out into the antenna-mic while his paws keep drumming the keys (the other antenna still curves tall above his ears). the screens tilt up toward him, EACH showing the notepat interface from the SCREEN reference RIGHT-SIDE UP — the candy-colored twin 4×3 tile banks — lit like paper, glow-less, a different chord pressed on each machine; and the KEYBOARD decks below the screens are the real staggered colored QWERTY from the keyboard reference (not a tile grid). monarchs perched along the tops of the screens like an audience on a fence. his own monarch wings tipped open with the playing motion, delighted. bright late-morning sun — the brightest beat.`,
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
    faceVisible: false, // facing away, hardening (one creature, no split)
    text:
`BEAT — MOLT (the hardening begins) · MOOD: solemn wonder, the turn of the story. the five laptops now CLOSED and packed in a neat little stack on the grass beside him. the SAME single BUNNYFLY stands FACING AWAY from the camera on his hind legs, front paws rising into one last port de bras — and his whole body is CRYSTALLIZING into brushed metal from the wingtips inward: his own monarch wings hardening panel by panel into thin orange + black brushed-metal plates with tiny rivet dots, the change sweeping like frost down toward his woolly body and his TWO long ears, the metal creeping up around the red glasses. he stays ONE creature the entire time — the bunnyfly BECOMING a metal bunnyfly. CRITICAL: do NOT split him into a separate butterfly plus a leftover rabbit; there is NO second bunny, NO empty fur or shed body on the lawn, NO monarch flying away from him — only this one figure turning to metal in place, beginning to lift. warm gold light turning rose.`,
  },
  wing: {
    bunny: false,
    text:
`BEAT — WING (insert) · SHOT — EXTREME CLOSE-UP filling the frame: one of the bunnyfly's own monarch wings mid-transformation, inches away — the lower half still soft painted orange + black, the upper half already thin brushed-metal plates with tiny rivet dots along the wing bone, the change sweeping across the surface like frost. the small round lens-eye at the wing root catching one glint of sun. EMOTION — quiet awe, the hardening. no full figure, only the wing.`,
  },
  drone: {
    bunny: false, // metal bunnyfly described inline (one creature, keeps ears)
    text:
`BEAT — DRONE (the metal bunnyfly climbs) · MOOD: quiet sci-fi lift. mid-air over the park: the METAL BUNNYFLY — the same hero, now hardened to a brushed-metal bunny × monarch drone — climbs up through the frame. it KEEPS its bunny identity in metal: TWO long brushed-metal rabbit EARS folded back sleek, the round RED glasses now two glowing red lens-rings on its metal face, and its own monarch WINGS hardened panel by panel into thin brushed-metal plates, orange + black paint still showing, tiny rivet dots along the wing bones. six slender monarch legs attached at the THORAX, tucked neatly BENEATH the body, feet trailing back toward the abdomen tip (never splayed forward, never pointing toward the head), abdomen down-and-back, metal antennae forward. NO separate rabbit anywhere below — the bunny IS this flying machine now. a hand-drawn dotted signal-arc trails behind it. below, the park shrinks away — the lawn, the oak, the faint ground-grid seen from above, EMPTY of any figure. pale-blue sky deepening upward toward indigo at the top of the frame.`,
  },
  spacepov: {
    bunny: false,
    text:
`BEAT — SPACEPOV (insert) · SHOT — FIRST-PERSON POV through the metal bunnyfly's own red lens-eye (no full figure anywhere): the tips of its OWN two brushed-metal monarch wings frame the left and right edges of the view like outstretched arms, rivet dots catching sun, and the sleek metal rabbit ear-tips just visible at the top edges — and between them, far below, Earth as a small blue-green hand-hatched marble against heavy deep-indigo pencil space, stars picked out as bare cream paper, the faint hand-ruled orbit-lines curving away. EMOTION — leaving home, vast calm. the machine's first person.`,
  },
  space: {
    bunny: false,
    text:
`BEAT — SPACE (outer space) · MOOD: a held last breath, vast + calm. OUTER SPACE drawn in heavy deep-indigo colored pencil, the stars picked out as bare cream paper sparkling through. the METAL BUNNYFLY drifts up-frame among the stars — the same hero, brushed-metal bunny × monarch: two sleek metal rabbit EARS, red glowing lens-ring glasses, monarch wings hardened to metal panels, sunlight raking them warm orange. Earth hangs below as a small blue-green marble, hand-hatched. the park's ground-grid has become faint hand-ruled ORBIT-LINES curving through the dark. no other figure. the bunnyfly is everywhere now.`,
  },
};

function build(beat) {
  const parts = [MEDIUM];
  if (beat.transform) parts.push(BUNNYPILLAR, BUNNYFLY);
  else if (beat.larval) parts.push(BUNNYPILLAR);
  else if (beat.bunny) parts.push(BUNNYFLY);
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
async function qaPanel(b64, larval = false) {
  const body = {
    model: QA_MODEL,
    max_completion_tokens: 3000,
    messages: [{
      role: "user",
      content: [
        { type: "text", text:
          "A hand-drawn colored-pencil illustration of a woolly rabbit " +
          "character (a bunny × monarch-butterfly hybrid) who wears small " +
          "round RED glasses and has monarch butterfly wings of his own. " +
          "Zoom in and answer as a " +
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
    // larval bunnypillar has wing-BUDS only — don't require full wings.
    const wingsOk = larval ? true : (v.wing_count ?? 2) >= 2;
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

// ── OCR keyboard check: read the frontmost laptop keyboard and score how
// far its DARK keycaps are from the target set (V Z S W Q R T Y U O P).
// Returns { score, wrong, missing, legible }. score 0 = exact; lower is
// better. Used to pick the best-of-N candidate for screen beats.
async function qaKeyboard(b64) {
  const body = {
    model: QA_MODEL,
    max_completion_tokens: 1500,
    messages: [{
      role: "user",
      content: [
        { type: "text", text:
          "This hand-drawn illustration contains one or more laptop QWERTY " +
          "keyboards. Look ONLY at the frontmost / most legible keyboard. For " +
          "each LETTER key whose color you can judge, decide whether its keycap " +
          "is DARK / BLACK or PALE / LIGHT (ignore number keys, the space bar, " +
          "and modifier keys). List every letter whose keycap is clearly DARK " +
          "(dark_keys). The INTENDED dark letters are exactly these eleven: " +
          "V, Z, S, W, Q, R, T, Y, U, O, P — every other letter should be light. " +
          "Then compute wrong_dark = dark letters that are NOT in the intended " +
          "set, and missing_dark = intended letters that appear light. If no " +
          "keyboard letters are legible enough to judge, set legible=false. " +
          'Reply ONLY compact JSON: {"legible":true|false,"dark_keys":["..."],' +
          '"wrong_dark":["..."],"missing_dark":["..."]}' },
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
    if (!r.ok) return { score: 0, legible: false, note: `kb-qa-http-${r.status}`, wrong: [], missing: [] };
    const txt = (await r.json()).choices?.[0]?.message?.content || "{}";
    const m = txt.match(/\{[\s\S]*\}/);
    const v = m ? JSON.parse(m[0]) : {};
    if (v.legible === false) return { score: 0, legible: false, wrong: [], missing: [], note: "kb not legible" };
    // normalise + recompute locally (don't fully trust the model's own diff)
    const dark = (v.dark_keys || []).map((s) => String(s).toUpperCase().slice(0, 1));
    const target = new Set(KB_DARK_TARGET);
    const wrong = [...new Set(dark.filter((k) => /[A-Z]/.test(k) && !target.has(k)))];
    const missing = KB_DARK_TARGET.filter((k) => !dark.includes(k));
    return { score: wrong.length + missing.length, wrong, missing, legible: true };
  } catch (e) {
    return { score: 0, legible: false, wrong: [], missing: [], note: `kb-qa-error ${e.message}` };
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

// full generate → write + mirror. cached unless --force. never throws.
//   faceVisible beats: HARD-gate on qaPanel (eyes/ears/wings), re-roll on fail.
//   screens beats: also run qaKeyboard and keep the BEST-of-N candidate
//     (fewest dark-key errors); accept immediately on a perfect score 0.
async function generate(beat, refs, useEdit, outPath, label) {
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) { console.log(`✓ cached → ${rel}`); return true; }
  const needsKb = beat.screens === true;
  const attempts = needsKb ? KB_ATTEMPTS : (beat.faceVisible ? QA_ATTEMPTS : 1);
  const tag = [beat.faceVisible && "QA", needsKb && "KB-OCR"].filter(Boolean).join("+");
  console.log(`▸ ${label} · ${SIZE} · ${useEdit ? refs.length + " refs" : "no refs"}${tag ? " · " + tag : ""}`);
  let best = null; // { b64, kbScore }
  for (let qa = 1; qa <= attempts; qa++) {
    const b64 = await genOnce(build(beat), refs, useEdit, label);
    if (!b64) { if (best) break; return false; } // network exhausted
    // hard eye/ear/wing gate
    if (beat.faceVisible) {
      const verdict = await qaPanel(b64, beat.larval === true);
      if (!verdict.pass) {
        writeFileSync(outPath, Buffer.from(b64, "base64"));
        archiveRejected(outPath, `QA ${qa}/${attempts}: ${verdict.reason}`);
        continue;
      }
      console.log(`  ✓ QA pass (${verdict.reason || "clean"})`);
    }
    // keyboard OCR check → keep the best-scoring candidate
    if (needsKb) {
      const kb = await qaKeyboard(b64);
      const detail = kb.legible === false ? "not legible → accept"
        : `errors ${kb.score}${kb.wrong?.length ? ` wrong[${kb.wrong.join("")}]` : ""}${kb.missing?.length ? ` missing[${kb.missing.join("")}]` : ""}`;
      console.log(`  ⌨ kb ${qa}/${attempts}: ${detail}`);
      if (kb.legible === false) { best = { b64, kbScore: 0 }; break; }
      if (!best || kb.score < best.kbScore) best = { b64, kbScore: kb.score };
      if (kb.score === 0) break; // exact — stop early
      if (qa < attempts) continue; // try for a cleaner keyboard
    } else {
      best = { b64, kbScore: 0 };
    }
    break;
  }
  if (!best) { console.error(`✗ ${label} — no acceptable panel after ${attempts} attempts (rejects kept)`); return false; }
  writeFileSync(outPath, Buffer.from(best.b64, "base64"));
  try {
    copyFileSync(outPath, join(homedir(), "Desktop", `${SLUG}-beat-${basename(outPath).replace(`_${SLUG}-beat-`, "")}`));
  } catch {}
  console.log(`✓ → ${rel}${needsKb ? ` (kb errors: ${best.kbScore})` : ""}`);
  return true;
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
// larval/transform beats anchor on the bunnypillar (hatch PNG); the winged
// laptop + molt beats anchor on the bunnyfly (dance PNG). hatch + dance are
// the two anchor SOURCES and generate with no character ref. metal beats
// (bunny:false) carry no character ref.
function refsFor(name, beat) {
  const refs = [];
  if ((beat.larval || beat.transform) && name !== "hatch" && existsSync(ANCHOR_LARVAL)) refs.push(ANCHOR_LARVAL);
  else if (beat.bunny && !beat.larval && !beat.transform && name !== "dance" && existsSync(ANCHOR_FLY)) refs.push(ANCHOR_FLY);
  // screen ref (candy tiles) + clean uncolored MacBook geometry. The candy
  // KEYMAP_REF is intentionally NOT passed — the keyboard has no candy keys,
  // only dark piano-sharp keys, so a candy ref would fight the instruction.
  if (beat.screens) for (const r of [NOTEPAT_REF, NEO_REF]) if (existsSync(r)) refs.push(r);
  return refs;
}

// order: hatch first (larval anchor), then dance (fly anchor), then the rest
// in beat order — so both anchor PNGs exist before the beats that need them.
const RANK = { hatch: -2, dance: -1 };
const order = [...BEAT_NAMES].filter(wantBeat).sort((a, b) =>
  (RANK[a] ?? 0) - (RANK[b] ?? 0));

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
