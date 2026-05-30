#!/usr/bin/env node
// hellsine/bin/gen-sections-pixel.mjs — AI-generated 16-bit pixel-art
// reinterpretation of each hellsine storyline panel. Same beats + identity
// anchors + scene arcs as gen-sections.mjs, but the MEDIUM / PALETTE /
// AVOID blocks ask gpt-image-2 for HARD-EDGED limited-palette pixel art
// (Chrono Trigger / FFVI / Castlevania SOTN cutscene tableaux), NOT
// photo-real felt-craft. Post-process with pop/bin/crisp-pixel-sections.mjs
// to downscale + crisp the pixels (gpt-image-2 tends to anti-alias even
// when told not to).
//
// Output:  pop/hellsine/out/hellsine-p-sec-NN-<id>.pixel-raw.png
//          (portrait 1024x1536 raw AI output — pass through the crisper)
//
// Usage:
//   node pop/hellsine/bin/gen-sections-pixel.mjs                   # cached
//   node pop/hellsine/bin/gen-sections-pixel.mjs --force           # regen all
//   node pop/hellsine/bin/gen-sections-pixel.mjs --only climax-a   # one panel
//   node pop/hellsine/bin/gen-sections-pixel.mjs --only bridge-a,bridge-b

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import * as progress from "../../lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const POP  = resolve(LANE, "..");
const REPO = resolve(POP, "..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}
const FORCE = flags.force === true;
const SIZE = "1024x1536"; // portrait; will be downscaled by crisper
const TAG  = "-p";
mkdirSync(`${LANE}/out`, { recursive: true });

// ── identity refs (mirrors hellsine/bin/gen-sections.mjs) ────────────
const SHOOT_DIR   = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
  `${LANE}/assets/pals-logo.png`,
  `${LANE}/assets/whistlegraph-butterfly.png`,
].filter((p) => {
  if (existsSync(p)) return true;
  console.warn(`  ⚠ ref missing, dropping: ${p}`);
  return false;
});

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

// ── PORTRAIT recomposition — tall 9:16 ───────────────────────────────
const PORTRAIT_NOTE =
`PORTRAIT OVERRIDE — recompose this beat for a TALL vertical 9:16 frame (NOT square, NOT widescreen). the action arranged vertically: figures stacked / staggered, environment extending UP into smoke columns + sky and DOWN into lava + basalt foreground (or studio floor / cosmic-chute depth for non-hellscape beats). give visualizer chrome breathing room near the top + bottom edges. keep every other rule from the shared material law identical.`;

// ── PIXEL-ART MEDIUM (replaces felt-craft MEDIUM) ───────────────────
const MEDIUM =
`HAND-DRAWN 16-BIT PIXEL-ART TABLEAU — render this scene as TRUE PIXEL ART in the style of late-90s SNES / Genesis / PC-Engine JRPG cutscene + intro art (think: Chrono Trigger pre-rendered cutscenes, Final Fantasy VI portrait scenes, Castlevania Symphony of the Night intro paintings, Seiken Densetsu 3 cinematic panels, Lunar: Eternal Blue animated intros, Phantasy Star IV cinematic frames). HARD-EDGED PIXELS — every edge is a stepped pixel boundary, NEVER smoothed, NEVER anti-aliased, NEVER soft. CHUNKY PIXEL GRID visibly readable across the entire frame: the pixel cell size should be CLEARLY larger than the smallest possible mark — think effective resolution of roughly 192×288 to 256×384 BLOWN UP to fill the canvas, so each "pixel" reads as a fat square block. INDEXED COLOR palette only — 24 to 32 distinct flat colors total across the whole image, NO smooth gradients, NO airbrush blends, NO photographic texture. SHADING is done with HAND-PLACED DITHER PATTERNS (checkerboard, 50% dither, ordered Bayer dither) where you need to suggest a transition between two flat colors — NEVER smooth blending. CRISP 1-PIXEL OUTLINES around figures and key objects (single-pixel-wide dark line, hand-placed). FACES use the JRPG sprite-portrait language: large simplified eyes (2-4 pixels each), simplified hair shapes built from 3-4 flat color zones with dithered shading at the edges, mouth as a small 1-3 pixel stroke. CLOTHING is flat color zones with single-pixel highlights and dithered shadow patches. ENVIRONMENT is built from tiled-style basalt / wood / sky chunks, each a flat color with dither shading at the seams. NOTHING SOFT, NOTHING BLURRED, NOTHING PHOTO-REAL — this is FLAT 16-bit pixel-art reinterpretation of the same scene, drawn pixel-by-pixel by a human pixel artist in the SNES era. DROP the felt fibre / wool texture / stop-motion realism from any prior brief — those material rules are SUSPENDED for the pixel-art reinterpretation. The identity beats (jeffrey's face, outfit, gear, pixsies, laptops, world) remain — but rendered in chunky indexed pixels, NOT in felt and NOT in photography.`;

const JEFFREY =
`PIXEL-ART JEFFREY — about 30, recognizable from the jeffrey reference photographs but RENDERED AS A 16-BIT SPRITE PORTRAIT: tousled medium-length brown hair drawn as 3-4 flat brown tones with dithered shading (mussed, loose strands suggested by single-pixel outline jags), CLEAN-SHAVEN or 1-pixel stubble shadow at most (NO full beard), pale-flesh skin as a flat warm-beige base with a single dithered cheek-pink patch + a 1-pixel nose shadow. Eyes are large simplified pixel-sprite eyes (2-4 px each). Peer-horizontal — never centred as a hero.

JEFFREY'S CANONICAL OUTFIT + GEAR (mandatory — render legibly in pixel-art shorthand whenever jeffrey is in frame at medium-shot or closer; substituting any of these for sweaters, hoodies, t-shirts, tank tops, robes, jackets, or any other garment is FORBIDDEN):
  · TOP — a PALE-BABY-BLUE BUTTON-DOWN SHIRT rendered as flat pale-blue pixels with a sharp pointed collar (drawn as a few angled pixels) and a vertical row of small 1-pixel BUTTONS down the front placket. NOT a sweater, NOT a hoodie, NOT a t-shirt. THIN DARKER-BLUE PIXEL PINSTRIPES run vertically down the fabric — single-pixel-wide vertical stripes spaced evenly across the chest + sleeves (clearly visible parallel pinstripe pattern in indexed colors). NEVER plain solid blue, NEVER pattern-free.
  · CHEST EMBLEM — a SMALL YELLOW BEAR EMBLEM (a tiny pixel-art cartoon bear silhouette, golden-yellow, roughly 6-10 px square) hand-placed on the left chest of the button-down, just above the pocket. always present.
  · PEN — a YELLOW SAILOR PRO GEAR FOUNTAIN PEN clipped at the shirt-pocket as a 2-3 pixel saturated-yellow vertical bar with a single grey 1-pixel clip. always visible whenever the shirt pocket is in frame.
  · GLASSES — RED PLASTIC GLASSES (saturated-red rectangular pixel frames) DANGLING by one earpiece hooked through the button-down's placket between the second + third button. always at the placket unless he's actively wearing them on his face (rare).
  · PANTS — WIDE-LEG MEDIUM-COBALT-BLUE TROUSERS (clearly darker + more saturated blue than the shirt, indexed as a distinct palette color), straight wide leg, no jeans, no shorts.
  · HEADPHONES — when at his desk (overture / early statement only), large over-ear headphones in dark slate-grey pixels, worn HALF ON (one cup over the right ear, the other cup pushed back behind the left ear). otherwise off / absent.
These gear items are the SINGLE MOST IMPORTANT IDENTITY CUES alongside his face — render them legibly in pixel shorthand every time.`;

const PIXSIES =
`PIXEL-ART PIXSIES — 4-7 humanoid grad-students rendered as 16-bit sprite portraits, a real spread of AGES (kid → elder) and a wide RACE + GENDER spectrum (women, men, boys, girls, femme, masc, androgynous; many ethnicities). ROUNDED HUMAN EARS ONLY — never pointed / elf / fae. each sprite has flat-color skin (warm-beige / olive / deep-brown / pale-pink etc. indexed), 3-4 flat hair tones, 1-pixel outlines, simplified sprite eyes (2-4 px). OUTFITS a mixed-up pastiche in pixel-art shorthand — some militaristic-tactical (flat olive + grey patches with small 1-pixel buckles), some super-cute girly (flat pink + lilac flat zones with dithered ruffle hints), some cyberpunk-techwear (flat black + neon-cyan single-pixel accents), some cardigan (flat warm-brown with single-pixel button row), all clashed eclectic the way grad-student wardrobes are. uncanny tells (1-pixel cyan-green LED beads at temple / ear / eye-edge) optional + subtle. each holds (when present) ONE small AC PALS laptop in one hand: rendered as a flat-color rectangular sprite (grey body + colored lid) with the PALS glyph (two-bubbly-people Keith-Haring linked outline from the pals-logo.png reference) painted on the lid in a unique INDEXED hue — cyan / magenta / lime-green / hot-pink / golden-yellow / electric-orange / deep-violet. NEVER apple, NEVER butterfly on pixsie lids.`;

const JEFFREY_LAPTOP =
`JEFFREY'S MACBOOK NEO — when shown, a CITRUS-GREEN pixel-art laptop with a TORN WHITE-PAPER SCRAP visible on the lid where the apple logo would be. On the scrap is the WHISTLEGRAPH BUTTERFLY DOODLE rendered in pixel art.

★ CRITICAL — THE DOODLE'S COMPOSITION MUST MATCH the attached whistlegraph-butterfly.png reference image (see the ref file in the input set). Reproduce the doodle's shape, proportions, wing curves, head shape, leg position, and stroke weight in pixel-art form — a chunky pixel-grid translation of that exact reference, NOT improvisation or stylistic reinterpretation.

Reference description (the PNG is authoritative): a wonky child-marker doodle drawn in a SINGLE THICK MEDIUM-GREY MARKER LINE on a slightly off-white paper scrap, with NO interior fill and NO color. The doodle is a SMILING STICK-FIGURE WITH BUTTERFLY WINGS composed of:
  (1) a small slightly-tilted RECTANGULAR HEAD on top, with TWO small dot EYES and a SHORT CURVED SMILE inside the rectangle;
  (2) a tall thin VERTICAL RECTANGULAR BODY descending straight down from the head;
  (3) FOUR LARGE ROUNDED BUTTERFLY-WING LOBES growing directly out of the SIDES of the body — TWO on the LEFT (an upper-left lobe + a lower-left lobe joined at the body's midline) and TWO on the RIGHT (an upper-right lobe + a lower-right lobe joined at the body's midline). Each lobe is a soft rounded blob shape with a single curved interior crease line suggesting a wing fold;
  (4) TWO SHORT STUBBY RECTANGULAR LEGS side-by-side below the body.

The figure is LEFT-RIGHT SYMMETRIC. Render it as a thick 2-3-pixel medium-grey stroke on an off-white paper-scrap rectangle, the embossed apple peeking around its torn pixel-edges.

This doodle is on JEFFREY'S green laptop ONLY — NEVER on a pixsie's laptop (pixsie lids carry the PALS glyph ONLY). The laptop is rendered as flat-color pixel shapes with single-pixel highlights — NO smooth plastic reflections, NO photographic gloss.`;

const WORLD_LAW =
`WORLD LAW — the SETTING varies by beat, but always rendered in 16-bit pixel-art tileset style:
• EARTH PANELS (overture-*, statement-a, statement-b) = jeffrey's studio zollo as pixel-art interior: warm amber desk-lamp pool (flat amber + dithered halo), WOODEN-PLANK floor (rows of 8-12 px brown planks with single-pixel seam lines + small dither knots), a small pixel-art plant in a pot, a pixel-art AC poster on the wall, a star-flecked indexed-blue night sky outside the windowpane.
• STUDIO-HOLE PANELS (statement-c, bridge-a, bridge-b, bridge-c) = the SAME pixel-art studio, but now with a PERFECT ROUND HOLE burned cleanly through the wooden floor — circular cut rendered as a stepped pixel circle, edges as a 1-pixel ember-red glow, charred + dithered-black smoking wood at the rim, dithered heat-shimmer pixels rolling up. DOWN INSIDE the hole: HELL clearly visible far below — sinusoidal LAVA RIVERS as orange + yellow flat pixel zones with dithered seams, basalt as dark-grey tiles, bruise-purple sky. warm orange hell-glow paints the studio with dithered orange pixels on the wood; cool desk-lamp + cold drone targeting beams paint from above. small soap-bubble pixel circles drifting up.
• TRANSITION (bridge-d) = mid-fall THROUGH the hole — the studio doorway + ceiling shrinking away above, basalt + lava rushing up below, the crew cannonballing as one pixel-sprite cluster. NO long cosmic chute.
• HELLSCAPE PANELS (develop-*, climax-*, coda-*) = pixel-art alien volcanic landscape — cracked CHARRED basalt foreground as tiled dark-grey + soot-yellow flat zones with dithered cracks, sinusoidal LAVA RIVERS curving across the ground as orange + yellow pixel sine waves (the lava itself runs in clean math sine waves, stepped to the pixel grid), tall walls of sine-shaped FLAME drawn as stacked orange + yellow pixel tongues, jagged obsidian spires receding into a bruise-purple horizon, dense atmospheric particulate (orange ember pixels + yellow spark pixels, white ash pixels, soap-bubble pixel circles, smoke columns as dithered grey, multi-hue matrix-rain as single-pixel-wide vertical streaks in yellow / red / purple / lime), heat-shimmer pixels above the lava. A SINGLE PURE-WHITE HORSE flames across the mid-distance basalt with red flames licking its pixel hooves + body. A SLEEK BULLET TRAIN streaks the far horizon as a pixel-sprite with lit-up windows + motion-streak lines. THE SINES ARE THE FIRE — every lava river + flame tongue traces a perfect pixel-stepped sine wave. NO sine beams from anyone's mouth. NOTE: develop-a + develop-b feature the crew IN A LAVA POOL — the lava acts as warm magical liquid; sprites darken at the waterline + char-shade but bodies stay buoyant + joyful.`;

const ARCS =
`CONTINUITY ARCS across the whole 18-panel set (the beat description below names this beat's stage):
• EYES: dim (dark sprite eyes) → shock-wide at the strike → wide-awe peering into the hole → bright-call mid-facetime → bright-greeting at the bust-in → grouped-wonder around the hole → mid-whoop cannonballing → mid-whoop splashing → laughing while swimming → IGNITION (fire-pixel ignites in sockets in a chain) → blazing red-orange flame-pixels in sockets (no whites) for the party → ember-pixels (soft glow, no flames) at dawn.
• CLOTHES: clean through overture → clean through statement → clean as he peers in the hole → clean through facetime + bust-in + cannonball → dithered-dark at the waterline on splashdown → wet + first char-pixels from swimming → first scorch-pixels + frayed edges at ignition → fully tattered (jagged pixel edges) + char-marked at the party → tattered but settled by dawn.
• PALS LIDS: jeffrey's MacBook Neo BUTTERFLY-scrap visible in overture, statement, coda; pixsie laptops APPEAR for the first time in bridge-b (each pixsie carries one when bursting in) but PALS lids are DARK until ignition; PALS FLICKER on during the swim (one or two glowing in their indexed hue) → full seven-hue glow at the party → soft pulse at dawn.
• FINGERTIPS: clean through earth + studio-hole + cannonball + splashdown + swim → tiny pixel flames at tips + scorch + ember-cracks on palms at ignition + party → embers at dawn.`;

// ── PIXEL-ART PALETTE (replaces the felt-craft PALETTE) ──────────────
const PALETTE =
`PALETTE — strictly LIMITED INDEXED 16-bit-style palette of 24-32 distinct flat colors per panel (NO smooth gradients between hues, NO airbrush blends; tonal transitions happen through DITHER PATTERNS only). Per arc-stage:
• EARTH PANELS (overture-*, statement-a, statement-b) = 8-12 indexed colors: warm amber desk-lamp pool, cool deep-blue night, pale-baby-blue + medium-cobalt for jeffrey, soft browns for the wood + hair, single bright accent for the bear emblem + pen.
• STUDIO-HOLE PANELS (statement-c, bridge-a, bridge-b, bridge-c) = 10-14 indexed colors: the earth palette PLUS a warm ember-red + orange + dithered-yellow for the hole's hellglow, bruise-purple for the deep, soot-black for the char.
• TRANSITION (bridge-d) = 10-14 indexed colors: studio doorway light from above, lava orange + yellow + ember-red rushing up from below, basalt grey.
• HELLSCAPE PANELS (develop-*, climax-*, coda-*) = 12-16 indexed colors: dominant warm lava-orange + sulphur-yellow + ember-red as the principal light, deep crimson sky, bruise-purple horizon, obsidian-black + dark-grey for basalt, single-pixel cyan / magenta / lime / hot-pink / gold / orange / violet accents for PALS lids, single-pixel white + yellow for sparks + ash. Dawn coda panels swap the crimson for a coral-pink + warm peach.
NO smooth gradient ramps. ALL tonal transitions handled with hand-placed dither patterns (checkerboard, 50%, ordered Bayer).`;

const DEVICE_ENGAGEMENT =
`DEVICE ENGAGEMENT — whenever jeffrey or any pixsie is holding a laptop, phone, or other device, the figure's HEAD AND GAZE MUST BE ON THE SCREEN (eyes down/toward the display, head tilted into the work, body posture absorbed in using the thing). NEVER the presentational "showing the laptop to the camera" pose where the device is turned sideways so its screen / lid faces the lens. The screen faces the user, not the viewer. If a lid (back of the screen) is in frame, it's because the figure is using the device naturally — not because they're modelling it.`;

// ── PIXEL-ART AVOID (replaces felt-craft AVOID) ──────────────────────
const AVOID =
`AVOID (pixel-art specific) — NO anti-aliasing of any kind (every edge is a hard stepped pixel boundary, NEVER blurred or smoothed), NO photo-real texture or surface, NO soft blur, NO airbrush gradients, NO smooth color blends (use DITHER PATTERNS instead), NO film grain, NO depth-of-field blur, NO 3D rendering tells (no rendered shadows, no ambient occlusion, no specular highlights — only hand-placed pixel highlights + dithered shadow patches), NO felt fibre / wool / fabric texture (this is the pixel-art reinterpretation, NOT the felt-craft set), NO photographic photo backdrops behind the figures (the whole world is pixel-art tiles), NO modern flat vector-illustration look (this is JRPG sprite art, NOT Figma flat-design), NO cartoon / plush / collaged look, NO apple logos or butterfly glyphs on pixsie lids (PALS only on pixsie lids, butterfly only on jeffrey's MacBook Neo lid scrap), NO sine beams pouring from anyone's mouth (the sines ARE the lava + fire itself), NO blood / gore / body horror (heat damage = pixel scorch + jagged frayed edges, not gore), NO readable text / wordmark / logo anywhere, NO pointed / elf / fae ears on pixsies (rounded human ears only), NO jeffrey centred as a hero (peer-horizontal), NO recursive screens, NO living-artist names, NO motion blur, NO presentational "showing the laptop to camera" poses.`;

// ── per-beat story — 18 panels (verbatim from gen-sections.mjs) ──────
const SECTION_ORDER = [
  "overture-a", "overture-b", "overture-c",
  "statement-a", "statement-b", "statement-c",
  "bridge-a", "bridge-b", "bridge-c", "bridge-d",
  "develop-a", "develop-b", "develop-c",
  "climax-a", "climax-b", "climax-c",
  "coda-a", "coda-b",
];

const SECTION_VARIANTS = {
  "overture-a":
`BEAT — OVERTURE A "before the keystroke" · EMOTION: focused calm, the hum before everything. ARC STAGE — eyes dim (no fire), clothes clean, NO pixsies present, PALS lids dark. EARTH. studio zollo, late.

★ LAPTOP STATE (mandatory): The MacBook Neo is CLOSED — lid flush down on top of the keyboard base. We see the GREEN OUTSIDE-OF-LID facing UP toward camera, the TORN WHITE-PAPER SCRAP with the WHISTLEGRAPH BUTTERFLY DOODLE taped in the centre of that green top panel (where the apple logo would be). NO LCD visible anywhere, no screen content, no screen glow — the laptop is shut. The butterfly scrap ONLY ever lives on the OUTSIDE-OF-LID, never on the LCD side and never on the base; the laptop being closed makes this unambiguous.

CAMERA: positioned OUTSIDE the window LOOKING IN through the night-time glass. We see jeffrey from OUTSIDE in three-quarter profile: he's seated at his desk inside the warmly-lit studio, his TORSO + ONE SHOULDER + the side of his face turned slightly toward camera; one hand resting gently ON TOP of the closed laptop's green lid, the other hand reaching for a small toy bunny / honey jar / pennies on the desk. Headphones half-on around his neck/ears.

He wears the canonical pale-baby-blue BUTTON-DOWN with darker-blue PINSTRIPES + yellow BEAR EMBLEM at the chest + yellow Sailor Pro Gear PEN clipped at the pocket + RED GLASSES dangling at the placket + wide-leg medium-cobalt TROUSERS + dark slate-grey HEADPHONES half-on. The button-down + pinstripes + pen + glasses are all clearly visible.

Through the window we read into the studio: warm desk-lamp pool, wooden floor, a plant, an AC poster on the wall. On the desk: the CLOSED green MacBook Neo with butterfly scrap face-up, a small toy BUNNY plushie, a small glass jar of golden HONEY with a wooden dipper, and a small stack of pennies (the MONEY) — the song's three lyric talismans.`,

  "overture-b":
`BEAT — OVERTURE B "the blinking light" · EMOTION: stillness, the moment before everything. ARC STAGE — eyes dim, clothes clean, NO pixsies, PALS dark. EARTH.

★ LAPTOP STATE (mandatory): The MacBook Neo is OPEN now, on the desk. Its LCD SCREEN faces JEFFREY (toward him, AWAY from camera). Camera ANGLE is positioned roughly opposite jeffrey's face — but the laptop lid is between camera and the LCD, so the LCD ITSELF IS NEVER VISIBLE to camera (we see only the TOP EDGE / HINGE / BACK OF THE LID from this angle, with the WHISTLEGRAPH BUTTERFLY DOODLE scrap visible on the LID-BACK turned toward us). NO LCD pixels, no readable text — but the WARM SCREEN GLOW spills outward onto jeffrey's face from below his chin, illuminating his cheeks + brow with a soft cool-white screen light (rendered as dithered pale pixels), eyes lowered to the (unseen) screen.

CAMERA: still OUTSIDE the window LOOKING IN, now turned to a more frontal three-quarter angle so we see JEFFREY'S FACE softly lit by the screen-glow from below; his pixel features readable + thoughtful. The laptop is angled in the foreground between us and him, lid-back with butterfly toward us, screen invisible.

He wears the canonical pale-baby-blue BUTTON-DOWN with darker-blue PINSTRIPES + yellow BEAR EMBLEM + yellow PEN + RED GLASSES at the placket + cobalt trousers + dark slate-grey HEADPHONES half-on.

Through the glass behind jeffrey, the otherwise-still indexed-blue night sky carries ONE tiny RED LIGHT blinking once, far away in the distance — too small for him to notice.`,

  "overture-c":
`BEAT — OVERTURE C "the swarm forms" · EMOTION: oblivious — the last calm moment. ARC STAGE — eyes dim, clothes clean, NO pixsies, PALS dark. EARTH. INTERIOR THIS TIME — we are inside the studio looking ACROSS the room from a low angle. the SKY OUTSIDE the window is now alive: the one red blink has become a dozen, then dozens of drones forming a moving constellation against the indexed-black, faint neon-orange targeting beams (single-pixel-wide orange streaks) sweeping across the studio glass from outside, still soundless. jeffrey is at his desk in three-quarter profile still focused on his MacBook Neo, headphones on, oblivious. THE LAPTOP SCREEN STAYS HIDDEN — angled AWAY from camera or otherwise out of view; do NOT show the screen's content. only the lid carries the canonical WHISTLEGRAPH BUTTERFLY DOODLE on the white-paper scrap. warm amber desk-lamp pixels on his face now competing with cold orange beams crossing his shoulder from the window. on the desk: the small toy BUNNY, the jar of HONEY, the stack of pennies (MONEY).`,

  "statement-a":
`BEAT — STATEMENT A "the swarm" · EMOTION: SHOCK — caught mid-recoil, no understanding yet. ARC STAGE — eyes WIDE in shock (still no fire), clothes about to tear, NO pixsies, PALS dark. EARTH. the WINDOW EXPLODES INWARD — pixel shards of glass mid-flight, frozen. a SWARM of black military drones outside, neon-orange targeting lasers crisscrossing the studio, ONE drone right at the broken pane staring in with a single red lens-eye. jeffrey recoils, BOTH PALMS UP defensive, his pixel face caught mid-shock — brow up, mouth open in a small dark oval, NO grin, NO fire-eyes. camera: WIDE, drones swarming on one side of the frame, jeffrey centred, glass pixels mid-air.`,

  "statement-b":
`BEAT — STATEMENT B "the floor laser" · EMOTION: disbelief — watching the floor get carved. ARC STAGE — eyes shock-wide, clothes still clean, NO pixsies, PALS dark, NO fire on him. EARTH. ONE lead drone hovers low in the middle of the studio, its barrel angled STRAIGHT DOWN. it fires a CLEAN PILLAR of cutting laser light — saturated red-orange flat pixels stacked vertically — straight into the wooden plank floor between jeffrey's feet. the beam is a perfect vertical column of pixels, carving a stepped pixel circle through the planks; spark pixels + curling pixel-smoke + flying splinter pixels fan out around the cut. jeffrey is leapt back against the desk, arms shielding his face, watching the floor get carved. NO fire-eyes yet — just wide shock.`,

  "statement-c":
`BEAT — STATEMENT C "the hole to hell" · EMOTION: awe displacing fear — the first faint curl of a grin. ARC STAGE — eyes wide-awe, clothes clean, NO pixsies, NO fire, NO PALS. STUDIO-HOLE. the circle of floor has DROPPED AWAY. a PERFECT ROUND PIXEL-STEPPED HOLE now opens in the wooden planks, edges glowing ember-red where the laser bit through, charred wood smoking. THROUGH the hole: HELL plainly visible — basalt tiles + sinusoidal lava rivers + bruise-purple sky, miles below and yet right there. jeffrey is on his knees at the rim, PEERING IN, hair lifting slightly, the warm orange glow lighting his face from below for the first time. pixel face: awe, no fear, the first faint curl of a grin.`,

  "bridge-a":
`BEAT — BRIDGE A "videocall the squad" · EMOTION: urgent excitement — calling friends to come over. ARC STAGE — eyes bright (no fire), clothes clean, pixsies APPEAR for the first time but only as glow on jeffrey's face from his LAPTOP screen (not yet in the room), PALS not yet relevant. STUDIO-HOLE.

★ DEVICE (mandatory): jeffrey is HOLDING HIS OPEN CITRUS-GREEN MACBOOK NEO IN ONE HAND like a tray / oversized tablet — palm flat under the base, the lid open at ~110° standing up off his palm, screen facing TOWARD HIS FACE so he can see the video call. The LID-BACK (with the WHISTLEGRAPH BUTTERFLY scrap taped on its centre) is what faces the CAMERA. The LCD screen content (a multi-pane FACETIME-style GRID of 4-6 pixsie faces in their own warm-lit pixel tiles) is angled AWAY from camera behind the lid edge — we don't see the screen content directly, only the SCREEN-GLOW spilling onto jeffrey's face from the laptop's screen side. NO PHONE anywhere in the frame.

jeffrey is mid-SHOUT into the laptop, mouth open as a small pixel oval, eyes wide. His FREE HAND (the one NOT holding the laptop) is pointing DOWN at the glowing floor hole.

He wears the canonical pale-baby-blue BUTTON-DOWN with darker-blue PINSTRIPES + yellow BEAR EMBLEM + yellow PEN + RED GLASSES at the placket + cobalt trousers.`,

  "bridge-b":
`BEAT — BRIDGE B "the squad busts in" · EMOTION: arrival energy — wide pixel grins. ARC STAGE — eyes bright (no fire yet), clothes clean, pixsies APPEAR in the room for the first time, each pixsie now carrying ONE AC laptop (PALS lids DARK), jeffrey also still has his MacBook Neo on the desk behind him. STUDIO-HOLE. the STUDIO DOOR explodes open — door slamming back on its hinges, splinter pixels flying. 4-6 pixsies pile through the doorway in a jumbled wave, mid-stride, each carrying their own glossy plastic AC laptop in one hand with the PALS lid showing dark (not yet glowing). outfits in pixel shorthand: kid pixsie in lime PJs + sneakers, elder pixsie in a cardigan with cane raised, femme grad-student in cyberpunk techwear, tactical-vest pixsie in boots, hot-pink-hair pixsie in a hoodie, beanie pixsie clutching a felt coffee. they arrive READY — wide bright pixel grins, eager eyes, peer-horizontal stack in the doorway. jeffrey is on his feet now, half-turned toward them, free arm thrown UP in greeting, the glowing floor hole still visible beside him.`,

  "bridge-c":
`BEAT — BRIDGE C "around the hole" · EMOTION: shared awe, shared grin — warmth on every pixel face. ARC STAGE — eyes bright-wonder (no fire yet), clothes clean, full crew present and ringed around the hole, PALS lids DARK but warm orange glow already dithering on every face from below. STUDIO-HOLE. the whole crew now ringed around the glowing round floor hole, peer-horizontal CIRCLE, jeffrey one member among them (NOT centred). they're leaning in, peering DOWN into hell — warm lava glow hitting every face from below as dithered orange pixels, fingertips on the rim of the cut planks. one pixsie holds her AC laptop OUT over the hole letting the (still dark) PALS lid catch the orange light, one pixsie crouches with hands on knees grinning, the kid pixsie kneels rim-side eyes huge, the elder grips her cane. STILL no fire-eyes — but the warm glow already paints them all.`,

  "bridge-d":
`BEAT — BRIDGE D "all in" · EMOTION: peer-horizontal cannonball — joy + commitment, no fear. ARC STAGE — eyes bright-wide (no fire yet), clothes clean, the whole crew mid-air, AC laptops in tow, the macbook neo spinning free with them. TRANSITION (mid-fall through the hole). CANNONBALL — the entire crew launching INTO the hole together in one frozen instant mid-leap: feet off the floor, knees pulled up, arms thrown around each other's shoulders, the citrus-green MacBook Neo spinning free in the air alongside them with the whistlegraph butterfly scrap still on the lid, AC laptops tucked under arms or pinwheeling. jeffrey is ONE BODY in the group cluster, somewhere in the middle, NOT centred. hair lifted, eyes bright but still clean — no fire yet. behind/above them the ruined studio doorway + ceiling recedes; below them the basalt + lava rivers already rushes up to meet them. peer-horizontal mid-air. camera: FROM BELOW INSIDE THE HOLE looking UP at the falling cluster against the studio doorway light, hell-glow on undersides, the round pixel-stepped disc of studio ceiling shrinking above.`,

  "develop-a":
`BEAT — DEVELOP A "splashdown" · EMOTION: shared whoop, caught between gasp + grin. ARC STAGE — eyes wide-mid-whoop (no fire yet), clothes darkening at the waterline, pixsies present, PALS dark, fingertips clean. HELLSCAPE LAVA POOL. IMPACT into a wide LAVA POOL. the crew hits the sinusoidal lava river in a huge spray — pixel splashes of glowing orange-red lava arcing up in fat pixel droplets, stepped waves rolling outward in concentric rings. jeffrey + pixsies frozen mid-plunge, half-submerged at varied depths, expressions caught between gasp and grin — eyes wide, mouths OPEN in a shared whoop. the citrus-green MacBook Neo splashes down beside them, lid still glowing softly. NO fire-eyes yet — but the lava is up to their chests, lighting every pixel face from within the pool.`,

  "develop-b":
`BEAT — DEVELOP B "swim in lava" · EMOTION: pure pool-day joy in hell. ARC STAGE — eyes laughing (no fire yet), clothes wet + first chars at the waterline, pixsies in the lava pool with jeffrey, PALS LIDS JUST FLICKERING ON (one or two glowing on floating laptops, rest still dark), fingertips clean. HELLSCAPE LAVA POOL. the crew floats + strokes through the LAVA RIVER like it's a warm pool — the lava holds them up the way water would. ONE pixsie on her back floating with arms behind her head, eyes closed grinning. ONE pixsie doing a slow backstroke, trailing a stepped sine-shaped wake. the KID pixsie cannonball-bobs in the middle splashing the ELDER, who is splashing back. JEFFREY is treading lava beside another pixsie laughing, his AC laptop floating like a pool toy beside him with the PALS lid just flickering on. tongues of pixel-sine-flame lick off the surface between them. the lava is their water. outfit pixels darkening at the waterline + starting to char. wicked grins arriving but eyes still clean.`,

  "develop-c":
`BEAT — DEVELOP C "ignition" · EMOTION: wickedness arriving — wide-eyed grins forming, not full demon yet. ARC STAGE — FIRE IGNITES in eye sockets in a chain (jeffrey first, then each pixsie), ALL PALS LIDS now full-glowing in seven hues, FINGERTIPS BEGINNING TO SINGE with tiny pixel flames, first scorch marks + frays. HELLSCAPE LAVA BANK. the crew now CLIMBING OUT + RISING UP from the lava onto the basalt bank — a glowing orange water-line of lava drips off them as they emerge (dithered orange-red dripping pixels). AT THIS MOMENT the FIRE IGNITES — first in jeffrey's eye sockets (one socket, then the other, rendered as small bright orange + yellow flame-pixel sprites where his eyes were) and then in a CHAIN around the crew, each pixsie's eyes lighting in sequence as they surface. all the PALS lids now full-glowing in seven hues. fingertips beginning to singe, tiny pixel flames flickering off finger tips. wide-eyed grins forming. first threads pulling loose, first scorch pixel marks.`,

  "climax-a":
`BEAT — CLIMAX A "the cover" (verbatim cover crop) · EMOTION: peak chaos, peak joy. ARC STAGE — eyes BLAZING with full live flame-pixel sprites in sockets (no whites), all PALS lids full seven-hue glow, fingertips on fire with tiny pixel flames + scorch + glowing ember-cracks, clothes TATTERED + char-marked (jagged pixel edges). THE COVER. the smooshed-into-lens wide-angle group portrait — jeffrey + pixsies dancing ON / OVER / AROUND active pixel-flame, BOTH PALMS UP at the lens, fire-eyes BLAZING, sinusoidal lava ribbons weaving between feet in pixel-stepped sine waves, sine-flame tongues dancing between faces, PALS lids glowing cyan / magenta / lime / hot-pink / gold / orange / violet, white horse + bullet train on the back horizon as small pixel sprites, multi-hue code-rain through smoke columns as single-pixel-wide vertical streaks, soap-bubble pixel circles rising, fingertip flame-pixels + scorched palm cracks, tattered clothes. peak chaos, peak joy, the PARTY. camera: THE COVER CROP VERBATIM — edge-to-edge, jeffrey at about 40% from left, smooshed into the lens.`,

  "climax-b":
`BEAT — CLIMAX B "in the heart of the dance" · EMOTION: peak joyful celebration, alternate vantage. ARC STAGE — eyes glowing warm with inner pixel-light, PALS lids full seven-hue, fingertips bright with warmth, clothes joyfully tattered. THE SAME CELEBRATION, ALTERNATE ANGLE. camera positioned WITHIN the dancing crew at chest-height: jeffrey is just OFF-CENTER on the left side of the frame, dancing with one arm raised, his profile in three-quarter view. the pixsies are scattered AROUND him at peer eye-level, each looking in a DIFFERENT direction — some looking up at the lava sky, two laughing together off to one side, one focused on dancing, one looking down at her PALS laptop, one mid-twirl with her pixel-hair flying — NEVER all looking at jeffrey, NEVER cult-leader composition. raised PALS laptops glow in their seven hues scattered across the frame. lava sines weaving gently between feet as pixel-stepped sine waves. peer-horizontal — jeffrey is just one member of the crew, NO ONE is centered.`,

  "climax-c":
`BEAT — CLIMAX C "the wide vista" · EMOTION: a vast warm chaos with the crew at its heart. ARC STAGE — eyes blazing, PALS full, fingertip flames, clothes tattered — but at scale now. PULL BACK to the WIDEST shot of the track — the dancing group is now SMALL in the lower-third of the frame, the FULL hellsine vista revealed around them: parallel sinusoidal lava rivers curving through the basalt foreground at varied amplitudes (each river a clean pixel-stepped sine), obsidian spires receding into bruise-purple horizon, the white horse flaming across the mid-distance, the bullet train streaking the far horizon, multi-hue matrix-rain streaming through smoke columns on either side, soap bubbles + embers everywhere. the dancing crew is the warm pulsing nucleus inside a vast pixel-art lava world.`,

  "coda-a":
`BEAT — CODA A "all chilling on the basalt" · EMOTION: chilled-out afterglow — nobody is posing, everyone at rest. ARC STAGE — eyes now just EMBERS (small soft orange pixel glow, no flames), clothes fully tattered + scorched (but the damage settled), fingertips embers + scorch (no flames), PALS lids dimmed to a soft pulse, dawn light. HELLSCAPE AT DAWN. dawn breaking over the obsidian horizon — bruise-purple softening to deep coral, the lava glow halved, smoke columns thinning. the crew is JUST CHILLING across the basalt + lava-pool edge — sat on warm rocks, leaning back on elbows, one stretched out flat looking up at the coral sky, ONE pixsie floating again in the cooled lava pool arms behind her head, ONE elder smoking a slow ember off the basalt edge, KID pixsie curled up dozing against an obsidian wedge, jeffrey is reclining against a warm rock among them (NOT centred). spent and content, the party's afterglow. fire-eyes now just EMBERS — soft orange pixel glow, no flames. clothes fully tattered + scorched (jagged pixel edges) but the heat is no longer hurting. PALS lids dimmed to a soft pulse. nobody is posing; everyone is at rest.`,

  "coda-b":
`BEAT — CODA B "they live here now" · EMOTION: calm settled smile, home found. ARC STAGE — eyes embers, clothes tattered-settled, fingertips quiet embers, PALS soft pulse, MacBook Neo open + screen visibly glowing with content. HELLSCAPE AT DAWN, INTIMATE. THREE-QUARTER ANGLE FROM JEFFREY'S SIDE so the camera sees jeffrey in three-quarter profile AND clearly sees the LAPTOP SCREEN OPEN ACROSS HIS LAP — the citrus-green MacBook Neo's screen glowing softly with a KIDLISP piece (green-on-black pixel-stepped terminal text with subtle warm orange ember flickers reflected). the lid (closed side facing away/up toward camera) shows the canonical WHISTLEGRAPH BUTTERFLY on the white-paper scrap. ONE pixsie ASLEEP against his shoulder, embers in her eyes. tucked beside him on the basalt: a small singed BUNNY plushie, a small heat-cracked jar of HONEY, a pile of WARM-glowing pennies (MONEY) — the three lyric talismans that came with him. the white horse a small silhouette on the obsidian behind him. the bullet train a thin red streak. jeffrey is smiling — a CALM smile, NOT the wicked climax grin.`,
};

// ── prompt construction ──────────────────────────────────────────────
function build(sectionBeat) {
  return [MEDIUM, JEFFREY, PIXSIES, JEFFREY_LAPTOP, DEVICE_ENGAGEMENT, WORLD_LAW, ARCS, sectionBeat, PALETTE, AVOID].join("\n\n")
    + "\n\n" + PORTRAIT_NOTE + "\n";
}

const apiKey = loadOpenAIKey();
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function buildForm(promptText) {
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", promptText);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const lower = ref.toLowerCase();
    const ext = lower.endsWith(".png") ? "png" : lower.endsWith(".webp") ? "webp" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  return fd;
}

async function generate(promptText, outPath, label) {
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) {
    console.log(`✓ cached → ${rel}`);
    return;
  }
  console.log(`▸ ${label} · ${SIZE} · ${REFS.length} refs`);
  const MAX_TRIES = 4;
  for (let attempt = 1; attempt <= MAX_TRIES; attempt++) {
    const t0 = Date.now();
    try {
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST",
        headers: { Authorization: `Bearer ${apiKey}` },
        body: buildForm(promptText),
      });
      if (!res.ok) {
        const err = await res.text();
        const transient = res.status === 429 || res.status >= 500;
        if (transient && attempt < MAX_TRIES) {
          const wait = 4000 * attempt;
          console.warn(`  ⚠ OpenAI ${res.status} (${label}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
          await sleep(wait);
          continue;
        }
        console.error(`✗ OpenAI ${res.status} (${label}): ${err.slice(0, 600)}`);
        return;
      }
      const json = await res.json();
      const b64 = json.data?.[0]?.b64_json;
      if (!b64) {
        console.error(`✗ no image (${label}): ${JSON.stringify(json).slice(0, 280)}`);
        return;
      }
      writeFileSync(outPath, Buffer.from(b64, "base64"));
      const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
      const u = json.usage || {};
      const tok = u.input_tokens ? ` · tok in=${u.input_tokens} out=${u.output_tokens}` : "";
      console.log(`✓ ${elapsed}s${tok} → ${rel}`);
      return;
    } catch (e) {
      const cause = e?.cause?.code || e?.cause?.message || e?.message || "unknown";
      if (attempt < MAX_TRIES) {
        const wait = 4000 * attempt;
        console.warn(`  ⚠ network fail (${label}: ${cause}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
        await sleep(wait);
        continue;
      }
      console.error(`✗ network fail (${label}): ${cause} — gave up after ${MAX_TRIES} tries`);
      return;
    }
  }
}

const onlySet = typeof flags.only === "string"
  ? new Set(flags.only.split(",").map((x) => x.trim().toLowerCase()))
  : null;
const wants = (name) => !onlySet || onlySet.has(name);

const pad = (n) => String(n).padStart(2, "0");
const jobs = [];
for (let i = 0; i < SECTION_ORDER.length; i++) {
  const name = SECTION_ORDER[i];
  if (!wants(name)) continue;
  jobs.push({
    prompt: build(SECTION_VARIANTS[name]),
    out: `${LANE}/out/hellsine${TAG}-sec-${pad(i)}-${name}.pixel-raw.png`,
    label: `hellsine${TAG} §${pad(i)} ${name} (pixel-raw)`,
  });
}

progress.begin({ type: "illy-pixel", label: `hellsine${TAG} pixel · ${jobs.length} panels` });
let done = 0;
for (const job of jobs) {
  await generate(job.prompt, job.out, job.label);
  progress.update((++done / jobs.length) * 100, { done, total: jobs.length });
}
progress.end();
console.log(`\n✓ hellsine PIXEL-RAW storyline panel set — ${jobs.length} job(s) · portrait (1024x1536)`);
console.log(`  next: node pop/bin/crisp-pixel-sections.mjs --lane hellsine --slug hellsine`);
