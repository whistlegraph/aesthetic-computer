#!/usr/bin/env node
// hellsine/bin/gen-sections.mjs — generate the hellsine STORYLINE panel
// set: 18 per-beat illustrations tracing jeffrey's drone-attack fall
// from earth into the hellsine party.
//
// Concept (jas, 2026-05-27):
//   the hellsine track visualized as one story — jeffrey at his
//   earth desk in studio zollo → a drone strike, one drone fires a
//   laser STRAIGHT DOWN and burns a hole through the studio floor
//   exposing hell below → jeffrey FACETIMES the squad → the squad
//   BUSTS THROUGH the studio door → they all CANNONBALL into the
//   hole together → SPLASHDOWN into lava → they SWIM in lava →
//   ignite → peak party on the basalt shore (climax-a is the
//   locked cover) → dawn, everyone CHILLING, they live here now.
//   eighteen beats arranged across the six README sections
//   (overture / statement / bridge / develop / climax / coda),
//   ≈ 9 s each over the 2:42 master.
//
// Material rules + identity are inherited from hellsine.illy.txt
// (felt-craft figures, plastic PALS-lid laptops, photo-real worlds,
// sinusoidal lava + sines-are-the-fire law). Each SECTION_VARIANT
// only re-stages composition + beat-specific emotion + lifecycle
// state of the eyes / felt / PALS arc.
//
// Output:  pop/hellsine/out/hellsine-p-sec-NN-<id>.png   (portrait 1024x1536)
// Landscape variant (--landscape):
//          pop/hellsine/out/hellsine-yt-sec-NN-<id>.png  (1536x1024 for YT)
//
// Usage:
//   node pop/hellsine/bin/gen-sections.mjs                  # cached, portrait
//   node pop/hellsine/bin/gen-sections.mjs --force          # regen all, portrait
//   node pop/hellsine/bin/gen-sections.mjs --landscape      # landscape 16:9
//   node pop/hellsine/bin/gen-sections.mjs --only climax-a  # one panel
//   node pop/hellsine/bin/gen-sections.mjs --only bridge-a,bridge-b

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
const LANDSCAPE = flags.landscape === true;
const SIZE = LANDSCAPE ? "1536x1024" : "1024x1536";
const TAG  = LANDSCAPE ? "-yt" : "-p";
mkdirSync(`${LANE}/out`, { recursive: true });

// ── identity refs (mirrors hellsine/bin/gen-illy.mjs) ────────────────
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

// ── LANDSCAPE recomposition — wide 3:2 / 16:9 ────────────────────────
const LANDSCAPE_NOTE =
`LANDSCAPE OVERRIDE — recompose this beat for a WIDE 3:2 / 16:9 frame (NOT square, NOT tall). the action stretched horizontally: figures spread across the width with breathing room left + right, the environment panorama spanning the full width — for hellscape beats: sine rivers curving across the basalt, obsidian spires receding to either side, bullet train + white horse + smoke columns + matrix-rain all visible across the wide horizon; for earth beats: the studio room laid out wide with window + desk + lamp + plant readable across the width; for chute beats: the descending shaft stretched into a wide angled cross-section. give visualizer chrome breathing room near the top + bottom edges. keep every other rule from the shared material law identical.`;

// ── shared scene law (distilled from hellsine.illy.txt) ──────────────
const MEDIUM =
`HAND-CRAFTED FELT TABLEAU placed inside a REAL PHOTOGRAPHIC environment. the figures are FELT — visible wool fibre on every surface of skin / hair / clothing, slightly fuzzy outlines where each body meets the photo backdrop, soft 3-D forms sculpted from felt. think high-end stop-motion (Wes Anderson Isle of Dogs / Aardman). NOT cartoon, NOT plush-toy, NOT cute, NOT collaged. refined felt-craft realism seamlessly inside the photographed world. EMOTION + POSTURE shifts beat to beat — read the EMOTION called out in this beat's description and let faces + bodies show it clearly.`;

const JEFFREY =
`FELT-PUPPET JEFFREY — about 30, recognizable from the jeffrey reference photographs: tousled medium-length brown felt-yarn hair (mussed, loose strands), CLEAN-SHAVEN or faint stubble at most (NO full beard, NO heavy scruff), pale-flesh felt skin with subtle cheek pink. peer-horizontal — never centred as a hero. his felt + eyes go through the arc described below per beat.

JEFFREY'S CANONICAL OUTFIT + GEAR (mandatory — must be CLEARLY VISIBLE whenever jeffrey is in frame at medium-shot or closer; substituting any of these for sweaters, hoodies, t-shirts, tank tops, robes, jackets, or any other garment is FORBIDDEN):
  · TOP — a PALE-BABY-BLUE felt BUTTON-DOWN SHIRT with a sharp pointed collar and a row of small felt buttons running down the front placket. NOT a sweater, NOT a cable-knit, NOT a cardigan, NOT a hoodie, NOT a t-shirt, NOT a sweatshirt, NOT a turtleneck, NOT a flannel — a true light-blue oxford-style button-down with visible buttons + collar at all times. it has THIN HAND-APPLIED DARKER-BLUE FELT PINSTRIPES running vertically down the fabric (visible parallel pinstripe pattern across the chest + sleeves). NEVER plain solid blue, NEVER pattern-free.
  · CHEST EMBLEM — a SMALL YELLOW FELT BEAR EMBLEM (a wonky cartoon bear silhouette, golden-yellow, the size of a 50¢ coin) hand-stitched onto the left chest of the button-down, just above the pocket. always present.
  · PEN — a YELLOW SAILOR PRO GEAR FOUNTAIN PEN clipped at the shirt-pocket of the button-down: short cigar-shaped saturated-yellow barrel, flat-topped saturated-yellow cap with a polished silver metal clip + a single thin gold trim ring at the cap join. ONLY the cap + clip protrude above the pocket — body of the pen is tucked inside. always visible whenever the shirt pocket is in frame.
  · GLASSES — RED PLASTIC GLASSES (saturated red rectangular frames, no lenses-tint) DANGLING by one earpiece hooked through the button-down's placket between the second + third button. always at the placket unless he's actively wearing them on his face (rare). NOT in his pocket, NOT in his hand — dangling.
  · PANTS — WIDE-LEG MEDIUM-COBALT-BLUE FELT TROUSERS (clearly darker + more saturated blue than the shirt), straight wide leg, no pleats, no cuffs. NOT jeans, NOT shorts, NOT joggers.
  · HEADPHONES — when at his desk (overture / early statement only), large over-ear felt headphones in a dark slate-grey, worn HALF ON (one ear cup over the right ear, the other cup pushed back behind the left ear so the left ear is exposed). otherwise off / absent.
THESE GEAR ITEMS ARE THE SINGLE MOST IMPORTANT IDENTITY CUES alongside his face — render them legibly every time. If the camera is at medium-shot or closer, the button-down + pinstripes + bear emblem + pen + glasses must all be visible.`;

const PIXSIES =
`FELT PIXSIES — 4-7 humanoid grad-students appearing in mid-story beats onward, a real spread of AGES (kid → elder) and a wide RACE + GENDER spectrum (women, men, boys, girls, femme, masc, androgynous; many ethnicities). ROUNDED HUMAN EARS ONLY — never pointed / elf / fae. thoughtful warmly-intelligent faces. OUTFITS a mixed-up pastiche — some militaristic-tactical, some super-cute girly, some cyberpunk-techwear, some cardigan, all of it clashed eclectic the way grad-student wardrobes are. uncanny tells (LED beads under felt skin at temple / ear / eye-edge in cyan-green pinpricks, occasional hairline felt seams) optional + subtle. each holds (when present) ONE small AC PALS laptop in one hand: glossy injection-moulded plastic shell with the PALS glyph (the two-bubbly-people Keith-Haring linked outline from the pals-logo.png reference) glowing on the lid in a unique hue — cyan / magenta / lime-green / hot-pink / golden-yellow / electric-orange / deep-violet. NEVER apple, NEVER butterfly on pixsie lids.`;

const JEFFREY_LAPTOP =
`JEFFREY'S MACBOOK NEO — when shown, a CITRUS-GREEN plastic MacBook Neo with a TORN WHITE-PAPER SCRAP taped over where the apple logo would be. On the scrap is the WHISTLEGRAPH BUTTERFLY DOODLE.

★ CRITICAL — THE DOODLE MUST EXACTLY MATCH the attached whistlegraph-butterfly.png reference image (see the ref file in the input set). Reproduce that doodle's EXACT line work, proportions, wing curves, head shape, leg position, and line weight — pixel-faithful copy, no improvisation, no stylistic reinterpretation, no anatomical correction. The reference png IS the doodle; if the rendered scrap doesn't read like that exact reference, it is wrong.

Reference description (a verbal aid, but the PNG is authoritative): a wonky child-marker doodle drawn in a SINGLE THICK MEDIUM-GREY MARKER LINE on a slightly off-white paper scrap, with NO interior fill and NO color. The doodle is a SMILING STICK-FIGURE WITH BUTTERFLY WINGS composed of:
  (1) a small slightly-tilted RECTANGULAR HEAD on top, with TWO small dot EYES and a SHORT CURVED SMILE inside the rectangle;
  (2) a tall thin VERTICAL RECTANGULAR BODY descending straight down from the head;
  (3) FOUR LARGE ROUNDED BUTTERFLY-WING LOBES growing directly out of the SIDES of the body — TWO on the LEFT (an upper-left lobe + a lower-left lobe joined at the body's midline) and TWO on the RIGHT (an upper-right lobe + a lower-right lobe joined at the body's midline). Each lobe is a soft rounded blob shape with a single curved interior crease line suggesting a wing fold;
  (4) TWO SHORT STUBBY RECTANGULAR LEGS side-by-side below the body.

The figure is LEFT-RIGHT SYMMETRIC. The four wing lobes are clearly WINGS attached to the body (NOT clouds, NOT hands, NOT arms reaching up, NOT a realistic insect with antennae). Render it CLEARLY LEGIBLE — a wonky hand-drawn child-marker doodle in a single thick stroke on a real taped paper scrap, the embossed apple peeking around its torn edges.

This doodle is on JEFFREY'S green laptop ONLY — NEVER on a pixsie's laptop (pixsie lids carry the PALS glyph ONLY). The lid is glossy plastic that reflects ambient light.`;

const WORLD_LAW =
`WORLD LAW — the SETTING varies by beat:
• EARTH PANELS (overture-*, statement-a, statement-b) = jeffrey's felt-crafted studio zollo: warm desk-lamp pool, WOODEN-PLANK floor, plant, AC poster, a real-photo night sky outside the window. the studio is felt, the night sky outside is photo-real. Camera = STANDARD lens, normal perspective.
• STUDIO-HOLE PANELS (statement-c, bridge-a, bridge-b, bridge-c) = the SAME felt-crafted studio zollo, but now with a PERFECT ROUND HOLE burned cleanly through the wooden floor — circular cut, edges glowing ember-red where the laser bit through, charred + smoking wood at the rim, heat-shimmer rolling up out of it. DOWN INSIDE the hole: HELL clearly visible far below — sinusoidal LAVA RIVERS, basalt, bruise-purple sky — miles down yet RIGHT THERE through the disc of floor. warm orange hell-glow paints the studio from below; cool desk-lamp + cold drone targeting beams paint from above. soap bubbles drifting up out of the hole. Camera = STANDARD lens.
• TRANSITION (bridge-d) = mid-fall THROUGH the hole — the studio doorway + ceiling shrinking away above, basalt + lava rushing up below, the felt crew cannonballing as one cluster. NO long cosmic chute. Camera = HANDHELD WIDE-ANGLE looking up.

★ HELLSCAPE WORLDLY SHIFT (develop-*, climax-*, coda-*) — once in hell, the LENS + CAMERA STYLE shifts dramatically to a FISH-EYE FIRST-PERSON GO-PRO / SKATE-VIDEO HANDHELD aesthetic. Strong wide-angle FISH-EYE LENS DISTORTION at the frame edges (vertical lines bow outward, horizon curves), generous DUTCH TILT on the horizon, low POV often near ground level or chest height as if the camera-operator is INSIDE the action with a handheld mini-cam, occasionally a felt hand reaches into the foreground holding the cam or a selfie pole. Shot styling = the "Spike Jonze + Ty Evans Yeah Right! skate doc + Jackass home-video" feel — energetic close-quarters chaos, action filling the FORE + MID grounds, peer-horizontal subjects often closer than they "should" be. NO motion blur and NO out-of-focus language — convey energy through pose + composition + lens distortion only. The fish-eye PUSHES the SINE-WAVE WORLD into more dimensional 3D — basalt valleys curl, sine lava rivers wrap around the lens, flame tongues lean toward the camera, the bruise-purple sky bowls overhead, obsidian spires lean inward. The world reads more SCULPTED + DIMENSIONAL than the flat earth shots — every hellscape beat should feel like a chunk of carved sine-volume you're standing inside of.

• HELLSCAPE PANELS = real-photo alien volcanic landscape, MAX SINE EVERYTHING — cracked CHARRED basalt foreground crusted with soot + sulphur, MULTIPLE PARALLEL sinusoidal LAVA RIVERS curving across the ground at varied amplitudes (every river is a perfect math sine wave, NEVER straight), tall walls of sine-shaped FLAME licking up, jagged obsidian spires receding into a bruise-purple horizon (the spires themselves carved into sine-curves), dense atmospheric particulate (orange embers + sparks, ash flakes, soap bubbles, smoke columns, multi-hue matrix-rain cascading through some smoke columns, faint glowing wireframe-grid + circuit-trace lines in basalt cracks, a few floating holographic HUDs), heat-shimmer above the lava. A SINGLE PURE-WHITE HORSE flames across the mid-distance basalt with red flames licking its hooves + body. A SLEEK BULLET TRAIN streaks the far horizon with lit-up windows + motion-streaked tracks. THE SINES ARE THE FIRE — every lava river + flame tongue traces a perfect sine wave. NO sine beams from anyone's mouth. NOTE: develop-a + develop-b feature the crew IN A LAVA POOL — the lava acts as a warm magical liquid the crew can splash into + swim in without being hurt; felt darkens at the waterline + chars but bodies stay buoyant + joyful.

★ COOLING HELL (coda-* only) — the hellscape begins COOLING DOWN in the final beats: lava rivers crusting over with darker obsidian skin, the bruise-purple sky shifting to a COLD STEEL-BLUE dawn instead of warm coral, SNOWFLAKES BEGINNING TO FALL from a thinning ashy sky and SETTLING on basalt + on the felt crew's shoulders + hair, surviving lava patches glowing dim red through frost. The "magma planet" gives way to a frostbitten ash-volcanic plain. Temperature drops sharply between climax-c → coda-a → coda-b. Coda-b is fully WINTER hell.`;

const ARCS =
`CONTINUITY ARCS across the whole 18-panel set (the beat description below names this beat's stage):
• EYES: dim (clean dark felt) → shock-wide at the strike → wide-awe peering into the hole → bright-call mid-facetime → bright-greeting at the bust-in → grouped-wonder around the hole → mid-whoop cannonballing → mid-whoop splashing → laughing while swimming → IGNITION (fire ignites in sockets in a chain) → blazing red-orange flames in sockets (no whites) for the party → embers (soft glow, no flames) at dawn.
• FELT: clean through overture → clean through statement (the strike is at the room, not yet at his clothes) → clean as he peers in the hole → clean through facetime + bust-in + cannonball → wet + darkening at the waterline on splashdown → wet + first chars from swimming → first scorch marks + frays at ignition → fully tattered + char-marked at the party → tattered but settled + dry by dawn.
• PALS LIDS: jeffrey's MacBook Neo BUTTERFLY-scrap visible in overture, statement, coda; pixsie laptops APPEAR for the first time in bridge-b (each pixsie carries one when bursting in) but PALS are DARK until ignition; PALS FLICKER on during the swim (one or two glowing) → full seven-hue glow at the party → soft pulse at dawn.
• FINGERTIPS: clean through earth + studio-hole + cannonball + splashdown + swim → tiny live flames singeing felt fibres at tips + scorch marks + glowing ember cracks on palms at ignition + party → embers at dawn.`;

const PALETTE =
`PALETTE — for earth panels: warm amber desk-lamp pool against cool blue night, the felt blues of jeffrey's shirt + trousers, soft browns; for chute panels: cool deep-blue chute with multi-hue matrix-rain streaks (yellow / red / purple / lime); for hellscape panels: dominant warm lava-orange + red as the principal light, deep crimson sky, bruise-purple horizon, obsidian black + sulphur yellow + soot, embers + ash, intermixed with the multi-hue matrix-techno layer (yellow / red / purple / lime / amber / magenta), PALS-lid hues (cyan / magenta / lime / hot-pink / gold / orange / violet) as the secondary punctuation light.`;

const DEVICE_ENGAGEMENT =
`DEVICE ENGAGEMENT — whenever jeffrey or any pixsie is holding a laptop, phone, or other device, the figure's HEAD AND GAZE MUST BE ON THE SCREEN (eyes down/toward the display, head tilted into the work, body posture absorbed in using the thing). NEVER the presentational "showing the laptop to the camera" pose where the device is turned sideways so its screen / lid faces the lens. The screen faces the user, not the viewer. If a lid (back of the screen) is in frame, it's because the figure is using the device naturally — not because they're modelling it. Two examples to copy from: someone hunched over a keyboard reading the terminal; someone scrolling a phone with thumb mid-swipe and eyes on the glass. Two examples to AVOID: someone holding a laptop OUT toward the camera with both hands; someone tilting a phone toward the lens to show what's on it.`;

const AVOID =
`AVOID — any cartoon / plush / collaged look (this is refined felt-craft realism); apple logos or butterfly glyphs on pixsie lids (PALS only on pixsie lids, butterfly only on jeffrey's MacBook Neo lid scrap); SINE BEAMS POURING FROM ANYONE'S MOUTH (the sines ARE the lava + fire itself, never an oral effect); blood / gore / body horror (the heat damage is real felt damage — fraying, char, scorch — not gore); any readable text, wordmark, or logo anywhere; pointed / elf / fae ears on pixsies (rounded human ears only); jeffrey centred as a hero (peer-horizontal); modern flat tech-illustration; recursive screens showing the surrounding scene; living-artist names; motion blur language (convey motion through pose only); presentational "showing the laptop to camera" poses (see DEVICE ENGAGEMENT — screen faces the user, not the viewer).`;

// ── per-beat story — 18 panels ───────────────────────────────────────
// Keys must match the SECTION_ORDER list below. Order matches
// hellsine.story.txt verbatim. Each variant adds beat-specific
// composition + emotion + arc-stage to the shared law above.

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
`BEAT — OVERTURE A "before the keystroke" · EMOTION: focused calm, the hum before everything. ARC STAGE — eyes dim (no fire), felt clean, NO pixsies present, PALS lids dark. EARTH. studio zollo, late.

★ LAPTOP STATE (mandatory): The MacBook Neo is CLOSED — lid flush down on top of the keyboard base. We see the GREEN OUTSIDE-OF-LID facing UP toward camera, the TORN WHITE-PAPER SCRAP with the WHISTLEGRAPH BUTTERFLY DOODLE taped in the centre of that green top panel (where the apple logo would be). NO LCD visible anywhere, no screen content, no screen glow — the laptop is shut. The butterfly scrap ONLY ever lives on the OUTSIDE-OF-LID, never on the LCD side and never on the base; the laptop being closed makes this unambiguous.

CAMERA: positioned OUTSIDE the window LOOKING IN through the photo-real night-time glass. We see felt-puppet jeffrey from OUTSIDE in three-quarter profile: he's seated at his desk inside the warmly-lit studio, his TORSO + ONE SHOULDER + the side of his face turned slightly toward camera; one felt hand resting gently ON TOP of the closed laptop's green lid, the other hand reaching for the small toy bunny / honey jar / pennies on the desk (he hasn't started typing yet — this is the quiet moment before he opens it). Headphones half-on around his neck/ears.

He wears the canonical pale-baby-blue felt BUTTON-DOWN with darker-blue PINSTRIPES + yellow felt BEAR EMBLEM at the chest + yellow Sailor Pro Gear PEN clipped at the pocket + RED PLASTIC GLASSES dangling at the placket + wide-leg medium-cobalt felt TROUSERS + dark slate-grey HEADPHONES half-on. The button-down + pinstripes + pen + glasses are all clearly visible. NOT a sweater, NOT a cable-knit, NOT a hoodie.

Through the window we read into the felt studio: warm desk-lamp pool, wooden floor, a plant, an AC poster on the wall. Tiny reflections of the night sky on the window glass overlay the scene. On the desk: the CLOSED green MacBook Neo with butterfly scrap face-up, a small toy felt BUNNY plushie, a small glass jar of golden HONEY with a wooden dipper, and a small stack of pennies (the MONEY) — the song's three lyric talismans. Camera: EXTERIOR vantage looking IN through a single windowpane, framed so the warm interior is the bright centre and a sliver of cool black night is around the edges.`,

  "overture-b":
`BEAT — OVERTURE B "the blinking light" · EMOTION: stillness, the moment before everything. ARC STAGE — eyes dim, felt clean, NO pixsies, PALS dark. EARTH.

★ LAPTOP STATE (mandatory): The MacBook Neo is OPEN now, on the desk. Its LCD SCREEN faces JEFFREY (toward him, AWAY from camera). Camera ANGLE is positioned roughly opposite jeffrey's face (we see HIS FACE, not his back this time) — but the laptop lid is between camera and the LCD, so the LCD ITSELF IS NEVER VISIBLE to camera (we see only the TOP EDGE / HINGE / BACK OF THE LID from this angle, with the WHISTLEGRAPH BUTTERFLY DOODLE scrap visible on the LID-BACK turned toward us). NO LCD pixels, no screen content, no readable text — but the WARM SCREEN GLOW spills outward onto jeffrey's face from below his chin, illuminating his cheeks + brow + the underside of his felt hair with a soft cool-white screen light, eyes lowered to the (unseen) screen. THE BUTTERFLY IS ONLY ON THE LID-BACK, NEVER ON THE LCD.

CAMERA: still OUTSIDE the window LOOKING IN, now turned to a more frontal three-quarter angle so we see JEFFREY'S FACE softly lit by the screen-glow from below; his felt features readable + thoughtful. The laptop is angled in the foreground between us and him, lid-back with butterfly toward us, screen invisible. Mood: the hum before everything.

He wears the canonical pale-baby-blue felt BUTTON-DOWN with darker-blue PINSTRIPES + yellow felt BEAR EMBLEM + yellow Sailor Pro Gear PEN at the pocket + RED PLASTIC GLASSES at the placket + cobalt felt trousers + dark slate-grey HEADPHONES half-on. All gear clearly visible. NOT a sweater, NOT a cable-knit, NOT a hoodie.

In the windowpane reflection (or visible through the glass behind jeffrey), the otherwise-still black night sky carries ONE tiny RED LIGHT blinking once, far away in the distance — too small for him to notice. Camera: EXTERIOR three-quarter looking IN through the glass at jeffrey's face, screen-glow under his chin, the red blink small in the dark sky beyond his shoulder.`,

  "overture-c":
`BEAT — OVERTURE C "the swarm forms" · EMOTION: oblivious — the last calm moment. ARC STAGE — eyes dim, felt clean, NO pixsies, PALS dark. EARTH. INTERIOR THIS TIME — we are inside the studio looking ACROSS the room from a low angle. the SKY OUTSIDE the window is now alive: the one red blink has become a dozen, then dozens of drones forming a moving constellation against the black, faint neon-orange targeting beams sweeping across the studio glass from outside, still soundless. jeffrey is at his desk in three-quarter profile still focused on his MacBook Neo, headphones on, oblivious. THE LAPTOP SCREEN STAYS HIDDEN — angled AWAY from camera or otherwise out of view; do NOT show the screen's content (no kidlisp, no terminal, no text). only the lid carries the canonical WHISTLEGRAPH BUTTERFLY DOODLE on the white-paper scrap. warm desk-lamp glow on his face now competing with cold orange beams crossing his shoulder from the window. on the desk: the small toy BUNNY plushie, the jar of HONEY, the stack of pennies (MONEY) all still in their quiet still-life positions. camera: across the room at low angle, jeffrey + the lit-up sky-through-the-window both readable, screen content NOT visible.`,

  "statement-a":
`BEAT — STATEMENT A "the swarm" · EMOTION: SHOCK — caught mid-recoil, no understanding yet. ARC STAGE — eyes WIDE in shock (still no fire), felt about to tear, NO pixsies, PALS dark. EARTH. the WINDOW EXPLODES INWARD — felt-and-glass shards mid-flight, slow-motion. a SWARM of black military drones outside, neon-orange targeting lasers crisscrossing the studio, ONE drone right at the broken pane staring in with a single red lens-eye. jeffrey recoils, BOTH PALMS UP defensive, his felt face caught mid-shock — brow up, mouth open, NO grin, NO fire-eyes. camera: WIDE, drones swarming on one side of the frame, jeffrey centred, glass mid-air.`,

  "statement-b":
`BEAT — STATEMENT B "the floor laser" · EMOTION: disbelief — watching the floor get carved. ARC STAGE — eyes shock-wide, felt still clean, NO pixsies, PALS dark, NO fire on him. EARTH. ONE lead drone hovers low in the middle of the studio, its barrel angled STRAIGHT DOWN. it fires a CLEAN PILLAR of cutting laser light — saturated red-orange — straight into the wooden plank floor between jeffrey's feet. the beam is a perfect vertical column, carving a slow circle through the planks; sparks + curling smoke + flying splinters fan out around the cut. jeffrey is leapt back against the desk, arms shielding his face, watching the floor get carved. NO fire-eyes yet — just wide shock. camera: LOW across the floor, the laser column dominating centre frame, jeffrey braced against the desk on the right.`,

  "statement-c":
`BEAT — STATEMENT C "the hole to hell" · EMOTION: awe displacing fear — the first faint curl of a grin. ARC STAGE — eyes wide-awe, felt clean, NO pixsies, NO fire, NO PALS. STUDIO-HOLE. the circle of floor has DROPPED AWAY. a PERFECT ROUND HOLE now opens in the wooden planks, edges glowing ember-red where the laser bit through, charred wood smoking. THROUGH the hole: HELL plainly visible — basalt + sinusoidal lava rivers + bruise-purple sky, miles below and yet right there, the heat already rolling up into the studio. jeffrey is on his knees at the rim, PEERING IN, hair lifting in the updraft, the warm orange glow lighting his face from below for the first time. felt face: awe, no fear, the first faint curl of a grin. drones still buzz overhead but the room has gone quiet around the hole. camera: HIGH THREE-QUARTER on the hole, jeffrey at the rim, the basalt hellscape clearly readable down inside the circle.`,

  "bridge-a":
`BEAT — BRIDGE A "videocall the squad" · EMOTION: urgent excitement — calling friends to come over. ARC STAGE — eyes bright (no fire), felt clean, pixsies APPEAR for the first time but only as glow on jeffrey's face from his LAPTOP screen (not yet in the room), PALS not yet relevant. STUDIO-HOLE. jeffrey kneeling on the studio floor next to the glowing round hole, the hellfire glow under-lighting him from below, the cool desk-lamp + cold drone targeting beams from above — a two-tone light bath.

★ DEVICE (mandatory): jeffrey is NOT holding a phone. He is HOLDING HIS OPEN CITRUS-GREEN MACBOOK NEO IN ONE HAND like a tray / oversized tablet — palm flat under the base, the lid open at ~110° standing up off his palm, screen facing TOWARD HIS FACE so he can see the video call. The LID-BACK (with the WHISTLEGRAPH BUTTERFLY scrap taped on its centre) is what faces the CAMERA. The LCD screen content (a multi-pane FACETIME-style GRID of 4-6 pixsie faces in their own warm-lit tiles — kid pixsie in lime PJs, elder pixsie in a cardigan, femme grad-student pixsie at her own kitchen counter, tactical-vest pixsie in a parked car, hot-pink-hair pixsie in a felt hoodie) is angled AWAY from camera behind the lid edge — we don't see the screen content directly, only the SCREEN-GLOW spilling onto jeffrey's face from the laptop's screen side, suggesting the call is in progress. NO PHONE anywhere in the frame.

jeffrey is mid-SHOUT into the laptop, mouth open, eyes wide. His FREE HAND (the one NOT holding the laptop) is pointing DOWN at the glowing floor hole. Peer-horizontal energy — he's not commanding, he's calling friends in.

He wears the canonical pale-baby-blue felt BUTTON-DOWN with darker-blue PINSTRIPES + yellow felt BEAR EMBLEM + yellow Sailor Pro Gear PEN at the pocket + RED PLASTIC GLASSES at the placket + cobalt felt trousers. All gear clearly visible. NOT a sweater, NOT a cable-knit, NOT a hoodie.

Camera: medium three-quarter on jeffrey from his side — the green laptop LID-BACK with butterfly scrap turned toward us, jeffrey's face lit by the screen-glow on the far side of the lid, the glowing round floor hole visible behind/beside him.`,

  "bridge-b":
`BEAT — BRIDGE B "the squad busts in" · EMOTION: arrival energy — wide felt grins. ARC STAGE — eyes bright (no fire yet), felt clean, pixsies APPEAR in the room for the first time, each pixsie now carrying ONE AC laptop (PALS lids DARK), jeffrey also still has his MacBook Neo on the desk behind him. STUDIO-HOLE. the STUDIO DOOR explodes open — door slamming back on its hinges, splinters flying. 4-6 felt pixsies pile through the doorway in a jumbled wave, mid-stride, each carrying their own glossy plastic AC laptop in one hand with the PALS lid showing dark (not yet glowing). outfits: kid pixsie in lime PJs + sneakers, elder pixsie in a cardigan with cane raised, femme grad-student in cyberpunk techwear, tactical-vest pixsie in boots, hot-pink-hair pixsie in a felt hoodie, beanie pixsie clutching a felt coffee. they arrive READY — wide bright grins, eager eyes, peer-horizontal stack in the doorway. jeffrey is on his feet now, half-turned toward them, free arm thrown UP in greeting, the glowing floor hole still visible beside him. drones forgotten. camera: LOW + WIDE from inside the studio, the doorway bursting open on the left, jeffrey + the glowing hole on the right.`,

  "bridge-c":
`BEAT — BRIDGE C "around the hole" · EMOTION: shared awe, shared grin — the warmth on every face. ARC STAGE — eyes bright-wonder (no fire yet), felt clean, full crew present and ringed around the hole, PALS lids DARK but warm orange glow already painting every face from below. STUDIO-HOLE. the whole crew now ringed around the glowing round floor hole, peer-horizontal CIRCLE, jeffrey one member among them (NOT centred). they're leaning in, peering DOWN into hell — warm lava glow hitting every face from below, fingertips on the rim of the cut planks. one pixsie holds her AC laptop OUT over the hole letting the (still dark) PALS lid catch the orange light, one pixsie crouches with hands on knees grinning, the kid pixsie kneels rim-side eyes huge, the elder grips her cane. jeffrey is roughly opposite the kid, hand out as if counting it off. expressions: shared grins, shared awe, the room is hot. STILL no fire-eyes — but the warm glow already paints them all. camera: HIGH THREE-QUARTER on the hole, the ringed crew visible all the way around it.`,

  "bridge-d":
`BEAT — BRIDGE D "all in" · EMOTION: peer-horizontal cannonball — joy + commitment, no fear. ARC STAGE — eyes bright-wide (no fire yet), felt clean, the whole crew mid-air. TRANSITION (mid-fall through the hole).

★ JEFFREY'S MACBOOK NEO IS ABSENT FROM THIS PANEL — it's not in his hands, not on his back, not spinning in the air. He left it behind in the studio. The pixsies are NOT carrying laptops either in this cannonball frame — hands are free for the leap. Render NO laptops anywhere in the frame.

CANNONBALL — the entire crew launching INTO the hole together in one frozen instant mid-leap: feet off the floor, knees pulled up, arms thrown around each other's shoulders, hands FREE. jeffrey is ONE BODY in the group cluster, somewhere in the middle, NOT centred. hair lifted, eyes bright but still felt-clean — no fire yet. behind/above them the ruined studio doorway + ceiling recedes; below them the basalt + lava rivers already rushes up to meet them. peer-horizontal mid-air. soap bubbles beginning to rise from below past them. camera: FROM BELOW INSIDE THE HOLE looking UP at the falling cluster against the studio doorway light, hell-glow on undersides, the round disc of studio ceiling shrinking above.`,

  "develop-a":
`BEAT — DEVELOP A "splashdown" · EMOTION: shared whoop, caught between gasp + grin. ARC STAGE — eyes wide-mid-whoop (no fire yet), felt darkening at the waterline, pixsies present, fingertips clean. HELLSCAPE LAVA POOL.

★ NO LAPTOPS IN THIS PANEL — jeffrey's MacBook Neo is NOT splashing down beside him. The pixsies aren't holding laptops either. Hands are free for the splash. Render NO laptops anywhere in the frame.

IMPACT into a wide LAVA POOL. the crew hits the sinusoidal lava river in a huge spray — felt-textured splashes of glowing orange-red lava arcing up in fat droplets, slow-motion sheets cresting around each body. waves rolling outward in concentric rings. jeffrey + pixsies frozen mid-plunge, half-submerged at varied depths, expressions caught between gasp and grin — eyes wide, mouths OPEN in a shared whoop. NO fire-eyes yet — but the lava is up to their chests, lighting every felt face from within the pool. camera: LOW across the lava surface, multiple splashes filling the frame, basalt banks visible on the edges, the round studio-hole far above lost in the upward glare.`,

  "develop-b":
`BEAT — DEVELOP B "swim in lava" · EMOTION: pure pool-day joy in hell. ARC STAGE — eyes laughing (no fire yet), felt wet + first chars at the waterline, pixsies in the lava pool with jeffrey, fingertips clean. HELLSCAPE LAVA POOL.

★ NO LAPTOPS IN THIS PANEL — jeffrey has NO laptop floating beside him. The pixsies have no laptops either. They're swimming with FREE HANDS — the laptops are gone. Render NO laptops anywhere in the frame.

the crew floats + strokes through the LAVA RIVER like it's a warm pool — the lava holds them up the way water would. ONE pixsie on her back floating with arms behind her head, eyes closed grinning. ONE pixsie doing a slow felt-arm backstroke, trailing a sine-shaped wake. the KID pixsie cannonball-bobs in the middle splashing the ELDER, who is splashing back. JEFFREY is treading lava beside another pixsie laughing, hands paddling the lava. tongues of sine-flame lick off the surface between them. the lava is their water. felt outfits darkening at the waterline + starting to char. wicked grins arriving but eyes still felt-clean. camera: LOW along the lava surface, swimming bodies arrayed peer-horizontally across the pool, sine ripples curving past them, basalt banks at the edges.`,

  "develop-c":
`BEAT — DEVELOP C "ignition" · EMOTION: wickedness arriving — wide-eyed grins forming, not full demon yet. ARC STAGE — FIRE IGNITES in eye sockets in a chain (jeffrey first, then each pixsie), FINGERTIPS BEGINNING TO SINGE with tiny live flames, first scorch marks + frays. HELLSCAPE LAVA BANK.

★ NO LAPTOPS IN THIS PANEL — the crew is rising from the lava with FREE HANDS, no laptops in sight. Jeffrey's MacBook Neo is gone. The pixsies' AC laptops are gone. Render NO laptops anywhere in the frame. (The PALS lids will reappear in climax.)

the crew now CLIMBING OUT + RISING UP from the lava onto the basalt bank — a glowing orange water-line of lava drips off their felt as they emerge. AT THIS MOMENT the FIRE IGNITES — first in jeffrey's eye sockets (one socket, then the other) and then in a CHAIN around the crew, each pixsie's eyes lighting in sequence as they surface. fingertips beginning to singe, tiny flames flickering off felt fibres at the tips. wide-eyed grins forming — not full demon yet, but the wickedness is arriving. felt damage taking root: first threads pulling loose, first scorch marks. camera: ALONG THE BASALT BANK looking down the line of rising bodies, the chain of ignition catchable across the frame, the lava pool steaming behind them.`,

  "climax-a":
`BEAT — CLIMAX A "the cover" (verbatim cover crop) · EMOTION: peak chaos, peak joy. ARC STAGE — eyes BLAZING with full live flames in sockets (no whites), all PALS lids full seven-hue glow, fingertips on fire with tiny flames + scorch marks + glowing ember cracks, felt TATTERED + char-marked. THE COVER. the smooshed-into-lens wide-angle group portrait from hellsine.illy.txt — jeffrey + pixsies dancing ON / OVER / AROUND active fire, BOTH PALMS UP at the lens, fire-eyes BLAZING, sinusoidal lava ribbons weaving between feet, sine-flame tongues dancing between faces, PALS lids glowing cyan / magenta / lime / hot-pink / gold / orange / violet, white horse + bullet train on the back horizon, multi-hue code-rain through smoke columns, soap bubbles rising, fingertip flames + scorched palms, tattered felt. peak chaos, peak joy, the PARTY. camera: THE COVER CROP VERBATIM — edge-to-edge, jeffrey at about 40% from left, smooshed into the lens.`,

  "climax-b":
`BEAT — CLIMAX B "the party is threatened" · EMOTION: peak joyful celebration disrupted — a sudden intrusion the crew is already pushing back against, FUN-CHAOTIC not scared. ARC STAGE — eyes blazing, PALS lids full seven-hue, fingertips bright, felt joyfully tattered.

★ INTRUDER (mandatory): a SMALL IMPISH ROBOT DRONE has crashed the hellsine party — a chrome-and-glitch IMP MACHINE about the size of a beach ball, with: a leering robotic face (LED-glowing red eyes + a metal teeth grin), spindly insect-leg attack appendages clutching glowing cyber-spears, datamosh / CRT-glitch artifacts rippling visibly across its chassis (pixel-tearing, scan-lines, RGB-channel splits, jpeg-block ghosting), and a single trailing exhaust contrail. It's mid-air swooping over the party from upper-right, mouth open in a tinny screech. ONE pixsie security team member is mid-action LEAPING UP TO BAT IT AWAY with a sine-shaped lava blade / felt slingshot. ANOTHER pixsie hurls a glowing PALS-lid disc at it like a frisbee weapon. JEFFREY in the middle of the frame is still mid-dance but glancing UP at the imp with a wicked grin (the party doesn't stop), one fire-eye half-tracked on the threat. The OTHER pixsies keep dancing, mostly oblivious — the security team has it handled.

WORLDLY-SHIFT FISH-EYE POV (inherits HELLSCAPE STYLE from WORLD LAW): strong wide-angle fish-eye distortion at edges, Dutch tilt, low chest-height POV like a Spike-Jonze skate-doc handheld cam right inside the dancing crew. The lens curve bows everything inward — sine lava rivers wrap around the lens, obsidian spires lean in, the imp drone reads BIG due to wide-angle proximity.

The pixsies' raised PALS laptops glow cyan / magenta / lime / hot-pink / gold / orange / violet scattered across the frame. lava sines weaving gently between feet. peer-horizontal — jeffrey one member of the crew, NO ONE centred. Camera: low + inside the dancing circle, fish-eye POV.`,

  "climax-c":
`BEAT — CLIMAX C "the wide vista" · EMOTION: a vast warm chaos with the crew at its heart. ARC STAGE — eyes blazing, PALS full, fingertip flames, felt tattered — but at scale now. PULL BACK to the WIDEST shot of the track — the dancing group is now SMALL in the lower-third of the frame, the FULL hellsine vista revealed around them: parallel sinusoidal lava rivers curving through the basalt foreground at varied amplitudes, obsidian spires receding into bruise-purple horizon, the white horse flaming across the mid-distance, the bullet train streaking the far horizon, multi-hue matrix-rain streaming through smoke columns on either side, soap bubbles + embers everywhere. the dancing crew is the warm pulsing nucleus inside a vast lava world. camera: VERY WIDE, low, the group anchored lower-centre, sky + vista filling the upper two-thirds.`,

  "coda-a":
`BEAT — CODA A "snow on the lava" · EMOTION: chilled-out afterglow with a cold edge — hell is cooling. ARC STAGE — eyes now just EMBERS (soft orange glow, no flames), felt fully tattered + scorched + DUSTED WITH SNOW, fingertips embers (no flames), PALS lids dimmed to a soft pulse.

★ COOLING HELLSCAPE (mandatory): the hellscape is COOLING DOWN. The sky is now COLD STEEL-BLUE dawn instead of warm coral. Lava rivers are CRUSTING OVER with darker obsidian skin (the sine wave still readable but now half-frozen). SNOWFLAKES FALL gently from a thinning ashy sky and settle on the basalt + on the felt crew's shoulders + hair. Surviving lava patches glow dim red through frost. Smoke columns thin and white instead of black. The "magma planet" is becoming a frostbitten ash-volcanic plain.

WORLDLY-SHIFT FISH-EYE POV (inherits HELLSCAPE STYLE from WORLD LAW): strong wide-angle fish-eye distortion at edges, Dutch tilt, low POV like a handheld skate-doc cam.

The crew is JUST CHILLING across the basalt — sat on now-warm-but-cooling rocks, leaning back on elbows with snow on their shoulders, one stretched out flat looking up at the steel-blue sky (snowflakes settling on her face), one elder warming hands on a dim ember + watching snow fall, KID pixsie curled up dozing against an obsidian wedge with a felt scarf of snow across her shoulder. Jeffrey reclines against a warm rock among them (NOT centred), snowflakes on his hair + shoulders, fire-eyes dim. Spent and content, but now sharing warmth as the heat dies. nobody is posing; everyone is at rest. Camera: wide fish-eye, the chilled-out crew arranged peer-horizontally across the cooling basalt, steel-blue snowy dawn filling the upper half.`,

  "coda-b":
`BEAT — CODA B "back through the portal" · EMOTION: quiet exhausted relief — home. ARC STAGE — eyes back to normal felt (NO fire, NO embers) but with a faint warm reflection caught in them from the closing portal, felt fully tattered + char-scorched, snowflakes settled on shoulders + hair, PALS not in frame.

★ MAJOR RESTAGE — JEFFREY HAS RETURNED TO HIS STUDIO. He has just stepped BACK through the round portal hole from hell into his felt-crafted studio zollo. He is ALONE now (the pixsies stayed in hell). He stands or sits softly in his studio, exhausted-but-home, the portal closing behind him.

★ THE PORTAL: behind/beside jeffrey, the SAME ROUND HOLE in the wooden plank floor that was burned by the laser at the start — but now it is CLOSING UP, the ember-red edges shrinking as the wood seals itself, only a faint warm afterglow + a few last sparks rising. A few SNOWFLAKES drift up FROM the hole + a few EMBERS drift up too — both hell-snow and hell-embers leaking back into the studio in the moment before the portal seals.

★ JEFFREY'S APPEARANCE — singed + snowy + alive:
  · His pale-blue button-down with pinstripes is RAGGED + CHAR-SCORCHED at the hem + cuffs + collar (real felt damage), with a small frayed hole on one shoulder.
  · The yellow Sailor Pro Gear pen + red plastic glasses are still in place at the pocket / placket.
  · SNOWFLAKES are visibly settled on his SHOULDERS, on his BROWN FELT HAIR, on his EYEBROWS, slowly melting in the warm studio air. A few flakes drift down past him.
  · A small streak of soot across one cheekbone, a smudge of grey ash on his trouser knee.
  · His face wears a CALM relieved smile (NOT a wicked grin) — eyes back to normal felt, glistening.
  · He carries his CITRUS-GREEN MACBOOK NEO under one arm — closed, butterfly scrap visible on the lid, slightly scorched but intact.

★ THE STUDIO BEHIND HIM (back-to-earth): warm desk-lamp pool glowing on the desk, the AC poster on the wall, the plant, the wooden plank floor — exactly the studio from overture-a. The desk still has the toy felt BUNNY plushie, the jar of HONEY, the small stack of PENNIES (MONEY) — all untouched, waiting for him. Night sky out the broken-and-now-felt-patched window. NO drones, NO pixsies, NO hell — just home. Snowflakes drifting in the air around him as the portal seals.

Camera: STANDARD lens (the world-shift returns to earth perspective — no more fish-eye), three-quarter medium shot with jeffrey foreground + the closing round portal-hole behind him slightly to one side, the warm studio interior framing the scene. NO motion blur. No pixsies in frame. The story comes full-circle: back to his desk, alive, changed by hell.`,
};

// ── prompt construction ──────────────────────────────────────────────
function build(sectionBeat) {
  const orient = LANDSCAPE ? `\n\n${LANDSCAPE_NOTE}` : `\n\n${PORTRAIT_NOTE}`;
  return [MEDIUM, JEFFREY, PIXSIES, JEFFREY_LAPTOP, DEVICE_ENGAGEMENT, WORLD_LAW, ARCS, sectionBeat, PALETTE, AVOID].join("\n\n")
    + orient + "\n";
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
    out: `${LANE}/out/hellsine${TAG}-sec-${pad(i)}-${name}.png`,
    label: `hellsine${TAG} §${pad(i)} ${name}`,
  });
}

progress.begin({ type: "illy", label: `hellsine${TAG} · ${jobs.length} panels` });
let done = 0;
for (const job of jobs) {
  await generate(job.prompt, job.out, job.label);
  progress.update((++done / jobs.length) * 100, { done, total: jobs.length });
}
progress.end();
console.log(`\n✓ hellsine storyline panel set — ${jobs.length} job(s) · ${LANDSCAPE ? "landscape (1536x1024)" : "portrait (1024x1536)"}`);
