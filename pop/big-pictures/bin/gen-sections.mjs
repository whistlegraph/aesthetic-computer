#!/usr/bin/env node
// big-pictures/bin/gen-sections.mjs — generate the amaythingra STORYLINE
// panel set: a portrait (9:16) cover + 16 per-beat illustrations (TWO per
// musical section, so the visualizer changes ~every 16 s) tracing one
// little story:
//
//   jeffrey is in a drab tech OFFICE quietly staring at a woman in grey;
//   MARK ZUCKERBERG onboards him into a metaverse game; the office gets
//   WARPED + REPLICATED inside it (the one woman-in-grey becomes an army
//   of grey selfie-girl avatars, the surfboard on the office wall becomes
//   his wakeboard). Out on the Meta-blue lagoon DRONE-WARS erupt on the
//   horizon; Mark's avatar tries to drag him back (a tug-of-war); jeffrey
//   plows the oblivious girls over (none react), then finds the warm
//   GROUP PIXSIES round-table — the human antidote to the hollow crowd —
//   before the world dissolves and he's spat BACK INTO THE OFFICE,
//   discombobulated, seeing the woman in grey anew.
//
// office → metaverse → office, a closed loop, 8 sections × 2 beats = 16.
//
// IMAGE STYLE + MOOD — lifted from the locked v2 cover prompt
//   (~/Documents/Shelf/gens/amaythingra-cover/cover-prompt.v2.txt):
//   a glossy "metaverse avatar / Horizon Worlds" render that is
//   deliberately DESATURATED, GREY-toned, overcast, washed-out, cold and
//   joyless — NOT vivid, NOT a bright cheerful cartoon. Deadpan,
//   satirical, crisp (no motion blur). NO laptop, NO matrix code.
//
// Output: pop/big-pictures/out/amaythingra-p-cover.png             (hero)
//         pop/big-pictures/out/amaythingra-p-sec-<i>-<name>-<a|b>.png (16)
// Landscape (--landscape): amaythingra-yt-* at 1536x1024.
//
// Usage:
//   node pop/big-pictures/bin/gen-sections.mjs                 # cached
//   node pop/big-pictures/bin/gen-sections.mjs --force         # regen all
//   node pop/big-pictures/bin/gen-sections.mjs --only vortex   # both halves
//   node pop/big-pictures/bin/gen-sections.mjs --only vortex-b # one half
//   node pop/big-pictures/bin/gen-sections.mjs --landscape     # -yt set

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
const SIZE  = LANDSCAPE ? "1536x1024" : "1024x1536";
const TAG   = LANDSCAPE ? "-yt" : "-p";
const SLUG  = "amaythingra";
mkdirSync(`${LANE}/out`, { recursive: true });

// The whole metaverse keys to ONE colour — Meta's brand blue. Threaded
// into every block so the virtual world reads as corporate Meta blue
// (the water, the glitch, everything), never polychrome / rainbow.
const META_BLUE = "#0668E1";   // Meta brand blue (RGB 6,104,225)

// ── identity refs (mirrors marimba/bin/gen-sections.mjs) ─────────────
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

// ── PORTRAIT / LANDSCAPE recomposition ───────────────────────────────
const PORTRAIT_NOTE =
`FRAME — compose for a TALL vertical 9:16 frame. HONOR THE SHOT DIRECTIVE in this beat exactly: a close-up / extreme close-up FILLS the frame with the face or detail (crop in tight — do NOT pull back); a first-person POV shows the scene through jeffrey's eyes; a wide shot lets the whole setting breathe. Keep a small calmer margin at the very top and very bottom for a wordmark + player UI. keep the established per-mode render style and the crisp NO-blur look.`;
const LANDSCAPE_NOTE =
`FRAME — compose for a WIDE 3:2 / 16:9 frame. HONOR THE SHOT DIRECTIVE in this beat exactly: a close-up / extreme close-up FILLS the frame (crop in tight — do NOT pull back); a first-person POV shows the scene through jeffrey's eyes; a wide shot lets the whole setting stretch horizontally. Leave the upper third a little calmer for chrome. keep the established per-mode render style and the crisp NO-blur look.`;

// ── shared scene law — STYLE + MOOD from cover-prompt.v2.txt ─────────
const MEDIUM =
`A COMPUTER-ANIMATED METAVERSE SCENE — a glossy real-time 3D game-engine render in the "metaverse avatar / Horizon Worlds" look: clean flat shading, smooth plasticky surfaces, simplified low-poly-ish geometry, slightly uncanny videogame-avatar characters with the soft rounded proportions and glossy plastic skin of a virtual-reality social-world avatar. This is NOT a photograph, NOT photoreal — it is a stylized glossy 3D-rendered CGI image, the way a corporate metaverse social app renders its avatars. Despite the cartoonish avatar render, the WHOLE frame is washed in META BLUE (#0668E1, Meta's corporate brand blue) — a cold, flat, monotone Meta-blue colour grade over everything (sky, water, avatars, light), like a corporate metaverse promo. NOT polychrome, NOT rainbow, NOT a cheerful saturated cartoon, NOT Pixar-colorful, NOT grey — monotone Meta blue, low-contrast, cold, ironic. Deadpan, satirical. CRITICAL: CRISP and SHARP throughout — NO motion blur, NO depth-of-field blur, NO soft-focus, NO "blurred to suggest movement"; movement is conveyed only through pose and body lean. jeffrey's EXPRESSION + MOOD genuinely SHIFT per beat — read the EMOTION called out and let his face and posture show it clearly.`;

const JEFFREY =
`JEFFREY — about 30, recognizable from the jeffrey reference photographs (medium-length brown hair, light beard, his real face) but RESTYLED as a glossy plasticky metaverse-avatar version of himself with smooth low-poly shading. He wears an open burgundy hoodie over a plain soft tee and casual board shorts (or chinos in the office), barefoot on the water / sneakers in the office. His burgundy hoodie is the ONLY faint warm colour in an otherwise colourless frame. He is the one avatar that feels alive; NOT centered as a lone hero — equal eye-line with others. NO laptop, NO device, NO phone in his hands ever.`;

const MARK =
`MARK ZUCKERBERG — recognizably him, rendered as a glossy plasticky metaverse avatar: pale smooth skin, short straight brown hair in a flat Caesar fringe, rounded boyish face, faint unblinking smile. He wears his signature plain crew-neck t-shirt (muted grey, not vivid) — calm, eager, faintly robotic onboarding-salesman energy, the proud host of the metaverse. Clearly a DIFFERENT person from jeffrey.`;

const GRAY_WOMAN =
`THE WOMAN IN GREY — a young woman jeffrey is quietly drawn to: an ALL-GREY office outfit (grey blouse / soft grey knit + grey slacks or skirt), neat hair, a calm pretty unbothered expression, NOT noticing him. The one person in the drab office he keeps glancing at. Rendered as the same glossy desaturated avatar as everyone else.`;

const GIRLS =
`THE SELFIE GIRLS — about 6–8 women rendered as glossy plasticky metaverse avatars, every one a REPLICA of the office WOMAN IN GREY: all wearing the same muted ALL-GREY look (grey athleisure / yoga-leggings / crop-cardigan "wellness influencer" styling, tasteful, never costumes, never lewd), an ETHNICALLY DIVERSE cast of faces (Black, East Asian, South Asian, Latina, white, Middle Eastern) sharing one identical robotic self-absorbed energy — the office crush mass-cloned. Each stands stiff and ROBOTIC, frozen, holding a phone at arm's length taking a selfie of HERSELF, eyes locked on her own screen, duck-lipped or blissed-out, utterly self-absorbed. They do NOT look at jeffrey, the camera, or each other. NO phone flashes, no glowing screens — plain dim grey phones.`;

const OFFICE =
`THE OFFICE — a drab open-plan corporate TECH OFFICE rendered in the same glossy desaturated 3D style: rows of low grey cubicles, dark dead monitors, flat fluorescent ceiling light, grey carpet, a couple of sad potted plants, a glass-walled meeting room behind. Mounted on one wall as trendy startup decor is a single real SURFBOARD (the motif that later becomes jeffrey's wakeboard). Mundane, soul-less, washed-out corporate grey.`;

const WORLD =
`THE METAVERSE WORLD — a wide virtual ocean off a low sand-dune coastline rendered in a game engine, the WHOLE scene in META BLUE (#0668E1): a flat Meta-blue sky and bright corporate META-BLUE WATER (#0668E1) with stylized low-poly waves, pale blue-tinted dune grass on a far sandbar, a calm shallow lagoon-like plane, hazy distant low dunes. Cold, monotone Meta blue, corporate and a little eerie — the office dream warped into a Meta-blue virtual beach. The water especially is unmistakably Meta blue (#0668E1).`;

const DRONE =
`THE DRONE WARS ON THE HORIZON — across the distant back horizon of the Meta-blue sea, a FEW SPLASHY EXPLOSIONS of water erupt from SEA-MINES / drone-strikes detonating far out: several tall white-grey PLUMES of water blasting straight up off the far waterline, crisp columns and fans of flung spray spaced across the horizon — a distant metaverse SEA-BATTLE playing out behind everything. ATMOSPHERIC distant water-plume blasts ONLY — clean crisp geometry, NO fire, NO gore, NO debris, NO visible boats or weapons — kept small on the far horizon in the faintly-colorized grey grade, never crowding jeffrey.`;

const PIXSIES =
`THE GROUP PIXSIES — a single warm round (or oval) TABLE of "AC freaks": a small ETHNICALLY-DIVERSE band of joyful creative people aged 20s–60s gathered close, trading hand-made papers and odd hacker gear and little glowing handmade gadgets, leaning in, laughing, genuinely connected; a couple of small KIDS sit on the ground beside the table, and an ELDER plays a battered upright piano just behind. They are the human antidote to the hollow selfie crowd — rendered with a touch MORE warmth and life than the grey avatars (still inside the same metaverse render, just a small warm island of real connection amid the grey). jeffrey is a PEER among them, never centered, never a leader. ONE focal table only — NOT multiple tables, NOT a cafe full of tables.`;

const PALETTE =
`PALETTE — a monotone META BLUE (#0668E1) grade across the whole metaverse: Meta-blue sky, Meta-blue water, Meta-blue-tinted avatars; jeffrey's burgundy hoodie the only warm note (and a slightly warmer pool of life around the pixsies table when present). Flat corporate light, NO rainbow, NO polychrome, NO vivid multi-colour — just cold Meta blue. Ironic, corporate.`;

const AVOID =
`AVOID — any photographic / photoreal look; any illustration / colored-pencil / gouache / painterly hand look; polychrome / RAINBOW / multicolour, cheerful Pixar palettes, neon, sunset glow, teal-and-orange grade, tropical aqua, or GREY water (the whole frame is monotone META BLUE #0668E1 — the water especially); motion blur of ANY kind, soft-focus, "blurred to suggest movement"; ANY laptop, computer, green MacBook, phone-in-jeffrey's-hand, screen/monitor showing content, tablet held by jeffrey; ANY matrix code, binary digits, falling 1s and 0s, glowing code; the drone-war blasts rendered as fire/flame/gore/debris/boats/weapons (clean distant WATER PLUMES only) or a giant tsunami wall of water; multiple pixsies tables / a cafe of tables (ONE focal table only); jeffrey centered as a hero or lone-leader; the selfie girls reacting, noticing, or looking at jeffrey/camera; any selfie girl in a costume / lewd / runway outfit; jeffrey in a tank top, costume, or formalwear; any readable real-world brand wordmark or logo anywhere; app / phone / Story UI chrome, HUD, menus, health bars, or text overlaid; any readable text or wordmark in the image; any second wakeboard rider; phones showing the surrounding scene recursively.`;

// ── REAL-WORLD render mode (office beats — before/after the game) ────
// Photoreal + textured, the tangible real world vs the glossy game.
const R_MEDIUM =
`A REALISTIC PHOTOGRAPH — a real cinematic photo of a real place shot on a real camera: true photoreal materials and TEXTURES (skin pores, hair strands, fabric weave, brushed metal, worn carpet, smudged glass), natural light + shadow, real depth, faint grain. This is the tangible REAL WORLD before the game — it must look believably real and richly textured. NOT a 3D render, NOT a glossy plastic avatar, NOT cartoonish, NOT low-poly. Still colour-graded cool + desaturated for the drab corporate mood, but fully PHOTOGRAPHIC. Crisp, no motion blur. Deadpan, mundane, a little lonely. jeffrey's EXPRESSION + MOOD shift per beat — read the EMOTION and show it.`;
const R_JEFFREY =
`JEFFREY — about 30, the REAL man from the reference photographs, PHOTOREAL: real face + real skin texture, light beard, tousled medium-length brown hair. He wears a real burgundy hoodie (real cotton-fleece texture) over a plain tee and chinos, sneakers. A real person, photographed — NOT an avatar.`;
const R_MARK =
`MARK ZUCKERBERG — recognizably the real him, PHOTOREAL: pale real skin, short brown Caesar-cut hair, his signature plain grey crew-neck t-shirt (real fabric). A real person, photographed. Clearly a DIFFERENT person from jeffrey.`;
const R_GRAY_WOMAN =
`THE WOMAN IN GREY — a real young woman jeffrey is quietly drawn to, PHOTOREAL: an all-grey office outfit (real grey knit / blouse + grey slacks or skirt), neat hair, calm, going about her work, NOT noticing him.`;
const R_OFFICE =
`THE OFFICE — a real, drab open-plan corporate TECH OFFICE, photoreal + textured: real grey fabric cubicle partitions, real monitors + keyboards, fluorescent ceiling panels, worn grey carpet, real potted plants, a glass-walled meeting room with real smudges + reflections. Mounted on one wall as startup decor: a real SURFBOARD (real fibreglass sheen). The monitors + a glass-walled meeting-room screen glow a faint META BLUE (#0668E1); subtle Meta-blue corporate accents. Mundane, washed-out corporate.`;
const R_PALETTE =
`PALETTE — cool, desaturated, grey-graded but PHOTOGRAPHIC: real muted fluorescent-lit colour, jeffrey's burgundy hoodie the one warm note. No vivid saturation.`;
const R_AVOID =
`AVOID — any glossy 3D-render / plastic avatar / Horizon-Worlds / low-poly / cartoon / Pixar / illustration look (this is a REAL PHOTOGRAPH); motion blur, soft-focus; vivid saturation, neon; any laptop / phone / screen content in jeffrey's hands; readable brand wordmarks / logos; UI / HUD / text overlays.`;
const REALIZE = new Map([[OFFICE, R_OFFICE], [GRAY_WOMAN, R_GRAY_WOMAN], [MARK, R_MARK]]);

// ── DEGRADATION render mode (spawn / portal-void / dissolve) ─────────
// When the metaverse tears or assembles: NOT grey wireframe — FULL
// super-psychedelic aesthetic.computer glitch.
const PSYCH_MEDIUM =
`A META-BLUE DIGITAL GLITCH RENDER — the metaverse tearing apart / assembling, rendered ENTIRELY in META BLUE (#0668E1): glowing Meta-blue polygon shards, Meta-blue wireframe, datamosh smears, scan-lines and luminous Meta-blue trails over a deep blue-black void. MONOCHROME META BLUE — NOT polychrome, NOT rainbow, NOT multicolour, NOT psychedelic-colour; cold corporate Facebook/Meta blue only. Crisp glowing geometry, no motion blur. Deadpan, eerie, corporate-cosmic.`;
const PSYCH_JEFFREY =
`JEFFREY — recognizable from the references (brown hair, beard, his real face), his avatar caught mid-transition, breaking apart into / assembling out of glowing META-BLUE polygon shards.`;
// Characters other than jeffrey still need their IDENTITY described in a
// psych beat — the medium blocks supply the look, not the cast. Without
// this, a beat whose body names MARK has only jeffrey described and only
// jeffrey's reference photos to go on, and the model draws a SECOND
// jeffrey (it did: the portal beat came back with two longhairs).
const PSYCH_MARK =
`MARK ZUCKERBERG — recognizably him (pale skin, short straight brown hair in a flat Caesar fringe, rounded boyish face, faint unblinking smile, plain crew-neck t-shirt), his avatar rendered in the same glowing Meta-blue polygon-shard idiom as the rest of the frame. Clearly a DIFFERENT person from jeffrey — NOT a second jeffrey: no long hair, no beard.`;
const PSYCH_CAST = new Map([[MARK, PSYCH_MARK]]);
// State the clean frame POSITIVELY. A bare "no giant hands" negative made
// gpt-image-2 draw them anyway (and it volunteered a Meta wordmark + a
// music-player UI on top) — naming a thing to forbid tends to summon it.
const PSYCH_FRAME =
`THE FRAME CONTAINS THE SCENE AND NOTHING ELSE — it is a clean, free-floating camera looking at the world. Every hand and arm in frame BELONGS to a full figure whose body is visible and attached. The image is bare of any graphic layer: no logos, no wordmarks, no brand marks, no lettering, no numbers, no buttons, no player controls, no progress bars, no interface of any kind anywhere in the picture.`;
const PSYCH_AVOID =
`AVOID — ANY polychrome / rainbow / multicolour / psychedelic-colour (this beat is MONOCHROME META BLUE #0668E1 only); grey; photoreal look; motion blur; ANY second jeffrey / duplicate of the same man.`;
const PSYCH_SET = new Set(["build-c", "build-d", "drop1-a", "outro-b", "outro-c", "outro-d"]);

// ── per-beat story — office → metaverse → office, 8 sections × 2 ──────
// SECTION_ORDER keys MUST match amaythingra.struct.json sections[]; each
// section has an `a` (first half) and `b` (second half) beat. The
// assembler shows a for the first ~half of the section then b.
const SECTION_ORDER = [
  "intro", "build", "drop1", "break", "vortex", "drop2", "outro", "resolve",
];

const COVER_VARIANT =
`COVER COMPOSITION — the locked amaythingra album-cover crop: jeffrey WAKEBOARDING across the flat Meta-blue metaverse water, framed UP CLOSE, full body large and near, knees bent, one arm out for balance, leaning into a confident carve that throws a sharp crisp fan of grey spray off the board's edge — a big open-mouthed delighted grin — carving THROUGH the scattered rows of stiff oblivious grey selfie-girl avatars (replicas of the office woman in grey), one or two caught mid-topple but still filming themselves. He rides as one element among the figures, equal eye-line, NOT a hero-center pose.`;

const BEATS = {
  intro: {
    a: { blocks: [OFFICE, GRAY_WOMAN, MARK], body:
`SHOT — WIDE ESTABLISHING from a HIGH corner of the ceiling, looking down across the whole drab grey open-plan office: rows of cubicles, jeffrey small at his desk lower-left, THE WOMAN IN GREY at her desk across the floor, MARK ZUCKERBERG loitering by the glass meeting room, the SURFBOARD on the wall. EMOTION — establishing, mundane, a little lonely. sets the ordinary world.` },
    b: { blocks: [OFFICE], body:
`SHOT — MEDIUM, eye-level, on jeffrey alone at his cubicle: slouched back in his chair, idly spinning a pen, the dead grey monitor glow on his face, paperwork untouched. EMOTION — flat boredom, restless. just him and the grey.` },
    c: { blocks: [OFFICE, GRAY_WOMAN], body:
`SHOT — FIRST-PERSON POV, literally through jeffrey's eyes (his FACE IS NOT SHOWN): the near foreground is the edge of his own desk — a grey keyboard, a mug, his own forearms resting — and across the office, soft-focus pulling sharp, THE WOMAN IN GREY at her desk, the thing he keeps looking at. EMOTION — quiet longing, a stolen glance.` },
    d: { blocks: [OFFICE], body:
`SHOT — EXTREME CLOSE-UP on jeffrey's face filling the frame, slight low angle: half-lidded eyes drifting, a tiny wistful daydreaming half-smile, grey office bokeh behind. EMOTION — lost in a daydream. crop in TIGHT, face fills frame.` },
  },
  build: {
    a: { blocks: [OFFICE, MARK], body:
`SHOT — MEDIUM TWO-SHOT: MARK ZUCKERBERG has arrived at jeffrey's cubicle, leaning an arm on the partition, mid-pitch, holding a sleek grey VR HEADSET out toward jeffrey; jeffrey leans back, caught off guard. EMOTION — Mark salesy + eager, jeffrey wary-curious.` },
    b: { blocks: [OFFICE, MARK], body:
`SHOT — CLOSE-UP on the grey VR HEADSET held out in Mark's hands in the foreground, jeffrey's hand just entering frame to take it, both faces soft behind. EMOTION — the offer, the small fateful choice. detail-tight on the device + hands.` },
    c: { blocks: [OFFICE], body:
`SHOT — FIRST-PERSON POV (jeffrey's FACE NOT SHOWN): his own avatar hands rise into frame lifting the VR headset toward the camera-as-his-eyes, and the grey office at the edges is already DISSOLVING into floating low-poly polygon shards + pale portal light. EMOTION — the threshold, vertigo starting.` },
    d: { blocks: [OFFICE, MARK], body:
`SHOT — WIDE, low angle: a tall glowing rectangular PORTAL of pale light has torn open in the middle of the grey office (the grey low-poly beach faintly visible inside); jeffrey silhouetted stepping toward it, the room breaking apart into polygon shards around him, MARK gesturing him through. EMOTION — awe, being pulled in.` },
  },
  drop1: {
    a: { blocks: [WORLD], body:
`SHOT — LOW ANGLE looking UP at jeffrey's avatar SPAWNING into being in a blank flat-Meta-blue void: glossy low-poly polygon shards + wireframe panels snapping together up his body, burgundy hoodie resolving, a faint wireframe horizon grid drawing in beneath. EMOTION — uncanny arrival, blinking wonder.` },
    b: { blocks: [WORLD], body:
`SHOT — FIRST-PERSON POV (jeffrey's FACE NOT SHOWN): looking down at his own newly-formed glossy plastic avatar hands turning over in front of the camera-eyes, then the Meta-blue beach + flat silver water rendering in around them. EMOTION — disoriented curiosity, "where am I".` },
    c: { blocks: [WORLD, GIRLS, DRONE], body:
`SHOT — WIDE ESTABLISHING, the full Meta-blue lagoon revealed: rows of the REPLICATED woman-in-grey selfie-girl avatars stretching back toward the horizon, every one filming herself; far out, the DRONE-WARS water-plumes erupting; a wakeboard materializing on the water near jeffrey (small in frame). EMOTION — dawning unease at the scale of it.` },
    d: { blocks: [WORLD, GIRLS], body:
`SHOT — CLOSE-UP, two of the identical grey selfie-girl replicas side by side filling the frame, same face, same pose, both filming themselves, utterly oblivious — the uncanny clone reveal. jeffrey's shoulder/edge just in frame, recoiling. EMOTION — creeping dread, recognition.` },
  },
  break: {
    a: { blocks: [WORLD], body:
`SHOT — CLOSE-UP, low over the water, on the WAKEBOARD (the office wall-surfboard, transformed) materializing on the flat Meta-blue surface with a faint spawn-shimmer, jeffrey's bare avatar feet stepping onto it, ripples spreading. EMOTION — tentative, figuring it out. tight on board + feet + water.` },
    b: { blocks: [WORLD, GIRLS], body:
`SHOT — MEDIUM on jeffrey rising to standing on the wakeboard, arms out, wobbling to find balance among the frozen grey selfie-girls. EMOTION — focused, a determined little grin.` },
    c: { blocks: [WORLD, PIXSIES], body:
`SHOT — FIRST-PERSON POV (jeffrey's FACE NOT SHOWN): skimming forward low over the Meta-blue water on the board, the front tip of the wakeboard + his own hands in foreground, and far ahead a small WARM GLOW — the GROUP PIXSIES round table — drawing his eye across the cold lagoon. EMOTION — a flicker of hope, drawn toward warmth.` },
    d: { blocks: [WORLD, GIRLS, PIXSIES], body:
`SHOT — WIDE HIGH ANGLE (bird's-eye-ish) looking down: jeffrey small on his board amid the scattered ranks of frozen grey selfie-girls, the warm little pixsies island glowing far across the Meta-blue lagoon, his wake cutting a line between them. EMOTION — the choice laid out, the maze ahead.` },
  },
  vortex: {
    a: { blocks: [WORLD, GIRLS, DRONE], body:
`SHOT — DYNAMIC WIDE, low and fast: jeffrey carving hard, leaning into a turn that throws a big crisp fan of grey spray, weaving through the maze of frozen grey replicas; the DRONE-WARS plumes bigger and closer on the horizon. EMOTION — exhilarated flow, wild grin.` },
    b: { blocks: [WORLD, MARK, GIRLS], body:
`SHOT — OVER-THE-SHOULDER from behind MARK ZUCKERBERG: Mark's back/shoulder fills the near foreground as he lunges and GRABS the back of jeffrey's hoodie, yanking — jeffrey wrenching away on the board beyond him, water churning. EMOTION — the conflict erupts; Mark possessive, jeffrey alarmed.` },
    c: { blocks: [WORLD, MARK], body:
`SHOT — EXTREME CLOSE-UP: jeffrey's gritted, straining face and the burgundy hoodie fabric STRETCHED taut in Mark's gripping plastic hand, the tug-of-war at its tightest. EMOTION — defiant strain, the fight. tight, visceral detail.` },
    d: { blocks: [WORLD, GIRLS], body:
`SHOT — WIDE, the release: jeffrey WRENCHES free and is flung forward in a huge arc of grey spray, MARK tumbling backward into the water behind, flailing; the oblivious grey girls film on. EMOTION — triumphant break-free, momentum.` },
  },
  drop2: {
    a: { blocks: [WORLD, GIRLS], body:
`SHOT — WIDE comedic, the GAG: jeffrey's carving board plows straight through a cluster of the grey selfie-girl replicas, two or three toppling sideways like dominoes (one a WHITE/blonde replica sloshing over), every one still filming herself, ZERO reaction. EMOTION — gleeful chaos; total non-reaction comedy.` },
    b: { blocks: [WORLD, GIRLS], body:
`SHOT — CLOSE-UP on ONE blonde grey replica caught mid-topple, water sloshing up past her shoulder, her face locked in the same blissed-out duck-lipped selfie expression, phone still raised, oblivious. EMOTION — deadpan absurdity. tight on her non-reacting face.` },
    c: { blocks: [WORLD, PIXSIES], body:
`SHOT — MEDIUM, warm: jeffrey carves up to the GROUP PIXSIES round table and steps off the board, the diverse band turning to welcome him in as a peer, the cold Meta-blue lagoon behind, a warm candle-lit pocket here. EMOTION — relief, arrival, belonging.` },
    d: { blocks: [PIXSIES], body:
`SHOT — CLOSE-UP, warm, candlelit: jeffrey and two of the pixsies leaning in together over a small glowing handmade gadget on the table, hands and faces close, real delight, the elder's piano + a kid just behind in soft focus. EMOTION — genuine human connection, the heart of the piece.` },
  },
  outro: {
    a: { blocks: [WORLD, PIXSIES, DRONE], body:
`SHOT — WIDE: jeffrey rides AWAY from the pixsies table back out onto the open Meta-blue lagoon, glancing back to wave; the pixsies wave warmly after him, the DRONE-WARS plumes receding small on the horizon. EMOTION — contented, a soft satisfied smile, leaving the warmth.` },
    b: { blocks: [WORLD], body:
`SHOT — FIRST-PERSON POV (jeffrey's FACE NOT SHOWN): his own hands + the front of the wakeboard in foreground beginning to BREAK APART into floating low-poly polygon shards as the grey world glitches and dissolves around his viewpoint. EMOTION — the ride ending, reality tugging back.` },
    c: { blocks: [], body:
`SHOT — EXTREME CLOSE-UP on jeffrey's face, half of it already DISSOLVING into drifting polygon shards, eyes wide, the grey world gone to flat loading-void behind. EMOTION — bittersweet, being pulled out, a flicker of loss. face fills frame.` },
    d: { blocks: [], body:
`SHOT — ABSTRACT WIDE: the entire Meta-blue metaverse SHATTERS into a rushing storm of low-poly polygon shards + pale portal light streaming/reversing past the camera toward a white-out. EMOTION — the violent yank back through, disorientation. no figures, pure transition.` },
  },
  resolve: {
    a: { blocks: [OFFICE, MARK], body:
`SHOT — MEDIUM: jeffrey snaps BACK at his office desk, ripping the VR HEADSET up off his face, hair mussed, swaying, one hand gripping the desk, thoroughly seasick-discombobulated; MARK ZUCKERBERG stands nearby, pleased and faintly smug. EMOTION — dazed, reeling, just-yanked-out.` },
    b: { blocks: [OFFICE, GRAY_WOMAN], body:
`SHOT — FIRST-PERSON POV (jeffrey's FACE NOT SHOWN): a swimming, slightly warped view of the grey office settling back into focus — the desk edge + headset in his hands in foreground, and across the room THE WOMAN IN GREY resolving sharp, looking up toward him with mild concern. EMOTION — vertigo clearing, his eyes finding her.` },
    c: { blocks: [OFFICE, GRAY_WOMAN, MARK], body:
`SHOT — MEDIUM: steadied now, jeffrey stands and takes a real step across the grey office toward THE WOMAN IN GREY, who turns fully toward him, openly curious; MARK lingers smug in the background, the SURFBOARD on the wall. EMOTION — tentative courage, the loop about to close changed.` },
    d: { blocks: [OFFICE, GRAY_WOMAN], body:
`SHOT — CLOSE-UP TWO-SHOT: jeffrey and THE WOMAN IN GREY a couple feet apart, both breaking into a real, shy first smile — a genuine human connection, no replication, no game. soft grey office behind, surfboard on the wall. EMOTION — warm, hopeful, changed. tight on the two faces.` },
  },
};

function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}
const POV_NOTE =
`POV INTEGRITY — this is jeffrey's OWN first-person viewpoint (his eyes ARE the camera). The only part of jeffrey in frame is his own hands / forearms / body in the near foreground. ABSOLUTELY NO second jeffrey anywhere — no other man resembling jeffrey, no mirror / reflection / screen showing jeffrey's face. Any other people are clearly DIFFERENT individuals.`;
function build(beat, tightCrop, psych) {
  const orient = LANDSCAPE
    ? `\n\n${LANDSCAPE_NOTE}`
    : (tightCrop ? "" : `\n\n${PORTRAIT_NOTE}`);
  const pov = beat.body.includes("FIRST-PERSON POV") ? `\n\n${POV_NOTE}` : "";
  if (psych) {  // degradation: Meta-blue glitch
    const cast = beat.blocks.map((b) => PSYCH_CAST.get(b)).filter(Boolean);
    // POV beats WANT foreground hands; every other beat wants a clean frame.
    const frame = pov ? [] : [PSYCH_FRAME];
    return [PSYCH_MEDIUM, PSYCH_JEFFREY, ...cast, beat.body, ...frame, PSYCH_AVOID].join("\n\n") + pov + orient + "\n";
  }
  if (beat.blocks.includes(OFFICE)) {  // real-world office: photoreal + textured
    const rb = beat.blocks.map((b) => REALIZE.get(b) || b);
    return [R_MEDIUM, R_JEFFREY, ...rb, beat.body, R_PALETTE, R_AVOID].join("\n\n") + pov + orient + "\n";
  }
  return [MEDIUM, JEFFREY, ...beat.blocks, beat.body, PALETTE, AVOID].join("\n\n")  // glossy Meta-blue metaverse
    + pov + orient + "\n";
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

// One gpt-image-2 edit call → write outPath. Per-file cached; --force
// regens. Transient 429/5xx + network blips retried; never throws.
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
        // 401s from OpenAI image edits are often spurious under load —
        // retry them too (a genuine bad key would fail every call).
        const transient = res.status === 429 || res.status === 401 || res.status >= 500;
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

// --only vortex (both halves) | vortex-b (one half) | cover
const onlySet = typeof flags.only === "string"
  ? new Set(flags.only.split(",").map((x) => x.trim().toLowerCase()))
  : null;
const wants = (section, half) =>
  !onlySet || onlySet.has(section) || onlySet.has(`${section}-${half}`);

const jobs = [];
if (!onlySet || onlySet.has("cover"))
  jobs.push({ prompt: build({ blocks: [WORLD, GIRLS], body: COVER_VARIANT }, true, false), out: `${LANE}/out/${SLUG}${TAG}-cover.png`, label: `${SLUG}${TAG} cover` });
for (let i = 0; i < SECTION_ORDER.length; i++) {
  const name = SECTION_ORDER[i];
  for (const half of Object.keys(BEATS[name])) {
    if (!wants(name, half)) continue;
    jobs.push({
      prompt: build(BEATS[name][half], false, PSYCH_SET.has(`${name}-${half}`)),
      out: `${LANE}/out/${SLUG}${TAG}-sec-${i}-${safeName(name)}-${half}.png`,
      label: `${SLUG}${TAG} §${i} ${name}-${half}`,
    });
  }
}

progress.begin({ type: "illy", label: `${SLUG}${TAG} · ${jobs.length} panels` });
let done = 0;
for (const job of jobs) {
  await generate(job.prompt, job.out, job.label);
  progress.update((++done / jobs.length) * 100, { done, total: jobs.length });
}
progress.end();
console.log(`\n✓ ${SLUG} storyline panel set — ${jobs.length} job(s) · office → metaverse → office (16 beats)`);
