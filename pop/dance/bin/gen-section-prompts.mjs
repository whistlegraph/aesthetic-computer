#!/usr/bin/env node
// gen-section-prompts.mjs — (re)write the trancenwaltzi section
// cover-prompt.txt files from ONE source of truth.
//
// Direction (2026-05-19, pass 2 — v41): the chill rebuild of
// trancenwaltzi reads lightly sinister. Visuals = a MIDNIGHT-
// UNRAVELLING descent: the SAME Trader Joe's + jeffrey + his crew + the
// locked whistlegraph/PALS identity, shot at 3am in thick fog while the
// walls of reality come apart. Overtly unsettling. Pass-2 art notes
// (jas):
//   • butterfly: model keeps dropping the arms — DON'T composite;
//     instead make whistlegraph-butterfly.png an explicit hard prompt
//     anchor, arms = the #1 detail, bigger + legible.
//   • per-section LASER ARC: undesignated measurement/security beams
//     raking the fog, DISSIPATED + BROKEN UP by the haze (glowing
//     dashes/patches, dark gaps — never clean lines), globally tinting
//     each scene; near-none at intro → multi-colour storm by
//     drop2/outro. The lasers are the ONLY saturated colour in the
//     otherwise drained grim frame (jas: "more colour but keep grim").
//   • jeffrey CLOSER / larger in frame, fewer pixies crowding.
//   • pixies are obviously ROBOTIC creations of jeffrey — handmade
//     small cute automata, NOT humans. Less girly (a minority accent,
//     not the majority); still small cute beings.
//
// jeffrey identity + the whistlegraph-butterfly geometry + PALS-only
// pixie laptops + shirt/pen stay locked. Mood / framing / lasers /
// pixie-construction are the pass-2 changes.
//
// Usage:
//   node pop/dance/bin/gen-section-prompts.mjs            # write all
//   node pop/dance/bin/gen-section-prompts.mjs --dry      # print only
//
// Backs up any existing cover-prompt.txt → cover-prompt.<ISO>.bak.

import { writeFileSync, existsSync, copyFileSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";

const DRY = process.argv.includes("--dry");
const ROOT = `${homedir()}/Documents/Working Desktop/gens/trancenwaltzi-sections`;

// ── LOCKED jeffrey/identity blocks — VERBATIM, do not edit ───────────
const LOCK_SOURCE_FIDELITY =
`SOURCE FIDELITY (TOP PRIORITY) — stay EXTREMELY TRUE to the provided reference images. (a) JEFFREY: his face, hair, beard, body AND his OUTFIT + ACCESSORIES must read as the SAME real person and the SAME real clothes shown in the jeffrey reference photographs — the actual shirt, its real small chest insignia, the way he dresses; do not invent fashion or restyle him. (b) THE LAPTOP DOODLE: reproduce the attached whistlegraph-butterfly.png reference EXACTLY — it is NOT a butterfly, it is a wonky hand-inked STICK-FIGURE LITTLE PERSON whose TWO BENT ARMS are raised up-and-out, each ending in a big rounded cloud shape; copy that figure line for line, both arms included; never simplify it into a butterfly with wings on its body. Faithfulness to these source images outranks everything else in the scene.`;

const LOCK_OUTFIT =
`OUTFIT: match the outfit shown in the jeffrey reference photos exactly — real everyday clothes (button-down / hoodie / printed tee / cardigan / sweater / flannel). natural fabric, photoreal.`;

// Butterfly block — pass-2: explicit ATTACHED-IMAGE anchor, ARMS = the
// single most important detail (renders keep dropping them), and a bit
// LARGER / legible since jeffrey + laptop are closer now.
const LOCK_LAPTOP_BUTTERFLY =
`JEFFREY'S LAPTOP DOODLE (CRITICAL — READ CAREFULLY): jeffrey's CITRUS-GREEN macbook neo is present in every section — open + glowing on his face/hands while he hacks, or its lid clearly facing camera in the cart/on a shelf. On the lid, where an apple logo would be, a torn SCRAP OF WHITE PAPER is taped, with a wonky hand-inked doodle on it in thick dark marker. ONE OF THE ATTACHED REFERENCE IMAGES (whistlegraph-butterfly.png) IS that exact doodle — COPY IT LINE FOR LINE. IMPORTANT: it is NOT a butterfly — do NOT draw a butterfly, do NOT put wings on a body. It is a wonky childlike STICK-FIGURE LITTLE PERSON: a roughly rectangular slightly-tilted smiling HEAD (two dot eyes + a small curved smile) on a tall thin rectangular BODY; from the upper body **TWO clearly-drawn skinny ARMS — one on the LEFT, one on the RIGHT — reach UP AND OUTWARD like a child drawing someone with both arms raised, and EACH HAND ends in a big rounded puffy CLOUD shape** (so it reads as a little person holding up / turning into two clouds, NOT a butterfly); at the bottom the body has TWO short stubby legs JOINED into one connected block split by a single short centre line. THE TWO RAISED ARMS ARE THE WHOLE POINT and THE SINGLE MOST IMPORTANT DETAIL: every prior render wrongly dropped the arms and fused the clouds straight onto the body like butterfly wings — that is the #1 mistake and is WRONG; there MUST be a visible skinny LEFT arm AND a symmetric skinny RIGHT arm between the body and each cloud; never armless, never one-armed, never lopsided, never butterfly-like. Legs always ONE connected block, never split. Render it clearly legible (jeffrey + the laptop are close now) so both raised arms are unmistakable — a hand-drawn doodle on a real taped paper scrap, the embossed apple peeking around its torn edges, NOT a clean logo. This doodle is on JEFFREY'S green laptop ONLY — NEVER on the shirt, NEVER on a crew member. Any visible screen shows plausible real content, NEVER a thumbnail of this scene. NO apple logos, NO text on laptops.`;

const LOCK_SHIRT =
`JEFFREY'S SHIRT — render his shirt EXACTLY as it appears in the provided jeffrey reference photographs: a casual real-wardrobe BUTTON-DOWN, mostly buttoned but RELAXED (top button or two undone, collar a little open; NOT fully done up, NOT gaping wide, NOT over a graphic tee). NO big chest print / graphic (the whistlegraph doodle is NEVER on the shirt — it lives only on the green laptop). It DOES keep the shirt's own small real chest INSIGNIA visible in the source photos — a little classic WINNIE-THE-POOH-style embroidered/woven badge that is part of the actual shirt; reproduce that real insignia faithfully, small and subtle. Do NOT add any plush, keyring, pin, sticker, or dangling toy — it is a flat woven emblem ON the shirt fabric, nothing hanging off him. PLUS: a bright YELLOW PEN clipped into the chest pocket, clearly visible, every section.`;

// The crew — pass-3: they look like the CUTE HUMAN PEOPLE from before
// (jas loves exactly that), just PASSABLE-as-human androids with
// SUBTLE tells — small embedded LEDs. NOT clunky robots. They ADORE /
// ANNOY / HELP / HURT jeffrey while he shops the aisles. Less girly
// (minority). Small, cute, varied, grim + uncanny.
const ROBOT_CREW =
`THE CREW — small, CUTE little PEOPLE that jeffrey made: they look almost entirely HUMAN — real human faces, skin, hair, proportions, child-sized, adorable, a genuinely diverse mix (varied faces, skin tones, builds, apparent ages — kids to elders, all looks, no single type) — PASSABLE as human at a glance. They are secretly his ANDROIDS, with only SUBTLE TELLS: tiny embedded LEDs glowing faintly under the skin (a soft point at a temple, behind an ear, along the neck or in the eyes), maybe one hairline seam — small, easy to miss, mostly hidden. NOT clunky tin robots, NOT boxy automata, NOT exposed wires/screws, NOT mechanical scrap, NOT smooth featureless mannequins — they look like cute real humans you'd only clock as synthetic on a second look. Girly or cute styling on only a FEW — a minority accent, NOT the majority; the rest plain / everyday / utilitarian. They cluster around jeffrey AS HE SHOPS and behave with real feeling toward him — some ADORING him (gazing up, clinging, devoted), some ANNOYING him (pestering, underfoot, tugging, in the way), some HELPING him (loading the cart, handing him things, steadying him), some HURTING him (a too-hard grip, a scratch, a small wound — casual, not cruel). Mixed all together, but UNCANNY under the grim fog — too intent, too still between actions, a few faintly glitching/greying at the edges like the room. small, cute, eerie, devoted and dangerous. natural human ears — NEVER pointed elf/fae/animal ears, NO antennae.`;

// LASER arc — shared aesthetic; the per-section SECTION line says how
// many beams + what colour for that step of the descent.
const LASER_BASE =
`MEASUREMENT / SECURITY LASERS — thin straight undesignated measurement-or-security laser beams cut through the space, but they are DISSIPATED and BROKEN UP by the thick fog: never clean continuous lines — they scatter and glow VOLUMETRICALLY in the haze, fragmenting into bright dashes and soft patches with dark gaps where the fog is densest, edges bleeding into the mist. These beams are the ONLY saturated colour in an otherwise cold, drained, grim frame, and they globally tint / illuminate the whole scene in their hue(s). They rake at angles across the aisle (raking the shelves, jeffrey, the crew, the fog) like an automated scan of a place that no longer exists. Keep the grim midnight-unravelling base dominant — the lasers add colour and unease, they do NOT make it bright or cheerful.`;

const LOCK_NOTEXT =
`NO text, NO captions, NO logos baked into the image. NO sparkle-overlay chrome, NO waveforms. framed for both vertical 9:16 and square 1:1.`;

// (b) outro host-hero — PALS-on-pixie-laptops rule stays locked.
const LOCK_OUT_PIXIE_LAPTOPS =
`CREW LAPTOPS — some of jeffrey's crew carry their own tiny ac macbook neos (open + cold-glowing, or holstered). crew laptop lids carry the PALS logo and ONLY the PALS logo. the attached pals-logo.png IS the PALS logo — ALWAYS exactly TWO Keith-Haring-style hand-drawn outline people side by side (TWO pals, never one/three), a DIFFERENT pose per laptop, clean contrasting outline over a mixed-colour back panel. the two-pals mark is ONLY ever on a laptop BACK / lid, never on a screen; screens show real glowing content; no apple logos, no text. ABSOLUTELY NO WHISTLEGRAPH DOODLE ON ANY CREW LAPTOP — zero, ever; the whistlegraph doodle is jeffrey's laptop ALONE; every crew laptop is PALS-only.`;

const LOCK_OUT_SHIRT = LOCK_SHIRT;
const LOCK_OUT_NOTEXT =
`NO text, NO captions, NO baked-in logos or readable brand names. NO sparkle-overlay chrome, NO waveforms. framed for both square 1:1 and vertical 9:16.`;

// ── pass-2 mood / framing ────────────────────────────────────────────
const MOOD_PHOTO =
`a CANDID PHONE PHOTOGRAPH taken at MIDNIGHT — cold, drained, desaturated and failing; thick OPAQUE fog hanging in the air indoors; very light grain; shot like a real off-hand phone snapshot at 3am. NOT an illustration, NOT colored-pencil, NOT cinematic-glossy, NOT a horror-movie poster — a real, flat, deadpan photo that happens to be of something genuinely WRONG. airless, quiet, uneasy; reality has thinned. The ONLY colour comes from fog-broken measurement/security laser beams (below) — everything else is grim grey.`;

const MOOD_SUBJECT =
`subject: jeffrey, recognizable from the jeffrey reference photographs (face shape, eye spacing, jawline, beard/hair colour). REAL person, photoreal skin and hair. He is SHOPPING THE TRADER JOE'S AISLES with his RED SHOPPING CART — pushing/leaning on the cart in a grocery aisle, mid grocery-run, the cart with bags + flowers + his green laptop in it. SHOT CLOSE on him — he is LARGE in the frame, roughly waist/chest-and-up and near the camera (close, NOT a distant wide shot) — but it is clearly an in-aisle shopping scene, not a void. He is CALM-BLANK — not smiling, not afraid, not deadpan-comic; quietly absent, eyes a little unfocused, far away, as if he has not noticed the room coming apart, while his little crew swarm him (adoring, annoying, helping, hurting him). NEVER cheerful, NEVER grinning. He is the clear subject but never heroic-posed — just close, still and far away amid the cart and the crew.`;

const STORE_CONST =
`the SAME Trader Joe's grocery store (cedar-plank ceiling, wood-panelled shelving, hand-lettered chalkboard signs, the painted maritime/island mural, buckets of cut flowers, a red cart, brown kraft bags) — long after close, EMPTY of any shoppers or staff, lit only by failing, flickering, half-dead fluorescent tubes, the cold laptop glow and the fog-broken lasers. KEEP ALL PACKAGING + SIGNAGE GENERIC AND ILLEGIBLE — no readable real brand wordmarks or logos anywhere.`;

// Per-section: setting beat + the LASER/COLOUR step of the arc
// (near-none at intro → multi-colour storm by drop2/outro).
const SECTIONS = {
  intro:
`SECTION: INTRO — just inside the front doors at midnight, close on jeffrey; fog bleeds low across the floor, the entrance behind him a flat grey nothing. ${STORE_CONST} far shelf edges already fraying into grey STATIC. LASERS: almost none — at most ONE faint thin beam barely visible deep in the fog, near-monochrome cold grey-blue, hardly any colour yet. jeffrey eases a cart out, blank, not noticing; one or two of his little crew stand by, still.`,
  break1:
`SECTION: BREAK1 — close on jeffrey in the cut-flower corner, flowers greying and DISSOLVING into mist; a shelf bank behind already gone to fog, the mural peeling into void. ${STORE_CONST} LASERS: ONE cold CYAN measurement beam slices the fog, broken into glowing dashes by the haze; faint cyan global tint, everything else still drained grey. he stands too still, eyes empty; a crew member half-turned beside him.`,
  build1:
`SECTION: BUILD1 — close on jeffrey at a cart in an aisle that stretches WRONG (perspective bending, fog a wall halfway down, tubes blinking out). ${STORE_CONST} shelves losing their right angles. LASERS: TWO beams now — cyan crossing a dim amber — fog-fragmented into patches, low rising saturation. a couple of his crew ride the cart, motionless.`,
  drop1:
`SECTION: DROP1 — close on jeffrey at the wooden tasting counter; the back wall behind peeling open into a SEAM of churning grey void, crew members taking sample cups from the edge of nothing. ${STORE_CONST} LASERS: 2–3 beams, the FIRST real colour — RED + MAGENTA security lasers raking, shredded by the fog into glowing blotches, the whole scene washed faint red-magenta over the grey. jeffrey holds a cup, staring through it, vacant.`,
  break2:
`SECTION: BREAK2 — tight on jeffrey at the near-dark checkout; registers + far wall ERASED into flat fog, reality paper-thin, only laptop glow + one dying tube. ${STORE_CONST} LASERS: sparse but SATURATED — one or two intense beams (deep electric BLUE + acid GREEN) in the dark, the fog eating most of each into broken segments, strong local colour pooling. he leans, eyes nearly shut, gone.`,
  build2:
`SECTION: BUILD2 — close on jeffrey mid-aisle as the unravelling ACCELERATES: the floor + ceiling starting to tear, fog churning faster, the crew swarming him more (clinging, tugging, helping, one gripping too hard). ${STORE_CONST} LASERS: MULTIPLYING — three-to-five beams now (cyan, magenta, amber, green) sweeping faster and proliferating, fog-shredded into racing dashes, colour rising hard toward the storm but not yet full. he keeps moving the cart, blank, as it builds.`,
  drop2:
`SECTION: DROP2 — close on jeffrey as the front of the store fully comes apart: walls TORN open into churning grey fog/void, floor + shelves unravelling into static, the store becoming nowhere. ${STORE_CONST} LASERS: a MULTI-COLOUR STORM — many beams (red, green, blue, violet, amber) criss-crossing as the walls tear, the fog SHREDDING them all into scattered glowing patches, the scene globally lit in shifting multi-colour over the grim. his crew + jeffrey serenely, indifferently glowing; a thin cold upload-stream of pale particles rising, not celebratory.`,
  outro: null, // host-hero template below
};

function buildCalm(sectionKey) {
  return [
MOOD_PHOTO,
LOCK_SOURCE_FIDELITY,
MOOD_SUBJECT,
`SETTING: inside ${STORE_CONST} thick fog pools in the aisles and the WALLS OF REALITY ARE COMING APART — shelf edges fraying into static, the mural peeling into grey void, whole sections dissolving into fog and nothing, geometry bending where it shouldn't. no other shoppers, no staff — only jeffrey and his crew.`,
LOCK_OUTFIT,
ROBOT_CREW,
LASER_BASE,
SECTIONS[sectionKey],
LOCK_LAPTOP_BUTTERFLY,
LOCK_SHIRT,
`FACE READABLE + CROP-SAFE — SHOT CLOSE: jeffrey large in frame, his head fully in with clear headroom, face in the vertical-centre/upper band occupying roughly 45–60% of frame width, eyes visible, facing roughly toward camera (candid, not stiffly posed). Framed so it crops cleanly to BOTH a tall vertical 9:16 and a square 1:1 — keep jeffrey, his face, his laptop + the whistlegraph doodle, and the 2–4 nearest crew within the centre ~80% with margin of fogged background on every edge. Keep the UPPER-LEFT (top ~30% on the left third) clear of faces/focal subjects — soft fogged background for titling. Do NOT crop the top of his head.`,
LOCK_NOTEXT,
  ].join("\n\n") + "\n";
}

function buildOutro() {
  return [
MOOD_PHOTO,
`THE VIBE — "...come in. it's already gone." jeffrey CLOSE and large in frame, the calm HOST welcoming YOU into the unravelling: arms thrown WIDE, one hand reaching toward camera — but his face BLANK and far-away, no grin, no fear, just an empty welcoming gesture as the store dissolves behind him into fog. NOT a selfie, NOT a wide distant shot — a close uneasy talk-show-host welcome into nowhere, a few of his crew dim around him.`,
`FRAMING — CLOSE on jeffrey (chest/shoulders up, large), his open cold-glowing laptop prominent low-front with the whistlegraph doodle clearly legible, only 3–5 of his crew fanned in the fog around him along what is LEFT of the shelves before they fray. NOT a wide empty room, NOT a distant group shot — jeffrey big, close, filling the frame.`,
`SETTING — INSIDE the SAME Trader Joe's at MIDNIGHT, almost entirely DISSOLVED: shelving, signs and the island mural tearing open and unravelling into churning grey fog/void; a red cart half-eaten by fog. only failing tubes + laptop glow + the lasers. KEEP ALL PACKAGING + SIGNAGE GENERIC AND ILLEGIBLE — no readable brand wordmarks anywhere; the store is becoming nowhere.`,
`${LASER_BASE} HERE THE DENSEST: a flooding LATTICE of many measurement/security laser beams in many colours (red, green, blue, violet, amber) filling the dissolving void, the fog breaking every beam into scattered glowing patches, jeffrey arms-wide lit in shifting multi-colour over the grim grey.`,
`"UPLOADING" — from the open laptops a thin STREAM of pale, cold light-particles drifts upward through the fog: still happening, quiet, indifferent, NOT celebratory, NOT scary-glitchy — just continuing as the room ends.`,
ROBOT_CREW,
`JEFFREY — recognizable from the provided jeffrey reference photographs (face shape, eye spacing, jawline, beard/hair colour), REAL person, photoreal. CALM-BLANK — no grin, no fear, eyes a little unfocused, arms wide in an empty welcoming gesture; absent, as if he has not noticed the room is gone. SOURCE FIDELITY (top priority): same real person + same real clothes as the references; faithfulness to the source images outranks everything.`,
LOCK_LAPTOP_BUTTERFLY,
LOCK_OUT_PIXIE_LAPTOPS,
LOCK_OUT_SHIRT,
`CROP-SAFE — CLOSE on jeffrey: keep his blank face + welcoming arms, his laptop + the legible whistlegraph doodle, and the nearest crew within centre ~80%, margin of fogged background every edge; crops cleanly to BOTH square 1:1 album cover AND tall vertical 9:16. his head fully in frame, face vertical-centre/upper, ~45–60% of frame width, eyes visible, toward camera. UPPER-LEFT reserved for titling: top ~30% of the left third = soft fogged void only. do NOT crop the top of his head.`,
LOCK_OUT_NOTEXT,
  ].join("\n\n") + "\n";
}

const ISO = new Date().toISOString().replace(/[:.]/g, "-");
const order = ["intro", "break1", "build1", "drop1", "break2", "build2", "drop2", "outro"];
for (const sec of order) {
  const body = sec === "outro" ? buildOutro() : buildCalm(sec);
  const file = join(ROOT, sec, "cover-prompt.txt");
  if (DRY) { console.log(`\n========= ${sec} =========\n${body}`); continue; }
  if (existsSync(file)) {
    const bak = join(ROOT, sec, `cover-prompt.${ISO}.bak`);
    copyFileSync(file, bak);
    console.log(`  backup → ${bak}`);
  }
  writeFileSync(file, body);
  console.log(`✓ ${file} (${body.length} chars)`);
}
console.log(DRY ? "\n(dry run — nothing written)" : `\n✓ wrote ${order.length} section prompts (pass 2 / v41 — lasers + closer + passable-android crew + de-butterflied doodle)`);
