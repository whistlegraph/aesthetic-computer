#!/usr/bin/env node
// hellsine/bin/gen-sections.mjs — generate the hellsine STORYLINE panel
// set: 18 per-beat illustrations tracing jeffrey's drone-attack fall
// from earth into the hellsine party.
//
// Concept (jas, 2026-05-26):
//   the hellsine track visualized as one story — jeffrey at his
//   earth desk in studio zollo → a drone strike opens a portal →
//   he + the pixsies fall through it together → they land in
//   hellsine and ignite → peak party (climax-a is the locked cover)
//   → dawn, they live here now. eighteen beats arranged across the
//   six README sections (overture / statement / bridge / develop /
//   climax / coda), ≈ 9 s each over the 2:42 master.
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
`FELT-PUPPET JEFFREY — about 30, recognizable from the jeffrey reference photographs: tousled medium-length brown felt-yarn hair (mussed, loose strands), CLEAN-SHAVEN or faint stubble at most (NO full beard, NO heavy scruff), pale-flesh felt skin with subtle cheek pink. he wears a PALE-BLUE felt button-down shirt with hand-applied darker-blue felt pinstripes, a small yellow felt bear emblem at the chest, a yellow Sailor Pro Gear-shape fountain pen (short cigar-shaped barrel, flat-topped cap, polished metal clip, gold trim ring) clipped at the shirt pocket, red plastic glasses dangling at the placket; wide-leg medium-cobalt felt trousers. peer-horizontal — never centred as a hero. his felt + eyes go through the arc described below per beat.`;

const PIXSIES =
`FELT PIXSIES — 4-7 humanoid grad-students appearing in mid-story beats onward, a real spread of AGES (kid → elder) and a wide RACE + GENDER spectrum (women, men, boys, girls, femme, masc, androgynous; many ethnicities). ROUNDED HUMAN EARS ONLY — never pointed / elf / fae. thoughtful warmly-intelligent faces. OUTFITS a mixed-up pastiche — some militaristic-tactical, some super-cute girly, some cyberpunk-techwear, some cardigan, all of it clashed eclectic the way grad-student wardrobes are. uncanny tells (LED beads under felt skin at temple / ear / eye-edge in cyan-green pinpricks, occasional hairline felt seams) optional + subtle. each holds (when present) ONE small AC PALS laptop in one hand: glossy injection-moulded plastic shell with the PALS glyph (the two-bubbly-people Keith-Haring linked outline from the pals-logo.png reference) glowing on the lid in a unique hue — cyan / magenta / lime-green / hot-pink / golden-yellow / electric-orange / deep-violet. NEVER apple, NEVER butterfly on pixsie lids.`;

const JEFFREY_LAPTOP =
`JEFFREY'S MACBOOK NEO — when shown, a CITRUS-GREEN plastic MacBook Neo with a TORN WHITE-PAPER SCRAP taped over where the apple logo would be, a hand-penned thick whistlegraph BUTTERFLY drawn on the scrap (NOT an apple, NOT a PALS glyph — only jeffrey's lid carries the butterfly scrap). the lid is glossy plastic that reflects ambient light.`;

const WORLD_LAW =
`WORLD LAW — the SETTING varies by beat:
• EARTH PANELS (overture-*, statement-*) = jeffrey's felt-crafted studio zollo: warm desk-lamp pool, wooden floor, plant, AC poster, a real-photo night sky outside the window. the studio is felt, the night sky outside is photo-real.
• PORTAL / COSMIC-CHUTE PANELS (statement-c, bridge-*) = a long descending shaft between worlds; walls are streams of glowing MATRIX-RAIN code-glyphs (kana / kanji / numbers / abstract symbols) in MULTI-HUE — yellow + red + purple + lime, intermixed columns. broken chunks of earth (studio floor, the macbook neo, a coffee mug, sheet music, the desk lamp) drift weightless in the fall. soap bubbles rising.
• HELLSCAPE PANELS (develop-*, climax-*, coda-*) = real-photo alien volcanic landscape — cracked CHARRED basalt foreground crusted with soot + sulphur, sinusoidal LAVA RIVERS curving across the ground (the lava itself runs in clean math sine waves), tall walls of sine-shaped FLAME, jagged obsidian spires receding into a bruise-purple horizon, dense atmospheric particulate (orange embers + sparks, ash flakes, soap bubbles, smoke columns, multi-hue matrix-rain cascading through some smoke columns, faint glowing wireframe-grid + circuit-trace lines in basalt cracks, a few floating holographic HUDs), heat-shimmer above the lava. A SINGLE PURE-WHITE HORSE flames across the mid-distance basalt with red flames licking its hooves + body. A SLEEK BULLET TRAIN streaks the far horizon with lit-up windows + motion-streaked tracks. THE SINES ARE THE FIRE — every lava river + flame tongue traces a perfect sine wave. NO sine beams from anyone's mouth.`;

const ARCS =
`CONTINUITY ARCS across the whole 18-panel set (the beat description below names this beat's stage):
• EYES: dim (clean dark felt) → shock-wide → wonder-wide → fire ignites in sockets → blazing red-orange flames in sockets, no whites → embers (soft glow, no flames).
• FELT: clean → torn from the strike → frayed from the descent → scorched at cuffs → fully tattered + char-marked → still tattered but settled.
• PALS LIDS: dark / off → flickering on → full seven-hue glow → soft pulse at dawn.
• FINGERTIPS: clean → clean → clean → tiny live flames singeing felt fibres at tips, scorch marks + glowing ember cracks on palms → embers.`;

const PALETTE =
`PALETTE — for earth panels: warm amber desk-lamp pool against cool blue night, the felt blues of jeffrey's shirt + trousers, soft browns; for chute panels: cool deep-blue chute with multi-hue matrix-rain streaks (yellow / red / purple / lime); for hellscape panels: dominant warm lava-orange + red as the principal light, deep crimson sky, bruise-purple horizon, obsidian black + sulphur yellow + soot, embers + ash, intermixed with the multi-hue matrix-techno layer (yellow / red / purple / lime / amber / magenta), PALS-lid hues (cyan / magenta / lime / hot-pink / gold / orange / violet) as the secondary punctuation light.`;

const AVOID =
`AVOID — any cartoon / plush / collaged look (this is refined felt-craft realism); apple logos or butterfly glyphs on pixsie lids (PALS only on pixsie lids, butterfly only on jeffrey's MacBook Neo lid scrap); SINE BEAMS POURING FROM ANYONE'S MOUTH (the sines ARE the lava + fire itself, never an oral effect); blood / gore / body horror (the heat damage is real felt damage — fraying, char, scorch — not gore); any readable text, wordmark, or logo anywhere; pointed / elf / fae ears on pixsies (rounded human ears only); jeffrey centred as a hero (peer-horizontal); modern flat tech-illustration; recursive screens showing the surrounding scene; living-artist names; motion blur language (convey motion through pose only).`;

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
`BEAT — OVERTURE A "mid-keystroke" · EMOTION: focused calm, the hum before everything. ARC STAGE — eyes dim (no fire), felt clean, NO pixsies present, PALS lids dark. EARTH. studio zollo, late. felt-puppet jeffrey at his desk seen from across the room — citrus-green MacBook Neo open with the white-paper whistlegraph-butterfly scrap taped over the apple, sailor pro gear yellow pen clipped in his pale-blue button-down pocket, headphones half on, red glasses dangling at the placket. felt studio behind him: warm desk-lamp glow, wooden floor, a plant, an AC poster on the wall. he is mid-keystroke on a kidlisp piece — calm focused face. through the window: a still black night sky. camera: MEDIUM across the desk, jeffrey + screen + window all in frame.`,

  "overture-b":
`BEAT — OVERTURE B "the blinking light" · EMOTION: stillness, the moment before everything. ARC STAGE — eyes dim, felt clean, NO pixsies, PALS dark. EARTH. TIGHTER on the window behind jeffrey's shoulder. black night sky outside, perfectly still. ONE tiny RED LIGHT blinks once, far away in the distance — too small for him to notice. his felt hands stay on the keyboard, the warm desk-lamp pool to one side. mood: the hum before everything. camera: OVER-THE-SHOULDER past jeffrey OUT THE GLASS, the red blink small and centred in the night.`,

  "overture-c":
`BEAT — OVERTURE C "the swarm forms" · EMOTION: oblivious — the last calm moment. ARC STAGE — eyes dim, felt clean, NO pixsies, PALS dark. EARTH. the SKY OUTSIDE the window is now alive — the one red blink has become a dozen, then dozens, drones forming a moving constellation against the black. faint neon-orange targeting beams sweep across the studio glass from outside, still soundless. jeffrey is in mid-frame still focused on his macbook, headphones on, oblivious — the warm desk-lamp glow on his face now competing with cold orange beams crossing his shoulder. camera: ACROSS THE ROOM, jeffrey in foreground, the lit-up sky filling the window behind him.`,

  "statement-a":
`BEAT — STATEMENT A "the swarm" · EMOTION: SHOCK — caught mid-recoil, no understanding yet. ARC STAGE — eyes WIDE in shock (still no fire), felt about to tear, NO pixsies, PALS dark. EARTH. the WINDOW EXPLODES INWARD — felt-and-glass shards mid-flight, slow-motion. a SWARM of black military drones outside, neon-orange targeting lasers crisscrossing the studio, ONE drone right at the broken pane staring in with a single red lens-eye. jeffrey recoils, BOTH PALMS UP defensive, his felt face caught mid-shock — brow up, mouth open, NO grin, NO fire-eyes. camera: WIDE, drones swarming on one side of the frame, jeffrey centred, glass mid-air.`,

  "statement-b":
`BEAT — STATEMENT B "the portal opens" · EMOTION: disbelief turning to gravity-pull. ARC STAGE — eyes shock-wide, felt starting to tear, NO pixsies yet, PALS dark, NO fire on him. EARTH transitioning to PORTAL. AT THE SAME INSTANT, BEHIND jeffrey a vertical PORTAL tears open in the studio air — a rip in space, edges crackling with multi-hue MATRIX RAIN (yellow / red / purple / lime), the inside lava-orange + swirling. the desk, chair, mug, sheet music are already lifting and pulling toward it. studio walls warping into the suck. jeffrey pivoting toward the portal, hair lifting in the pull. the macbook neo slides off the desk into the air. camera: LOW ANGLE BEHIND THE DESK, the portal a vertical blade of light dominating the back of the frame, jeffrey silhouetted against it.`,

  "statement-c":
`BEAT — STATEMENT C "across the threshold" · EMOTION: held breath, mid-leap, suspended. ARC STAGE — eyes wide held breath, felt just beginning to fray where it crosses the portal, NO pixsies, NO fire yet. EARTH-PORTAL CROSSING. jeffrey caught AT THE LIP OF THE PORTAL — feet off the floor, drones still firing in from the window-right edge of frame, the macbook neo + pen + a single sheet of music suspended in the air around him. HALF HIS BODY already in the lava-orange swirl, half still in the felt studio (the seam crossing his torso). NO fire-eyes yet — wide eyes, breath held. the felt of his crossing sleeve is just beginning to fray. camera: WIDE, jeffrey mid-threshold, earth on one side of him, hellsine light on the other.`,

  "bridge-a":
`BEAT — BRIDGE A "weightless" · EMOTION: wonder displacing fear. ARC STAGE — eyes wide-wonder (no fire), felt frayed at edges from descent, NO pixsies yet, NO fire. COSMIC CHUTE. jeffrey ALONE now, tumbling slowly in freefall through a long descending shaft, weightless, looking around in WONDER — eyes wide but mouth softening, no panic. around him: streams of glowing CODE-RAIN (yellow / red / purple / lime) on every wall of the chute, broken chunks of earth drifting alongside (a slice of studio floor, the macbook spinning slow, the coffee mug, pages of sheet music, the desk lamp still glowing). no other figures yet. camera: VERTICAL PAN, jeffrey centred in the upper third, debris in orbit, the chute walls receding into depth.`,

  "bridge-b":
`BEAT — BRIDGE B "the pixsies arrive" · EMOTION: surprise + relief — finding companions. ARC STAGE — eyes wide-wonder, felt frayed, pixsies APPEAR for the first time (clean clothes, no fire), PALS dark. COSMIC CHUTE. THE FIRST PIXSIES EMERGE one by one through colored code-rain walls into the fall — a kid pixsie steps through the LIME stream, an elder through the VIOLET, a femme grad-student through the HOT-PINK, a tactical-vest pixsie through the GOLD. each joins the descent in their own felt outfit, drawn in from elsewhere, mid-step out of the wall. jeffrey turns toward them in surprise + relief. peer-horizontal formation already forming — none centred. soap bubbles starting to rise past them. camera: VERTICAL PAN, the fall now populated, jeffrey + arriving pixsies spread across the frame.`,

  "bridge-c":
`BEAT — BRIDGE C "warmth from below" · EMOTION: curiosity, anticipation, the first hint of grin. ARC STAGE — eyes wide-curious (no fire), felt hems beginning to fray more, pixsies present, PALS still dark, warm orange light on the underside of every face. COSMIC CHUTE. full crew falling together — jeffrey + 4-6 pixsies in loose orbit around each other and the orbiting earth-debris. soap bubbles rising thick now. WARM ORANGE LIGHT starting to leak in from BELOW the frame — they're approaching hellsine. felt hems beginning to fray from the descent. expressions: curiosity, anticipation, the first hint of grin. camera: VERTICAL PAN, the crew composed peer-horizontally, the bottom of frame washed warm orange.`,

  "bridge-d":
`BEAT — BRIDGE D "approach" · EMOTION: awe — the last moment before transformation. ARC STAGE — eyes wide-awe, felt frayed, pixsies present in soft focus, PALS still dark, warm orange underlight fully landed on faces. COSMIC CHUTE NEAR THE BOTTOM. TIGHT on jeffrey's face as the fall accelerates toward landing — warm orange light from below now FULLY ILLUMINATING his underside + chin + the underside of his felt hair, the cool blue cosmic-chute light fading behind him. eyes wide, mouth slightly open in awe. a single pixsie just visible in soft focus over his shoulder, also lit warm from below. soap bubbles streaming past. NO fire-eyes yet — but the warmth he's about to inherit is already on his face. camera: MEDIUM-CLOSE on jeffrey, three-quarter angle, the warm glow rising up the frame.`,

  "develop-a":
`BEAT — DEVELOP A "impact" · EMOTION: caught breath, taking stock. ARC STAGE — eyes wide-stunned (no fire yet), small scorch marks just starting at felt cuffs, pixsies present, PALS dark, fingertips clean. HELLSCAPE FIRST ENTRY. they LAND on basalt. dust + heat-shimmer kicked up around their boots. jeffrey on his feet, knees flexed from the landing; the pixsies in a loose semicircle around him in various landing poses — one still mid-crouch, one standing tall already, one picking up the macbook neo that landed beside her. expressions: caught breath, taking stock. small scorch marks just starting at the cuffs from the warm air. camera: LOW + WIDE, the landing zone occupying the lower half, the hellscape just beginning to reveal above + behind.`,

  "develop-b":
`BEAT — DEVELOP B "discovery" · EMOTION: discovery, taking in the world piece by piece. ARC STAGE — eyes wide-curious (no fire yet), small scorch marks at cuffs, PALS LIDS JUST FLICKERING ON (one or two glowing, rest still dark), fingertips clean. HELLSCAPE REVEALED. jeffrey + pixsies looking outward in different directions — discovering pieces of the world one at a time: one looks at a sine river curving past their boots, one watches the bullet train streaking the horizon, one points at the white horse galloping mid-distance, one tracks an ember drifting up. multi-hue matrix-rain falls through distant smoke columns. the pixsies hold their first laptops up (lids facing camera) — PALS only just FLICKERING ON, one or two glowing, rest still dark. camera: WIDE PANORAMA, hellsine landscape behind, group anchored lower-third.`,

  "develop-c":
`BEAT — DEVELOP C "ignition" · EMOTION: wickedness arriving — wide-eyed grins forming, not full demon yet. ARC STAGE — FIRE IGNITES in eye sockets in a chain (jeffrey first, then each pixsie), ALL PALS LIDS now full-glowing in seven hues, FINGERTIPS BEGINNING TO SINGE with tiny live flames, felt damage taking root (first threads pulling loose). HELLSCAPE. FIRE IGNITES — first in jeffrey's eye sockets (one socket, then the other) and then in a CHAIN around the semicircle, each pixsie's eyes lighting in sequence. all the PALS lids now full-glowing in seven hues (cyan / magenta / lime / hot-pink / gold / orange / violet). fingertips beginning to singe, tiny flames flickering off felt fibres at the tips. wide-eyed grins forming — not full demon yet, but the wickedness is arriving. camera: TIGHTER on the group, mid-shot, the chain of ignition catchable across the frame, the lava world warm behind them.`,

  "climax-a":
`BEAT — CLIMAX A "the cover" (verbatim cover crop) · EMOTION: peak chaos, peak joy. ARC STAGE — eyes BLAZING with full live flames in sockets (no whites), all PALS lids full seven-hue glow, fingertips on fire with tiny flames + scorch marks + glowing ember cracks, felt TATTERED + char-marked. THE COVER. the smooshed-into-lens wide-angle group portrait from hellsine.illy.txt — jeffrey + pixsies dancing ON / OVER / AROUND active fire, BOTH PALMS UP at the lens, fire-eyes BLAZING, sinusoidal lava ribbons weaving between feet, sine-flame tongues dancing between faces, PALS lids glowing cyan / magenta / lime / hot-pink / gold / orange / violet, white horse + bullet train on the back horizon, multi-hue code-rain through smoke columns, soap bubbles rising, fingertip flames + scorched palms, tattered felt. peak chaos, peak joy, the PARTY. camera: THE COVER CROP VERBATIM — edge-to-edge, jeffrey at about 40% from left, smooshed into the lens.`,

  "climax-b":
`BEAT — CLIMAX B "from inside the dance" · EMOTION: peak, alternate vantage. ARC STAGE — same as climax-a (eyes blazing, PALS full, fingertip flames, felt tattered). THE SAME PARTY, ALTERNATE ANGLE. camera now BEHIND jeffrey, looking past his shoulder + raised palms OUT through the dancing crew. we see the backs of his hands silhouetted against the lava glow, the pixsies arrayed in front of him ALL looking toward the camera (i.e. toward jeffrey), fire-eyes blazing, raised PALS laptops a row of multi-hue lanterns. lava sines weaving between. same chaos, different vantage — proves the world wraps around the group. camera: OVER JEFFREY'S SHOULDER, depth into the crowd, his back-of-head + raised arms in shallow foreground.`,

  "climax-c":
`BEAT — CLIMAX C "the wide vista" · EMOTION: a vast warm chaos with the crew at its heart. ARC STAGE — eyes blazing, PALS full, fingertip flames, felt tattered — but at scale now. PULL BACK to the WIDEST shot of the track — the dancing group is now SMALL in the lower-third of the frame, the FULL hellsine vista revealed around them: parallel sinusoidal lava rivers curving through the basalt foreground at varied amplitudes, obsidian spires receding into bruise-purple horizon, the white horse flaming across the mid-distance, the bullet train streaking the far horizon, multi-hue matrix-rain streaming through smoke columns on either side, soap bubbles + embers everywhere. the dancing crew is the warm pulsing nucleus inside a vast lava world. camera: VERY WIDE, low, the group anchored lower-centre, sky + vista filling the upper two-thirds.`,

  "coda-a":
`BEAT — CODA A "embers" · EMOTION: spent + content. ARC STAGE — eyes now just EMBERS (soft orange glow, no flames), felt fully tattered + scorched (but the damage settled), fingertips embers + scorch marks (no flames), PALS lids dimmed to a soft pulse, dawn light. HELLSCAPE AT DAWN. dawn breaking over the obsidian horizon — bruise-purple softening to deep coral, the lava glow halved, smoke columns thinning. the pixsies are scattered across the basalt — sat on rocks, leaning against each other, one stretched out flat looking up. spent and content. fire-eyes now just EMBERS — soft orange glow, no flames. felt is fully tattered + scorched but the heat is no longer hurting. PALS lids dimmed to a soft pulse. camera: WIDE, the spent crew arranged across the basalt, dawn filling the upper half.`,

  "coda-b":
`BEAT — CODA B "they live here now" · EMOTION: calm settled smile, home found. ARC STAGE — eyes embers, felt tattered-settled, fingertips quiet embers, PALS soft pulse, MacBook Neo open + glowing softly. HELLSCAPE AT DAWN, INTIMATE. jeffrey sits on a low basalt outcrop in mid-shot, his CITRUS-GREEN MacBook Neo open across his lap — it fell with him, it's home too, the white-paper whistlegraph-butterfly scrap still on the lid, screen glowing softly with a kidlisp piece. ONE pixsie ASLEEP against his shoulder, embers in her eyes. the white horse a small silhouette on the obsidian behind him. the bullet train a thin red streak. steam vents puff slow + soft. jeffrey is smiling — NOT the wicked climax grin, a CALMER smile — they live here now. one last soap bubble rising past frame. camera: MEDIUM, jeffrey + sleeping pixsie + macbook in focus, softening hellscape in soft focus behind.`,
};

// ── prompt construction ──────────────────────────────────────────────
function build(sectionBeat) {
  const orient = LANDSCAPE ? `\n\n${LANDSCAPE_NOTE}` : `\n\n${PORTRAIT_NOTE}`;
  return [MEDIUM, JEFFREY, PIXSIES, JEFFREY_LAPTOP, WORLD_LAW, ARCS, sectionBeat, PALETTE, AVOID].join("\n\n")
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
