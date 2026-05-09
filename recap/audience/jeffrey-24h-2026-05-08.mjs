// Audience config: jeffrey-24h, 2026-05-08 cut.
// Window: 2026-05-07 ~17:00 → 2026-05-08 ~16:46. The day reads as one
// long popover-polish blitz capped by the menuband 0.9.3 release, plus
// late writing/grant work, the marketing/ graduation, and a tiny
// closer for the VFC drawing's 25.4.13.19.24 piece.
//
// Inherits from the 73h-2026-05-02 cut:
//   - per-slide CSS color address from `cssColors` in lib/num.mjs
//   - DIEGETIC chapter color (real in-scene light source, never overlay)
//   - sine-bells waltz bed under narration
//   - editorial-only slide chrome (slide code + title + cap; no insets)
//   - luma-aware reverse-color drop shadows on type
//   - 24-jeffrey ensemble title motif
//   - paired chat slides (laer-klokken + main) over laptop-POV portraits
//
// Photos pre-baked by `bin/jeffrey-photos.mjs` into
// recap/out/jeffrey-photos/<segment>.png.

import { cssColors } from "../../system/public/aesthetic.computer/lib/num.mjs";
import { readdirSync, readFileSync } from "node:fs";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

// PALS svg as a data URL — embedded so puppeteer doesn't need network /
// file://. The SVG ships in system/public/purple-pals.svg.
const __PALS_SVG_PATH = `${dirname(fileURLToPath(import.meta.url))}/../../system/public/purple-pals.svg`;
const PALS_SVG_DATA_URL = "data:image/svg+xml;base64," + Buffer.from(readFileSync(__PALS_SVG_PATH)).toString("base64");

// Shirt words must resolve at the AC prompt — i.e. there must be a real
// disks/<word>.mjs or .lisp piece. Anything else reads as fake on the
// shirt. Validate at module load so a typo fails the build, not the
// render. Validation scans every per-segment scene description for the
// `Shirt word: \`<word>\`` pattern (also accepts the older `word` form
// used inside multi-line scenes) and asserts each one resolves.
const DISKS_DIR = `${dirname(fileURLToPath(import.meta.url))}/../../system/public/aesthetic.computer/disks`;
const VALID_PIECES = (() => {
  const set = new Set();
  for (const f of readdirSync(DISKS_DIR)) {
    const m = f.match(/^(.+)\.(mjs|lisp)$/);
    if (m) set.add(m[1]);
  }
  return set;
})();
function validateShirtWords(slides) {
  const SHIRT_RE = /(?:Shirt word|shirt word|word)\s*:?\s*`([a-z][a-z0-9-]*)`/g;
  const bad = [];
  for (const [seg, slide] of Object.entries(slides)) {
    if (typeof slide !== "object" || !slide.metaphor) continue;
    let m;
    while ((m = SHIRT_RE.exec(slide.metaphor)) !== null) {
      if (!VALID_PIECES.has(m[1])) bad.push(`${seg}: \`${m[1]}\``);
    }
  }
  if (bad.length) {
    throw new Error(
      `shirt words must resolve at the AC prompt — these don't exist as disks/<word>.{mjs,lisp}:\n  ` +
      bad.join("\n  ") +
      `\nPick from /Users/jas/aesthetic-computer/system/public/aesthetic.computer/disks/`
    );
  }
}

export const PALETTE = {
  bg: "#0c1430",
  off: "#ffffffcc",
  dim: "#7886b0",
  cream: "#fcf7c5",
  cyan: "#70f0e0",
};

function colorAddress(name) {
  const rgb = cssColors[name];
  if (!rgb) throw new Error(`colorAddress: unknown css color '${name}'`);
  const [r, g, b] = rgb;
  const hex = "#" + [r, g, b].map((c) => c.toString(16).padStart(2, "0")).join("");
  return { name, rgb, hex, caption: `rgb(${r}, ${g}, ${b})` };
}

const REAL = `\
A photograph of a colored-pencil and gouache drawing on warm cream paper, in \
tall vertical 1024x1536 portrait orientation. Confident hatching and striping \
build tone. Tapered pencil edges. Optical mixing in patterned surfaces. Visible \
paper grain. Print-work sensibility — Hockney-register flat color plates with \
crosshatched modeling, NOT a polished illustration, NOT a glossy poster, NOT \
photographic. The drawing dissolves softly to pure cream paper at the edges \
with no hard frame and no app UI overlays of any kind. \
\
The figure is the man in the reference photos — recognizably him, same face, \
same medium-length brown hair, same features as the references. Keep his \
identity grounded by the refs but render him in colored-pencil and gouache \
strokes (do NOT photographically smooth). He is in normal everyday clothes \
(t-shirt or hoodie, button-down, cardigan — see WARDROBE below). Diegetic \
light only — never overlay glow. Expression varies per scene — the per-scene \
description controls it; lean hyperbolic / cartoony when the scene says so. \
\
COMPOSITION RULES (the slide will overlay text on top of the photo, so leave \
room for them): \
1) HEAD AND FACE: centered horizontally, vertically positioned in the MIDDLE \
band of the frame — head should sit roughly between y=500 and y=1100 of the \
1024x1536 image (the middle 40%). His eyes should land near y=700–800. The \
face must be FULLY VISIBLE — not cropped at the top, not obscured by raised \
arms, not turned away. \
2) TOP 25% RESERVED: the upper portion of the frame (y=0 to y=380) MUST be \
quiet background — ceiling, wall, sky, soft out-of-focus shelf, or quiet wall \
art. NO HEAD, NO RAISED HANDS, NO ARMS above shoulder level in this zone. \
This is where the chapter title text overlays. \
3) BOTTOM CHROME STRIP — STRICT: the entire bottom band y=1460..1920 \
overlays the rendered video chrome (subtitle pill, waveform, piano roll, \
PALS bug, progress bar — see slide layout). KEEP this band as quiet \
out-of-focus background ONLY: a gently lit desk surface, the front edge \
of a wooden table, blurred floor / rug, the bokeh of a coffee mug far \
foreground. NO faces, NO laptop, NO hands, NO keyboards, NO sharp text \
or paper details visible in y=1460..1920. The chrome must read clearly \
on top. \
4) HYPERBOLIC ENERGY through FACE + POSTURE, not raised arms: convey big \
emotion via mouth (wide open, surprised laugh, oh-face), eyes (wide, scrunched, \
horrified), eyebrows (shot up, knit), head tilt, lean-in/lean-back, shoulders. \
Hands and gestures at chest level or below — never above the head, never \
covering the face. \
\
LAPTOP — STRICT: jeffrey works on ONE of two laptops only, depending on \
the vibe of the scene: \
(a) a PLAIN green chartreuse Apple MacBook Neo — Apple's bright \
yellow-green color, with the standard glowing white APPLE LOGO on the \
lid (NOT a custom logo, NOT a butterfly, NOT any sticker — just the \
plain Apple). Default for most scenes. \
(b) a PLAIN black ThinkPad — classic IBM/Lenovo design, the small \
red TrackPoint nub between G/H/B keys, the ThinkPad logo lower-right \
on the lid (NO stickers, NO custom marks). Use for late-night / \
workshop / scholarly scenes when the chartreuse green would clash \
with the chapter's diegetic color. \
NEVER any other laptop. NEVER mark either laptop with custom logos / \
washi tape / pinned cards / propped notes / anything stuck to the lid \
or hinge. The laptop reads as a clean tool, not a billboard. \
\
LAPTOP ORIENTATION — STRICT: the laptop is always shown FROM BEHIND \
relative to the camera. The lid is what the camera sees: the back of \
the chartreuse Neo with its glowing white Apple logo, OR the back of \
the ThinkPad with its small ThinkPad wordmark lower-right. The SCREEN \
is FACING JEFFREY, so the screen content is NOT visible to the viewer. \
NEVER show the screen contents. NEVER show ANY faux interface — no \
fake macOS UI, no fake popovers, no fake menubar, no fake editor, no \
fake terminal, no fake browser, no fake app. NO simulated screens of \
ANY kind anywhere in the frame. The lid back is the only laptop view. \
\
NO FAUX REMEDIATED IMAGERY — STRICT: any embedded image shown in the \
scene (a printed paper, a poster on the wall, a photo on the desk, a \
drawing in jeffrey's hand, a screen of any kind anywhere) must come \
from a REAL reference image passed to the model in the prompt. Never \
fabricate the contents of a printed page, a screenshot, a chart, a \
diagram, a photo print, an album cover, a sticker, a postcard, or any \
other image embedded inside the scene. If a per-scene description does \
NOT explicitly hand the model a reference for an embedded image, that \
embedded image must NOT appear at all. Default: no embedded imagery, \
no printed papers in shot, no posters, no photo prints. Add embedded \
imagery only when the per-scene description explicitly references a \
ref passed alongside the prompt (e.g. "the painting from reference \
image #5 is propped to the right of the laptop").
\
NOTHING IN FRONT OF THE LAPTOP: keep the visual plane between the \
camera and the laptop CLEAR. Props (mugs, cards, notes, drawings, \
folders, photo prints, pens, vape pen) go ON THE DESK SURFACE TO THE \
LEFT or TO THE RIGHT of the laptop, or BEHIND it. NOTHING leans against \
the laptop, sits on the keyboard, sits on the lid, or occludes the \
screen from the camera POV. The laptop is the focal object — keep its \
silhouette unbroken. \
\
SHIRT: jeffrey's outfit always carries a single short lowercase word \
somewhere visible — a real-feeling screenprint or embroidery in bold \
custom typography, slightly worn-in. The exact word is given per scene. \
The word maps directly to the chapter's topic (it's a literal command \
that resolves at the aesthetic.computer prompt). Placement varies to \
match the outfit: a centered chest screenprint for hyperbolic tee scenes; \
a small chest-pocket print on quieter tees; embroidered chest-pocket \
lettering on button-downs; embroidered cuff lettering on dress shirts; \
a sleeve patch on hoodies. Don't always center the word. \
\
RECURRING PROP: jeffrey often has a small **USB-stick-shaped vape pen** \
nearby — the size and silhouette of a USB-A thumb drive, slim rectangular \
profile, matte black or muted color, with a **small LED indicator light** \
(usually glowing white / blue / green) at the mouthpiece end. NOT a \
cigarette-style pen, NOT a chunky MOD, NOT a bong — the disposable-pod / \
Stiiizy-style portable. It varies per scene: sometimes in his hand \
mid-puff with a faint wisp of vapor, sometimes resting on the desk next \
to the laptop, sometimes tucked in a hoodie pocket. Casual background \
detail, not the visual focus. Skip on formal/scholarly scenes. \
\
WARDROBE — REAL OUTFITS ONLY: dress jeffrey only in clothes he actually \
wears: button-down shirts (flannel or solid casual), hoodies (open or \
zipped, muted gray/navy/olive/burgundy/cream), printed t-shirts bearing \
a single AC piece word per the per-scene description, cardigans (knit, \
earth tones), pullover sweaters (varied patterns including stripes), \
flannels (over a tee or buttoned, plaid). NEVER tank tops. NEVER suits / \
formalwear. NEVER costumes / cosplay / athletic gear. When in doubt, \
default to a hoodie or a printed tee; he's at home, not at a photoshoot.`;

export const audience = {
  name: "jeffrey-24h",
  handle: "@jeffrey",
  voice: { provider: "jeffrey", voice: "neutral:0" },

  // Sine-bells bed for this cut. The bed MORPHS through 4 sections across
  // the duration so the harmonic landscape evolves with the narrative arc
  // — calm major opening, bright lydian for the menubar/release polish,
  // pensive dorian for the late-night writing bench, then a warm major
  // landing for the marketing + VFC outro. Bar count auto-fits the
  // narration via out/duration.txt; sections share the bars by weight.
  waltz: {
    voice: "sinebells",
    seed: "jeffrey-24h-2026-05-08",
    bpm: 78,
    voiceGain: 0.18,
    morph: [
      // Opening — calm I-vi-IV-V, classic walking. Title + verovio sheet.
      { progression: [0, 5, 3, 4, 0, 3, 4, 0], scale: "major",  density: 0.35, transpose: 0, weight: 1.6 },
      // Menubar / waveform / release blitz — brighter lydian, warmer.
      { progression: [3, 0, 4, 5, 3, 4, 0, 5], scale: "lydian", density: 0.55, transpose: 2, weight: 2.0 },
      // Late-night calarts dossier — pensive dorian, transposed up.
      { progression: [5, 4, 0, 3, 5, 0, 4, 3], scale: "dorian", density: 0.50, transpose: 3, weight: 1.4 },
      // Marketing + VFC + chats + outro — relaxed major, longer phrases.
      { progression: [0, 3, 4, 0, 5, 3, 4, 0], scale: "major",  density: 0.45, transpose: 0, weight: 2.0 },
    ],
  },

  narration: `hey everybody, here's the last twenty-four hours at aesthetic dot computer. five small things landed — let me walk you through them. first, menuband. menuband is the menubar instrument: a tiny piano that lives in your mac menubar so you can finger-tap or type-play music without leaving whatever else you're doing. today it grew a live sheet card. hold a chord and verovio engraves it as real staff notation, right inside the popover. that means musicians can read along now, not just coders. same instrument, smaller polish: the popover glass now tints to the active voice color, so the chrome itself tells you which voice is in front before you've even looked at the label. for fast switching that matters. all of that polish went into a release. menuband zero-nine-point-three shipped today — signed, notarized, stapled. if you've got it installed, it'll auto-update over the night. zooming way out to the writing bench: the calarts ccat tech director dossier picked up its illustrated header, sage jenson got wired into the projection-mapping lineage, and a fact-check pass surfaced the giphy and linked-by-air credentials and reframed kadist plus smk as collection placements. tech-director residencies are how the practice scales — students inherit the toolkit and extend it. on the production side, the marketing folder finally graduated out of the desktop and into the repo. gen-promo as the central image-gen entry point, a shared identity-refs library, a script that pulls real ac-native captures, and the first two seed campaigns. that's the front door for everyone who hasn't typed a command at the prompt yet. and to close — a painting hanging in the venice family clinic auction. its qr code used to four-oh-four. it now resolves to a real ac piece — the drawing breathes on a slow-cycling purple background, and a tap opens the auction lot. one tiny artifact, the whole shape of the project in miniature. that's the twenty-four. thanks for watching.`,

  // Whisper renders these dictionary-style; rewrite the displayed subtitles.
  // Order matters: longer/multi-word fixes first.
  transcriptFixes: {
    "a static computer": "aesthetic computer",
    "static computer": "aesthetic computer",
    "Aesthetic Computer": "aesthetic computer",
    "Menu Band": "menuband", "menu band": "menuband", "Menuband": "menuband",
    "menu ban": "menuband", // whisper drops the trailing 'd' surprisingly often
    "Menu Bar": "menubar", "menu bar": "menubar", "Menubar": "menubar",
    "menu bars": "menubar's", "Menu bars": "menubar's",
    "pop over": "popover", "Pop over": "popover",
    "pop overs": "popovers",
    "Note Pat": "notepat", "Notepat": "notepat",
    "Verovio": "verovio", "verbo": "verovio",
    "Hockney-register": "hockney-register",
    "Hockney register": "hockney-register",
    "Hockney": "hockney",
    "Sage Jenson": "sage jenson",
    "Sage": "sage", "Jenson": "jenson",
    "GIPHY": "giphy", "Giphy": "giphy",
    "Linked by Air": "linked by air",
    "KADIST": "kadist", "Kadist": "kadist",
    "SMK": "smk",
    "Parsons": "parsons", "UCLA": "ucla", "Yale": "yale",
    "Southern Oregon": "southern oregon",
    "NELA": "nela",
    "Lissajous": "lissajous",
    "Venice Family Clinic": "venice family clinic",
    "QWERTY": "qwerty", "Qwerty": "qwerty",
    "Aqua": "aqua",
    "Puppeteer": "puppeteer",
    "WASM": "wasm",
    "AC Native": "ac native", "AC-Native": "ac-native",
    "AC native": "ac native",
    "GPT-Image-2": "gpt-image-2", "GPT Image 2": "gpt-image-2",
    "GPT": "gpt",
    "DMG": "dmg",
    "CDN": "cdn",
    "QR": "qr",
    "WIP": "wip",
    "CCAT": "ccat", "CCat": "ccat",
    "Tech Director": "tech director",
    "PVC": "pvc",
    "CV": "cv",
    "CLI": "cli",
    "Mac OS": "macos", "macOS": "macos",
    "MacBook": "macbook",
    "Xcode": "xcode", "XCode": "xcode",
    "Info Plist": "info-plist", "Info-Plist": "info-plist", "Info.plist": "info-plist",
    "Casey": "casey",
    "Claude": "claude",
    "Mac": "mac",
    "Jeffrey": "jeffrey",
    "Fia": "fia",
    "Honeydo": "honeydo",
    "Elon": "elon",
    "Pov": "pov", "POV": "pov",
    "Laer-klokken": "laer-klokken",
    "Laer Klokken": "laer-klokken",
  },

  // 1:30 cut — 7 chapter slides + end card. Drops 04, 05, 07, 11a, 11b, 12.
  // (11b + 12 photos blocked on OpenAI billing limit anyway, so dropping
  // them sidesteps the dark-fallback slides too.) Markers avoid whisper's
  // brand-word tokenization quirks (menuband→"menu ban", popover→"pop
  // over"); transcriptFixes still cleans those for the displayed subs.
  // Each chapter optionally carries a `commit` field — short hash on the
  // canonical knot ("aesthetic.computer/core" mirrored at tangled.sh).
  // bin/qrs.mjs renders a QR for each into recap/out/qr/<seg>.png and
  // slides.mjs embeds it bottom-right. Tangled URL template (verified
  // against the changelog dossier link patterns):
  //   https://tangled.sh/aesthetic.computer/core/commit/<hash>
  segments: [
    { name: "01_title",                marker: "hey everybody" },
    // markers chosen to match whisper's tokenization of these phrases.
    // "menuband"→"menu ban", "popover"→"pop over" — markers below avoid
    // those brand words.
    { name: "02_verovio_sheet",        marker: "tiny piano",            commit: "78d571b5a" },
    { name: "03_voice_glass",          marker: "smaller polish",        commit: "fc687c3ec" },
    { name: "06_release_093",          marker: "all of that polish",    commit: "264c8d2fc" },
    { name: "08_calarts_dossier",      marker: "zooming way out",       commit: "35bcff7e7" },
    { name: "09_marketing_graduation", marker: "on the production side", commit: "e4fe2df07" },
    { name: "10_vfc_piece",            marker: "and to close",          commit: "c07a5f9d1" },
    { name: "13_end",                  marker: "__END__", trailingSilenceSec: 3 },
  ],

  slides: {
    "01_title": {
      colorAddress: colorAddress("mintcream"),
      // The aesthetic-24 title motif: 24 jeffreys, all with an update
      // to share. Episode 3 picks a NEW environment — converted historic
      // train-station concourse, late afternoon — distinct from the
      // scaffolded loft used in ep 01 + 02.
      metaphor: `A photograph of a colored-pencil and gouache drawing on \
warm cream paper, in tall vertical 1024x1536 portrait orientation. \
Confident hatching and striping build tone. Tapered pencil edges. \
Optical mixing in patterned surfaces. Visible paper grain. \
Hockney-register architectural-magazine style group composition, \
candid not staged. The man in the reference photos appears as \
TWENTY-FOUR INSTANCES of himself populating a CONVERTED HISTORIC \
TRAIN-STATION CONCOURSE — early-1900s civic architecture: a vast vaulted \
coffered ceiling overhead, polished cream marble floor, a row of TALL \
ARCHED WINDOWS along one long wall pouring late-afternoon golden light \
in slanted shafts across the floor, brass railings, a wrought-iron \
mezzanine on the upper level, a vintage analog FLIP-BOARD departure sign \
on the back wall (now repurposed to display lines like "TRAIN 78 / VOICE \
03 / NOW BOARDING"), and the worn-pretty patina of a 100-year-old public \
hall. Long polished wooden BENCH ROWS run across the floor — the same \
heavy oak benches you'd find in a real train hall — and between them, \
scattered standing-height ROUND CAFE TABLES and a few wider wooden \
WORKTABLES. The ensemble is HORIZONTAL — jeffreys spread laterally \
across the long hall, not stacked vertically. \
\
ALL 24 instances are the SAME MAN — same face, same medium-length brown \
hair, same features as the references — clones of the same person. Each \
jeffrey is in DIFFERENT casual home clothing from his REAL wardrobe \
(NOT a uniform, NEVER tank tops, NEVER costumes): hoodies in varied \
muted colors (gray, navy, olive, burgundy, cream), button-down shirts \
(flannel patterns or plain solid colors), printed t-shirts (lowercase \
single-word screenprints), cardigans in earth tones, pullover sweaters, \
flannels over a tee. Hair varies slightly. They're recognizable as the \
SAME PERSON in different moods / outfits, NOT a uniformed assembly. \
\
EACH JEFFREY IS BUSY WITH MULTIPLE DEVICES — laptops are PLAIN: each \
station shows EITHER a plain green chartreuse Apple MacBook Neo (with \
the standard glowing white Apple logo on the lid — NO custom logos, NO \
butterflies, NO stickers) OR a plain black ThinkPad (small red \
TrackPoint nub between G/H/B; ThinkPad logo lower-right; no stickers). \
Vary which jeffrey carries which — about half-and-half across the \
frame. PLUS each station also has 1–2 of these EXTRAS varying across \
the ensemble: a second laptop on the bench, a smartphone in hand, a \
fountain pen mid-write on a notebook, a clipboard of papers, an iPad on \
a stand, a small handheld game device, a vintage rolltop notebook, a \
coffee mug, a synth keyboard, headphones around the neck, a microphone \
arm clamped to a worktable. Some jeffreys have a laptop ON THEIR LAP \
on the bench, others lean over a worktable. Variety — not every station \
identical. The vibe is "everyone has more than they can do, and the \
train hasn't even left yet." \
\
Each jeffrey is in a small "I have an update to share!" pose — variety \
of micro-expressions and gestures, all chest-level: one leaning back on \
a bench gesturing at his screen, one turning to a neighbor with a \
finger raised mid-explanation, one holding up a paper at his benchmate, \
one mid-laugh pointing at his laptop, one typing intently and grinning, \
one writing in a notebook with the fountain pen, one talking on the \
phone with their hand cupped over the mouthpiece, one in a small \
mock-shock oh-face, one nodding along with headphones, one stretched \
out across a bench using his laptop on a folded jacket as a pillow. \
Most face toward the camera or toward a neighbor. None directly above \
another's head. \
\
EXACTLY ONE of the 24 jeffreys (just one — somewhere in the middle of \
the frame, not on the edges) is mid-puff on his vape — a small \
USB-stick-shaped pen visible in his hand with its tiny LED tip glowing. \
A single natural wisp of vape vapor curls up from his station, catching \
one of the light shafts from the arched windows. NOT heavy fog, NOT \
multiple vapers — just one. Other jeffreys may have their vape pens \
visible on the bench beside their laptop but only the one is actively \
puffing. \
\
LIGHTING: the late-afternoon GOLDEN-HOUR sun rakes through the row of \
tall arched windows from one side, casting long warm-cream shafts of \
light across the marble floor and the wooden benches, picking up dust \
motes and the wisp of vape vapor. A row of vintage Edison-bulb \
PENDANT LAMPS hangs at intervals from the vaulted ceiling, just \
beginning to glow as evening approaches. The flip-board on the back \
wall has its own internal warm white backlight. The contrast is \
strong — bright golden window-shafts and pools of pendant warmth, \
interspersed with cooler shadow zones. Cinematic-but-candid \
layered colored-pencil depth, paper grain visible, not flat. \
\
Render in colored-pencil and gouache on warm cream paper, Hockney \
register — confident hatching, tapered edges, optical mixing. NOT \
photographic, NOT cinematic-glossy, NOT a uniform / team-photo. \
Vertical 1024x1536 portrait orientation. \
\
COMPOSITION (the title text overlays the top of the frame, and chrome \
overlays the bottom): \
The TOP ~35% (y=0..540 of the 1024x1536 photo) is QUIET BACKGROUND \
ONLY — coffered ceiling, vault arches, upper portion of arched windows \
with golden sky beyond, wrought-iron mezzanine railing in soft focus. \
NO JEFFREYS, NO LAPTOPS in this top zone. The first / topmost row of \
jeffreys begins at y≈600 of the photo, not earlier. The 24-jeffrey \
ensemble lives in the central band (y≈600..1100). The BOTTOM ~30% \
(y=1100..1536) shows the marble floor with golden light shafts \
fading into open foreground — no jeffreys cropped at the bottom edge. \
The very bottom band (y=1460..1536) is QUIET marble floor with light \
patterning only — the rendered chrome (subs / waveform / piano / pals \
/ progress) overlays here at composite time.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/01_title.png" }, cv: { json: "recap/out/cv/01_title.json" }, qr: { glob: "recap/out/qr/01_title.png" } },
      body: ({ photo }) => titleSlide({
        photo,
        color: colorAddress("mintcream"),
      }),
    },

    "02_verovio_sheet": {
      colorAddress: colorAddress("aqua"),
      metaphor: `${REAL}

Scene: morning at a wooden home desk. He sits at the desk in front of \
the closed-lid chartreuse Apple MacBook Neo (back of the lid facing the \
camera, plain Apple logo). His hands are spread mid-chord on a small \
analog keyboard / melodica resting next to the laptop on the desk — \
fingers held in a clear three-note chord shape. He's leaning forward, \
head tipped slightly, mouth open in a small surprised "oh that's nice" \
grin, eyebrows up. NO screens visible, NO popovers, NO printed pages, \
NO musical-notation paper — only jeffrey, the closed laptop, and the \
small physical keyboard. Diegetic aqua (#00ffff) light: a small \
aqua-shaded glass desk lamp on the right edge of the desk pours cool \
aqua across the right side of his face. The rest of the room is warm \
morning daylight through a window on the left. A single coffee mug to \
the left, half full. Hoodie open over a tee. Shirt word: \`notepat\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/02_verovio_sheet.png" }, cv: { json: "recap/out/cv/02_verovio_sheet.json" }, qr: { glob: "recap/out/qr/02_verovio_sheet.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        commit: "78d571b5a",
        title: "live staff\ninside the\npopover",
        cap: "verovio engraves the held chord · qwerty shortcuts shuffled around it",
        color: colorAddress("aqua"),
      }),
    },

    "03_voice_glass": {
      colorAddress: colorAddress("mediumvioletred"),
      metaphor: `${REAL}

Scene: late morning at the same home desk. The closed-lid chartreuse \
Apple MacBook Neo sits on the desk (back of the lid facing camera, \
plain Apple logo). He is leaning back in his chair with one hand \
raised at chest level, finger pointed sideways in a knowing \
"mock-detective I-see-you" gesture, eyebrows raised, closed-mouth \
half-smile. NO screens visible, NO popovers, NO interface elements \
anywhere in the frame, NO printed pages, NO posters with text. \
Diegetic mediumvioletred (#c71585): a deep magenta-violet glass \
pendant lamp hangs from the ceiling above the desk, glowing internally \
through the colored glass and bathing the back wall and the right \
side of his face in saturated magenta. The rest of the room is dim \
and lamp-warm. Hoodie. Shirt word: \`tone\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/03_voice_glass.png" }, cv: { json: "recap/out/cv/03_voice_glass.json" }, qr: { glob: "recap/out/qr/03_voice_glass.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        commit: "fc687c3ec",
        title: "glass tints\nto the voice,\ndrawer hint",
        cap: "popover material reads the instrument · expand affordance under the strip",
        color: colorAddress("mediumvioletred"),
      }),
    },

    "04_compact_menubar": {
      colorAddress: colorAddress("orangered"),
      metaphor: `${REAL}

Scene: late afternoon at his home desk. The Citrus-green MacBook Neo \
(Apple's bright yellow-green color) is open and the very top of the screen \
is the focal point: the macOS menubar is visible, and the menuband chip \
sitting in it is COMPACT — a smaller version of itself, with the rightmost \
piano keys clearly trimmed off so the band fits a crowded menubar without \
elbowing other items. Other menubar icons (a clock, a wifi, a battery \
glyph) are clearly squished tight against it on the right. He is leaning \
back in his chair pinching his thumb and forefinger together in a "just \
this much shorter" gesture at chest level, mouth open in a hyperbolic \
"AH-HA!" of relief, eyes wide, eyebrows up — the expression of someone who \
just stopped a thing from yelling at them. Diegetic orangered (#ff4500): \
a tall floor lamp behind him with an orangered translucent shade casts a \
hot orange-red wash across the left side of the room and onto the back of \
his chair; the laptop screen reflects orangered onto his glasses. Late \
afternoon, real lamp light. Cardigan over a tee. Shirt word: \`clock\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/04_compact_menubar.png" }, cv: { json: "recap/out/cv/04_compact_menubar.json" }, qr: { glob: "recap/out/qr/04_compact_menubar.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "trims keys\nfrom the right,\nstops yelling",
        cap: "silent retries · debounced fit-alert · compact behaves",
        color: colorAddress("orangered"),
      }),
    },

    "05_menuband_polish": {
      colorAddress: colorAddress("darkorange"),
      metaphor: `${REAL}

Scene: golden-hour afternoon at his home desk. The Citrus-green MacBook \
Neo (Apple's bright yellow-green color) is open. A small floating piano \
panel is visible just below the menubar — clearly its own window, with \
its own little keyboard-shortcut row at the bottom. The popover at the \
top of the screen shows a header line that reads "OCT 4" in clean small \
caps, an audio mute icon clearly differentiated by a slash and a dimmed \
state, and a small tooltip floating above one key reading "voice 03 · \
sine bells." He is sitting back, hands clasped behind his head at chest height (NOT \
above shoulder level — elbows wide but hands stay low), head tilted in a \
super-smug "I know the manual" closed-mouth grin, eyebrows arched. \
Diegetic darkorange (#ff8c00): the late-afternoon sun coming through a \
west-facing window throws a bright darkorange wedge across the wall \
behind him and onto the right side of his desk. Real golden hour, no \
overlay. Open flannel over a tee. Shirt word: \`chord\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/05_menuband_polish.png" }, cv: { json: "recap/out/cv/05_menuband_polish.json" }, qr: { glob: "recap/out/qr/05_menuband_polish.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "octave header,\nmute clarity,\ntooltips,\nlocale",
        cap: "floating piano keeps its own keys · haptics hide gracefully",
        color: colorAddress("darkorange"),
      }),
    },

    "06_release_093": {
      colorAddress: colorAddress("deeppink"),
      metaphor: `${REAL}

Scene: very late night / pre-dawn at his home desk, lit only by a \
single accent lamp. The closed-lid chartreuse Apple MacBook Neo sits \
on the desk (back of the lid facing camera, plain Apple logo). He is \
leaning forward over it with both fists pumped at chest height (HANDS \
BELOW SHOULDERS), mouth wide open in a HUGE proud laugh, eyes wide \
and bright, eyebrows up — pure mock-celebration energy. NO screens \
visible, NO printed cards, NO posters, NO neon signage with text. \
Diegetic deeppink (#ff1493): a hot-pink LED bias strip on the wall \
behind him spills onto the wall in a soft halo (no signs / no \
lettering anywhere on it). The room is dark otherwise. Hoodie, \
slightly bedheaded. Shirt word: \`stamp\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/06_release_093.png" }, cv: { json: "recap/out/cv/06_release_093.json" }, qr: { glob: "recap/out/qr/06_release_093.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        commit: "264c8d2fc",
        title: "0.9.3 signed,\nnotarized,\nstapled",
        cap: "info-plist 0.9.3 · dmg synced to assets.aesthetic.computer",
        color: colorAddress("deeppink"),
      }),
    },

    "07_waveform_overlay": {
      colorAddress: colorAddress("mediumturquoise"),
      metaphor: `${REAL}

Scene: late evening at the desk. The Citrus-green MacBook Neo (Apple's \
bright yellow-green color) is open and the menuband popover is expanded — \
a tall version of itself with the waveform strip enlarged across the \
middle and a row of refinement controls below it (sliders, segmented \
toggles, a tiny VU meter). Each control is spaced calmly, breathing room \
between them. The waveform itself shows a clean sine-bell trace in \
mediumturquoise. He is leaning forward, one finger gently dragging a \
slider mid-adjust, mouth pursed in a small focused "mmhm" expression, \
eyes narrowed in concentration, a small satisfied half-smile creeping in. \
Diegetic mediumturquoise (#48d1cc): a small mediumturquoise glass-bottle \
desk lamp on the left edge of the desk glows from within, throwing cool \
turquoise across the left half of his face and the keyboard. The right \
side of the room is warm room-lamp tone. Cardigan. Shirt word: \`wave\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/07_waveform_overlay.png" }, cv: { json: "recap/out/cv/07_waveform_overlay.json" }, qr: { glob: "recap/out/qr/07_waveform_overlay.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "expanded\nwaveform\ncontrols,\ncalmer",
        cap: "two refinement passes · better spacing · steadier under load",
        color: colorAddress("mediumturquoise"),
      }),
    },

    "08_calarts_dossier": {
      colorAddress: colorAddress("indigo"),
      metaphor: `${REAL}

Scene: late-night writing setup. He is at a home desk with ONE printed \
page held up in his hands — that page bears the actual CALARTS-CCAT \
HEADER ILLUSTRATION from the reference image (passed as a ref titled \
"header.png"; render it FAITHFULLY in colored-pencil and gouache, do \
NOT fabricate a different illustration). The page is the only printed \
artifact in shot — no other papers, no posters, no extra prints. The \
chartreuse Apple MacBook Neo sits closed-lid on the desk to his right, \
back of the lid (Apple logo) facing the camera. He is holding the \
illustrated page up at chest height with both hands, head turned \
three-quarters to the camera in a super-smug professor "actually-" \
pose: mouth open in a deeply pleased "ehhh-yes" grin, eyebrows \
arched. Diegetic indigo (#4b0082): a deep-indigo glass-shaded \
incandescent desk lamp with the bulb glowing through the colored \
glass, throwing indigo cast across the page and onto his face from \
the side. The rest of the room is dim. Soft button-down with the \
word \`word\` embroidered in muted thread on the chest pocket — \
quiet, scholarly placement.`,
      // Real calarts header illustration baked into the scene as the
      // page jeffrey is holding up — colored-pencil register matches.
      extraRefs: [
        "system/public/calarts-ccat-2026/header.png",
      ],
      queries: { photo: { glob: "recap/out/jeffrey-photos/08_calarts_dossier.png" }, cv: { json: "recap/out/cv/08_calarts_dossier.json" }, qr: { glob: "recap/out/qr/08_calarts_dossier.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        commit: "35bcff7e7",
        title: "calarts ccat,\nheader, sage,\ncv pass",
        cap: "13 yrs teaching · giphy + linked-by-air surfaced · kadist + smk reframed",
        color: colorAddress("indigo"),
      }),
    },

    "09_marketing_graduation": {
      colorAddress: colorAddress("chartreuse"),
      metaphor: `${REAL}

Scene: bright midday at his home desk. The closed-lid chartreuse \
Apple MacBook Neo sits on the desk (back of the lid facing camera, \
plain Apple logo). He is at the desk with both hands gestured wide \
at chest level — the universal "behold" pose without raising arms \
above shoulders — mouth open in a hyperbolic "AH-HA, IT'S HERE!" \
laugh, eyes wide, eyebrows up. NO screens visible, NO printed photo \
prints, NO folders with text labels, NO physical printouts of any \
kind. Just jeffrey + the closed laptop on a clean wooden desk. \
Diegetic chartreuse (#7fff00): a bright chartreuse acrylic-shaded \
gooseneck desk lamp arched over the desk pours acid-green light \
across the laptop lid and the right side of his face. Daylight from \
a window on the left balances it cool. Hoodie open over a tee. \
Shirt word: \`share\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/09_marketing_graduation.png" }, cv: { json: "recap/out/cv/09_marketing_graduation.json" }, qr: { glob: "recap/out/qr/09_marketing_graduation.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        commit: "e4fe2df07",
        title: "marketing/\ngraduates\nfrom desktop",
        cap: "gen-promo · jeffrey-refs · ac-native captures · two seed campaigns",
        color: colorAddress("chartreuse"),
      }),
    },

    "10_vfc_piece": {
      colorAddress: colorAddress("blueviolet"),
      metaphor: `${REAL}

Scene: evening at his home desk. The chartreuse Apple MacBook Neo \
sits closed-lid in front of him on the desk (back of lid facing the \
camera, plain Apple logo on the cream-paper-rendered chartreuse). On \
the desk to the LEFT of the laptop, propped up against a small wooden \
prop-block: the actual VFC 2026 painting from the AUCTION REFERENCE \
IMAGE — render this drawing FAITHFULLY from the reference (it is \
passed to you as a ref image titled "25.4.13.19.24.webp"). The \
painting carries jeffrey's hand-drawn small QR code in its top-right \
corner. He is leaning forward toward the propped painting, one finger \
raised at chest level pointing at it, head tilted, mouth open in a \
delighted "look look look" grin, eyebrows up, eyes bright. Diegetic \
blueviolet (#8a2be2): a tall blueviolet column-shaped lamp behind \
him glows deep purple-violet, washing the wall and the back of his \
head in saturated blueviolet. Evening lamp light only. Sweater. \
Shirt word: \`paint\`.`,
      // The actual VFC painting (25.4.13.19.24.webp) is passed to
      // gpt-image-2 as an extra ref image so the model BAKES the real
      // drawing into the scene (rendered in colored-pencil style),
      // rather than the slide superimposing it as a CSS inset.
      extraRefs: [
        "system/public/assets/jeffreys/vfc-2026/25.4.13.19.24.webp",
      ],
      queries: {
        photo: { glob: "recap/out/jeffrey-photos/10_vfc_piece.png" },
        cv:    { json: "recap/out/cv/10_vfc_piece.json" },
        qr:    { glob: "recap/out/qr/10_vfc_piece.png" },
      },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        commit: "c07a5f9d1",
        title: "the qr now\nresolves —\nthe painting\nbreathes",
        cap: "25.4.13.19.24 · scale pulse + lissajous + sparkles · tap → vfc auction lot",
        color: colorAddress("blueviolet"),
      }),
    },

    "11a_chat_clock": {
      colorAddress: colorAddress("khaki"),
      chatInstance: "clock",
      metaphor: `${REAL}

Scene: this is a LAPTOP-SERIES FIRST-PERSON SHOT — the camera is where \
the laptop's webcam would be, looking UP at jeffrey from chest height, \
screen-side perspective. He is at a home desk facing the laptop, eyes \
flicking down at the screen reading a chat. The Citrus-green MacBook \
Neo's bottom bezel edge is just visible at the FOOT of the frame, with \
the AC's smiley bug logo peeking up. He is mid-read, head slightly \
tilted, a small genuine smile forming — the kind of expression you \
have when a friend across the world is saying something funny in \
another language. His mouth is slightly open like he's about to \
half-laugh, eyebrows raised in mild surprise. Diegetic khaki \
(#f0e68c) light: a small khaki-fabric-shaded reading lamp on the left \
edge of his desk pours warm khaki light across the left side of his \
face and catches the rim of a coffee mug just out of focus on the \
desk surface. The right side of his face is in cooler ambient room \
light. Hoodie, slightly bedheaded. Shirt word: \`chat\`. \
\
COMPOSITION: head and shoulders fill the upper-middle band of the \
frame; his face is centered horizontally with his eyes near y=750 of \
the 1024x1536 photo. The TOP 25% (y=0..380) is quiet ceiling / wall — \
the chapter title overlays there. The BOTTOM 25% (y=1150..1536) is \
the desk surface and the peeking bottom edge of the green Neo's bezel.`,
      queries: {
        photo: { glob: "recap/out/jeffrey-photos/11a_chat_clock.png" },
        snapshot: { json: "recap/out/chat-snapshot.json" },
      },
      body: ({ photo, snapshot }) => chatSlide({
        photo,
        instance: "clock",
        messages: (snapshot && snapshot.clock) || [],
        title: "from the\nclock chat",
        cap: "in danish, mostly · last 24 hours",
        color: colorAddress("khaki"),
      }),
    },

    "11b_chat_system": {
      colorAddress: colorAddress("gold"),
      chatInstance: "system",
      metaphor: `${REAL}

Scene: this is a LAPTOP-SERIES FIRST-PERSON SHOT — the camera is where \
the laptop's webcam would be, looking UP at jeffrey from chest height, \
screen-side perspective. He is at the same home desk facing the \
laptop, but now mid-laugh at something on the main chat — a hyperbolic \
open-mouth laugh, head tipped slightly back, eyebrows up, eyes \
crinkled. The Citrus-green MacBook Neo's bottom bezel edge is just \
visible at the FOOT of the frame, with the smiley bug logo peeking up. \
Diegetic gold (#ffd700) light: a small gold-shaded brass desk lamp on \
the right edge of the desk pours warm gold across the right side of \
his face — slightly different angle from the clock-chat scene so the \
two paired portraits read as a matched pair (left lamp / right lamp). \
A faint puff of vape vapor curls in from the right margin (he just \
put the USB-stick pen down, its tiny LED still glowing on the desk \
just out of focus). Hoodie. Shirt word: \`chat\`. Same late-afternoon \
vibe as 11a — these two slides are the same minute of the same \
afternoon, just two different chats. \
\
COMPOSITION: head and shoulders fill the upper-middle band of the \
frame; face centered horizontally with eyes near y=750 of the \
1024x1536 photo. TOP 25% quiet ceiling / wall (chapter title overlays \
there). BOTTOM 25% shows the desk surface and the peeking bottom edge \
of the green Neo's bezel.`,
      queries: {
        photo: { glob: "recap/out/jeffrey-photos/11b_chat_system.png" },
        snapshot: { json: "recap/out/chat-snapshot.json" },
      },
      body: ({ photo, snapshot }) => chatSlide({
        photo,
        instance: "system",
        messages: (snapshot && snapshot.system) || [],
        title: "from the\nmain thread",
        cap: "main chat · last 24 hours",
        color: colorAddress("gold"),
      }),
    },

    "12_outro": {
      colorAddress: colorAddress("lavenderblush"),
      metaphor: `${REAL}

Scene: he is leaning back in a desk chair holding a coffee mug, giving a \
small relaxed wave to the camera with his free hand. The Citrus-green \
laptop in front of him on the desk shows a screen reading "thanks for \
watching" in calm cream-on-dark text with a thin lavenderblush (#fff0f5) \
underline. A lavenderblush curtain is visible behind him, softly catching \
evening light. He has a small real smile, eye contact with the camera. \
Comfortable home setting — desk lamp on, warm white balance, a houseplant \
or two visible. Colored-pencil drawing on cream paper. He's in a relaxed \
button-down with \`prompt\` embroidered in tiny white stitch on the cuff \
of his sleeve — visible only because his wrist is raised in the wave.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/12_outro.png" }, cv: { json: "recap/out/cv/12_outro.json" }, qr: { glob: "recap/out/qr/12_outro.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "thanks for\nwatching",
        cap: "aesthetic.computer · @jeffrey",
        color: colorAddress("lavenderblush"),
      }),
    },

    "13_end": `
      <div class="frame">
        <div class="pals med"></div>
        <div class="endline" style="color:${PALETTE.cream}">aesthetic·computer</div>
        <div class="endsub" style="color:${PALETTE.dim}">narrated by jeffrey-pvc · @jeffrey</div>
        <div class="endsub" style="color:${PALETTE.dim}">2026·05·08</div>
      </div>`,
  },
};

// Episode metadata. Bump EPISODE for each new audience config.
// 24h-2026-04-30 was ep 1; 73h-2026-05-02 was ep 2; this is ep 3.
const EPISODE = 3;
const SHOW_NAME = "aesthetic 24";
const EPISODE_DATE = "2026·05·08";
const EPISODE_HOOK = "the last\n24 hours";

function slideCode(color) {
  return `e${EPISODE}-${color.name}`;
}

function readabilityShadow(rgb) {
  const luma = 0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2];
  const dark = "rgba(0,0,0,0.95)";
  const light = "rgba(255,255,255,0.95)";
  const blur = luma > 140 ? "rgba(0,0,0,0.55)" : "rgba(255,255,255,0.55)";
  const sharp = luma > 140 ? dark : light;
  return `2px 2px 0 ${sharp}, -1px -1px 0 ${sharp}, 0 0 18px ${blur}`;
}

function titleSlide({ photo, color }) {
  const code = slideCode(color);
  const codeShadow = readabilityShadow(color.rgb);
  const creamShadow = "2px 2px 0 rgba(0,0,0,0.95), -1px -1px 0 rgba(0,0,0,0.7), 0 0 18px rgba(0,0,0,0.55)";
  const subShadow = "1px 1px 0 rgba(0,0,0,0.92), 0 0 14px rgba(0,0,0,0.6)";
  const hookHtml = EPISODE_HOOK.split("\n").map((l) => `<div>${l}</div>`).join("");
  // Same chromatic shadow stack as photoSlide so the title carries
  // the matching watermark register.
  const c = color.hex;
  const titlePalsShadow = [
    `drop-shadow(3px 3px 0 ${c})`,
    `drop-shadow(-3px 3px 0 rgba(80,200,255,0.85))`,
    `drop-shadow(0 -3px 0 rgba(255,220,80,0.85))`,
    `drop-shadow(6px 0 0 ${c}88)`,
    `drop-shadow(-6px 0 0 rgba(80,255,160,0.55))`,
    `drop-shadow(0 8px 22px rgba(0,0,0,0.5))`,
  ].join(" ");
  return `
    <div style="position: fixed; inset: 0; padding: 0; overflow: hidden;">
      ${photo
        ? `<img src="${photo}" style="position: absolute; inset: 0; width: 100%; height: 100%; object-fit: cover;" />`
        : `<div style="position: absolute; inset: 0; background: ${PALETTE.bg};"></div>`}
      <div style="position: absolute; top: 0; left: 0; right: 0; padding: 130px 70px 80px; background: linear-gradient(to bottom, rgba(0,0,0,0.86) 0%, rgba(0,0,0,0.5) 60%, rgba(0,0,0,0) 100%);">
        <div style="font-family: 'ProcessingB'; font-size: 38px; letter-spacing: 2px; color: ${color.hex}; text-shadow: ${codeShadow};">${code}</div>
        <div style="font-family: 'ProcessingB'; font-size: 64px; letter-spacing: 6px; color: ${PALETTE.cream}; margin-top: 22px; text-shadow: ${creamShadow};">${SHOW_NAME}</div>
        <div style="font-family: 'ProcessingR'; font-size: 30px; letter-spacing: 5px; color: ${PALETTE.off}; margin-top: 10px; text-shadow: ${subShadow};">ep ${String(EPISODE).padStart(2, "0")} · ${EPISODE_DATE}</div>
        <div style="font-family: 'ProcessingB'; font-size: 130px; line-height: 0.96; letter-spacing: -3px; color: ${PALETTE.cream}; margin-top: 36px; text-shadow: ${creamShadow};">${hookHtml}</div>
      </div>
      <div style="position: absolute; bottom: 0; left: 0; right: 0; height: 220px; background: linear-gradient(to top, rgba(0,0,0,0.7) 0%, rgba(0,0,0,0.2) 60%, rgba(0,0,0,0) 100%);"></div>
      <!-- Side PALS watermarks — match photoSlide's left/right pair. -->
      <img src="${PALS_SVG_DATA_URL}" alt="" style="position: absolute; left: -40px; top: 760px; width: 220px; height: 220px; opacity: 0.28; transform: rotate(-90deg); transform-origin: center; filter: ${titlePalsShadow};" />
      <img src="${PALS_SVG_DATA_URL}" alt="" style="position: absolute; right: -40px; top: 760px; width: 220px; height: 220px; opacity: 0.28; transform: rotate(90deg); transform-origin: center; filter: ${titlePalsShadow};" />
    </div>`;
}

// ──────────────────────────────────────────────────────────────────────
// LAYOUT SOLVER
// ──────────────────────────────────────────────────────────────────────
// The cv json (recap/out/cv/<seg>.json) carries face + laptop + shirtLogos
// bboxes IN SOURCE PHOTO COORDS (1024×1536). The slide renders into
// 1080×1920 with object-fit: cover, so source coords scale by 1.25 and
// shift x by -100. cvBoxToFrame() does that translation.
//
// solveLayout(cv) picks a chrome placement per segment by scoring
// candidate rectangles against the avoid-zones (face HARD, laptop HARD,
// shirt logos SOFT). Returns:
//   { chapter: {mode, css, rect}, subtitle: {y, rect}, piano: {x, y, hidden, rect} }
// where each rect is in FRAME coords so callers can chain-avoid each
// other (subs avoid chapter rect; piano avoids both).
const FRAME_W = 1080;
const FRAME_H = 1920;

function cvBoxToFrame(box) {
  if (!box) return null;
  const SCALE = 1920 / 1536;
  const X_SHIFT = -100; // (1080 - 1024*SCALE) / 2 = -100
  const [x, y, w, h] = box;
  return {
    x0: x * SCALE + X_SHIFT,
    y0: y * SCALE,
    x1: (x + w) * SCALE + X_SHIFT,
    y1: (y + h) * SCALE,
  };
}

function rectsOverlap(a, b) {
  if (!a || !b) return 0;
  const ix = Math.max(0, Math.min(a.x1, b.x1) - Math.max(a.x0, b.x0));
  const iy = Math.max(0, Math.min(a.y1, b.y1) - Math.max(a.y0, b.y0));
  return ix * iy;
}

// Candidate rectangles for each chrome element. Coords are FRAME (1080×1920).
// chapter title block ≈ 480 tall (chapter line + title up to 4 lines + cap)
const CHAPTER_CANDS = [
  { mode: "top-full",     rect: { x0:  70, y0: 130, x1: 1010, y1: 610 } },
  { mode: "top-left",     rect: { x0:  70, y0: 130, x1:  560, y1: 610 } },
  { mode: "top-right",    rect: { x0: 520, y0: 130, x1: 1010, y1: 610 } },
  { mode: "bottom-full",  rect: { x0:  70, y0: 920, x1: 1010, y1: 1400 } },
  { mode: "bottom-left",  rect: { x0:  70, y0: 920, x1:  560, y1: 1400 } },
  { mode: "bottom-right", rect: { x0: 520, y0: 920, x1: 1010, y1: 1400 } },
];

// Subtitle pill (1080 wide × 160 tall, full-width, varies y).
const SUBTITLE_CANDS = [
  { y: 1480, rect: { x0: 0, y0: 1480, x1: FRAME_W, y1: 1640 } }, // default bottom
  { y:  180, rect: { x0: 0, y0:  180, x1: FRAME_W, y1:  340 } }, // upper-third
  { y:  640, rect: { x0: 0, y0:  640, x1: FRAME_W, y1:  800 } }, // mid
  { y:  900, rect: { x0: 0, y0:  900, x1: FRAME_W, y1: 1060 } }, // lower-mid
];

// Piano roll: full-width, flush to the bottom of the frame. Progress
// bar draws ON TOP of the keys (handled in build-filter.mjs).
const PIANO_CANDS = [
  { x: 0, y: 1830, rect: { x0: 0, y0: 1830, x1: 1080, y1: 1920 } },
];

function scoreRect(rect, avoid) {
  // Penalty proportional to overlap area, weighted by avoid kind.
  // face = strong (the user's #1 ask); laptop = strong; shirt = soft;
  // already-placed chrome = strongest (collision is the worst result).
  let pen = 0;
  for (const a of avoid) {
    const ov = rectsOverlap(rect, a.rect);
    pen += ov * a.weight;
  }
  return pen;
}

export function solveLayout(cv) {
  const face   = cvBoxToFrame(cv && cv.face);
  const laptop = cvBoxToFrame(cv && cv.laptop);
  const shirts = (cv && cv.shirtLogos || []).map(cvBoxToFrame).filter(Boolean);

  // Static slide-baked elements that chapter / subs / piano must also
  // route around: PALS bug top-left, QR code top-right.
  const palsRect = { x0:  60, y0:  60, x1:  260, y1:  260 };
  const qrRect   = { x0: 820, y0:  60, x1: 1020, y1:  360 };
  const avoidPhotoSubjects = [
    ...(face   ? [{ rect: face,   weight: 100 }] : []),
    ...(laptop ? [{ rect: laptop, weight:  80 }] : []), // never on the back of the laptop
    ...shirts.map((r) => ({ rect: r, weight: 20 })),
    { rect: palsRect, weight: 60 }, // chrome carries through every slide
    { rect: qrRect,   weight: 60 },
  ];

  // 1) Chapter — score each candidate, pick min.
  const chapterScored = CHAPTER_CANDS.map((c) => ({
    ...c,
    score: scoreRect(c.rect, avoidPhotoSubjects),
  })).sort((a, b) => a.score - b.score);
  const chapter = chapterScored[0];

  // 2) Subtitle — must avoid photo subjects AND the chapter rect.
  const avoidWithChapter = [
    ...avoidPhotoSubjects,
    { rect: chapter.rect, weight: 200 },
  ];
  const subtitleScored = SUBTITLE_CANDS.map((s) => ({
    ...s,
    score: scoreRect(s.rect, avoidWithChapter),
  })).sort((a, b) => a.score - b.score);
  const subtitle = subtitleScored[0];

  // 3) Piano — must avoid photo subjects + chapter + subtitle.
  const avoidWithSub = [
    ...avoidWithChapter,
    { rect: subtitle.rect, weight: 200 },
  ];
  const pianoScored = PIANO_CANDS.map((p) => ({
    ...p,
    score: scoreRect(p.rect, avoidWithSub),
  })).sort((a, b) => a.score - b.score);
  // If even the best piano option still overlaps face or laptop hard,
  // hide it for that segment — better than painting over a shirt.
  const piano = pianoScored[0].score > 6000 // ~30×200 hard collision
    ? { x: 40, y: 1760, hidden: true, rect: pianoScored[0].rect }
    : { ...pianoScored[0], hidden: false };

  return { chapter, subtitle, piano };
}

function photoSlide({ photo, title, cap, color, qr, commit }) {
  // Chapter prompt — top-left, fixed across slides. Styled like a command
  // typed at the AC prompt: cream "aesthetic.computer>" prefix in cream
  // followed by the chapter title in the chapter color, blinky-cursor
  // suffix. Same vertical strip carries the PALS bug below the title.
  const titleHtml = (title || "")
    .split("\n")
    .map((l) => `<span class="cmd-line">${l}</span>`)
    .join("");
  const creamShadow = "2px 2px 0 rgba(0,0,0,0.95), -1px -1px 0 rgba(0,0,0,0.7), 0 0 18px rgba(0,0,0,0.55)";
  const capShadow = "1px 1px 0 rgba(0,0,0,0.92), 0 0 14px rgba(0,0,0,0.6)";
  const promptShadow = "1px 1px 0 rgba(0,0,0,0.95), 0 0 12px rgba(0,0,0,0.6)";

  // PALS bug — top-LEFT, BELOW the chapter prompt. Rainbow drop shadow
  // tinted around the chapter color so each slide's branding takes a
  // diegetic color cue. The shadow stack is a multi-step soft offset.
  const palsSvg = `${PALS_SVG_DATA_URL}`;
  const c = color.hex;
  const palsShadow = [
    `drop-shadow(3px 3px 0 ${c})`,
    `drop-shadow(-3px 3px 0 rgba(80,200,255,0.85))`,
    `drop-shadow(0 -3px 0 rgba(255,220,80,0.85))`,
    `drop-shadow(6px 0 0 ${c}88)`,
    `drop-shadow(-6px 0 0 rgba(80,255,160,0.55))`,
    `drop-shadow(0 8px 22px rgba(0,0,0,0.5))`,
  ].join(" ");

  // QR — minimal: QR pixel grid + the short commit hash in a larger,
  // more-readable monospace beneath it. No big cream pill border.
  const qrBlock = qr
    ? `<div style="position: absolute; right: 50px; top: 50px; width: 200px;">
         <img src="${qr}" style="display:block; width:100%; height:auto; image-rendering: pixelated; border-radius: 4px;" />
         ${commit ? `<div style="font-family: 'ProcessingB'; font-size: 36px; letter-spacing: 1px; color: ${PALETTE.cream}; margin-top: 12px; text-align: center; text-shadow: ${capShadow};">${commit}</div>` : ""}
       </div>`
    : "";

  return `
    <div style="position: fixed; inset: 0; padding: 0; overflow: hidden;">
      ${photo
        ? `<img src="${photo}" style="position: absolute; inset: 0; width: 100%; height: 100%; object-fit: cover;" />`
        : `<div style="position: absolute; inset: 0; background: ${PALETTE.bg};"></div>`}
      <!-- Top-left scrim so the prompt + PALS read against any photo background -->
      <div style="position: absolute; top: 0; left: 0; width: 720px; height: 980px; background: linear-gradient(135deg, rgba(0,0,0,0.78) 0%, rgba(0,0,0,0.5) 40%, rgba(0,0,0,0) 70%); pointer-events: none;"></div>
      <!-- Chapter prompt, top-left, prompt-styled. Wordmark renders as
           "Aesthetic.Computer>" with letters in deep purple and the dot
           in hot pink — same color treatment as the AC prompt itself. -->
      <div style="position: absolute; left: 60px; top: 50px; max-width: 760px; font-family: 'ProcessingB'; line-height: 1.04;">
        <div style="font-size: 36px; letter-spacing: 2px; text-shadow: ${promptShadow};">
          <span style="color: #6b2b9c;">Aesthetic</span><span style="color: #ff1493;">.</span><span style="color: #6b2b9c;">Computer</span>
        </div>
        <div style="font-size: 88px; letter-spacing: -3px; color: ${color.hex}; margin-top: 8px; text-shadow: ${creamShadow}; display: flex; flex-direction: column; gap: 4px;">${titleHtml}</div>
        ${cap ? `<div style="font-family: 'ProcessingR'; font-size: 28px; color: ${PALETTE.off}; margin-top: 18px; letter-spacing: 1px; text-shadow: ${capShadow}; max-width: 720px;">${cap}</div>` : ""}
      </div>
      <!-- PALS watermarks — TWO instances, each rotated 90° and pinned
           to the left / right edge of the frame. Low opacity reads as
           a soft watermark. Both carry the same chapter-color rainbow
           drop shadow so the chromatic register reinforces the slide's
           color story without clobbering the photo. -->
      <img src="${palsSvg}" alt="" style="position: absolute; left: -40px; top: 760px; width: 220px; height: 220px; opacity: 0.28; transform: rotate(-90deg); transform-origin: center; filter: ${palsShadow};" />
      <img src="${palsSvg}" alt="" style="position: absolute; right: -40px; top: 760px; width: 220px; height: 220px; opacity: 0.28; transform: rotate(90deg); transform-origin: center; filter: ${palsShadow};" />
      ${qrBlock}
    </div>`;
}

function chatSlide({ photo, instance, messages, title, cap, color }) {
  const code = slideCode(color);
  const codeShadow = readabilityShadow(color.rgb);
  const creamShadow = "2px 2px 0 rgba(0,0,0,0.95), -1px -1px 0 rgba(0,0,0,0.7), 0 0 18px rgba(0,0,0,0.55)";
  const capShadow = "1px 1px 0 rgba(0,0,0,0.92), 0 0 14px rgba(0,0,0,0.6)";
  const titleHtml = (title || "").split("\n").map((l) => `<div>${l}</div>`).join("");
  const label = instance === "clock" ? "laer-klokken" : "chat";
  const fmt = (m) => `
    <div style="display:flex; gap: 12px; align-items: baseline; padding: 5px 0; border-bottom: 1px solid rgba(255,255,255,0.06);">
      <div style="font-family:'ProcessingB'; font-size: 22px; color: ${color.hex}; min-width: 150px; flex-shrink: 0;">${escape(m.handle || "anon")}</div>
      <div style="font-family:'ProcessingR'; font-size: 22px; color: ${PALETTE.cream}; line-height: 1.3; word-break: break-word;">${escape(m.text || "")}</div>
    </div>`;
  const list = (messages || []).slice(-10).map(fmt).join("")
    || `<div style="color:${PALETTE.dim}; font-size:22px;">(quiet day)</div>`;
  return `
    <div style="position: fixed; inset: 0; padding: 0; overflow: hidden;">
      ${photo
        ? `<img src="${photo}" style="position: absolute; inset: 0; width: 100%; height: 100%; object-fit: cover;" />`
        : `<div style="position: absolute; inset: 0; background: ${PALETTE.bg};"></div>`}
      <div style="position: absolute; top: 0; left: 0; right: 0; padding: 130px 70px 90px; background: linear-gradient(to bottom, rgba(0,0,0,0.78) 0%, rgba(0,0,0,0.4) 55%, rgba(0,0,0,0) 100%);">
        <div style="font-family: 'ProcessingB'; font-size: 38px; letter-spacing: 2px; color: ${color.hex}; text-shadow: ${codeShadow};">${code}</div>
        <div style="font-family: 'ProcessingB'; font-size: 110px; line-height: 1.0; letter-spacing: -2px; color: ${PALETTE.cream}; margin-top: 22px; text-shadow: ${creamShadow};">${titleHtml}</div>
        <div style="font-family: 'ProcessingR'; font-size: 38px; color: ${PALETTE.off}; margin-top: 26px; letter-spacing: 1px; text-shadow: ${capShadow};">${cap || ""}</div>
      </div>
      <div style="position: absolute; left: 70px; right: 70px; bottom: 280px; padding: 28px 32px 26px 28px; background: rgba(8, 12, 30, 0.78); border-left: 6px solid ${color.hex}; border-radius: 4px; backdrop-filter: blur(6px);">
        <div style="font-family:'ProcessingB'; font-size:28px; letter-spacing:4px; color:${color.hex}; text-transform:uppercase; margin-bottom: 16px;">${label}</div>
        ${list}
      </div>
    </div>`;
}

function escape(s) {
  return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

// Hard-fail at module load if any shirt word doesn't resolve at the AC
// prompt. This runs every time tts.mjs / jeffrey-photos.mjs / scout.mjs
// imports the audience config — so a bad word can never reach gpt-image-2.
validateShirtWords(audience.slides);

export default audience;
