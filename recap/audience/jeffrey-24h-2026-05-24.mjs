// Audience config: jeffrey-24h, 2026-05-24 cut (episode 4).
// Window: 2026-05-23 ~13:00 → 2026-05-24 ~12:00. The day reads as one
// long pivot DOWN INTO the audio stack: lib/pop.mjs jukeboxing the
// released pixsies singles in-browser, then drvkforlife.com handing
// off to Shopify, then a fresh C-DSP chain (pop/dsp + master.mjs),
// a Swift mascot recording wizard (wave-wizard) that produced a wall
// of new hellsine sample takes (cut-shakes), and a new marketing
// campaign — jeffrey fixing andreessen in an IG story.
//
// Inherits everything structural from the 73h-2026-05-02 + 24h-2026-05-08
// cuts:
//   - per-slide css color address from `cssColors` in lib/num.mjs
//   - DIEGETIC chapter color (real in-scene light source, never overlay)
//   - sine-bells waltz bed under narration (morphs across sections)
//   - editorial-only slide chrome (slide code + title + cap; no insets)
//   - luma-aware reverse-color drop shadows on type
//   - 24-jeffrey ensemble title motif (new venue per episode)
//   - shirt-word validation: each `Shirt word: \`foo\`` must resolve as
//     /disks/foo.{mjs,lisp} or the module load fails.
//
// Photos pre-baked by `bin/jeffrey-photos.mjs` into
// recap/out/jeffrey-photos/<segment>.png.

import { cssColors } from "../../system/public/aesthetic.computer/lib/num.mjs";
import { readdirSync, readFileSync } from "node:fs";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

const __PALS_SVG_PATH = `${dirname(fileURLToPath(import.meta.url))}/../../system/public/purple-pals.svg`;
const PALS_SVG_DATA_URL = "data:image/svg+xml;base64," + Buffer.from(readFileSync(__PALS_SVG_PATH)).toString("base64");

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
  const lift = (c) => Math.min(255, c + Math.floor((255 - c) * 0.55));
  const brightHex = "#" + [lift(r), lift(g), lift(b)].map((c) => c.toString(16).padStart(2, "0")).join("");
  return { name, rgb, hex, brightHex, caption: `rgb(${r}, ${g}, ${b})` };
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
SHARPNESS — STRICT: the drawing is CRISP throughout. NO motion blur. NO \
pervasive depth-of-field blur. NO soft-focus haze. NO "blurred to suggest \
movement." Confident pencil lines render the subject AND background equally \
sharp — foreground, middle ground, and back wall all in clean focus. \
Movement is conveyed through POSE (mid-gesture, leaning, wrist cocked), \
never through blur. \
\
COMPOSITION RULES (the slide will overlay text on top of the photo, so leave \
room for them): \
1) HEAD AND FACE: centered horizontally, vertically positioned in the MIDDLE \
band of the frame — head should sit roughly between y=500 and y=1100 of the \
1024x1536 image (the middle 40%). His eyes should land near y=700–800. The \
face must be FULLY VISIBLE — not cropped at the top, not obscured by raised \
arms, not turned away. \
2) TOP 25% RESERVED: the upper portion of the frame (y=0 to y=380) MUST be \
QUIET background — ceiling, wall, sky, a single quiet wall ornament. Quiet \
means simple and uncluttered, NOT blurred — render it just as crisply as \
the rest of the drawing. NO HEAD, NO RAISED HANDS, NO ARMS above shoulder \
level in this zone. This is where the chapter title text overlays. \
3) BOTTOM CHROME STRIP — STRICT: the entire bottom band y=1460..1920 \
overlays the rendered video chrome (subtitle pill, waveform, piano roll, \
PALS bug, progress bar — see slide layout). KEEP this band UNCLUTTERED — \
a clean desk surface, the front edge of a wooden table, a plain floor or \
rug — but render it SHARPLY (no bokeh, no soft focus). NO faces, NO laptop, \
NO hands, NO keyboards, NO sharp text or paper details that would compete \
with the chrome overlay. The chrome must read clearly on top of a calm-but- \
crisp surface. \
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
ref passed alongside the prompt. \
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

  // 24-hour cuts are SPOKEN — the waltz bed below is the music, not an
  // autotuned overlay on the narration. Opt out of the sing pass so it
  // doesn't rewrite words.json with coarser sung-event boundaries (which
  // would break the per-segment marker matching downstream).
  sing: false,

  // Classic hip-hop kit defaults (used by bin/audio-preview.mjs). Drums
  // are synthesized in node — no samples — so the bed reads as one
  // coherent kit instead of a sample mosaic. Tuned light on the low
  // end so the bass + melody can sit on top.
  kick:  { pattern: "boom-bap", gain: 0.55, dur: 0.40, pitchStart: 130, pitchEnd: 78 },
  snare: { pattern: "2+4",      gain: 0.55 },
  hihat: { grid: 8,             gain: 0.30 },
  // Held bass — TRUE sustain (flat plateau). dur sets the hit length;
  // the synth envelope holds at full level for the middle of dur then
  // releases out. With beat1+3 + bar=2.79s at 86bpm, dur=1.4s gives a
  // genuine "held" feel up to the next hit.
  bass:  { pattern: "beat1+3",  gain: 0.45, freq: 73, dur: 1.4 },
  // Pulled BACK so vocals sit on top. Counter at 0.40 of section gain.
  melody: { gain: 0.32, counter: true, counterGain: 0.40 },

  // Sine-bells bed for this cut. Morph arc tracks the day's narrative:
  // calm open for pixsies + drvk handoff → bright lydian under the
  // pop/dsp + wave-wizard discovery beats → grounded dorian under the
  // hellsine sample-factory chapter → warm major landing for the
  // andreessen campaign + outro.
  waltz: {
    voice: "sinebells",
    seed: "jeffrey-24h-2026-05-24",
    bpm: 78,
    voiceGain: 0.18,
    morph: [
      // Opening — calm I-vi-IV-V. Title + pixsies player.
      { progression: [0, 5, 3, 4, 0, 3, 4, 0], scale: "major",  density: 0.35, transpose: 0, weight: 1.4 },
      // Handoff + dsp + wave-wizard — brighter lydian, more motion.
      { progression: [3, 0, 4, 5, 3, 4, 0, 5], scale: "lydian", density: 0.55, transpose: 2, weight: 2.0 },
      // Hellsine sample factory — grounded dorian, low and round.
      { progression: [5, 4, 0, 3, 5, 0, 4, 3], scale: "dorian", density: 0.50, transpose: 3, weight: 1.5 },
      // Andreessen ig + outro — relaxed major, longer phrases.
      { progression: [0, 3, 4, 0, 5, 3, 4, 0], scale: "major",  density: 0.45, transpose: 0, weight: 1.8 },
    ],
  },

  narration: `hey everybody, here's the last twenty-four hours at aesthetic dot computer. five updates this round. first, the pixsies player. every released pixsies single is now its own ac piece. type the song name at the prompt and a manifest-driven jukebox boots — preloaded audio, live waveform, an illy backdrop that auto-advances by section. tap an illy thumb to scrub. second, the mastering pivot. there's a new pop slash dsp folder — a tiny c library called acdsp with physically-modeled compressors and an eq knowledge graph. one source compiles to three runtimes: pop cli, browser wasm, and ac native. ffmpeg's acompressor was the audible tell on every recent master. that ends. third, wave wizard. a small swift mascot app for the mac. pointy purple hat, white beard, a wand whose star tip glows yellow when it's recording and cyan when it's listening. feed it a json of sample names and it walks you through them, auto-trimming each take on silence. fourth, that wizard already produced a hellsine sample wall — rattle takes sliced by amplitude into individual shakes, plus a cards bank from fast to hard and two burst rhythms. real-world percussion for a kick-and-shake track. and last, a new marketing campaign: jeffrey fixing andreessen, instagram-story format. a downtown san francisco vc boardroom, peer-horizontal, deadpan everyday-help. that's the twenty-four. thanks for watching, see you tomorrow.`,

  // Whisper renders these dictionary-style; rewrite the displayed subtitles.
  // Fixes are derived from THIS specific recap.mp3's whisper output — every
  // entry below is something whisper actually emitted on this take.
  // Re-derive after any narration change.
  transcriptFixes: {
    "a static computer": "aesthetic computer",
    "Aesthetic Computer": "aesthetic computer",
    "hey, everybody!": "hey everybody",
    "the pixi's player": "the pixsies player",
    "pixi's player": "pixsies player",
    "pixi's single": "pixsies single",
    "pixi's": "pixsies", "Pixi's": "pixsies",
    "Marimbaaba": "marimbaba", "marimbaaba": "marimbaba",
    "marimba ba": "marimbaba",
    "helpa beach": "helpabeach", "Helpa Beach": "helpabeach",
    "transinewaltz": "trancenwaltz", "Transinewaltz": "trancenwaltz",
    "transpenta": "trancepenta", "Transpenta": "trancepenta",
    "an ili": "an illy",
    "ili backdrop": "illy backdrop",
    "ili thumb": "illy thumb",
    "one rapper per song": "one wrapper per song",
    "rapper per song": "wrapper per song",
    "ac dsp": "acdsp", "AC DSP": "acdsp", "AC dsp": "acdsp",
    "mastering clay": "mastering cli",
    "an achy native": "as ac native",
    "achy native": "ac native",
    "AC Native": "ac native",
    "fm peg's ac compressor": "ffmpeg's acompressor",
    "fm peg's": "ffmpeg's",
    "fm peg": "ffmpeg",
    "ac compressor": "acompressor",
    "FFmpeg": "ffmpeg",
    "WASM": "wasm",
    "wave wizard": "wave-wizard", "Wave Wizard": "wave-wizard",
    "saves a clean wave": "saves a clean wav",
    "adjacent of sample": "a json of sample",
    "adjacent of": "a json of",
    "Mac": "mac", "MacBook": "macbook",
    "JSON": "json", "WAV": "wav",
    "DSP": "dsp", "EQ": "eq",
    "hell sign": "hellsine", "Hell Sign": "hellsine",
    "card s bank": "cards bank",
    "kick and shake": "kick-and-shake",
    "Andresen": "andreessen", "andresen": "andreessen",
    "instagram story": "instagram-story",
    "Instagram": "instagram",
    "IG": "ig", "VC": "vc",
    "peer horizontality": "peer-horizontality",
    "Peer Horizontality": "peer-horizontality",
    "deadpan every day": "deadpan everyday",
    "every day help": "everyday help",
    "San Francisco": "san francisco",
    "Jeffrey": "jeffrey",
    "CLI": "cli", "QR": "qr",
  },

  // ~6 chapter slides + title + outro + end card. Markers chosen to be
  // distinct, single-occurrence phrases that whisper will recognize.
  // Each chapter optionally carries a `commit` field — short hash on the
  // canonical knot. bin/qrs.mjs renders a QR for each into
  // recap/out/qr/<seg>.png and slides.mjs embeds it bottom-right.
  // Markers match raw whisper output BEFORE transcriptFixes apply, so they
  // need to use the words whisper actually emits — e.g. "pixsies" comes
  // out as "pixi's", "hellsine" as "hell sign".
  segments: [
    { name: "01_title",              marker: "hey everybody" },
    { name: "02_pixsies_player",     marker: "every released",            commit: "d36c1b71e" },
    { name: "04_pop_dsp",            marker: "the mastering pivot" },
    { name: "05_wave_wizard",        marker: "a small swift mascot" },
    { name: "06_hellsine_samples",   marker: "sample wall" },
    { name: "07_andreessen",         marker: "a new marketing campaign" },
    { name: "08_outro",              marker: "thanks for watching" },
    { name: "09_end",                marker: "__END__", trailingSilenceSec: 3 },
  ],

  slides: {
    "01_title": {
      colorAddress: colorAddress("coral"),
      // Episode 4 ensemble venue: a sun-warmed, converted 1920s ANALOG
      // RECORDING STUDIO — high ceiling with diffusion panels, parquet
      // floor, vintage tape machines along one wall, a grand piano in
      // the back corner, ribbon mics on tripods scattered across the
      // floor. Late-morning skylight pours through tall clerestory
      // windows in soft warm wedges. Distinct from ep 02 (loft) and
      // ep 03 (train-station concourse).
      metaphor: `A photograph of a colored-pencil and gouache drawing on \
warm cream paper, in tall vertical 1024x1536 portrait orientation. \
Confident hatching and striping build tone. Tapered pencil edges. \
Optical mixing in patterned surfaces. Visible paper grain. \
Hockney-register architectural-magazine style group composition, \
candid not staged. The man in the reference photos appears as \
TWENTY-FOUR INSTANCES of himself populating a CONVERTED 1920s \
ANALOG RECORDING STUDIO — a vast warm-wood interior with a high \
beamed ceiling hung with hexagonal sound-diffusion panels in cream \
and pale wood, a polished honey-toned parquet floor, a row of TALL \
ARCHED CLERESTORY WINDOWS along the upper portion of the long wall \
pouring late-morning sun in slanted golden-cream shafts down across \
the floor, a SECOND-FLOOR MEZZANINE on the back wall reached by a \
narrow iron staircase, a row of VINTAGE REEL-TO-REEL TAPE MACHINES \
sitting on low wooden cabinets along one side wall (their large \
silver reels catching the light), and a GRAND PIANO with its lid \
propped open tucked into the deep back corner under the mezzanine. \
Scattered around the room: a half-dozen tall RIBBON-MICROPHONE \
TRIPODS, several worn wooden stools, a couple of long oak tables \
pushed together as workbenches, and one threadbare oriental rug \
laid down between the piano and the tape machines. The ensemble is \
HORIZONTAL — jeffreys spread laterally across the studio floor, not \
stacked vertically. \
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
the ensemble: a second laptop on the bench, a small studio condenser \
mic clipped to the workbench, a smartphone in hand, a fountain pen \
mid-write on a notebook, a clipboard of papers, an iPad on a stand, a \
small handheld game device, a vintage rolltop notebook, a coffee mug, \
a small synth keyboard, headphones around the neck, a microphone arm \
clamped to a worktable, a Hi-Z guitar laid across knees. Some jeffreys \
sit on stools at the workbenches, others on the parquet floor with \
the laptop on their lap, one or two lean against the tape-machine \
cabinets, one stands at the grand piano sketching out a chord. \
Variety — not every station identical. The vibe is "the studio is \
quietly busy with twenty-four parallel hands-on tasks." \
\
Each jeffrey is in a small "I have an update to share!" pose — variety \
of micro-expressions and gestures, all chest-level: one leaning back on \
a stool gesturing at his screen, one turning to a neighbor with a \
finger raised mid-explanation, one holding up a notebook page to his \
benchmate, one mid-laugh pointing at his laptop, one typing intently \
and grinning, one writing in a notebook with the fountain pen, one \
talking on the phone with hand cupped over the mouthpiece, one in a \
small mock-shock oh-face, one nodding along with headphones, one \
plucking a string on the laid-across guitar, one at the piano \
sketching a chord with one hand. Most face toward the camera or toward \
a neighbor. None directly above another's head. \
\
EXACTLY ONE of the 24 jeffreys (just one — somewhere in the middle of \
the frame, not on the edges) is mid-puff on his vape — a small \
USB-stick-shaped pen visible in his hand with its tiny LED tip glowing. \
A single natural wisp of vape vapor curls up from his station, catching \
one of the light shafts from the clerestory windows. NOT heavy fog, \
NOT multiple vapers — just one. Other jeffreys may have their vape pens \
visible on the bench beside their laptop but only the one is actively \
puffing. \
\
LIGHTING: the late-morning sun rakes through the tall arched \
clerestory windows from one side, casting long warm-cream shafts of \
light across the parquet floor and the wooden workbenches, picking up \
dust motes and the wisp of vape vapor. A row of small vintage \
PENDANT LAMPS hangs at intervals from the beamed ceiling, just barely \
glowing in the daylight. The big silver tape-machine reels along the \
side wall catch the rim of the light. The contrast is strong — \
bright golden window-shafts and pools of pendant warmth, interspersed \
with cooler shadow zones beneath the mezzanine. Cinematic-but-candid \
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
ONLY — beamed ceiling with hexagonal diffusion panels, upper portion \
of arched clerestory windows with bright sky beyond, mezzanine railing \
in soft focus. NO JEFFREYS, NO LAPTOPS in this top zone. The first / \
topmost row of jeffreys begins at y≈600 of the photo, not earlier. \
The 24-jeffrey ensemble lives in the central band (y≈600..1100). The \
BOTTOM ~30% (y=1100..1536) shows the parquet floor with golden light \
shafts fading into open foreground — no jeffreys cropped at the bottom \
edge. The very bottom band (y=1460..1536) is QUIET parquet floor with \
light patterning only — the rendered chrome (subs / waveform / piano \
/ pals / progress) overlays here at composite time.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/01_title.png" }, cv: { json: "recap/out/cv/01_title.json" }, qr: { glob: "recap/out/qr/01_title.png" } },
      body: ({ photo }) => titleSlide({
        photo,
        color: colorAddress("coral"),
      }),
    },

    "02_pixsies_player": {
      colorAddress: colorAddress("coral"),
      metaphor: `${REAL}

Scene: late morning at a wooden home desk. He sits in front of the \
closed-lid chartreuse Apple MacBook Neo (back of the lid facing the \
camera, plain Apple logo). Four small printed 4x6 photo prints are \
fanned out face-up on the desk in front of the laptop — the prints \
are abstract textured rectangles in warm coral and cream tones, not \
real album covers, just suggestive of sleeve art. He is leaning \
forward with one hand cupped around the side of his head as if \
listening hard, the other hand mid-tap on the desk next to the \
prints in a "queue this one next" gesture, mouth slightly open in a \
focused-but-pleased half-smile, eyebrows up. Diegetic coral \
(#ff7f50): a small coral-glass desk lamp on the right edge of the \
desk pours warm coral light across the right side of his face and \
the four prints. The rest of the room is in soft morning daylight. \
A coffee mug to the left, half empty. Open hoodie over a tee. \
Shirt word: \`play\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/02_pixsies_player.png" }, cv: { json: "recap/out/cv/02_pixsies_player.json" }, qr: { glob: "recap/out/qr/02_pixsies_player.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        commit: "d36c1b71e",
        title: "every single,\na real piece\nat the prompt",
        cap: "manifest-driven jukebox · live waveform · auto-advancing illy",
        color: colorAddress("coral"),
      }),
    },

    "04_pop_dsp": {
      colorAddress: colorAddress("gold"),
      metaphor: `${REAL}

Scene: afternoon at a long oak workbench. The chartreuse Apple \
MacBook Neo sits closed on the bench (back of the lid facing camera, \
plain Apple logo). To the right of the laptop on the bench sits a \
small VINTAGE HARDWARE COMPRESSOR — a chunky steel chassis with two \
large round VU meters on the front, a few black knobs, a single \
black toggle switch, the kind of unit you'd find in a 1970s \
broadcast rack. Its needles are pinned mid-arc as if catching a \
signal. He is leaning forward with one hand resting on top of the \
compressor in a fond mechanical-sympathy way, the other hand mid-air \
in a small "ahh that's the curve" gesture, mouth slightly open in a \
quiet-aha closed-mouth smile, eyebrows up. NO screens visible, NO \
schematics on the wall (the per-scene description does not pass a \
schematic ref). Diegetic gold (#ffd700): a single gold-shaded \
pendant desk lamp hanging above the workbench pours warm gold light \
straight down onto the compressor's VU meters and the top edge of \
the laptop lid. The rest of the room is dim and lamp-warm. Cream \
pullover sweater. Shirt word: \`tone\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/04_pop_dsp.png" }, cv: { json: "recap/out/cv/04_pop_dsp.json" }, qr: { glob: "recap/out/qr/04_pop_dsp.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "physical comps,\nportable c,\nthree runtimes",
        cap: "acdsp · pop/dsp + master.mjs · pop · wasm · ac native",
        color: colorAddress("gold"),
      }),
    },

    "05_wave_wizard": {
      colorAddress: colorAddress("blueviolet"),
      metaphor: `${REAL}

Scene: late afternoon at the same workbench. The chartreuse Apple \
MacBook Neo sits open in front of him — but as always shown FROM \
BEHIND, back of the lid facing the camera, plain Apple logo, screen \
content NOT visible to the viewer. To the right of the laptop sits \
a SMALL STUDIO CONDENSER MICROPHONE on a short tabletop boom arm, \
its diaphragm pointed at his mouth, the mic body matte black with a \
mesh windscreen. He is leaning toward the mic with his lips slightly \
parted, eyes wide in a delighted "I am about to record a card-flip \
sound" expression, mouth in a small mid-laugh shape, one hand \
holding a small stack of playing cards mid-shuffle just below his \
chin (the cards in mid-riffle, fanned). Diegetic blueviolet \
(#8a2be2): a tall blueviolet glass pendant hangs above the bench, \
its colored glass throwing saturated blueviolet light across the \
back wall, the right side of his face, and the back of the laptop \
lid. The rest of the room is dim. Open zip hoodie over a tee. \
Shirt word: \`wand\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/05_wave_wizard.png" }, cv: { json: "recap/out/cv/05_wave_wizard.json" }, qr: { glob: "recap/out/qr/05_wave_wizard.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "the wizard\nlistens, trims,\nand saves",
        cap: "swift mascot · auto-stop on silence · timeline of takes",
        color: colorAddress("blueviolet"),
      }),
    },

    "06_hellsine_samples": {
      colorAddress: colorAddress("crimson"),
      metaphor: `${REAL}

Scene: early evening at the workbench. The black ThinkPad sits closed \
on the bench (back of the lid facing camera, small ThinkPad wordmark \
lower-right). A small wooden HAND PERCUSSION SHAKER — a maraca-style \
egg-shaped rattle in pale wood with a short handle — sits on the \
bench to the right of the laptop. He holds a SECOND matching wooden \
shaker firmly in one hand at chest level, wrist cocked mid-shake \
(no motion blur — render the shaker crisply, movement conveyed by \
the cocked wrist alone), eyes slightly squinted in focused-listening \
concentration, mouth in a small flat "is that the take" line. \
Diegetic crimson (#dc143c): a tall crimson glass floor lamp on the \
left side of the room throws saturated crimson light across the back \
wall and onto the left side of his face and the bench surface. Late \
afternoon. Plaid flannel shirt buttoned over a tee. Shirt word: \`beat\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/06_hellsine_samples.png" }, cv: { json: "recap/out/cv/06_hellsine_samples.json" }, qr: { glob: "recap/out/qr/06_hellsine_samples.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "rattles sliced\nby length,\ncards by feel",
        cap: "cut-shakes.mjs · rattle-intro takes · cards fast/medium/slow/hard + 2 bursts",
        color: colorAddress("crimson"),
      }),
    },

    "07_andreessen": {
      colorAddress: colorAddress("khaki"),
      metaphor: `${REAL}

Scene: late afternoon at the home desk. The chartreuse Apple MacBook \
Neo sits open in front of him (back of the lid facing the camera as \
always, plain Apple logo, no screen content visible). He is sitting \
upright in his chair, head tipped slightly to one side, one hand \
resting flat on the desk to the side of the laptop, the other hand \
at his side. His expression is a calm, slightly amused closed-mouth \
half-smile — the look of someone editing a draft they're quietly \
pleased with. NO screens visible. NO printed reference images of \
the campaign on the desk (the per-scene description does not pass \
a campaign ref). Diegetic khaki (#f0e68c): a single khaki-shaded \
desk lamp on the right edge of the desk pours warm khaki light \
across the right side of his face and the desk surface. Late \
afternoon daylight from a window on the left. Olive cardigan over \
a button-down. No printed shirt word for this chapter.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/07_andreessen.png" }, cv: { json: "recap/out/cv/07_andreessen.json" }, qr: { glob: "recap/out/qr/07_andreessen.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "peer-horizontal,\nig story,\nboardroom help",
        cap: "marketing/campaigns/jeffrey-fixing-andreessen-igstory",
        color: colorAddress("khaki"),
      }),
    },

    "08_outro": {
      colorAddress: colorAddress("mintcream"),
      metaphor: `${REAL}

Scene: dusk at the home desk. The chartreuse Apple MacBook Neo sits \
closed on the desk in front of him (back of the lid facing the \
camera, plain Apple logo). He is leaning slightly back in his chair, \
one hand resting on top of the closed laptop in a comfortable \
sign-off gesture, the other holding a small ceramic mug at chest \
level. His expression is a relaxed small smile, real eye contact \
with the camera, eyebrows level — the soft "see you tomorrow" face. \
Diegetic mintcream (#f5fffa): a single mintcream pendant lamp \
hanging above the desk pours soft cool light down onto the closed \
laptop and the right side of his face. Warm-cool balance — evening \
softness. Cream pullover sweater. Shirt word: \`sing\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/08_outro.png" }, cv: { json: "recap/out/cv/08_outro.json" }, qr: { glob: "recap/out/qr/08_outro.png" } },
      body: ({ photo, cv, qr }) => photoSlide({
        photo,
        cv,
        qr,
        title: "thanks for\nwatching,\nsee you tomorrow",
        cap: "aesthetic.computer · @jeffrey · ep 04 · 2026·05·24",
        color: colorAddress("mintcream"),
      }),
    },

    "09_end": `
      <div class="frame">
        <div class="pals med"></div>
        <div class="endline" style="color:${PALETTE.cream}">aesthetic·computer</div>
        <div class="endsub" style="color:${PALETTE.dim}">narrated by jeffrey-pvc · @jeffrey</div>
        <div class="endsub" style="color:${PALETTE.dim}">2026·05·24</div>
      </div>`,
  },
};

// Episode metadata. Bump EPISODE for each new audience config.
// Show wordmark + episode code render on the title slide.
const EPISODE = 4;
const SHOW_NAME = "aesthetic 24";
const EPISODE_DATE = "2026·05·24";
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
      <img src="${PALS_SVG_DATA_URL}" alt="" style="position: absolute; left: -8px; top: 1500px; width: 170px; height: 170px; opacity: 0.28; transform: rotate(-90deg); transform-origin: center; filter: ${titlePalsShadow};" />
      <img src="${PALS_SVG_DATA_URL}" alt="" style="position: absolute; right: -8px; top: 360px; width: 170px; height: 170px; opacity: 0.28; transform: rotate(90deg); transform-origin: center; filter: ${titlePalsShadow};" />
    </div>`;
}

function photoSlide({ photo, title, cap, color, qr, commit }) {
  const titleHtml = (title || "")
    .split("\n")
    .map((l) => `<span class="cmd-line">${l}</span>`)
    .join("");
  const creamShadow = "3px 3px 0 rgba(0,0,0,1), -2px -2px 0 rgba(0,0,0,1), 0 0 6px rgba(0,0,0,0.9)";
  const capShadow = "2px 2px 0 rgba(0,0,0,1), -1px -1px 0 rgba(0,0,0,0.95), 0 0 4px rgba(0,0,0,0.9)";
  const promptShadow = "2px 2px 0 rgba(0,0,0,1), 0 0 4px rgba(0,0,0,0.9)";

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
      <div style="position: absolute; top: 0; left: 0; width: 720px; height: 980px; background: linear-gradient(135deg, rgba(0,0,0,0.78) 0%, rgba(0,0,0,0.5) 40%, rgba(0,0,0,0) 70%); pointer-events: none;"></div>
      <div style="position: absolute; left: 60px; top: 50px; max-width: 760px; font-family: 'ProcessingB'; line-height: 1.04;">
        <div style="font-size: 36px; letter-spacing: 2px; text-shadow: ${promptShadow};">
          <span style="color: #c47cff;">Aesthetic</span><span style="color: #ff5cb8;">.</span><span style="color: #c47cff;">Computer</span>
        </div>
        <div style="font-size: 88px; letter-spacing: -3px; color: ${color.brightHex || color.hex}; margin-top: 8px; text-shadow: 1px 1px 0 rgba(0,0,0,0.85), 0 0 6px rgba(0,0,0,0.65); display: flex; flex-direction: column; gap: 4px;">${titleHtml}</div>
        ${cap ? `<div style="font-family: 'ProcessingB'; font-size: 38px; color: ${PALETTE.cream}; margin-top: 22px; letter-spacing: 1px; text-shadow: ${capShadow}; max-width: 720px; line-height: 1.18;">${cap}</div>` : ""}
      </div>
      <img src="${palsSvg}" alt="" style="position: absolute; left: -8px; top: 1500px; width: 170px; height: 170px; opacity: 0.28; transform: rotate(-90deg); transform-origin: center; filter: ${palsShadow};" />
      <img src="${palsSvg}" alt="" style="position: absolute; right: -8px; top: 360px; width: 170px; height: 170px; opacity: 0.28; transform: rotate(90deg); transform-origin: center; filter: ${palsShadow};" />
      ${qrBlock}
    </div>`;
}

// Hard-fail at module load if any shirt word doesn't resolve at the AC
// prompt. This runs every time tts.mjs / jeffrey-photos.mjs / scout.mjs
// imports the audience config — so a bad word can never reach gpt-image-2.
validateShirtWords(audience.slides);

export default audience;
