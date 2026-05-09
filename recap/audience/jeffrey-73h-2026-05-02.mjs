// Audience config: jeffrey-73h, 2026-05-02 cut.
// Window: ~2026-04-29 10:33 → 2026-05-02 11:33. Compressing three days of
// commits into one recap because the menubar audio stack basically grew
// up over the same three days and reads as a single arc, not three
// separate ones. Voice: jeffrey-pvc. Style: lowercase, calm, descriptive.
//
// Inherits from the 04-30 / 05-01 cuts:
//   - per-slide CSS color address from `cssColors` in lib/num.mjs
//   - duotone tint over the photo (chapter color stains the portrait)
//   - sine-bells waltz bed under narration (pure synth, AC-native logic)
//
// Slides remain full-bleed gpt-image-2 photos (jeffrey at a real home
// desk, using the imagined feature on his laptop) with a top gradient.
// Photos pre-baked by `bin/jeffrey-photos.mjs` into
// recap/out/jeffrey-photos/<segment>.png.

import { cssColors } from "../../system/public/aesthetic.computer/lib/num.mjs";

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
Photographic candid lifestyle photo of the man in the reference photos. Real \
photograph, photo-realistic, the kind of casual iPhone snapshot a friend would \
take of him at home — NOT illustrated, NOT painted, NOT cinematic, NOT \
magazine-glossy, NOT AI-poster-glossy. Slight film grain, slightly off-center \
framing, real natural room light. Identity: same face, same medium-length brown \
hair, same actual features as the references — recognizably him across the \
various refs. Keep real skin texture; do NOT smooth or prettify. He is in normal \
everyday clothes (t-shirt or hoodie, slightly bedheaded). Vertical 1024x1536 \
portrait orientation. Expression varies per scene — the per-scene description \
controls it; lean hyperbolic / cartoony when the scene says so. \
\
COMPOSITION RULES (the slide will overlay text and a small inset card on top \
of the photo, so leave room for them): \
1) HEAD AND FACE: centered horizontally, vertically positioned in the MIDDLE \
band of the frame — head should sit roughly between y=500 and y=1100 of the \
1024x1536 image (the middle 40%). His eyes should land near y=700–800. The \
face must be FULLY VISIBLE — not cropped at the top, not obscured by raised \
arms, not turned away. \
2) TOP 25% RESERVED: the upper portion of the frame (y=0 to y=380) MUST be \
quiet background — ceiling, wall, sky, soft out-of-focus shelf, or quiet wall \
art. NO HEAD, NO RAISED HANDS, NO ARMS above shoulder level in this zone. \
This is where the chapter title text overlays. \
3) BOTTOM-LEFT QUADRANT: keep the area roughly from (x=0, y=950) to (x=420, \
y=1400) calm — desk surface, body/torso, floor, or out-of-focus foreground. \
A small artifact inset card overlays here on some slides. \
4) HYPERBOLIC ENERGY through FACE + POSTURE, not raised arms: convey big \
emotion via mouth (wide open, surprised laugh, oh-face), eyes (wide, scrunched, \
horrified), eyebrows (shot up, knit), head tilt, lean-in/lean-back, shoulders. \
Hands and gestures at chest level or below — never above the head, never \
covering the face. \
\
SHIRT: jeffrey's outfit always carries a single short lowercase word \
somewhere visible — a real-feeling screenprint or embroidery in bold \
custom typography, slightly worn-in. The exact word and placement are \
given per scene below. The word maps directly to the chapter's topic \
(it's a literal command). Placement varies to match the outfit: a \
centered chest screenprint for hyperbolic tee scenes; a small chest- \
pocket print on quieter tees; embroidered chest-pocket lettering on \
button-downs; embroidered cuff lettering on dress shirts; a sleeve \
patch on hoodies; stitched chest letters on cardigans. Don't always \
center the word — vary the placement so each chapter's wardrobe reads \
differently. \
\
RECURRING PROP: jeffrey often has a small **USB-stick-shaped vape pen** \
nearby — the size and silhouette of a USB-A thumb drive, slim rectangular \
profile, matte black or muted color, with a **small LED indicator light** \
(usually glowing white / blue / green) at the mouthpiece end. NOT a \
cigarette-style pen, NOT a chunky MOD, NOT a bong — the disposable-pod / \
Stiiizy-style portable. It varies per scene: sometimes in his hand \
mid-puff with a faint wisp of vapor, sometimes resting on the desk next \
to the laptop, sometimes tucked in a hoodie pocket. Casual background \
detail, not the visual focus. Skip on formal/scholarly scenes; lean in \
on relaxed couch / desk / garage / late-night scenes. \
\
WARDROBE — REAL OUTFITS ONLY: dress jeffrey only in clothes he actually \
wears: button-down shirts (flannel or solid casual), hoodies (open or \
zipped, muted gray/navy/olive/burgundy/cream), printed t-shirts bearing \
a single AC piece word per the per-scene description, cardigans (knit, \
earth tones), pullover sweaters (varied patterns including stripes), \
flannels (over a tee or buttoned, plaid). NEVER tank tops. NEVER suits / \
formalwear. NEVER costumes / cosplay / athletic gear / fashion-runway \
pieces. When in doubt, default to a hoodie or a printed tee; he's at \
home, not at a photoshoot.`;

export const audience = {
  name: "jeffrey-73h",
  handle: "@jeffrey",
  voice: { provider: "jeffrey", voice: "neutral:0" },

  // Sine-bells bed for this cut. A bit longer (40 bars) so it covers the
  // longer 73-hour narration without obvious looping. Same calm pace.
  waltz: {
    voice: "sinebells",
    seed: "jeffrey-73h-2026-05-02",
    bpm: 76,
    scale: "major",
    progression: [0, 5, 3, 4, 0, 3, 4, 0],
    bars: 40,
    voiceGain: 0.18,
  },

  narration: `hey everybody, here's the last seventy-three hours at aesthetic computer. this is a triple-length cut because the menubar audio stack basically grew up over three days. menuband shipped six releases in a row — zero-four added a mute toggle and sandbox-safe key capture, zero-five took perf patches from esteban uribe, zero-six landed the voice palette lcd with bundled ywft processing and dark mode, zero-seven moved to a sharp risograph drop shadow on the title with a one-pixel family-color misregister, zero-eight thought it caught a bug where two ywft files shared a postscript name, and zero-nine actually fixed it by loading the dot-ttf descriptor from the bundle url and asserting the family before drawing. today's commit lands the integrated chord finder right in the popover — it watches the piano voicing and names what you're playing — plus chromatic notepat coloring so each key carries a hue at its semitone, and a tighter mini-meter that fits next to the chord readout. a real waveform strip now slides out from under the piano popover when notes hit and tucks back when they stop, with shared waveform state across the menubar, the popover, and the new floating palette so all three surfaces draw the same trace. the floating palette itself got proper popover controls — instrument picker, transport, mute — a strip footer that names the current voice and held notes, and dragging that's pinned to the title bar. the metal shader for the waveform moved into its own dot-metal file and now compiles at runtime, the multi-channel synth path stopped clobbering itself when polyphony stacked up, shift and capslock now hold a note in linger mode, and the whole app does less work when hidden. on the menubar side, the polygon status icon went from per-session colors to a single homogenized hue that tracks how many sessions are awaiting input, and the claude-stop hook flips active to awaiting instead of clearing — the lid-ambient mode detects all three shapes of claude-is-running. on the hardware bench, the hp seventy-five eighty-five b drafting plotter came up over rs-two-thirty-two with a hardware-flow serial sender, a live-limits probe that reads sheet geometry off the machine, and a native hp-gl composer that renders svg paths as faceted polylines, with the pals mark as the first real test. inside fedac native, the grand-piano sample amp went from one-point-eight to three so it sits level with the sine and square voices through notepat, and a new toughbook directory landed at the top level to scaffold the next ac native os bring-up on a panasonic. zooming out, most of the writing time went into a paper. arxiv-keymaps now has a tikz figure comparing qwerty, dvorak, and colemak, a daw chromatic-staircase mapping figure, a wicki-hayden hex layout, vim modes, singmaster's cube notation, and an asymmetric-instruments figure with guitar and violin string-tuning panels. the prose grew too — a steelman against isomorphic layouts, a notation-surface section, an orality-and-literacy parallel pulling from walter ong with wasd as the worked example, a community-authored framing for the lgbtq-plus initialism, and live-coding mini-notations with the section's sources verified. the papers index itself got load-bearing infrastructure — a deadlines page wired into prompt autocomplete, a question-mark-review mode that surfaces vault draft links, a submissions ledger with the siggraph asia scaffold, the keymaps draft formally registered, the public cv hidden, fia's opportunities pulled in from the honeydo inbox, and the original jeffrey platter restored with hq originals in a lightbox and an opinions list. ac-electron pushed zero-one-point-four-four — the xbox controller bridge file was orphaned in the build, so the controller wasn't actually wired through to the webview; that's loaded for real now, with a-button and space respawn and the new ac-desktop cli, and arena widened its reconcile dead zone so the multiplayer snap doesn't fight gentle motion. and the recap pipeline itself grew up — pre-baked jeffrey-photos cached per segment, and a per-cut waltz generator that pulls from the ac native os instrument stack so each cut sits under a custom soft sine-bell bed. from the chats — laer-klokken and the main thread both had voices running alongside all of this; the next slide pulls the recent messages so the people behind the keyboard are part of the cut, not just the commits. that's the seventy-three hours. thanks for watching, and i'll be back tomorrow.`,

  transcriptFixes: {
    "a static computer": "aesthetic computer",
    "static computer": "aesthetic computer",
    "Menu Band": "menuband", "menu band": "menuband", "Menuband": "menuband",
    "Menu Bar": "menubar", "menu bar": "menubar",
    "Note Pat": "notepat", "Notepat": "notepat",
    "YWFT Processing": "ywft processing", "YWFT": "ywft", "Y W F T": "ywft",
    "PostScript": "postscript", "Post Script": "postscript",
    "NS Font": "nsfont", "NSFont": "nsfont",
    "dot-TTF": "dot-ttf", "dot TTF": "dot-ttf", ".TTF": ".ttf",
    "Riso": "riso", "Risograph": "risograph",
    "LED": "led", "LCD": "lcd",
    "QWERTY": "qwerty", "Qwerty": "qwerty",
    "Dvorak": "dvorak", "Colemak": "colemak",
    "Wicki-Hayden": "wicki-hayden", "Wicki Hayden": "wicki-hayden",
    "Singmaster": "singmaster",
    "Vim": "vim", "VIM": "vim",
    "Walter Ong": "walter ong",
    "WASD": "wasd",
    "LGBTQ": "lgbtq", "LGBT": "lgbt",
    "Mac OS": "macos", "macOS": "macos",
    "MIDI": "midi",
    "GPT-Image-2": "gpt-image-2", "GPT Image 2": "gpt-image-2",
    "Fed AC": "fedac", "FedAC": "fedac",
    "HP-GL": "hp-gl", "HPGL": "hp-gl",
    "RS-232": "rs-232", "RS 232": "rs-232",
    "PALS": "pals",
    "Arxiv": "arxiv", "ArXiv": "arxiv",
    "TikZ": "tikz", "Tik Z": "tikz",
    "DAW": "daw",
    "Metal": "metal",
    "Xcode": "xcode", "XCode": "xcode",
    "Toughbook": "toughbook", "Panasonic": "panasonic",
    "Xbox": "xbox", "X-box": "xbox",
    "Electron": "electron",
    "SIGGRAPH Asia": "siggraph asia", "SIGGRAPH": "siggraph",
    "Casey": "casey",
    "Esteban Uribe": "esteban uribe", "Esteban": "esteban",
    "Claude": "claude",
    "Mac": "mac", "MacBook": "macbook",
    "Jeffrey": "jeffrey", "Fia": "fia",
    "Honeydo": "honeydo",
    "CV": "cv", "CLI": "cli", "PVC": "pvc",
  },

  segments: [
    { name: "01_title",                marker: "hey everybody" },
    { name: "02_menuband_arc",         marker: "shipped six releases" },
    { name: "03_waveform_strip",       marker: "a real waveform strip" },
    { name: "04_floating_palette",     marker: "the floating palette itself" },
    { name: "05_menuband_engineering", marker: "the metal shader" },
    { name: "06_slab_polygon_lid",     marker: "the polygon status" },
    { name: "07_hp_plotter",           marker: "on the hardware bench" },
    { name: "08_fedac_native",         marker: "the grand piano sample" },
    { name: "09_arxiv_figures",        marker: "zooming out" },
    { name: "10_arxiv_prose",          marker: "isomorphic layouts" },
    { name: "11_papers_infra",         marker: "a deadlines page" },
    { name: "12_electron_arena",       marker: "the xbox controller bridge" },
    { name: "13_recap_pipeline",       marker: "and the recap pipeline" },
    { name: "14a_chat_clock",          marker: "from the chats" },
    { name: "14b_chat_system",         marker: "the main thread" },
    { name: "15_outro",                marker: "thanks for watching" },
    { name: "16_end",                  marker: "__END__", trailingSilenceSec: 3 },
  ],

  slides: {
    "01_title": {
      colorAddress: colorAddress("mintcream"),
      // The aesthetic-24 title motif: 24 jeffreys, 24 Citrus-green Neos,
      // vertical space, all with an update to share. Constant across
      // every episode — vary lighting / architectural conceit if needed
      // but keep the ensemble.
      metaphor: `Photographic real-estate / architectural-magazine style \
group photo, candid not staged. The man in the reference photos appears as \
TWENTY-FOUR INSTANCES of himself populating a tall, cool, vertically \
oriented MULTI-TIER SCAFFOLDED LOFT — open wooden platforms / metal \
scaffolding stacked vertically across the frame, exposed beams, open \
risers between tiers. About four to six tiers stacked top to bottom of \
the 1024x1536 portrait frame, with three to five jeffreys per tier at \
their own little stations. Airy, daylit, warm-cream walls behind, soft \
natural light from skylights or tall windows. \
\
ALL 24 instances are the SAME MAN — same face, same medium-length brown \
hair, same features as the references — clones of the same person. Each \
jeffrey is in DIFFERENT casual home clothing from his REAL wardrobe \
(NOT a uniform, NEVER tank tops, NEVER costumes): hoodies in varied \
muted colors (gray, navy, olive, burgundy, cream), button-down shirts \
(flannel patterns or plain solid colors), printed t-shirts (lowercase \
single-word screenprints — soft cotton casual), cardigans in earth \
tones, pullover sweaters with subtle patterns or stripes, flannels over \
a tee. Hair varies slightly in disarray. They're recognizable as the \
SAME PERSON in different moods / outfits, NOT a uniformed assembly. \
\
EACH JEFFREY IS BUSY WITH MULTIPLE DEVICES — he's a busy guy. Standard \
station setup: at LEAST one open Citrus-green Apple MacBook Neo (Apple's \
bright yellow-green color) — the green laptops are the unifying visual \
across all 24 stations. PLUS each station also has 1–2 of these EXTRAS \
varying across the ensemble: a second laptop (sometimes an older black \
ThinkPad next to the green Neo), a smartphone in hand, a fountain pen \
mid-write on a notebook, a clipboard of papers, an iPad on a stand, a \
small handheld game device, an extra external monitor, a coffee mug, a \
synth keyboard, headphones around the neck, a microphone arm. Some \
jeffreys have a laptop ON THEIR LAP and another on the desk. Some have \
the Neo open with a phone in their other hand. Variety — not every \
station identical. The vibe is "everyone has more than they can do." \
\
Each jeffrey is in a small "I have an update to share!" pose — variety \
of micro-expressions and gestures, all chest-level: one leaning back \
gesturing at his screen, one turning to a neighbor with a finger raised \
mid-explanation, one holding up a paper at his desk-mate, one mid-laugh \
pointing at his laptop, one typing intently and grinning, one writing \
in a notebook with the fountain pen, one talking on the phone, one in \
a small mock-shock oh-face, one nodding along with headphones. Most \
face toward the camera or toward a neighbor. None directly above \
another's head. \
\
EXACTLY ONE of the 24 jeffreys (just one — somewhere in the middle of \
the frame, not on the edges) is mid-puff on his vape — a small \
USB-stick-shaped pen visible in his hand with its tiny LED tip glowing. \
A single natural wisp of vape vapor curls up from his station, catching \
the warm light as it drifts toward the upper tier. NOT heavy fog, NOT \
multiple vapers — just one, a small atmospheric detail. Other jeffreys \
may have their vape pens visible on their desks (resting beside the \
laptop) but only the one is actively puffing. \
\
LIGHTING (richer for the cover): the loft has multiple practical light \
sources to give the frame depth — warm Edison-bulb pendant lamps hanging \
between tiers, a row of soft mintcream string lights running along one \
wall or the ceiling beams, a tall floor lamp throwing warm light from one \
corner, plus the bright daylight through skylights / tall windows. The \
mix creates pools of warm light and softer shadow, layered across the \
scaffolding, with the vape vapor catching glints. Cinematic-but-candid \
photographic depth, not flat magazine evenness. \
\
Real photograph, photo-realistic — slightly architectural-magazine but \
candid, like a friend dropped by during a busy work morning. Slight film \
grain, real shadows, real depth. Identity grounded by the references. NOT \
illustrated, NOT painted, NOT cinematic-glossy, NOT a uniform/team-photo. \
Vertical 1024x1536 portrait orientation. \
\
COMPOSITION (the title text overlays the top of the frame): \
The TOP ~35% (y=0..540 of the 1024x1536 photo) is QUIET BACKGROUND ONLY \
— empty wall, ceiling beams, sky through skylight, upper-tier rafters, \
or hanging plant. NO JEFFREYS, NO LAPTOPS in this top zone. The first \
/ topmost row of jeffreys begins at y≈600 of the photo, not earlier. \
The 24-jeffrey ensemble lives in the central band (y≈600..1150). The \
BOTTOM 25% (y=1150..1536) shows the floor / foundation / open foreground \
— no jeffreys cropped at the bottom edge.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/01_title.png" } },
      body: ({ photo }) => titleSlide({
        photo,
        color: colorAddress("mintcream"),
      }),
    },

    "02_menuband_arc": {
      colorAddress: colorAddress("orangered"),
      metaphor: `${REAL}

Scene: he is at a home desk with a Citrus-green Apple MacBook Neo (Apple's \
bright yellow-green color) open in front of him. Anchored to the top-right \
corner of the screen, a small floating panel droops down from the menu bar — \
inside it, a tiny piano keyboard with each key glowing a different rainbow \
hue, a thin level meter, and a single bold word in chunky custom geometric \
letterforms across the top. A purple square app icon with two black piano \
keys peeks from the dock. Pinned along the top edge of the laptop's lid \
with painter's tape: a paper ladder of six index cards in ascending order, \
hand-numbered "0.4 / 0.5 / 0.6 / 0.7 / 0.8 / 0.9" in black sharpie, with \
the topmost card circled twice in fat orangered marker and a victory check \
beside it. He is leaning forward toward the laptop with both fists pumped at chest \
height in a mock-celebratory pose, FACE FULLY VISIBLE turned to the camera, \
mouth wide open in a HUGE proud laugh, eyes wide and bright, eyebrows up, \
head upright (not tilted back). Both hands stay below shoulder level. An \
orangered desk lamp directly to his right throws a hot orangered wash \
across the left side of his face, the laptop lid, and the index-card \
ladder; the right side of the room stays cool and unlit. Hoodie, slightly \
bedheaded. Shirt word (visible under the open hoodie): \`menuband\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/02_menuband_arc.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "01 / 15 · menuband · 0.4 → today",
        title: "six releases,\nvoice palette\n→ chord finder",
        cap: "chord finder · rainbow keys · slim meter · sharp title",
        color: colorAddress("orangered"),
      }),
    },

    "03_waveform_strip": {
      colorAddress: colorAddress("deeppink"),
      metaphor: `${REAL}

Scene: he is at a kitchen table in front of his Citrus-green MacBook Neo \
(Apple's bright yellow-green color), a black wired mechanical keyboard \
pulled forward into his lap, both hands mid-chord on the home row. A small \
popover is open in the upper-right of the screen with a tiny piano keyboard \
at the top and, just below it, a row of about sixteen short stacked-segment \
bars lit hot-pink, mid-bounce — clearly reacting to the chord he's holding. \
Wired over-ear headphones are around his neck, one cup off-ear so he can \
still hear the room. His mouth is open in a wide surprised laugh, eyebrows \
up, like the bars jumped higher than he expected. Diegetic deeppink: a \
hot-pink LED bias strip taped to the back of the laptop lid spills onto \
the wall behind him in a soft halo, and a small pink neon "OPEN" sign on \
the windowsill throws a second pink wash across his cheek and the keys. A \
half-empty water glass picks up the pink as a magenta highlight on its \
rim. Evening, kitchen, lived-in. Shirt word: \`menuband\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/03_waveform_strip.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "02 / 15 · menuband · audio strip",
        title: "slides under\nthe piano,\non note activity",
        cap: "shared meter · menubar · popover · palette",
        color: colorAddress("deeppink"),
      }),
    },

    "04_floating_palette": {
      colorAddress: colorAddress("mediumspringgreen"),
      metaphor: `${REAL}

Scene: bright morning at his wooden home desk, Citrus-green MacBook Neo \
(Apple's bright yellow-green color) open in front of him. A small \
translucent tool window has clearly been ripped free of the top menu bar \
and is hovering above the desktop area — you can see a faint \
mediumspringgreen tracer line in the air showing the path it just \
travelled down from the top of the screen. The window is alive: a dark \
inset visualizer at the top with bright pills along its edge spelling out \
three held notes, a chunky stylized piano in the middle, a smaller \
computer-keyboard map beneath it. He is mid-drag, one fingertip pressed \
precisely on the slim grip bar at the very top of the window — not the \
body — eyes wide and mouth open in a huge cartoon laugh, like he just \
caught a butterfly. His other hand thrown up in mock-celebration. A short \
string of mediumspringgreen LED fairy lights is taped along the wall \
behind the monitor, washing his face and the floating window in soft \
#00fa9a glow; that glow is the only colored light in the frame. Shirt word: \
\`menuband\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/04_floating_palette.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "03 / 15 · floating palette",
        title: "tear it off,\ntake it\nwith you",
        cap: "grab the top strip · floats over anything",
        color: colorAddress("mediumspringgreen"),
      }),
    },

    "05_menuband_engineering": {
      colorAddress: colorAddress("royalblue"),
      metaphor: `${REAL}

Scene: he is at a wooden home desk, both hands deliberately planted on \
the keyboard of an open Citrus-green MacBook Neo (Apple's bright \
yellow-green color) — left pinky pinning the shift key, ring finger \
latched on caps lock, right hand spread across four white keys at once. \
A small popover floats above the menubar showing a real two-octave piano \
keyboard with FOUR keys simultaneously lit royalblue (#4169e1), the \
labels rendered in UPPERCASE as the linger cue, and a tiny royalblue \
tilde flourish curling off the music note icon. A horizontal level strip \
below the piano glows the same royalblue, four overlapping lanes clearly \
stacked (polyphony, not one). The royalblue light from the screen rakes \
across his face and catches the underside of his chin and his glasses. A \
small royalblue desk lamp on the left edge of the desk adds a second \
source on his left cheek. He is leaning into the keyboard like a smug \
church organist mid-chord — chest puffed, eyebrows up, a knowing \
closed-mouth proud-dad smirk aimed at the camera, mock-conductor \
confidence. He is wearing a soft button-down with the word \`menuband\` \
embroidered in clean white stitch on the chest pocket — small, subtle, \
the kind of detail you'd only notice if you leaned in.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/05_menuband_engineering.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "04 / 15 · engine room",
        title: "shader compiles\nitself,\neight voices,\nhold to ring",
        cap: "quiet when hidden · shift / caps-lock linger",
        color: colorAddress("royalblue"),
      }),
    },

    "06_slab_polygon_lid": {
      colorAddress: colorAddress("slateblue"),
      metaphor: `${REAL}

Scene: he is sitting on a rug on the floor at night, a closed Citrus-green \
MacBook Neo (Apple's bright yellow-green color) on the rug in front of him, \
lid down. He is leaning his ear an inch from the closed laptop's hinge \
seam, listening — eyebrows shot up to his hairline, eyes wide, mouth \
slightly open in a "wait, really?" expression, mock-stunned that the \
closed laptop is humming a soft pad at him. A slateblue (#6a5acd) \
night-table lamp on the floor behind him is the only room light, washing \
one side of his face and the laptop lid in moody slateblue. On the wall \
behind him, a second small monitor is left awake — its screen tiny in the \
frame but legible: the macOS menubar with a small five-sided polygon icon, \
all five edges the same slateblue hue, sitting next to the clock. He has \
one hand cupped behind his ear, the other holding a mug. Late-night, very \
quiet, one houseplant casting a long shadow. Shirt word: \`chat\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/06_slab_polygon_lid.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "05 / 15 · slab · one shape, one hue",
        title: "n tabs,\nn-sided shape,\ngreen slides red\nas the queue grows",
        cap: "closed lid hums when claude is waiting",
        color: colorAddress("slateblue"),
      }),
    },

    "07_hp_plotter": {
      colorAddress: colorAddress("olivedrab"),
      metaphor: `${REAL}

Scene: he is kneeling on the concrete floor of a cluttered home garage \
next to a real, large 1980s-era HP 7585B drafting plotter — a wide \
refrigerator-sized beige-cream pen plotter with a black control strip on \
the right, eight technical pens visible in a circular carousel along the \
top edge, and a fresh sheet of white paper loaded across the bed. A \
thick chunky vintage serial cable with two big screws on its connector \
runs from the back of the plotter down to a Citrus-green MacBook Neo \
(Apple's bright yellow-green color) sitting open on an upturned milk \
crate next to him. The plotter's pen arm is mid-stroke, drawing the PALS \
mark — a small purple hand-shaped glyph, like a stylized wave with \
sprouting fingers — in glossy purple ink on the paper. Diegetic olivedrab \
(#6b8e23) light: a clamp-on metal-shop work lamp hooked to a shelf \
above, fitted with an olivedrab gel that throws an olivedrab wash across \
the plotter's case and the paper; a long olivedrab fluorescent tube \
buzzes overhead; olivedrab safety glasses pushed up on his forehead \
catching the light. He is throwing his head back in a giant belly-laugh, \
mouth wide open, eyes crinkled shut, both hands half-raised in a "it's \
actually drawing!" mock-celebration. Concrete floor, pegboard wall behind. \
Shirt word: \`line\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/07_hp_plotter.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "06 / 15 · hp 7585b plotter",
        title: "hp 7585b,\nover serial",
        cap: "first plot: the pals mark, in purple",
        color: colorAddress("olivedrab"),
      }),
    },

    "08_fedac_native": {
      colorAddress: colorAddress("sienna"),
      metaphor: `${REAL}

Scene: he is in a real home garage / workshop — concrete floor, exposed \
stud wall, a workbench with a vise, scattered cables, a clip-on shop \
lamp. He is standing in the middle of the shot holding up an open \
Panasonic Toughbook with both hands like a championship trophy: a \
chunky ruggedized laptop, black-and-tan industrial plastic with big \
bumpered corner armor, a thick keyboard, an integrated handle on the \
lid hinge — clearly a field machine, not a consumer laptop. The \
Toughbook screen is on and clearly shows a custom operating system \
booting: green-on-black terminal text scrolling a boot log, mono font, \
a small ASCII banner at the top, the screen actually emitting a soft \
phosphor green glow onto his face and chin. The room is lit \
diegetically by an old sienna-shaded enamel desk lamp clamped to the \
workbench, throwing warm sienna (#a0522d) brown-orange light across \
the back wall, plus late sienna sunset coming through a small dusty \
workshop window on the right — both real practical light sources in \
the scene, no overlay. His Citrus-green MacBook Neo (Apple's bright \
yellow-green color) sits closed on the workbench in the background, \
clearly the everyday laptop set aside while the Toughbook gets booted. \
His expression is HYPERBOLIC mock-celebratory: huge proud-dad \
eyebrow-raise, mouth open in a triumphant "ehhhh" grin, like he just \
won a small trophy at a county fair. He's in a worn flannel button-down \
with \`os\` embroidered in white stitch on the chest pocket — subtle, \
hand-tailored feeling, perfect for the workshop.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/08_fedac_native.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "07 / 15 · ac native os",
        title: "piano level,\ntoughbook\nbench open",
        cap: "piano sits with sine + square · toughbook is next",
        color: colorAddress("sienna"),
      }),
    },

    "09_arxiv_figures": {
      colorAddress: colorAddress("indigo"),
      metaphor: `${REAL}

Scene: a home desk completely buried in printed pages of the same \
academic preprint, fanned out and overlapping. Visible on specific \
sheets: one page shows three keyboard outlines drawn in clean \
black-on-white technical line art, stacked vertically with the letter \
caps labeled. Another shows a honeycomb of hexagonal buttons each \
labeled with a note name, with three of the hexes shaded to mark a \
triad. Another shows two horizontal strings with EADGBE and GDAE marked \
along them. A small staircase chart with labeled steps sits at one \
corner. He is holding an indigo highlighter mid-stroke, finger raised \
in super-smug professor-pointing mode — that "actually-" gesture, mouth \
open, eyes wide, deeply pleased with himself. The Citrus-green MacBook \
Neo (Apple's bright yellow-green color) on the desk shows the paper's \
PDF preview, screen tilted toward camera. Diegetic indigo light source: \
a deep-indigo glass-shaded incandescent desk lamp with the bulb glowing \
through the colored glass, throwing indigo cast across the pages and \
onto his face from the side. The room behind is dim; indigo is the \
only colored light. Shirt word: \`papers\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/09_arxiv_figures.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "08 / 15 · arxiv-keymaps · figures",
        title: "qwerty/dvorak,\ndaw chromatic,\nwicki-hayden,\nvim, singmaster",
        cap: "asymmetric instruments · guitar + violin tunings",
        color: colorAddress("indigo"),
      }),
    },

    "10_arxiv_prose": {
      colorAddress: colorAddress("darksalmon"),
      metaphor: `${REAL}

Scene: he is curled into a worn reading chair, knees up, the Walter Ong \
"Orality and Literacy" paperback open across one thigh — cream cover, \
red title block visible on the back as it folds, slim Methuen-era \
paperback creased at the spine. In his other hand he holds a printed \
manuscript page; two phrases are circled in darksalmon highlighter and \
clearly legible across the page: "WASD" and "notation surface." A \
darksalmon Tiffany-style stained-glass reading lamp is clamped to the \
chair arm and pours warm pink-orange light across his face, the book, \
and the printed page — every shadow is salmon-tinted because the lamp \
is the only light source. His Citrus-green MacBook Neo (Apple's bright \
yellow-green color) is on the rug at his feet, screen tilted up, \
showing the .tex draft in a plain editor. A small stack of paperbacks \
leans against the chair leg. His expression is hyperbolic \
scholar-amazed: head tipped back a quarter-turn, eyes wide and round \
behind his glasses, mouth open in a silent "oh!" — the look of \
realizing one paragraph just answered another. Evening, no overhead \
light, just the salmon glow. He's in a soft cream button-down with the \
word \`papers\` embroidered in muted thread on the chest pocket — \
quiet, scholarly, fits the reading-chair register.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/10_arxiv_prose.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "09 / 15 · arxiv-keymaps · prose",
        title: "ong + wasd,\nnotation surface,\nlgbtq+ framing",
        cap: "live-coding mini-notations · tidal, strudel, hydra, abc",
        color: colorAddress("darksalmon"),
      }),
    },

    "11_papers_infra": {
      colorAddress: colorAddress("mediumaquamarine"),
      metaphor: `${REAL}

Scene: he is at a wooden home desk in front of a Citrus-green MacBook \
Neo (Apple's bright yellow-green color). The laptop screen shows a \
calendar-style list of upcoming dates with venue names visibly readable \
in the rows — "SIGGRAPH ASIA," "fia-grant," "keymaps," "platter" — \
stacked like a month view. Running along the desk's left edge is a neat \
column of FOUR real mediumaquamarine (#66cdaa) sticky notes, each \
hand-labeled in black sharpie with one of those same names, arranged \
like a physical mirror of the on-screen list. Beside the laptop, a \
small iPad in a stand shows a prompt-style input with the word \
"deadlines" half-typed and a dropdown suggestion underneath it, glowing \
faintly. Next to a few rows on the laptop, smaller secondary lines \
visibly lead to draft links — described as little tab-stops of \
paper-clipped index cards taped to the bezel. On the wall behind him, \
a small real wooden picture frame holds a printed grid of jeffrey \
portraits — the restored platter, hanging like a family photo. \
Diegetic mediumaquamarine: a small mediumaquamarine-shaded gooseneck \
desk lamp clamped to the desk pours green-cyan light across his face \
and the stickies; a thin LED strip behind the laptop washes the wall \
the same hue. He is leaning forward toward the camera with both palms turned up at \
chest level — game-show host "BEHOLD" style, hands NEVER above shoulder \
level — face fully visible centered in the frame, mouth wide open in big \
proud laugh, eyes wide, eyebrows up. Hyperbolic, theatrical, like a \
game-show host revealing a board. Shirt word: \`papers\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/11_papers_infra.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "10 / 15 · papers · infrastructure",
        title: "deadlines page,\ndraft links,\nvenue list,\nplatter back",
        cap: "fia's opportunities · cv hidden · keymaps on the books",
        color: colorAddress("mediumaquamarine"),
      }),
    },

    "12_electron_arena": {
      colorAddress: colorAddress("crimson"),
      metaphor: `${REAL}

Scene: he is at a real home desk gripping a black Xbox Wireless \
Controller in both hands, thumbs on the sticks — the matte-black shell, \
the four colored face buttons in their classic ABXY diamond, the \
textured grips clearly visible. The green A button is glowing hot \
crimson from underneath, like it just got slammed. His mouth is wide \
open in a screaming oh-face — a hyperbolic "I'M IN" gamer howl, eyes \
huge, eyebrows up, head tilted slightly back. Behind the controller, \
his open Citrus-green MacBook Neo (Apple's bright yellow-green color) \
shows a 3D first-person arena scene: a tessellated grid floor, a low \
horizon, a small remote player avatar visible across the field, a \
speed meter HUD glowing in the corner. Beside the laptop, a small \
black command-line window with green-on-black text scrolls quietly. \
Diegetic crimson: a long crimson gaming LED strip taped to the back \
edge of the monitor, washing the wall behind in deep crimson, with a \
smaller crimson rope light under the desk catching the controller from \
below — the entire room half-bathed in red as if a "PLAYER 1 READY" \
sign somewhere off-frame just lit up. Empty energy-drink can on the \
desk. Slightly bedhead, hoodie. Slight motion blur on the thumbs. Shirt \
word (visible under the open hoodie): \`arena\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/12_electron_arena.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "11 / 15 · ac-electron + arena",
        title: "xbox bridge\nactually loads,\nA + space\nrespawn",
        cap: "ac-desktop cli · arena dead zone widened",
        color: colorAddress("crimson"),
      }),
    },

    "13_recap_pipeline": {
      colorAddress: colorAddress("palevioletred"),
      metaphor: `${REAL}

Scene: he is at a real wooden home desk, leaned slightly forward, \
fanning out a thick stack of fresh polaroid-style prints toward the \
camera like a poker hand. Each print in the fan is unmistakably a \
DIFFERENT staged candid of HIM — same face across all of them, but in \
clearly different rooms, outfits, poses (one at a kitchen table, one \
in a garage, one in evening lamplight, one mid-laugh, one deadpan). \
Two more of the same prints are pinned to a corkboard on the wall \
behind him. A small old-fashioned brass desk bell with a round \
push-button sits next to the laptop, catching a highlight. An open \
Citrus-green MacBook Neo (Apple's bright yellow-green color) on the \
desk shows a green-on-black terminal scrolling a build log — \
recognizable shell text, no readable filenames. A single sheet of \
hand-drawn waltz sheet music — three-four time signature, pencil \
quarter notes — is taped to the desk lamp arm. Diegetic palevioletred \
(#db7093) light is everywhere and clearly REAL: a small palevioletred \
Tiffany-style stained-glass desk lamp glows warm pink across his \
cheekbone and the polaroids; a thin palevioletred neon sign on the \
wall reads "RECAP" in a single cursive word; palevioletred fairy \
lights wrap the monitor bezel; a palevioletred-tinted curtain backlights \
the window behind him. Hyperbolic expression: head thrown back in a full \
belly laugh, eyes scrunched, both shoulders up — caught in the middle \
of cracking up at being inside the recap that's about the recap. Hoodie, \
slightly bedheaded. Shirt word (visible under the open hoodie): \`tapes\`.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/13_recap_pipeline.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "12 / 15 · recap pipeline",
        title: "portraits\npre-baked,\nsine-bells\nwaltz bed",
        cap: "each cut gets its own soft bell waltz",
        color: colorAddress("palevioletred"),
      }),
    },

    // Chat slides ALWAYS come in pairs: one for laer-klokken (clock) and one
    // for the main chat (system). Each gets its own laptop-series first-person
    // POV portrait so the viewer feels like they're looking out from inside
    // jeffrey's screen mid-conversation. The chat instance is named on the
    // slide config so jeffrey-photos.mjs picks the laptop-POV arc beat.
    "14a_chat_clock": {
      colorAddress: colorAddress("khaki"),
      chatInstance: "clock",
      // Laptop-series first-person POV. The camera IS the laptop screen.
      // Per-scene: jeffrey is reading laer-klokken (the Danish chat) — light
      // smile, head slightly tilted, eyes scanning the messages. He's at a
      // home desk. Khaki diegetic light: a small khaki-shaded reading lamp
      // on the left edge of the desk pours warm khaki across his face.
      metaphor: `${REAL}

Scene: this is a LAPTOP-SERIES FIRST-PERSON SHOT — the camera is where the \
laptop's webcam would be, looking UP at jeffrey from chest height, screen-side \
perspective. He is at a home desk facing the laptop, eyes flicking down at \
the screen reading a chat. The Citrus-green Apple MacBook Neo's bottom bezel \
edge is just visible at the FOOT of the frame, with the AC's smiley bug logo \
peeking up. He is mid-read, head slightly tilted, a small genuine smile \
forming — the kind of expression you have when a friend across the world is \
saying something funny in another language. His mouth is slightly open like \
he's about to half-laugh, eyebrows raised in mild surprise. Diegetic khaki \
(#f0e68c) light: a small khaki-fabric-shaded reading lamp on the left edge \
of his desk pours warm khaki light across the left side of his face and \
catches the rim of a coffee mug just out of focus on the desk surface. The \
right side of his face is in cooler ambient room light. Hoodie, slightly \
bedheaded. Shirt word: \`chat\`. Late-afternoon home desk vibe. \
\
COMPOSITION: head and shoulders fill the upper-middle band of the frame; \
his face is centered horizontally with his eyes near y=750 of the 1024x1536 \
photo. The TOP 25% (y=0..380) is quiet ceiling / wall — the chapter title \
overlays there. The BOTTOM 25% (y=1150..1536) is the desk surface and the \
peeking bottom edge of the green Neo's bezel.`,
      queries: {
        photo: { glob: "recap/out/jeffrey-photos/14a_chat_clock.png" },
        snapshot: { json: "recap/out/chat-snapshot.json" },
      },
      body: ({ photo, snapshot }) => chatSlide({
        photo,
        instance: "clock",
        messages: (snapshot && snapshot.clock) || [],
        chapter: "13 / 15 · laer-klokken",
        title: "from the\nclock chat",
        cap: "in danish, mostly · last 73 hours",
        color: colorAddress("khaki"),
      }),
    },

    "14b_chat_system": {
      colorAddress: colorAddress("gold"),
      chatInstance: "system",
      // Companion to 14a: same first-person laptop-POV setup, but he's now
      // reading the main thread — bigger laugh, more engaged, gold lamp.
      metaphor: `${REAL}

Scene: this is a LAPTOP-SERIES FIRST-PERSON SHOT — the camera is where the \
laptop's webcam would be, looking UP at jeffrey from chest height, screen-side \
perspective. He is at the same home desk facing the laptop, but now mid-laugh \
at something on the main chat — a hyperbolic open-mouth laugh, head tipped \
slightly back, eyebrows up, eyes crinkled. The Citrus-green MacBook Neo's \
bottom bezel edge is just visible at the FOOT of the frame, with the smiley \
bug logo peeking up. Diegetic gold (#ffd700) light: a small gold-shaded brass \
desk lamp on the right edge of the desk pours warm gold across the right \
side of his face — slightly different angle from the clock-chat scene so \
the two paired portraits read as a matched pair (left lamp / right lamp). \
A faint puff of vape vapor curls in from the right margin (he just put the \
USB-stick pen down, its tiny LED still glowing on the desk just out of focus). \
Hoodie. Shirt word: \`chat\`. Same late-afternoon vibe as 14a — these two \
slides are the same minute of the same afternoon, just two different chats. \
\
COMPOSITION: head and shoulders fill the upper-middle band of the frame; \
face centered horizontally with eyes near y=750 of the 1024x1536 photo. \
TOP 25% quiet ceiling / wall (chapter title overlays there). BOTTOM 25% \
shows the desk surface and the peeking bottom edge of the green Neo's bezel.`,
      queries: {
        photo: { glob: "recap/out/jeffrey-photos/14b_chat_system.png" },
        snapshot: { json: "recap/out/chat-snapshot.json" },
      },
      body: ({ photo, snapshot }) => chatSlide({
        photo,
        instance: "system",
        messages: (snapshot && snapshot.system) || [],
        chapter: "14 / 15 · chat",
        title: "from the\nmain thread",
        cap: "main chat · last 73 hours",
        color: colorAddress("gold"),
      }),
    },

    "15_outro": {
      colorAddress: colorAddress("lavenderblush"),
      metaphor: `${REAL}

Scene: he is leaning back in a desk chair holding a coffee mug, giving a \
small relaxed wave to the camera with his free hand. The Citrus-green laptop \
in front of him on the desk shows a screen reading "thanks for watching" \
in calm cream-on-dark text with a thin lavenderblush (#fff0f5) underline. \
A lavenderblush curtain is visible behind him, softly catching evening \
light. He has a small real smile, eye contact with the camera. \
Comfortable home setting — desk lamp on, warm white balance, a houseplant \
or two visible. Real iPhone candid snapshot energy. He's in a relaxed button-down with \`prompt\` embroidered in tiny white stitch on the cuff of his sleeve — visible only because his wrist is raised in the wave.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/15_outro.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "15 / 15 · outro",
        title: "thanks for\nwatching",
        cap: "aesthetic.computer · @jeffrey",
        color: colorAddress("lavenderblush"),
      }),
    },

    "16_end": `
      <div class="frame">
        <div class="pals med"></div>
        <div class="endline" style="color:${PALETTE.cream}">aesthetic·computer</div>
        <div class="endsub" style="color:${PALETTE.dim}">narrated by jeffrey-pvc · @jeffrey</div>
        <div class="endsub" style="color:${PALETTE.dim}">2026·04·29 → 2026·05·02</div>
      </div>`,
  },
};

// Slide chrome only — chapter color is DIEGETIC inside the photo (real
// in-scene light source) and the only on-slide overlays are EDITORIAL:
// chapter line + title + caption + color chip. No artifact insets — any
// production-URL or app visuals that need to appear should appear as
// content on screens within the rendered photo, not as overlay cards.
//
// Each title gets a luma-aware drop shadow so it stays readable against
// any photo: a sharp reverse-color 2px offset plus a soft halo. Bright
// chapter colors get black shadows; dark chapter colors get white ones.
// Episodic series number — every recap cut is an "episode" in the
// jeffrey-recap series ("aesthetic 24"). Bump for each new audience
// config. (The 24h-2026-04-30 cut was episode 1; this 73h-2026-05-02
// cut, which also folds in the 24h-2026-05-01 work, is episode 2.)
const EPISODE = 2;
const SHOW_NAME = "aesthetic 24";
const EPISODE_DATE = "2026·05·02";
const EPISODE_HOOK = "the last\n73 hours";

// Slide code: "e<N>-<color>". This is the ONLY editorial overlay on the
// photo per frame — the narration carries the verbal content; the photo
// carries the vibe. Title and cap are kept on each slide as data (for
// the SEO description / future use) but no longer rendered in the chrome.
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

// Title slide — the "newscast intro" frame. Shows the show wordmark
// "aesthetic 24", the episode tag, and the episode hook (the line
// jeffrey says first). Standard slide-code in the upper-left for
// consistency with the rest of the deck.
function titleSlide({ photo, color }) {
  const code = slideCode(color);
  const codeShadow = readabilityShadow(color.rgb);
  const creamShadow = "2px 2px 0 rgba(0,0,0,0.95), -1px -1px 0 rgba(0,0,0,0.7), 0 0 18px rgba(0,0,0,0.55)";
  const subShadow = "1px 1px 0 rgba(0,0,0,0.92), 0 0 14px rgba(0,0,0,0.6)";
  const hookHtml = EPISODE_HOOK.split("\n").map((l) => `<div>${l}</div>`).join("");
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
    </div>`;
}

function photoSlide({ photo, title, cap, color }) {
  const titleHtml = (title || "").split("\n").map((l) => `<div>${l}</div>`).join("");
  const code = slideCode(color);
  // Slide code in chapter color (luma-aware shadow). Title + cap in cream
  // with plain dark drop shadow so the photo's chapter color stays an
  // accent, not the dominant typographic statement. No bottom chip /
  // swatch / rgb — the slide code is the only chapter-color element.
  const codeShadow = readabilityShadow(color.rgb);
  const creamShadow = "2px 2px 0 rgba(0,0,0,0.95), -1px -1px 0 rgba(0,0,0,0.7), 0 0 18px rgba(0,0,0,0.55)";
  const capShadow = "1px 1px 0 rgba(0,0,0,0.92), 0 0 14px rgba(0,0,0,0.6)";
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
    </div>`;
}

// Chat slide — single-instance variant. Renders ONE chat (clock OR system)
// over a laptop-POV portrait backdrop. The episode includes two of these
// per cut: 14a (clock = laer-klokken) and 14b (system = main chat). The
// `messages` array is pre-resolved by the caller from `chat-snapshot.json`.
//
// Layout: the photo fills the frame (laptop-POV first-person of jeffrey
// reading), with a top gradient holding the chapter line + title + cap,
// and a bottom-right "card" panel listing the messages over a smoked
// scrim so they stay readable against any photo. The slide-code chip
// uses the chapter color (luma-aware shadow).
function chatSlide({ photo, instance, messages, title, cap, color }) {
  const code = slideCode(color);
  const codeShadow = readabilityShadow(color.rgb);
  const creamShadow = "2px 2px 0 rgba(0,0,0,0.95), -1px -1px 0 rgba(0,0,0,0.7), 0 0 18px rgba(0,0,0,0.55)";
  const capShadow = "1px 1px 0 rgba(0,0,0,0.92), 0 0 14px rgba(0,0,0,0.6)";
  const titleHtml = (title || "").split("\n").map((l) => `<div>${l}</div>`).join("");
  const label = instance === "clock" ? "laer-klokken" : "chat";
  // Trim to the most recent messages that fit in the panel (~10 fits comfortably
  // at the chosen panel size); use slice(-10) to keep the FRESHEST.
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

export default audience;
