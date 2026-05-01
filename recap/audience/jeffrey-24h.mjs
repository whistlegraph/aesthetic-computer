// Audience config: jeffrey-24h (last 24 hours, photographic / jeffrey-as-protagonist).
// Voice: jeffrey-pvc. Style: lowercase, calm, descriptive.
//
// Each content slide is a full-bleed gpt-image-2 photo (jeffrey at a real home
// desk, using the imagined feature on his laptop) with a bottom-third gradient
// overlay carrying chapter / title / caption text. Photos are pre-baked by
// `bin/jeffrey-photos.mjs` into recap/out/jeffrey-photos/<segment>.png and
// loaded via the standard scout `glob:` query.
//
// `metaphor` on each slide is the prompt fed to gpt-image-2 (with the platter
// SHOOT_REFS + SELFIE_REFS for identity grounding). It is ignored by slides.mjs.

export const PALETTE = {
  bg: "#0c1430",
  accent: "#ff8a3d",
  cyan: "#70f0e0",
  lime: "#a0f070",
  magenta: "#ff70d0",
  yellow: "#ffd860",
  cream: "#fcf7c5",
  off: "#ffffffcc",
  dim: "#7886b0",
};

// Shared identity + tone preamble. Real iphone-snapshot energy, NOT cinematic.
const REAL = `\
Photographic candid lifestyle photo of the man in the reference photos. Real \
photograph, photo-realistic, the kind of casual iPhone snapshot a friend would \
take of him at home — NOT illustrated, NOT painted, NOT cinematic, NOT \
magazine-glossy, NOT AI-poster-glossy. Slight film grain, slightly off-center \
framing, real natural room light. Identity: same face, same medium-length brown \
hair, same actual features as the references — recognizably him across the \
various refs. Keep real skin texture; do NOT smooth or prettify. He is at a \
real home desk in normal everyday clothes (t-shirt or hoodie, slightly bedheaded). \
Real, deadpan, very "him". Vertical 1024x1536 portrait orientation.`;

export const audience = {
  name: "jeffrey-24h",
  handle: "@jeffrey",
  voice: { provider: "jeffrey", voice: "neutral:0" },

  narration: `hey everybody, here's a quick window into the last twenty-four hours at aesthetic computer. the headline this round is menuband — version 0.3 of the mac menubar app shipped with a real audio synth, sample pads, a metal-rendered visualizer, and click sounds on every key. the landing page got a fresh polish too — a thinner window, a wackier note pattern, and a tagline that quietly digs at garageband. on the visual side, jeffrey-platter went public — a dashboard of every confirmed jeffrey portrait now lives at papers dot aesthetic dot computer slash jeffrey. underneath the hood, lith got new caddy plumbing — an install-before-reload webhook, an explicit redirect, and a try-files chain that finally serves the platter cleanly. on the backend, the say piece grew a little time machine — a rehydrate script pulls every old utterance back from cache into the database. through it all, the oven kept its rhythm, baking three hundred plus pdf updates in the background. that's the day. thanks for watching, and i'll be back tomorrow with the next twenty-four.`,

  // Whisper renders these dictionary-style; rewrite the displayed subtitles.
  // Order matters: longer/multi-word fixes first.
  transcriptFixes: {
    "a static computer": "aesthetic computer",
    "static computer": "aesthetic computer",
    "papers.aesthetic.com/jeffrey": "papers.aesthetic.computer/jeffrey",
    "Jeffrey-Platter": "jeffrey-platter",
    "Jeffrey Platter": "jeffrey-platter",
    "jeffrey platter": "jeffrey-platter",
    "Menu Band": "menuband",
    "menu band": "menuband",
    "Menuband": "menuband",
    "Menu Bar": "menubar",
    "menu bar": "menubar",
    "Garage Band": "garageband",
    "garage band": "garageband",
    "GarageBand": "garageband",
    "tri-files": "try-files",
    "Lyth": "lith",
    "Lith": "lith",
    "Caddy": "caddy",
    "TTS": "tts",
    "MongoDB": "mongo",
    "Mac": "mac",
    "MacBook": "macbook",
    "Jeffrey": "jeffrey",
  },

  segments: [
    { name: "01_title", marker: "hey everybody" },
    { name: "02_menuband", marker: "the headline this round" },
    { name: "03_landing", marker: "the landing page" },
    { name: "04_platter", marker: "went public" },
    { name: "05_lith", marker: "got new caddy plumbing" },
    { name: "06_say", marker: "grew a little time machine" },
    { name: "07_oven", marker: "through it all" },
    { name: "08_outro", marker: "thanks for watching" },
    { name: "09_end", marker: "__END__", trailingSilenceSec: 3 },
  ],

  slides: {
    "01_title": {
      metaphor: `${REAL}

Scene: he is sitting at a wooden home desk in a wrinkled t-shirt, holding up a \
single sheet of plain white printer paper next to his face. The paper has \
"24 HRS" hand-drawn on it in thick black sharpie, slightly crooked letters. He \
is giving a deadpan grin with one slightly raised eyebrow, mid-laugh, \
eye-contact with the camera. An open silver MacBook on the desk in the \
background shows a calendar app with a single highlighted day. Soft afternoon \
light from a window on the right, warm white balance. A hot-pink (#ff6b9d) \
coffee mug sits on the desk — the only pop of accent color. Wide shot, his \
whole upper body and the desk visible. Vertical, 1024x1536.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/01_title.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "00 / 08 · the last 24 hours",
        title: "aesthetic computer",
        cap: "2026·04·29",
        titleColor: PALETTE.cream,
        chapterColor: PALETTE.cyan,
      }),
    },

    "02_menuband": {
      metaphor: `${REAL}

Scene: he is hunched over a silver MacBook on a kitchen table, leaning very \
close to the screen with one finger pointing dramatically at the upper-right \
corner of the display where a small popover window has appeared in the macOS \
menu bar. The popover on screen clearly shows: tiny illustrated piano keys at \
the bottom, a horizontal row of small square sample pads above the keys, and a \
thin audio visualizer with vertical cyan (#4ecdc4) and hot-pink (#ff6b9d) bars \
dancing. His other hand is hovering above the laptop keyboard miming a tiny \
piano-playing gesture, comically exaggerated. He is wearing a hoodie, slightly \
bedheaded. Bright midday natural light from a window. A real coffee mug sits \
next to the laptop. Real candid energy, slightly absurd, deadpan focus.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/02_menuband.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "01 / 07 · menuband",
        title: "menuband 0.3",
        cap: "synth · sample pads · metal visualizer · click sounds",
        titleColor: PALETTE.accent,
        chapterColor: PALETTE.cyan,
      }),
    },

    "03_landing": {
      metaphor: `${REAL}

Scene: he is sitting on the floor of a bright apartment living room, legs \
crossed, with his silver MacBook perched on the seat of an armchair in front \
of him — an absurd ergonomic situation he is clearly content with. The laptop \
screen shows a web browser open to a website: a thin titlebar reading \
"menuband" in lowercase, a tiled wallpaper of small repeating musical-note \
glyphs in cream and pale yellow, and a tagline line of text beneath. He is \
leaning forward, scrolling with one finger on the trackpad, looking quietly \
pleased with himself. Soft warm lamp light from a floor lamp. A house plant in \
the corner. Wood floor, a rug. Real iPhone candid energy.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/03_landing.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "02 / 07 · landing page",
        title: "thinner window,\nwackier notes",
        cap: "menuband.aesthetic.computer",
        titleColor: PALETTE.yellow,
        chapterColor: PALETTE.cyan,
      }),
    },

    "04_platter": {
      metaphor: `${REAL}

Scene: he is at a home desk, his silver MacBook screen filled with a tight \
grid of small thumbnail portrait photos — dozens of headshots and selfies that \
are clearly all of HIM, the same man, across many years and outfits. He is \
holding a small printed 4x6 photograph of himself up beside the laptop screen, \
comparing the printed photo to one of the on-screen thumbnails with an \
exaggerated mock-puzzled expression — eyebrows together, lips pursed, \
mock-serious investigation. Soft afternoon light from a window. A normal \
bedroom or studio behind him, slightly cluttered. Real candid, slightly \
absurd recursion gag, deadpan.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/04_platter.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "03 / 07 · jeffrey-platter",
        title: "the dashboard,\npublic now",
        cap: "papers.aesthetic.computer/jeffrey/",
        titleColor: PALETTE.magenta,
        chapterColor: PALETTE.cyan,
      }),
    },

    "05_lith": {
      metaphor: `${REAL}

Scene: he is crouched on the floor underneath his home desk, surrounded by a \
real tangle of black ethernet and power cables, with a silver laptop balanced \
precariously on his knees. The laptop screen shows a terminal window with \
"Caddyfile" config text — multiple lines of routing rules — and one specific \
"try_files" line highlighted in orange (#ff8a3d). Bright yellow sticky notes \
are stuck to the underside of the desk above him reading "CADDY" and \
"WEBHOOK" in black sharpie. He is holding a coffee mug in his free hand, \
deadpan, looking up directly at the camera with mock weariness. Slightly dim \
under-the-desk light from the laptop glow plus one warm lamp peeking in from \
beyond the desk. Real, deadpan, very "him".`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/05_lith.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "04 / 07 · lith caddy",
        title: "install before\nreload",
        cap: "webhook · redirect · try_files",
        titleColor: PALETTE.accent,
        chapterColor: PALETTE.cyan,
      }),
    },

    "06_say": {
      metaphor: `${REAL}

Scene: he is at his home desk, the silver MacBook screen showing a scrolling \
list of text snippets — each row a short hand-written-style phrase next to a \
tiny green-on-black audio waveform thumbnail. Stacks of old microcassette \
tapes and one chunky USB cassette reader are piled on the desk around the \
laptop. He is holding a single cassette tape up to the laptop's built-in \
microphone in a mock-serious "recording in progress" pose, eyes wide deadpan, \
mouth slightly open as if to whisper into the tape. The wall behind him has \
five or six small sticky notes, each with a hand-written quoted phrase in \
sharpie. Warm afternoon light. Real, very absurd, very "jeffrey".`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/06_say.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "05 / 07 · say",
        title: "every utterance,\nrehydrated",
        cap: "tts-cache → mongo · backfill-sayings",
        titleColor: PALETTE.lime,
        chapterColor: PALETTE.cyan,
      }),
    },

    "07_oven": {
      metaphor: `${REAL}

Scene: he is at his home desk wearing a pair of bright red kitchen oven mitts, \
lifting a thick stack of warm printed paper out of an open cardboard moving \
box on the desk. The box has "PDF" written on the side in chunky black \
marker. The silver laptop next to the box on the desk shows a terminal log \
scrolling with repeating lines like "[papers] oven auto-build: 6 PDFs \
updated". He is giving a deadpan flat expression to the camera, mouth \
straight. Behind him on a low shelf, more stacks of warm-looking paper sit on \
a wire metal cooling rack like baked goods cooling. Late golden-hour window \
light. Real candid, deadpan, slightly absurd.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/07_oven.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "06 / 07 · oven",
        title: "papers, baking",
        cap: "305 auto-builds in 24 hours",
        titleColor: PALETTE.yellow,
        chapterColor: PALETTE.cyan,
      }),
    },

    "08_outro": {
      metaphor: `${REAL}

Scene: he is leaning back in his desk chair holding a coffee mug, giving a \
small exaggerated wave to the camera with his free hand. The silver laptop in \
front of him on the desk shows a screen reading "thanks for watching" in a \
calm cream-on-dark text layout with a thin cyan and pink underline. He is \
giving a small relaxed smile, real eye contact with the camera. Comfortable \
home setting — desk lamp on, evening softness, warm white balance. Wood desk, \
a houseplant or two visible. Real iPhone candid snapshot energy.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/08_outro.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "07 / 07 · outro",
        title: "thanks for\nwatching",
        cap: "aesthetic.computer · @jeffrey",
        titleColor: PALETTE.cream,
        chapterColor: PALETTE.cyan,
      }),
    },

    "09_end": `
      <div class="frame">
        <div class="pals med"></div>
        <div class="endline" style="color:${PALETTE.cream}">aesthetic·computer</div>
        <div class="endsub" style="color:${PALETTE.dim}">narrated by jeffrey-pvc · @jeffrey</div>
        <div class="endsub" style="color:${PALETTE.dim}">2026·04·29</div>
      </div>`,
  },
};

// Renders a full-bleed photo slide with a TOP gradient overlay carrying the
// chapter / title / caption text. Top padding leaves room for Instagram-stories
// progress chips and other top-of-frame UI. `position: fixed` ignores the
// body's 80px / 70px padding so the photo genuinely fills 1080x1920.
function photoSlide({ photo, chapter, title, cap, titleColor, chapterColor }) {
  const titleHtml = title.split("\n").map((l) => `<div>${l}</div>`).join("");
  return `
    <div style="position: fixed; inset: 0; padding: 0; overflow: hidden;">
      ${photo
        ? `<img src="${photo}" style="position: absolute; inset: 0; width: 100%; height: 100%; object-fit: cover;" />`
        : `<div style="position: absolute; inset: 0; background: ${PALETTE.bg};"></div>`}
      <div style="position: absolute; top: 0; left: 0; right: 0; padding: 180px 70px 110px; background: linear-gradient(to bottom, rgba(0,0,0,0.88) 0%, rgba(0,0,0,0.5) 60%, rgba(0,0,0,0) 100%);">
        <div style="font-family: 'ProcessingR'; font-size: 30px; letter-spacing: 6px; text-transform: uppercase; color: ${chapterColor};">${chapter}</div>
        <div style="font-family: 'ProcessingB'; font-size: 110px; line-height: 1.0; letter-spacing: -2px; color: ${titleColor}; margin-top: 18px;">${titleHtml}</div>
        <div style="font-family: 'ProcessingR'; font-size: 38px; color: ${PALETTE.off}; margin-top: 26px; letter-spacing: 1px;">${cap}</div>
      </div>
    </div>`;
}

export default audience;
