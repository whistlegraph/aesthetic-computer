// Audience config: jeffrey-24h, 2026-04-30 cut.
// Voice: jeffrey-pvc. Style: lowercase, calm, descriptive.
//
// New this cut: each content slide carries a CSS color-name "address"
// drawn from `cssColors` in lib/num.mjs — the AC color dictionary.
// The chapter tint, the on-screen swatch, and the rgb caption all
// derive from one named entry, so the slide doubles as a teachable
// color-vocabulary card. Future cuts can rotate through the rest of
// the dictionary; over time the recap series becomes a slow lesson
// in AC's color words.
//
// Content slides remain full-bleed gpt-image-2 photos (jeffrey at a
// real home desk, using the imagined feature on his laptop) with a
// top gradient overlay. Photos pre-baked by `bin/jeffrey-photos.mjs`
// into recap/out/jeffrey-photos/<segment>.png and loaded via the
// standard scout `glob:` query.

import { cssColors } from "../../system/public/aesthetic.computer/lib/num.mjs";

export const PALETTE = {
  bg: "#0c1430",
  off: "#ffffffcc",
  dim: "#7886b0",
  cream: "#fcf7c5",
  cyan: "#70f0e0",
};

// Helper: pull an [r,g,b] from cssColors and turn it into the strings
// the slide needs (hex for css, "rgb(r, g, b)" caption, name).
function colorAddress(name) {
  const rgb = cssColors[name];
  if (!rgb) throw new Error(`colorAddress: unknown css color '${name}'`);
  const [r, g, b] = rgb;
  const hex = "#" + [r, g, b].map((c) => c.toString(16).padStart(2, "0")).join("");
  return { name, rgb, hex, caption: `rgb(${r}, ${g}, ${b})` };
}

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

  narration: `hey everybody, here's the last twenty-four hours at aesthetic computer. the headline this round is menuband — three releases yesterday and one today, all chasing the same title font. zero-six landed the voice palette LCD with a bundled YWFT processing typeface, a qwerty key map, and a proper dark mode. zero-seven swapped the title to a sharp risograph drop shadow — max-contrast text, one-pixel family-color misregister, no soft halo — and gave the LED visualizer a light mode where the hot zone darkens at peak instead of brightening to white. zero-eight thought it caught a bug — both bundled YWFT files share a postscript name, so the bold cut was being silently overwritten — and switched to a font-descriptor family lookup. zero-nine, today, found that fix was wrong. NSFont's descriptor lookup never returns nil on a miss, it just substitutes the system font, so the title was rendering in system black with no error. the real fix loads the dot-ttf descriptor straight from the bundle URL and asserts the family name before drawing. on the menubar side, the polygon status icon got smarter — N sessions equals an N-sided shape, all edges share one hue, and that hue tracks how many sessions are awaiting input. the claude-stop hook now flips active to awaiting instead of clearing, and lid-ambient detects all three shapes of "claude is running." over in fedac native, the piano sample amp went from one-point-eight to three so it sits level with the sine and square voices through notepat. on the hardware bench, the HP seventy-five eighty-five B drafting plotter came up over RS-two-thirty-two — hardware-flow serial sender, a live-limits probe that reads the sheet geometry off the machine, and a native HP-GL composer that renders SVG paths as faceted polylines, with the PALS mark as the first real test. for casey's social-software meeting, a sixteen-by-nine notepat keymap slide — piano plus qwerty with hand-split tints, fading dashed connectors, and QR codes to notepat-dot-com and prompt-dot-ac slash menuband. underneath all of it, the recap pipeline grew up — a new jeffrey twenty-four-hour audience, plus a jeffrey-photos script that pre-bakes full-bleed gpt-image-two portraits with the platter shoot and selfie refs for identity grounding. that's the day. thanks for watching, and i'll be back tomorrow with the next twenty-four.`,

  // Whisper renders these dictionary-style; rewrite the displayed subtitles.
  // Order matters: longer/multi-word fixes first.
  transcriptFixes: {
    "a static computer": "aesthetic computer",
    "static computer": "aesthetic computer",
    "Menu Band": "menuband",
    "menu band": "menuband",
    "Menuband": "menuband",
    "Menu Bar": "menubar",
    "menu bar": "menubar",
    "YWFT Processing": "ywft processing",
    "YWFT": "ywft",
    "Y W F T": "ywft",
    "PostScript": "postscript",
    "Post Script": "postscript",
    "NS Font": "nsfont",
    "NSFont": "nsfont",
    "dot-TTF": "dot-ttf",
    "dot TTF": "dot-ttf",
    ".TTF": ".ttf",
    "Riso": "riso",
    "Risograph": "risograph",
    "LED": "led",
    "LCD": "lcd",
    "QWERTY": "qwerty",
    "Qwerty": "qwerty",
    "Mac OS": "macos",
    "macOS": "macos",
    "MIDI": "midi",
    "GPT-Image-2": "gpt-image-2",
    "GPT Image 2": "gpt-image-2",
    "Fed AC": "fedac",
    "FedAC": "fedac",
    "Note Pat": "notepat",
    "Notepat": "notepat",
    "HP-GL": "hp-gl",
    "HPGL": "hp-gl",
    "RS-232": "rs-232",
    "RS 232": "rs-232",
    "PALS": "pals",
    "Casey": "casey",
    "Claude": "claude",
    "Mac": "mac",
    "MacBook": "macbook",
    "Jeffrey": "jeffrey",
  },

  segments: [
    { name: "01_title", marker: "hey everybody" },
    { name: "02_menuband_06", marker: "zero-six landed" },
    { name: "03_menuband_07", marker: "zero-seven swapped" },
    { name: "04_menuband_08", marker: "zero-eight thought" },
    { name: "05_menuband_09", marker: "zero-nine, today" },
    { name: "06_polygon", marker: "the polygon status" },
    { name: "07_stop_ambient", marker: "the claude-stop hook" },
    { name: "08_piano", marker: "over in fedac native" },
    { name: "09_plotter", marker: "on the hardware bench" },
    { name: "10_keymap", marker: "for casey's social-software" },
    { name: "11_recap", marker: "underneath all of it" },
    { name: "12_outro", marker: "thanks for watching" },
    { name: "13_end", marker: "__END__", trailingSilenceSec: 3 },
  ],

  slides: {
    "01_title": {
      colorAddress: colorAddress("aliceblue"),
      metaphor: `${REAL}

Scene: he is sitting at a wooden home desk in a wrinkled t-shirt, holding up a \
single sheet of plain white printer paper next to his face. The paper has \
"24 HRS" hand-drawn on it in thick black sharpie, slightly crooked letters. He \
is giving a deadpan grin with one slightly raised eyebrow, mid-laugh, eye-contact \
with the camera. An open silver MacBook on the desk in the background shows a \
calendar app with a single highlighted day. Soft afternoon light from a window \
on the right, warm white balance. A pale-blue (#f0f8ff) ceramic mug sits on \
the desk — the only pop of accent color, very gentle. Wide shot, his whole \
upper body and the desk visible. Vertical, 1024x1536.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/01_title.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "00 / 11 · the last 24 hours",
        title: "aesthetic computer",
        cap: "2026·04·30",
        color: colorAddress("aliceblue"),
      }),
    },

    "02_menuband_06": {
      colorAddress: colorAddress("cornflowerblue"),
      metaphor: `${REAL}

Scene: he is hunched over a silver MacBook on a kitchen table, leaning very \
close to the screen with one finger pointing at a small popover window in the \
upper-right of the screen. The popover clearly shows: a horizontal LCD-style \
voice-name strip in the top with "GRAND PIANO" in pixel-style letters, a \
qwerty keyboard map below it tinted in two halves, and a thin LED visualizer \
at the bottom in a cool cornflower-blue (#6495ed) tone. The room is in dark \
mode — the popover is dark with bright highlights, matching the system. He \
is wearing a hoodie, slightly bedheaded, deadpan focus. Evening light through \
a window. A coffee mug on the table. Real candid energy.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/02_menuband_06.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "01 / 11 · menuband 0.6",
        title: "voice lcd,\nqwerty map,\ndark mode",
        cap: "ywft processing bundled · slab/menuband",
        color: colorAddress("cornflowerblue"),
      }),
    },

    "03_menuband_07": {
      colorAddress: colorAddress("mediumvioletred"),
      metaphor: `${REAL}

Scene: he is leaning over a silver MacBook on a wooden home desk, peering \
intently at a small popover at the top of the screen. The popover shows a \
voice title rendered in crisp high-contrast black text on a light substrate \
with a single hard 1-pixel medium-violet-red (#c71585) drop shadow offset \
down-right — clearly a Risograph misregister, NOT a soft halo. Below the \
title, an LED visualizer in light-mode ink-on-paper styling, with the hot \
zone darkening at peak. He is in a t-shirt, posture relaxed, holding a print \
proof sheet of risograph swatches in his free hand for visual rhyme. Bright \
midday window light. Real candid, deadpan focus.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/03_menuband_07.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "02 / 11 · menuband 0.7",
        title: "riso shadow,\nlight-mode led",
        cap: "1px misregister · ink-on-paper visualizer",
        color: colorAddress("mediumvioletred"),
      }),
    },

    "04_menuband_08": {
      colorAddress: colorAddress("goldenrod"),
      metaphor: `${REAL}

Scene: he is at a home desk holding two printed pages of code side by side, \
squinting at them with mock-serious detective focus. Both pages show the same \
font name "YWFT-Processing" in goldenrod-yellow (#daa520) highlighter on the \
text — a visual nod to "two files, same name." The silver MacBook on the desk \
shows a code editor with one specific line highlighted. He has a goldenrod \
yellow sticky-note stuck to his forehead with "BOLD?" written on it in black \
sharpie, deadpan absurd energy. Soft afternoon window light. Real, slightly \
goofy, very "him".`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/04_menuband_08.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "03 / 11 · menuband 0.8",
        title: "thought we\ncaught it",
        cap: "shared postscript name · descriptor lookup",
        color: colorAddress("goldenrod"),
      }),
    },

    "05_menuband_09": {
      colorAddress: colorAddress("chartreuse"),
      metaphor: `${REAL}

Scene: he is at a home desk in front of a silver MacBook, leaning forward with \
both hands raised in a small mock-celebratory pose, eyes wide, mouth slightly \
open. The laptop screen shows the menuband popover with the voice title now \
rendered in CRISP bold YWFT Processing — clearly a custom typeface, hand-cut \
geometric letterforms, sitting in a frame highlighted with chartreuse-green \
(#7fff00) marker. A chartreuse highlighter pen lies on the desk next to a \
printed page of Swift code with one line circled. Bright morning window light. \
Real candid, deadpan-but-pleased.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/05_menuband_09.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "04 / 11 · menuband 0.9",
        title: "actual fix:\nload by url,\nassert family",
        cap: "AppDelegate.ywftBoldDescriptor · familyName check",
        color: colorAddress("chartreuse"),
      }),
    },

    "06_polygon": {
      colorAddress: colorAddress("darkorchid"),
      metaphor: `${REAL}

Scene: he is at a home desk in front of a silver MacBook. In the upper-right \
corner of the laptop's menu bar, several geometric polygon icons are visible \
side by side — a triangle, a square, and a pentagon — each rendered in a single \
solid dark-orchid (#9932cc) tone with all edges the same hue. He is holding a \
small piece of paper showing his hand-sketch of the same shape progression: \
"1=line, 2=line, 3=triangle, 4=square…" annotated in pen. Deadpan focused, \
slight nod. Soft window light. Real candid, very "him".`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/06_polygon.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "05 / 11 · slab polygon icon",
        title: "n sessions,\nn-sided shape",
        cap: "shared hue · tracks awaiting ratio",
        color: colorAddress("darkorchid"),
      }),
    },

    "07_stop_ambient": {
      colorAddress: colorAddress("slategray"),
      metaphor: `${REAL}

Scene: he is sitting cross-legged on the floor in front of a closed silver \
MacBook, lid down. A faint slate-gray (#708090) glow comes from the closed \
laptop's edge — the menubar visible because the lid-ambient mode is awake. He \
is holding a small handwritten sign that reads "AWAITING" in bold marker, \
deadpan-tired expression like a parent waiting outside a kid's bedroom. The \
room is dim evening light, lamp in the background. Real candid, slightly \
absurd, very deadpan.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/07_stop_ambient.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "06 / 11 · claude-stop · lid-ambient",
        title: "active →\nawaiting",
        cap: "three shapes of 'claude is running'",
        color: colorAddress("slategray"),
      }),
    },

    "08_piano": {
      colorAddress: colorAddress("coral"),
      metaphor: `${REAL}

Scene: he is at a home desk leaning into the laptop's built-in microphone with \
a pair of cheap coral-orange (#ff7f50) over-ear headphones around his neck. \
The silver MacBook screen shows a waveform editor with two stacked tracks: a \
small flat sine wave on top, a much taller piano-sample wave below. He is \
turning an imaginary knob in the air with one hand, mock-audio-engineer \
gesture. The desk has a small toy upright piano sitting on it as a visual \
gag. Warm afternoon light. Real, deadpan focus, mildly absurd.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/08_piano.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "07 / 11 · fedac/native audio",
        title: "piano amp\n1.8 → 3.0",
        cap: "level with sine + square through notepat",
        color: colorAddress("coral"),
      }),
    },

    "09_plotter": {
      colorAddress: colorAddress("seagreen"),
      metaphor: `${REAL}

Scene: he is crouched next to a real, large 1980s-era HP 7585B drafting \
plotter — a wide white pen-plotter the size of a kitchen table, with multiple \
plotter pens visible in a carousel and a sheet of paper loaded. A thick \
serial cable in sea-green (#2e8b57) runs from the plotter to a silver \
MacBook on the floor next to it. He is holding the laptop on his lap, \
deadpan-grinning at the camera while the plotter behind him is mid-stroke \
drawing the PALS mark — a small purple geometric symbol — onto the paper. \
Garage / studio setting, real working space, slight clutter. Bright \
overhead light. Real candid, very "him", a little proud.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/09_plotter.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "08 / 11 · plotters/hp7585b",
        title: "hp 7585b,\nover rs-232",
        cap: "live-limits probe · hp-gl composer · pals test",
        color: colorAddress("seagreen"),
      }),
    },

    "10_keymap": {
      colorAddress: colorAddress("hotpink"),
      metaphor: `${REAL}

Scene: he is in a casual home setting holding a printed 16:9 sheet of paper \
in landscape orientation up to the camera. The sheet shows a piano keyboard \
on top with hot-pink (#ff69b4) and cyan tinted halves, a qwerty keyboard map \
below, dashed connector lines linking C·D·E·F·G keys to their qwerty twins, \
and two QR codes in opposite bottom corners. The hot-pink half of the piano \
on the sheet visibly stands out. He is presenting it deadpan, slight smile, \
eye contact with the camera, like a teacher holding up an answer key. \
Bright window light. Real candid energy.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/10_keymap.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "09 / 11 · slides/notepat-keymap",
        title: "16:9 keymap\nfor casey",
        cap: "social-software meeting · piano + qwerty",
        color: colorAddress("hotpink"),
      }),
    },

    "11_recap": {
      colorAddress: colorAddress("darkturquoise"),
      metaphor: `${REAL}

Scene: he is at a home desk holding a small stack of fresh polaroid-style \
prints, fanning them out toward the camera. Each printed photo in his hand \
is clearly a different staged candid of HIM — same face, different scenes — \
a recursion gag. The silver MacBook on the desk shows a terminal with a \
build pipeline log, with one line in dark-turquoise (#00ced1) text reading \
"jeffrey-photos: 11 segments cached." Behind him on a wall, a few of the \
same prints are tacked up with pins. Soft afternoon light. Real candid, \
slightly absurd, deadpan.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/11_recap.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "10 / 11 · recap pipeline",
        title: "jeffrey-24h\naudience,\npre-baked\nportraits",
        cap: "gpt-image-2 · platter shoot + selfie refs",
        color: colorAddress("darkturquoise"),
      }),
    },

    "12_outro": {
      colorAddress: colorAddress("peachpuff"),
      metaphor: `${REAL}

Scene: he is leaning back in a desk chair holding a coffee mug, giving a \
small exaggerated wave to the camera with his free hand. The silver laptop \
in front of him on the desk shows a screen reading "thanks for watching" in \
calm cream-on-dark text with a thin peach-puff (#ffdab9) underline. He is \
giving a small relaxed smile, real eye contact with the camera. Comfortable \
home setting — desk lamp on, evening softness, warm white balance. Wood \
desk, a houseplant or two visible. Real iPhone candid snapshot energy.`,
      queries: { photo: { glob: "recap/out/jeffrey-photos/12_outro.png" } },
      body: ({ photo }) => photoSlide({
        photo,
        chapter: "11 / 11 · outro",
        title: "thanks for\nwatching",
        cap: "aesthetic.computer · @jeffrey",
        color: colorAddress("peachpuff"),
      }),
    },

    "13_end": `
      <div class="frame">
        <div class="pals med"></div>
        <div class="endline" style="color:${PALETTE.cream}">aesthetic·computer</div>
        <div class="endsub" style="color:${PALETTE.dim}">narrated by jeffrey-pvc · @jeffrey</div>
        <div class="endsub" style="color:${PALETTE.dim}">2026·04·30</div>
      </div>`,
  },
};

// Renders a full-bleed photo slide with a TOP gradient overlay carrying the
// chapter / title / caption text + a small color-address chip in the
// bottom-right corner. The chip names a CSS color from the AC dictionary —
// over the recap series these chips become a slow lesson in the palette.
function photoSlide({ photo, chapter, title, cap, color }) {
  const titleHtml = title.split("\n").map((l) => `<div>${l}</div>`).join("");
  return `
    <div style="position: fixed; inset: 0; padding: 0; overflow: hidden;">
      ${photo
        ? `<img src="${photo}" style="position: absolute; inset: 0; width: 100%; height: 100%; object-fit: cover;" />`
        : `<div style="position: absolute; inset: 0; background: ${PALETTE.bg};"></div>`}
      <div style="position: absolute; top: 0; left: 0; right: 0; padding: 180px 70px 110px; background: linear-gradient(to bottom, rgba(0,0,0,0.88) 0%, rgba(0,0,0,0.5) 60%, rgba(0,0,0,0) 100%);">
        <div style="font-family: 'ProcessingR'; font-size: 30px; letter-spacing: 6px; text-transform: uppercase; color: ${color.hex};">${chapter}</div>
        <div style="font-family: 'ProcessingB'; font-size: 110px; line-height: 1.0; letter-spacing: -2px; color: ${color.hex}; margin-top: 18px;">${titleHtml}</div>
        <div style="font-family: 'ProcessingR'; font-size: 38px; color: ${PALETTE.off}; margin-top: 26px; letter-spacing: 1px;">${cap}</div>
      </div>
      <div style="position: absolute; bottom: 110px; right: 70px; display: flex; align-items: center; gap: 22px; padding: 18px 26px 18px 22px; background: rgba(0,0,0,0.72); border: 1px solid rgba(255,255,255,0.18); border-radius: 6px;">
        <div style="width: 56px; height: 56px; background: ${color.hex}; border: 1px solid rgba(255,255,255,0.35); border-radius: 4px;"></div>
        <div style="display: flex; flex-direction: column; gap: 4px;">
          <div style="font-family: 'ProcessingB'; font-size: 30px; letter-spacing: 1px; color: ${PALETTE.cream};">${color.name}</div>
          <div style="font-family: 'ProcessingR'; font-size: 22px; letter-spacing: 1px; color: ${PALETTE.dim};">${color.caption}</div>
        </div>
      </div>
    </div>`;
}

export default audience;
