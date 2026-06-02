// Audience config: serpentine — Serpentine FAE Fellowship video introduction.
// A proposal pitch (~2:50), NOT a daily recap. Voice: jeffrey-pvc (default in
// /api/say). Register: lowercase, warm, a little funny, sincere — "clarity and
// sincerity, not production value" (their words).
//
// `narration` is the verbatim text POSTed to /api/say. Markers in `segments`
// are matched (case-insensitive, punctuation-stripped) against whisper's
// word-level transcript, so each marker phrase appears verbatim in narration.
//
// Slides are text-only AC-palette cards so this renders WITHOUT gpt-image.
// To add per-segment illustrations later, swap a slide string for the
// {queries, body} form (see general.mjs) or wire recap/bin/jeffrey-photos.mjs.
// Storyboard + illustration prompts: grants/serpentine-fae-2026/VIDEO-MOCK.md

export const PALETTE = {
  bg: "#06120c",        // near-black terminal green-black
  accent: "#3dff88",    // terminal green
  cyan: "#70f0e0",
  amber: "#ffbf3d",
  magenta: "#ff70d0",
  cream: "#e8ffe8",
  off: "#eafff0cc",
  dim: "#5f8f78",
};

export const audience = {
  name: "serpentine",
  handle: "@aesthetic.computer",
  voice: { provider: "jeffrey", voice: "neutral:0" },

  // This is a spoken proposal, NOT a sung recap — keep the plain ~2:50
  // narration. Disable the autotune/pitchsnap "sing" pass (which otherwise
  // stretches the timeline ~3× and rewrites words.json/segments.json).
  sing: false,

  narration: `hey, i'm jeffrey scudder. i'm a painter, and for six years i've been building my own computer to make things with — aesthetic computer. here's what i can't stop thinking about. as everyone races to build ai, computing is getting pulled into the data center. your laptop is becoming a thin client, renting its intelligence by the month. the personal computer you actually own is quietly disappearing. i think home computing needs a revolution at the operating system layer. so i built one. aesthetic computer boots a fifty dollar surplus laptop into a creative instrument in seven seconds — no desktop, no account, no cloud. it has its own little language, kidlisp, and it is a commons: everything is open, and it all boots from a single usb stick. twenty years ago the laptop orchestra proved a room of laptops could be a real instrument, but it stayed locked inside rich universities. on surplus hardware, that can be planetary, and held in common. language models are the force doing the enclosing, but they are also what lets one artist build a whole operating system. so here is my question: can a creative os take ai in as a local, owned faculty, on your own terms, instead of a tenant of the cloud? that is my art and convergence question. thanks for watching.`,

  // Whisper renders these dictionary-style; rewrite the displayed subtitles.
  // Longer/multi-word fixes first so they win before single-word fixes.
  transcriptFixes: {
    "Kid Lisp": "kidlisp",
    "kid lisp": "kidlisp",
    "KidLisp": "kidlisp",
    "think pad": "thinkpad",
    "Think Pad": "thinkpad",
    "AC Native OS": "aesthetic computer native os",
    "ac native os": "aesthetic computer native os",
    "USB stick": "usb stick",
    // whisper (ggml-base.en) hears the surname "scudder" as "scutter" —
    // fix it in the displayed subtitles so the proposal video spells the
    // applicant's name correctly.
    "Scutter": "Scudder",
    "scutter": "scudder",
  },

  // NOTE: whisper (ggml-base.en) tokenizes contractions as single
  // space-joined word entries ("here s", "that s", "it s"), and align.mjs
  // matches markers word-by-word against those entries — so any marker
  // containing an apostrophe/contraction fails to match. Markers below are
  // deliberately contraction-free contiguous phrases verified against the
  // transcript. (It also rendered "scudder" as "scutter", so the title
  // marker avoids the surname.) Re-verify if the narration text changes.
  segments: [
    { name: "01_title", marker: "hey" },
    { name: "02_problem", marker: "the data center" },
    { name: "03_os_layer", marker: "operating system layer" },
    { name: "04_creative_os", marker: "surplus laptop" },
    { name: "05_commons", marker: "it is a commons" },
    { name: "06_llms", marker: "language models are the force" },
    { name: "07_poised", marker: "can a creative" },
    { name: "09_outro", marker: "my art and convergence" },
    { name: "10_end", marker: "__END__", trailingSilenceSec: 3 },
  ],

  slides: {
    "01_title": `
      <div class="frame">
        <div class="title-stack">
          <div class="kicker" style="color:${PALETTE.accent}">aesthetic computer</div>
          <div class="huge" style="color:${PALETTE.cream}">a computer<br>of your own</div>
          <div class="sub" style="color:${PALETTE.amber}">a proposal · art × convergence</div>
        </div>
        <div class="datestamp" style="color:${PALETTE.dim}">jeffrey alan scudder</div>
      </div>`,

    "02_problem": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">the condition</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.cream}">computing is<br>being pulled<br>into the<br>data center</div>
          <div class="sub" style="color:${PALETTE.amber}">your laptop → a thin client</div>
        </div>
      </div>`,

    "03_os_layer": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">the need</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.accent}">a revolution<br>at the OS layer</div>
          <div class="sub" style="color:${PALETTE.off}">not another app — a different ground</div>
        </div>
      </div>`,

    "04_creative_os": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">01 · the creative os</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.cream}">a $50 laptop<br>→ an instrument<br>in 7 seconds</div>
          <div class="sub" style="color:${PALETTE.amber}">no desktop · no account · no cloud</div>
        </div>
      </div>`,

    "05_commons": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">02 · the commons</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.cream}">a planetary<br>laptop orchestra</div>
          <div class="sub" style="color:${PALETTE.amber}">kidlisp · a url per piece · held in common</div>
        </div>
      </div>`,

    "06_llms": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">03 · what llms change</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.accent}">a faculty,<br>not a landlord</div>
          <div class="sub" style="color:${PALETTE.off}">local · owned · on your own terms</div>
        </div>
      </div>`,

    "07_poised": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">why aesthetic computer</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.cream}">it holds the<br>whole stack</div>
          <div class="sub" style="color:${PALETTE.amber}">kernel · language · community · archive</div>
        </div>
      </div>`,

    "08_plan": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">the six months</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.cream}">harden · prototype<br>· orchestrate</div>
          <div class="sub" style="color:${PALETTE.off}">the os · local ai · the orchestra — in the open</div>
        </div>
      </div>`,

    "09_outro": `
      <div class="frame">
        <div class="title-stack">
          <div class="kicker" style="color:${PALETTE.accent}">art × convergence</div>
          <div class="huge" style="color:${PALETTE.cream}">keep a computer<br>personal —<br>and held<br>in common</div>
        </div>
        <div class="datestamp" style="color:${PALETTE.dim}">aesthetic.computer</div>
      </div>`,

    "10_end": `
      <div class="frame">
        <div class="title-stack">
          <div class="endline" style="color:${PALETTE.cream}">aesthetic<br>computer</div>
          <div class="endsub" style="color:${PALETTE.accent}">a computer of your own</div>
        </div>
        <div class="datestamp" style="color:${PALETTE.dim}">aesthetic.computer</div>
      </div>`,
  },
};
