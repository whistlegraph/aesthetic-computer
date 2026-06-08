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
  voice: { provider: "jeffrey", voice: "neutral:0", speed: 1.08 },

  // This is a spoken proposal, NOT a sung recap — keep the plain ~2:50
  // narration. Disable the autotune/pitchsnap "sing" pass (which otherwise
  // stretches the timeline ~3× and rewrites words.json/segments.json).
  sing: false,

  narration: `Hey, I'm Jeffrey Alan Scudder. I'm a painter, and for six years I've been writing software and patterning for my own computer — a place to make things and give them away. It's called Aesthetic Computer. Here's the thing I can't stop thinking about. As the demand for AI compute pulls everything toward the data center, your laptop is turning into a thin client — a window onto someone else's machine, rented by the month. The personal computer, the one you actually own, is quietly disappearing. I think home computing needs a revolution at the operating system layer. Not another app — a different ground to stand on. I've spent six years building one. So that's what I built. Aesthetic Computer native OS boots a surplus laptop — a fifty-dollar ThinkPad the upgrade cycle threw away — straight into a creative instrument in about seven seconds, greeting you by name. And here the picture, the code, and the music are one medium. You play notepat — tunes you can type, so a song is something you write down and pass along, the way folk music always has — and you slide between drawing, programming, and playing without ever leaving the instrument. It is a commons. You make pieces in JavaScript or KidLisp, or add runtime support for new kinds, and we build it all in public, our own language models working right alongside us. It holds paintings, tapes, and chats too, each at its own web address. Twenty years ago the Princeton Laptop Orchestra proved a room of laptops could be a real instrument, but at fifteen hundred dollars a seat it stayed locked inside rich universities. Last year, when Windows ten ended, two hundred and forty million working computers became obsolete overnight. That is the raw material for a planetary orchestra — held in common, not thrown away. Three people with surplus laptops in a park can be a laptop orchestra. Language models change all of this. They are what pulls computing into the data center — but they are also what lets one artist build and tend a whole operating system that once took a company. So here is the question I want to live inside for six months: can a creative OS take language models in as a local, owned faculty, on your own terms, instead of a tenant of the cloud? I think Aesthetic Computer can answer that, because it already holds the whole stack — the kernel, the languages, the community, the archive. With the fellowship, I'd harden the OS, prototype that local faculty, and grow the orchestra with my community here in Los Angeles — all of it in the open. That's my art and convergence question: what does it take to keep a computer personal — and held in common? Thanks for listening.`,

  // Whisper renders these dictionary-style; rewrite the displayed subtitles.
  // Longer/multi-word fixes first so they win before single-word fixes.
  transcriptFixes: {
    "Kid Lisp": "kidlisp",
    "kid lisp": "kidlisp",
    "KidLisp": "kidlisp",
    "Java Script": "javascript",
    "java script": "javascript",
    "JavaScript": "javascript",
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
    { name: "05b_quote", marker: "three people" },
    { name: "06_llms", marker: "language models change" },
    { name: "07_poised", marker: "the whole stack" },
    { name: "08_plan", marker: "with the fellowship" },
    { name: "09_outro", marker: "art and convergence" },
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
          <div class="huge" style="color:${PALETTE.cream}">notepat —<br>tunes you<br>can type</div>
          <div class="sub" style="color:${PALETTE.amber}">picture · code · music — one medium</div>
        </div>
      </div>`,

    "05_commons": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">02 · the commons</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.cream}">many kinds<br>of work, in<br>common</div>
          <div class="sub" style="color:${PALETTE.amber}">js + kidlisp · paintings · tapes · chats · a url each</div>
        </div>
      </div>`,

    // 05b_quote is rendered in the widescreen cut as a paper-extract scene
    // (the PLOrk PDF with a live highlight); this slide is only the portrait
    // fallback.
    "05b_quote": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">the raw material</div>
        <div class="title-stack">
          <div class="huge" style="color:${PALETTE.cream}">three people<br>in a park</div>
          <div class="sub" style="color:${PALETTE.amber}">240M pcs freed when windows 10 ended · plork in a park</div>
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
          <div class="sub" style="color:${PALETTE.amber}">kernel · languages · community · archive</div>
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
