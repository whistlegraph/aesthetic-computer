// Audience config: fia (jas's girlfriend, non-technical).
// Voice: jeffrey-pvc (default in /api/say). Style: lowercase, warm, ends in a rhyme.
//
// `narration` is the verbatim text POSTed to /api/say.
// `segments` anchor each slide to a phrase in the narration; whisper word-level
// timestamps determine real durations. The marker is matched case-insensitively
// against the transcript with punctuation stripped, so it must appear in the
// audio (not be a paraphrase of it).

export const PALETTE = {
  bg: "#201040",
  accent: "#ff69b4",
  cyan: "#70f0e0",
  lime: "#a0f070",
  magenta: "#ff70d0",
  yellow: "#ffd860",
  cream: "#fcf7c5",
  off: "#ffffffcc",
  dim: "#9080c0",
};

export const audience = {
  name: "fia",
  handle: "@fifi",
  voice: { provider: "jeffrey", voice: "neutral:0" },

  // Narration as one paragraph for clean TTS. Lowercase house-voice for jas.
  // The voice says "fifi" (her handle, sans @ — easier to pronounce) instead
  // of the nickname "fi", since she goes by @fifi on AC.
  narration: `hey fifi, here's the last two days at the keyboard for you. the big one — notepat finally has a little remote that lives inside ableton, so when i'm playing, anyone can pop a tiny keyboard panel right into their session. the keys glow in piano colors, octaves stack, and the whole thing feels like a real instrument now. the multiplayer arena got dressed up too — a minimap in the top right, plain english labels instead of jargon, and every spectator gets their own quiet color. the camera piece, the one we call cap, learned to hold-to-record like baktok, so a tap doesn't startle it anymore. on the side, i wrote a little paper for parag about why audio feels late on linux — mostly love letters to interrupts — retranslated sucking on the complex into spanish, danish, chinese, and japanese, and rebuilt the menubar slab in proper swift so the mail submenu actually works. and through it all the oven kept its rhythm, baking pdfs in the background. fifi, you make the keystrokes kinder — every quiet line of code is a little you-reminder.`,

  // Each slide is anchored to a phrase that appears in the narration.
  // align.mjs finds the phrase in the whisper transcript and uses the start
  // timestamp of its first word as the slide start. The slide ends when the
  // next slide starts (or at audio end + trailingSilenceSec for the last).
  // Whisper substitutions for the displayed subtitle text. Match is
  // case-insensitive; replacement is verbatim. Use these to fix word forms
  // that whisper renders dictionary-style ("Notepad" for the AC piece
  // "notepat", "menu bar" → "menubar", etc.) or to correct mishears.
  transcriptFixes: {
    "Notepad": "notepat",
    "mini-map": "minimap",
    "Backtalk": "baktok",
    "menu bar": "menubar",
    "sub-menu": "submenu",
    "laid on Linux": "late on Linux",
    "you reminder": "you-reminder",
    "CAP": "cap",
  },

  segments: [
    { name: "01_title", marker: "hey fifi" },
    { name: "02_notepat", marker: "the big one" },
    { name: "03_arena", marker: "the multiplayer arena" },
    { name: "04_cap", marker: "the camera piece" },
    { name: "05_paper", marker: "i wrote a little paper" },
    { name: "06_translations", marker: "retranslated" },
    { name: "07_slab", marker: "rebuilt the" },
    { name: "08_oven", marker: "and through it all" },
    { name: "09_outro", marker: "fifi you make" },
    // End card runs after the audio ends; pipeline pads the audio with silence.
    { name: "10_end", marker: "__END__", trailingSilenceSec: 3 },
  ],

  // Each slide's HTML body. Rendered into a 1080x1920 frame by slides.mjs.
  // Use ${PALETTE.x} colors. CSS in slides.mjs handles font fallback.
  slides: {
    "01_title": `
      <div class="frame">
        <div class="pals big"></div>
        <div class="title-stack">
          <div class="kicker" style="color:${PALETTE.cyan}">aesthetic computer</div>
          <div class="huge" style="color:${PALETTE.cream}">@fifi</div>
          <div class="sub" style="color:${PALETTE.magenta}">last 48 hours @ the keyboard</div>
        </div>
        <div class="datestamp" style="color:${PALETTE.dim}">2026·04·24 → 2026·04·25</div>
      </div>`,
    "02_notepat": {
      queries: {
        icon: { glob: "ac-electron/build/icon.png" },
        paper: { glob: "system/public/papers.aesthetic.computer/notepat-26-arxiv-cards.pdf", pdfPage: 1, pdfWidth: 600 },
        // Match commits whose subject *starts with* notepat-related prefix so
        // we don't pick up unrelated commits that mention notepat in passing.
        commits: { commits: "^notepat|^build-notepat", since: "48 hours ago", limit: 5 },
      },
      body: ({ icon, paper, commits }) => `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">01 / 08 · notepat</div>
          <div class="title-row">
            ${icon ? `<img class="brand-icon" src="${icon}" />` : ""}
            <div class="title" style="color:${PALETTE.cyan}">notepat × ableton</div>
          </div>
          <div class="body" style="color:${PALETTE.cream}">
            a little remote that lives <em>inside</em> your DAW.
            piano colors, stacked octaves, square pads.
          </div>
          <div class="row-with-aside">
            <div class="commits">
              ${(commits || []).map((c) => `<div class="commit"><span class="hash">${c.hash.slice(0, 4)}</span>${(c.subject.split(":").slice(1).join(":").trim() || c.subject).slice(0, 56)}</div>`).join("")}
            </div>
            ${paper ? `<img class="paper-thumb" src="${paper}" />` : ""}
          </div>
          <div class="cap" style="color:${PALETTE.magenta}">"feels like a real instrument now."</div>
        </div>`,
    },
    "03_arena": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">02 / 08 · arena</div>
        <div class="title" style="color:${PALETTE.lime}">arena, dressed up</div>
        <div class="body" style="color:${PALETTE.cream}">
          minimap, top-right.<br/>
          plain english instead of jargon.<br/>
          every spectator gets their own quiet color.
        </div>
        <div class="commits">
          <div class="commit"><span class="hash">dfa8</span>minimap top-right + plain-English HUD</div>
          <div class="commit"><span class="hash">2fed</span>cool per-handle specColor + 'specs' label</div>
          <div class="commit"><span class="hash">06da</span>fix UDP snap drops + telemetry</div>
        </div>
      </div>`,
    "04_cap": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">03 / 08 · cap</div>
        <div class="title" style="color:${PALETTE.yellow}">hold to record</div>
        <div class="body" style="color:${PALETTE.cream}">
          the camera piece learned baktok manners.<br/>
          a tap doesn't startle it anymore.
        </div>
        <div class="commits">
          <div class="commit"><span class="hash">6839</span>cap + video: hold-to-record (BakTok-style)</div>
          <div class="commit"><span class="hash">a6b8</span>slide-up-to-zoom while holding record</div>
          <div class="commit"><span class="hash">6f54</span>portrait-native camera + drawn mic glyph</div>
        </div>
      </div>`,
    "05_paper": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">04 / 08 · papers</div>
        <div class="title" style="color:${PALETTE.cyan}">a paper for parag</div>
        <div class="body" style="color:${PALETTE.cream}">
          why audio feels late on linux —<br/>
          mostly love letters to interrupts.
        </div>
        <div class="paper-card">
          <div class="paper-title">arxiv-latency</div>
          <div class="paper-sub">IRQ + audio/input latency analysis</div>
          <div class="paper-meta">7-phase commit archaeology</div>
        </div>
      </div>`,
    "06_translations": {
      queries: {
        es: { glob: "system/public/papers.aesthetic.computer/sucking-on-the-complex-26-arxiv-es.pdf", pdfPage: 1, pdfWidth: 400 },
        da: { glob: "system/public/papers.aesthetic.computer/sucking-on-the-complex-26-arxiv-da.pdf", pdfPage: 1, pdfWidth: 400 },
        zh: { glob: "system/public/papers.aesthetic.computer/sucking-on-the-complex-26-arxiv-zh.pdf", pdfPage: 1, pdfWidth: 400 },
        ja: { glob: "system/public/papers.aesthetic.computer/sucking-on-the-complex-26-arxiv-ja.pdf", pdfPage: 1, pdfWidth: 400 },
      },
      body: ({ es, da, zh, ja }) => `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">05 / 08 · translations</div>
          <div class="title" style="color:${PALETTE.magenta}">sucking on the complex</div>
          <div class="cover-grid">
            ${[["es", "español", PALETTE.cyan, es], ["da", "dansk", PALETTE.lime, da], ["zh", "中文", PALETTE.yellow, zh], ["ja", "日本語", PALETTE.accent, ja]].map(([code, label, color, src]) => `
              <div class="cover">
                ${src ? `<img class="cover-img" src="${src}" />` : `<div class="cover-img placeholder"></div>`}
                <div class="cover-label"><span style="color:${color}">${code}</span> ${label}</div>
              </div>`).join("")}
          </div>
          <div class="body small" style="color:${PALETTE.cream}">
            reframed as engine, not adversary.
          </div>
        </div>`,
    },
    "07_slab": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">06 / 08 · slab</div>
        <div class="title" style="color:${PALETTE.lime}">menubar in swift</div>
        <div class="body" style="color:${PALETTE.cream}">
          native appkit rewrite.<br/>
          sf-symbol icons, passphrase modal,<br/>
          mail submenu wired to mbsync + mu.
        </div>
        <div class="commits">
          <div class="commit"><span class="hash">826d</span>native appkit rewrite with sf-symbol icons</div>
          <div class="commit"><span class="hash">585d</span>Notification hook fades ambient + TTS</div>
        </div>
      </div>`,
    "08_oven": {
      queries: {
        ovenCommits: { commits: "oven", since: "48 hours ago", limit: 6 },
        recentPdfs: { files: "system/public/papers.aesthetic.computer/*.pdf", sinceHours: 48, limit: 60 },
      },
      body: ({ ovenCommits, recentPdfs }) => {
        // Dedupe by paper stem (drop -26-arxiv-{cards,da,es,zh,ja}.pdf) so
        // the chip strip lists distinct papers, not translation duplicates.
        const stems = new Set();
        for (const p of recentPdfs || []) {
          stems.add(p.split("/").pop().replace(/-26-arxiv(-cards|-da|-es|-zh|-ja)?\.pdf$/, ""));
        }
        const distinct = [...stems];
        return `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">07 / 08 · oven</div>
          <div class="title" style="color:${PALETTE.yellow}">the oven, baking</div>
          <div class="body" style="color:${PALETTE.cream}">
            pdfs compiled while we sleep —<br/>
            ${distinct.length} distinct papers refreshed,<br/>
            ${(recentPdfs || []).length} files in 48 hrs.
          </div>
          <div class="ticker">
            ${(ovenCommits || []).slice(0, 4).map((c) => `<div class="tick"><span class="hash">${c.hash.slice(0, 4)}</span> ${c.subject.replace(/^\[?papers\]?[\s:]*/i, "").slice(0, 60)}</div>`).join("")}
          </div>
          <div class="pdf-strip">
            ${distinct.slice(0, 12).map((s) => `<div class="pdf-chip">${s.replace(/-/g, " ")}</div>`).join("")}
          </div>
        </div>`;
      },
    },
    "09_outro": `
      <div class="frame">
        <div class="chapter" style="color:${PALETTE.dim}">08 / 08 · outro</div>
        <div class="rhyme">
          <div class="line1" style="color:${PALETTE.cream}">@fifi, you make the</div>
          <div class="line1" style="color:${PALETTE.magenta}">keystrokes kinder —</div>
          <div class="line2" style="color:${PALETTE.cream}">every quiet line of code</div>
          <div class="line2" style="color:${PALETTE.cyan}">is a little you-reminder.</div>
        </div>
      </div>`,
    "10_end": `
      <div class="frame">
        <div class="pals med"></div>
        <div class="endline" style="color:${PALETTE.cream}">aesthetic·computer</div>
        <div class="endsub" style="color:${PALETTE.dim}">narrated by jeffrey-pvc · @jeffrey</div>
        <div class="endsub" style="color:${PALETTE.dim}">made with love · 2026·04·25</div>
      </div>`,
  },
};

export default audience;
