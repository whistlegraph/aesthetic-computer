// Audience config: general (public-facing 48-hour recap).
// Voice: jeffrey-pvc (default in /api/say). Style: lowercase, calm,
// descriptive. No nicknames, no rhyme — accessible to anyone who has
// never heard of aesthetic computer.
//
// `narration` is the verbatim text POSTed to /api/say. Markers in
// `segments` are matched (case-insensitive, punctuation-stripped)
// against whisper's word-level transcript, so each marker phrase must
// appear verbatim in the narration.

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

export const audience = {
  name: "general",
  handle: "@everyone",
  voice: { provider: "jeffrey", voice: "neutral:0" },

  narration: `hey everybody, here's a quick window into the last two days at aesthetic computer. the headline this round is yergersnap — a tiny mac menubar app that snaps an iphone screen into a paintable canvas, with percussion sounds on every click and an icon painted by julia yerger. on the keyboard side, notepat picked up two new bandmates — piano man and sample sally — characters with eye-tracking pupils that follow whichever notes you play. underneath the hood, the whole audio engine moved off karplus-strong synthesis onto a salamander grand piano sample bank, so chords actually breathe now. on the native side, ac-os gained bootpics — webcam snapshots taken at boot and shutdown, a tiny diary of every session. the say piece now logs every utterance to a database, so we can replay what the computer has said over time. a new papers pipeline grew around penrose, a constraint-based diagram language for academic figures. through it all, the oven kept compiling pdfs in the background. that's the week — thanks for watching.`,

  // Whisper renders these dictionary-style; rewrite the displayed subtitles.
  // Order matters — longer/multi-word fixes first so they win before
  // single-word fixes can rewrite a substring.
  transcriptFixes: {
    "carpless strong synthesis": "karplus-strong synthesis",
    "carpless strong": "karplus-strong",
    "saypiece": "say piece",
    "paper's pipeline": "papers pipeline",
    "Yerger Snap": "yergersnap",
    "Yergersnap": "yergersnap",
    "bootpicks": "bootpics",
    "boot pics": "bootpics",
    "BootPics": "bootpics",
    "Notepad": "notepat",
    "ACOs": "ac-os",
    "ACOS": "ac-os",
    "AC OS": "ac-os",
    "menu bar": "menubar",
    "Sample Sally": "sample sally",
    "Piano Man": "piano man",
  },

  segments: [
    { name: "01_title", marker: "hey everybody" },
    { name: "02_yergersnap", marker: "the headline this round" },
    { name: "03_notepat", marker: "on the keyboard side" },
    { name: "04_audio", marker: "underneath the hood" },
    { name: "05_native", marker: "on the native side" },
    // Whisper merges "say piece" into one word "saypiece" — anchor on
    // the next clean phrase. The displayed subtitle is fixed back to
    // "say piece" via transcriptFixes.
    { name: "06_say", marker: "logs every utterance" },
    // Whisper hears "paper's pipeline"; the apostrophe splits the token,
    // so anchor on the next clean phrase instead.
    { name: "07_papers", marker: "grew around penrose" },
    { name: "08_oven", marker: "through it all" },
    { name: "09_outro", marker: "thanks for watching" },
    { name: "10_end", marker: "__END__", trailingSilenceSec: 3 },
  ],

  slides: {
    "01_title": `
      <div class="frame">
        <div class="pals big"></div>
        <div class="title-stack">
          <div class="kicker" style="color:${PALETTE.cyan}">aesthetic computer</div>
          <div class="huge" style="color:${PALETTE.cream}">recap</div>
          <div class="sub" style="color:${PALETTE.accent}">the last 48 hours</div>
        </div>
        <div class="datestamp" style="color:${PALETTE.dim}">2026·04·26 → 2026·04·28</div>
      </div>`,

    "02_yergersnap": {
      queries: {
        icon: { glob: "slab/yergersnap/icon.iconset/icon_512x512.png" },
        commits: { commits: "yergersnap|MirrorTap|iphone-mirror-tap", since: "48 hours ago", limit: 5 },
      },
      body: ({ icon, commits }) => `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">01 / 08 · yergersnap</div>
          <div class="title-row">
            ${icon ? `<img class="brand-icon" src="${icon}" />` : ""}
            <div class="title" style="color:${PALETTE.accent}">yergersnap</div>
          </div>
          <div class="body" style="color:${PALETTE.cream}">
            a mac menubar app that snaps your<br/>
            <em>iphone screen</em> into a paintable canvas.
          </div>
          <div class="commits">
            ${(commits || []).map((c) => `<div class="commit"><span class="hash">${c.hash.slice(0, 4)}</span>${(c.subject.split(":").slice(1).join(":").trim() || c.subject).slice(0, 60)}</div>`).join("")}
          </div>
          <div class="cap" style="color:${PALETTE.magenta}">"icon painted by julia yerger."</div>
        </div>`,
    },

    "03_notepat": {
      queries: {
        icon: { glob: "ac-electron/build/icon.png" },
        paper: { glob: "system/public/papers.aesthetic.computer/notepat-26-arxiv-cards.pdf", pdfPage: 1, pdfWidth: 600 },
        commits: { commits: "^notepat|Piano Man|Sample Sally|piano-man|sample-sally", since: "48 hours ago", limit: 5 },
      },
      body: ({ icon, paper, commits }) => `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">02 / 08 · notepat</div>
          <div class="title-row">
            ${icon ? `<img class="brand-icon" src="${icon}" />` : ""}
            <div class="title" style="color:${PALETTE.cyan}">two new bandmates</div>
          </div>
          <div class="body" style="color:${PALETTE.cream}">
            piano man and sample sally —<br/>
            eye-tracking pupils follow your notes.
          </div>
          <div class="row-with-aside">
            <div class="commits">
              ${(commits || []).map((c) => `<div class="commit"><span class="hash">${c.hash.slice(0, 4)}</span>${(c.subject.split(":").slice(1).join(":").trim() || c.subject).slice(0, 56)}</div>`).join("")}
            </div>
            ${paper ? `<img class="paper-thumb" src="${paper}" />` : ""}
          </div>
        </div>`,
    },

    "04_audio": {
      queries: {
        commits: { commits: "Salamander|Karplus|piano sample|piano:|^audio:|piano rewrite", since: "48 hours ago", limit: 5 },
      },
      body: ({ commits }) => `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">03 / 08 · audio</div>
          <div class="title" style="color:${PALETTE.lime}">chords that breathe</div>
          <div class="body" style="color:${PALETTE.cream}">
            karplus-strong synthesis →<br/>
            <em>salamander grand piano</em> sample bank.
          </div>
          <div class="commits">
            ${(commits || []).map((c) => `<div class="commit"><span class="hash">${c.hash.slice(0, 4)}</span>${(c.subject.split(":").slice(1).join(":").trim() || c.subject).slice(0, 60)}</div>`).join("")}
          </div>
        </div>`,
    },

    "05_native": {
      queries: {
        commits: { commits: "bootpics|^ac-native|^fedac/native|^ac-os|ac-device|ac-inscribe", since: "48 hours ago", limit: 6 },
      },
      body: ({ commits }) => `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">04 / 08 · ac-os</div>
          <div class="title" style="color:${PALETTE.yellow}">bootpics</div>
          <div class="body" style="color:${PALETTE.cream}">
            webcam snapshots at boot + shutdown —<br/>
            a tiny diary of every session.
          </div>
          <div class="commits">
            ${(commits || []).map((c) => `<div class="commit"><span class="hash">${c.hash.slice(0, 4)}</span>${(c.subject.split(":").slice(1).join(":").trim() || c.subject).slice(0, 60)}</div>`).join("")}
          </div>
        </div>`,
    },

    "06_say": {
      queries: {
        commits: { commits: "say:|sayings|backfill-sayings|^system/say", since: "48 hours ago", limit: 5 },
      },
      body: ({ commits }) => `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">05 / 08 · say</div>
          <div class="title" style="color:${PALETTE.magenta}">every utterance, logged</div>
          <div class="body" style="color:${PALETTE.cream}">
            the say piece now writes each line<br/>
            to a mongo collection — replayable.
          </div>
          <div class="commits">
            ${(commits || []).map((c) => `<div class="commit"><span class="hash">${c.hash.slice(0, 4)}</span>${(c.subject.split(":").slice(1).join(":").trim() || c.subject).slice(0, 60)}</div>`).join("")}
          </div>
        </div>`,
    },

    "07_papers": {
      queries: {
        commits: { commits: "penrose|arxiv-penrose", since: "48 hours ago", limit: 4 },
      },
      body: ({ commits }) => `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">06 / 08 · papers</div>
          <div class="title" style="color:${PALETTE.cyan}">penrose pipeline</div>
          <div class="body" style="color:${PALETTE.cream}">
            constraint-based diagrams,<br/>
            compiled into academic figures.
          </div>
          <div class="paper-card">
            <div class="paper-title">arxiv-penrose</div>
            <div class="paper-sub">declarative figure pipeline</div>
            <div class="paper-meta">tex · references · ac-paper-layout</div>
          </div>
          <div class="commits">
            ${(commits || []).map((c) => `<div class="commit"><span class="hash">${c.hash.slice(0, 4)}</span>${(c.subject.split(":").slice(1).join(":").trim() || c.subject).slice(0, 60)}</div>`).join("")}
          </div>
        </div>`,
    },

    "08_oven": {
      queries: {
        ovenCommits: { commits: "oven", since: "48 hours ago", limit: 6 },
        recentPdfs: { files: "system/public/papers.aesthetic.computer/*.pdf", sinceHours: 48, limit: 60 },
      },
      body: ({ ovenCommits, recentPdfs }) => {
        const stems = new Set();
        for (const p of recentPdfs || []) {
          stems.add(p.split("/").pop().replace(/-26-arxiv(-cards|-da|-es|-zh|-ja)?\.pdf$/, ""));
        }
        const distinct = [...stems];
        return `
        <div class="frame">
          <div class="chapter" style="color:${PALETTE.dim}">07 / 08 · oven</div>
          <div class="title" style="color:${PALETTE.yellow}">papers, baking</div>
          <div class="body" style="color:${PALETTE.cream}">
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
          <div class="line1" style="color:${PALETTE.cream}">that's the week.</div>
          <div class="line2" style="color:${PALETTE.cyan}">thanks for watching.</div>
        </div>
        <div class="body small" style="color:${PALETTE.dim}; margin-top: 80px; text-align: center;">
          aesthetic.computer
        </div>
      </div>`,

    "10_end": `
      <div class="frame">
        <div class="pals med"></div>
        <div class="endline" style="color:${PALETTE.cream}">aesthetic·computer</div>
        <div class="endsub" style="color:${PALETTE.dim}">narrated by jeffrey-pvc · @jeffrey</div>
        <div class="endsub" style="color:${PALETTE.dim}">2026·04·28</div>
      </div>`,
  },
};

export default audience;
