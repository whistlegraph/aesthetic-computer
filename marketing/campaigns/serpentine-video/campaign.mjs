// campaign.mjs — config for the Serpentine widescreen application video.
// Consumed by marketing/bin/{compose-widescreen,storyboard-widescreen}.mjs.
// The campaign DIR (this folder) also holds the per-segment illustration
// folders (01_title/gens/native.png …). Segment names + timings come from
// recap/out/segments.json after `node recap/cli.mjs build <audience>`.
import { homedir } from "node:os";
const HOME = homedir();

export default {
  name: "serpentine",
  // recap audience that produces recap/out/{recap.mp3,words.json,segments.json}.
  // Build it first:  node recap/cli.mjs build serpentine
  audience: "serpentine",

  out: `${HOME}/Documents/Shelf/serpentine-fae-widescreen.mp4`,
  bed: `${HOME}/Documents/Shelf/serpentine-ambient-bed.mp3`,

  palette: { cream: "#fcf7c5", green: "#3dff88", shadow: "#000000" },
  wordmark: ["Aesthetic", "Computer"],   // rendered top-right with a bouncing dot between

  // opening title card — a soft landscape "commons / planetary orchestra" gen
  // (title-card/gens/v2.png), with YWFT type LIVE-rendered + per-character
  // animated in cream with the AC hard drop shadow over the sky.
  titleCard: {
    gen: "title-card/gens/v2.png",
    title: ["Aesthetic", "Computer"], subtitle: "A Computer of Your Own",
    ink: "#fcf7c5", shadow: "#06120c", dot: "#3dff88", subInk: "#ffe7b0",
    titleY: 150, titlePx: 116, subPx: 54, bgBlur: 4, dur: 4,
  },
  caption: { bandY: 0.85 },              // word-train vertical centre (fraction of height; lower = nearer bottom)

  // per-segment accent color — drives the active-word glow + the slide-number
  // superscript. An evolving arc across the film (AC palette).
  colors: {
    "01_title": "#3dff88",      // green
    "02_problem": "#70f0e0",    // cyan — the enclosure
    "03_os_layer": "#ffbf3d",   // amber — the idea
    "04_creative_os": "#3dff88",// green — the instrument
    "05_commons": "#ff70d0",    // magenta — the community
    "05b_quote": "#ffe026",     // yellow — the highlighted quote
    "06_llms": "#70f0e0",       // cyan — the machines
    "07_poised": "#ffbf3d",     // amber — the argument
    "08_plan": "#ff70d0",       // magenta — the six months, in community
    "09_outro": "#3dff88",      // green — the question
    "10_end": "#3dff88",
  },

  // segment name → on-screen display title (top-left, gets a slide-number superscript)
  titles: {
    "01_title": "A Computer of Your Own", "02_problem": "The Condition",
    "03_os_layer": "The Need", "04_creative_os": "The Creative OS",
    "05_commons": "The Commons", "05b_quote": "From the Paper", "06_llms": "What LLMs Change",
    "07_poised": "Why Aesthetic.Computer", "08_plan": "The Six Months",
    "09_outro": "Art x Convergence",
  },

  // paper-extract scenes: a segment shows a real AC-paper PDF page with a
  // yellow highlighter wiping across the quoted line(s), synced to the spoken
  // narration (the segment's narration IS the verbatim quote). marketing/bin/
  // paper-extract-scene.mjs renders it; compose uses it as that segment's bg.
  extracts: {
    "05b_quote": {
      pdf: "papers/arxiv-plork/plork.pdf",
      quote: "Three people with surplus laptops in a park can form a laptop orchestra.",
      cite: "Scudder, PLOrk'ing the Planet (2026), p.5",
    },
  },

  // whisper mis-hearing → correct caption spelling  [pattern, replacement, flags]
  textFixes: [
    ["Scutter", "Scudder", "gi"], ["\\bAllen\\b", "Alan", ""],
    ["notepad", "notepat", "gi"], ["kid-?lisp", "KidLisp", "gi"],
  ],

  // storyboard-only metadata (marketing/bin/storyboard-widescreen.mjs)
  storyboard: {
    coverKicker: "aesthetic computer · storyboard",
    coverTitle: "A Computer of Your Own",
    coverSub: "Serpentine — Future Art Ecosystems R&D Fellowship · Art × Convergence",
    spine: "A ~2-minute spoken video proposal. Personal computing is being pulled into the data center; Aesthetic Computer is the divergent counter-move — a bare-metal creative OS + commons that re-founds the personal computer on surplus hardware, with LLMs taken in as a local, owned faculty rather than a landlord.",
    onscreen: {
      "01_title": "a computer of your own — a proposal · art × convergence",
      "02_problem": "computing is being pulled into the data center · your laptop → a thin client",
      "03_os_layer": "a revolution at the OS layer · not another app — a different ground",
      "04_creative_os": "notepat — tunes you can type · picture · code · music — one medium",
      "05_commons": "many kinds of work, in common · js + kidlisp · paintings · tapes · chats · a url each",
      "05b_quote": "from the paper · PLOrk'ing the Planet (2026), p.5",
      "06_llms": "a faculty, not a landlord · local · owned · on your own terms",
      "07_poised": "it holds the whole stack · kernel · languages · community · archive",
      "08_plan": "the six months · harden the os · local faculty · grow the orchestra — in the open",
      "09_outro": "keep a computer personal — and held in common",
      "10_end": "aesthetic computer · a computer of your own",
    },
    markers: {
      "01_title": "hey", "02_problem": "the data center", "03_os_layer": "operating system layer",
      "04_creative_os": "surplus laptop", "05_commons": "it is a commons",
      "05b_quote": "three people", "06_llms": "language models change", "07_poised": "the whole stack",
      "08_plan": "with the fellowship", "09_outro": "art and convergence", "10_end": "__END__",
    },
    beats: {
      "01_title": "Who you are — title card over jeffrey at the citrus-green Neo.",
      "02_problem": "The enclosure — a laptop dissolving into the data center, tethered as a thin client.",
      "03_os_layer": "The pivot — home computing needs a revolution at the OS layer, not another app.",
      "04_creative_os": "The instrument — notepat: typable tunes, folk transmission, picture/code/music as one medium, on a dedicated OS.",
      "05_commons": "The commons — KidLisp + a planetary laptop orchestra on surplus hardware (PLOrk $1,500/seat vs 240M obsolete PCs).",
      "05b_quote": "Paper extract — the PLOrk paper fills the frame; a yellow highlighter wipes across the cited line as it's read aloud.",
      "06_llms": "The turn — LLMs as a local, owned faculty rather than a landlord.",
      "07_poised": "Why AC — it already holds the whole stack: kernel, language, community, archive.",
      "09_outro": "The question — keep a computer personal, and held in common.",
      "10_end": "Wordmark hold — Aesthetic•Computer.",
    },
  },
};
