#!/usr/bin/env node
// ─────────────────────────────────────────────────────────────────────────
// tezos/series/generate.mjs
//
// Generates KidLisp source for three keeps SUB-SERIES that mint into the live
// v11 contract (KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB):
//
//   • feedback — living textures: a seed shape + accumulating pixel feedback
//                (no per-frame wipe → trails compound forever)
//   • clock    — real-time pieces driven by wall-clock second triggers
//                (Ns / Ns... fire on the second; the work visibly ticks)
//   • melody   — tiny audiovisual songs: (melody ...) + amplitude-reactive form
//
// Output → tezos/series/manifest.json (source + a `code` slot the bridge fills
// after running each piece through the logged-in Electron webview).
//
// Syntax here is grounded in real .lisp fixtures (complex-timing, fia-birthday,
// justsound, lab, brush) and the melody-parser — not guessed.
//
// Usage:
//   node tezos/series/generate.mjs            # write manifest.json
//   node tezos/series/generate.mjs --seed 7   # re-roll palettes (motion stays)
//   node tezos/series/generate.mjs --print    # print sources, don't write
// ─────────────────────────────────────────────────────────────────────────

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const MANIFEST = path.join(__dirname, "manifest.json");

// ── args ──────────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const getArg = (k, d) => {
  const i = argv.indexOf(`--${k}`);
  return i >= 0 ? argv[i + 1] : d;
};
const seed = parseInt(getArg("seed", "1"), 10) || 1;
const printOnly = argv.includes("--print");

// ── tiny seeded RNG so palette re-rolls are reproducible ────────────────────
function mulberry32(a) {
  return function () {
    a |= 0;
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rng = mulberry32(seed * 2654435761);
const pick = (arr) => arr[Math.floor(rng() * arr.length)];

// Curated palettes (each a cycling set used inside (Ns... ...)). All names are
// CSS colors KidLisp accepts.
const PALETTES = [
  ["cyan", "teal", "blue", "navy"],
  ["magenta", "purple", "indigo", "blue"],
  ["white", "aqua", "skyblue"],
  ["pink", "red", "orange", "yellow"],
  ["lime", "green", "white"],
  ["red", "orange", "yellow", "lime", "green"],
  ["gold", "orange", "crimson"],
  ["aquamarine", "turquoise", "mediumblue"],
  ["violet", "deeppink", "white"],
  ["springgreen", "lime", "yellow"],
];
const cyc = (speed, palette) => `(${speed}s... ${palette.join(" ")})`;

// ─────────────────────────────────────────────────────────────────────────
// FEEDBACK — accumulating-buffer organisms (no per-frame wipe)
// ─────────────────────────────────────────────────────────────────────────
function feedbackSeries() {
  const p = () => pick(PALETTES);
  const cs = () => pick(["0.1", "0.15", "0.2", "0.3"]);

  const items = [
    {
      title: "drift",
      source: [
        "; feedback · drift",
        "(once (wipe black))",
        `(ink ${cyc(cs(), p())})`,
        "(circle (+ width/2 (* 40 (sin))) height/2 4)",
        "(scroll 2 0)",
        "(zoom 1.003)",
        "(blur 1)",
      ],
    },
    {
      title: "vortex",
      source: [
        "; feedback · vortex",
        "(once (wipe black))",
        `(ink ${cyc(cs(), p())})`,
        "(box (- width/2 3) (- height/2 3) 6 6)",
        "(spin 2)",
        "(zoom 1.02)",
        "(blur 1)",
      ],
    },
    {
      title: "swell",
      source: [
        "; feedback · swell",
        "(once (wipe navy))",
        `(ink ${cyc(cs(), p())})`,
        "(circle width/2 height/2 (+ 6 (* 5 (sin))))",
        "(zoom 1.012)",
        "(scroll 0 1)",
        "(blur 1)",
      ],
    },
    {
      title: "bloom",
      source: [
        "; feedback · bloom",
        "(once (wipe black))",
        `(ink ${cyc(cs(), p())})`,
        "(circle (+ width/2 (* 30 (sin))) height/2 5)",
        "(zoom 1.01)",
        "(spin 1)",
        "(blur 2)",
      ],
    },
    {
      title: "weave",
      source: [
        "; feedback · weave",
        "(once (wipe black))",
        `(ink ${cyc(cs(), p())})`,
        "(box (+ width/2 (* 50 (sin))) (- height/2 2) 4 4)",
        "(scroll 1 1)",
        "(zoom 1.004)",
        "(blur 1)",
      ],
    },
    {
      title: "ember",
      source: [
        "; feedback · ember",
        "(once (wipe black))",
        `(ink ${cyc(cs(), p())})`,
        "(circle width/2 (+ height/2 (* 40 (sin))) 3)",
        "(scroll 0 -1)",
        "(zoom 0.997)",
        "(blur 1)",
      ],
    },
  ];
  return items.map((i) => ({ ...i, source: i.source.join("\n") }));
}

// ─────────────────────────────────────────────────────────────────────────
// CLOCK — real-time pieces that visibly tick on wall-clock seconds
// ─────────────────────────────────────────────────────────────────────────
function clockSeries() {
  const p = () => pick(PALETTES);

  const items = [
    {
      title: "tick",
      source: [
        "; clock · tick",
        "(wipe black)",
        `(ink ${cyc("1", p())})`,
        "(circle width/2 height/2 (+ 10 (* 8 (sin))))",
        "(ink white)",
        "(1s (box 0 (- height 10) width 10))",
      ],
    },
    {
      title: "metronome",
      source: [
        "; clock · metronome",
        "(0.5s (wipe black))",
        `(ink ${cyc("1", p())})`,
        "(box (1s... 8 (- width 24)) (- height/2 8) 16 16)",
      ],
    },
    {
      title: "pulse",
      source: [
        "; clock · pulse",
        "(wipe (1s... black navy))",
        `(ink ${cyc("0.5", p())})`,
        "(circle width/2 height/2 (+ 6 (* 20 (sin))))",
      ],
    },
    {
      title: "grow",
      source: [
        "; clock · grow",
        "(8s (wipe black))",
        `(ink ${cyc("0.5", p())})`,
        "(circle width/2 height/2 (* 6 (mod frame 80)))",
        "(blur 1)",
      ],
    },
    {
      title: "pendulum",
      source: [
        "; clock · pendulum",
        "(wipe black)",
        `(ink ${cyc("1", p())})`,
        "(line width/2 0 (+ width/2 (* 60 (sin))) height)",
        "(ink white)",
        "(0.5s (circle (+ width/2 (* 60 (sin))) (- height 8) 5))",
      ],
    },
  ];
  return items.map((i) => ({ ...i, source: i.source.join("\n") }));
}

// ─────────────────────────────────────────────────────────────────────────
// MELODY — tiny audiovisual songs ((melody ...) + amplitude-reactive form)
// ─────────────────────────────────────────────────────────────────────────
function melodySeries() {
  const p = () => pick(PALETTES);

  const items = [
    {
      title: "ascend",
      source: [
        "; melody · ascend",
        "(wipe black)",
        '(melody "4cdefgab5c" 132)',
        `(ink ${cyc("0.1", p())})`,
        "(circle width/2 height/2 (+ 8 (* 50 amplitude)))",
        "(blur 1)",
      ],
    },
    {
      title: "waltz",
      source: [
        "; melody · waltz",
        "(wipe (0.5s... midnightblue black))",
        '(melody "5ceg5ceg 4c.." 120 "3/4" "waltz")',
        `(ink ${cyc("0.2", p())})`,
        "(circle width/2 height/2 (+ 6 (* 45 amplitude)))",
      ],
    },
    {
      title: "penta",
      source: [
        "; melody · penta",
        "(wipe black)",
        '(melody "5cdega6c" 144 "4/4" "swing")',
        `(ink ${cyc("0.15", p())})`,
        "(circle width/2 height/2 (+ 6 (* 45 amplitude)))",
        "(spin 1)",
      ],
    },
    {
      title: "duet",
      source: [
        "; melody · duet",
        "(wipe black)",
        '(melody "5ceg 3c4g" 120)',
        `(ink ${cyc("0.1", p())})`,
        "(circle (- width/2 30) height/2 (+ 6 (* 40 amplitude)))",
        "(circle (+ width/2 30) height/2 (+ 6 (* 40 amplitude)))",
      ],
    },
    {
      title: "bells",
      source: [
        "; melody · bells",
        "(wipe midnightblue)",
        '(melody "{sine}6cegb6ce" 160)',
        `(ink ${cyc("0.1", p())})`,
        "(circle width/2 (+ height/2 (* 30 (sin))) (+ 4 (* 30 amplitude)))",
      ],
    },
    {
      title: "drone",
      source: [
        "; melody · drone",
        "(wipe black)",
        '(melody "{sine}3c,,,3g,,," 60)',
        `(ink ${cyc("0.3", p())})`,
        "(box (- width/2 20) (- height/2 20) (+ 8 (* 60 amplitude)) (+ 8 (* 60 amplitude)))",
        "(blur 2)",
      ],
    },
  ];
  return items.map((i) => ({ ...i, source: i.source.join("\n") }));
}

// ── assemble ────────────────────────────────────────────────────────────────
const series = {
  feedback: feedbackSeries(),
  clock: clockSeries(),
  melody: melodySeries(),
};

// Preserve any codes already captured by the bridge across re-generation,
// keyed by source text (so re-rolling palettes doesn't wipe minted codes).
let prevBySource = new Map();
if (fs.existsSync(MANIFEST)) {
  try {
    const prev = JSON.parse(fs.readFileSync(MANIFEST, "utf8"));
    for (const list of Object.values(prev.series || {})) {
      for (const it of list) if (it.code) prevBySource.set(it.source, it.code);
    }
  } catch {}
}

let total = 0;
for (const list of Object.values(series)) {
  for (const it of list) {
    it.code = prevBySource.get(it.source) || null;
    total++;
  }
}

const manifest = {
  contract: "KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB",
  network: "mainnet",
  seed,
  generatedAt: null, // stamped by the caller / bridge; kept null for determinism
  note: "Sub-series tagged by `series`. Run tezos/series/bridge.mjs to mint-cache as @jeffrey.",
  series,
};

if (printOnly) {
  for (const [name, list] of Object.entries(series)) {
    console.log(`\n══════════ ${name} (${list.length}) ══════════`);
    for (const it of list) {
      console.log(`\n── ${name}/${it.title} ──`);
      console.log(it.source);
    }
  }
} else {
  fs.writeFileSync(MANIFEST, JSON.stringify(manifest, null, 2));
  console.log(`✅ Wrote ${MANIFEST}`);
  console.log(
    `   ${total} pieces — feedback:${series.feedback.length} clock:${series.clock.length} melody:${series.melody.length} (seed ${seed})`,
  );
  const carried = [...prevBySource.values()].length;
  if (carried) console.log(`   carried ${carried} already-minted $code(s) forward`);
  console.log(`\nNext: launch the Electron app (production, logged in as @jeffrey), then:`);
  console.log(`   node tezos/series/bridge.mjs            # run + cache all pieces`);
}
