// vunn, 26.07.12
// A GROWING FRACTAL TREE/FERN instrument — the melody RECURSIVELY BRANCHES a
// plant. A thin wrapper over lib/pads.mjs (the shared pad engine: UTC-clock beat
// grid, `params[0]` rate override e.g. `vunn 0.5`, the tap/XY "pump", audio
// polling). This file only describes what makes vunn vunn: its koto/harp score,
// its self-similar L-system tree, and its botanical paint.
//
// ALLEGORY — every note GROWS A BRANCH GENERATION. The tree is built one
// generation per beat: each new note takes the tips of the previous generation
// and sprouts a self-similar pair of child branches from each (fractal
// subdivision). PITCH → the branch split ANGLE + child LENGTH ratio, so the
// melodic contour literally shapes the tree's silhouette. The trunk is the bass
// — it thickens and sways on the sub-bass beat. At the loop seam the whole tree
// fades and REGROWS from the seed. Distinct from vindle (a single wandering
// vine): vunn is a RECURSIVE, self-similar branching fractal — a fern/tree.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// Koto/harp pentatonic. One plucked note per beat → one branch generation. The
// phrase rises then folds, so the tree first reaches, then bushes out. The tree
// REGROWS every LOOP_LEN beats (the loop seam) — one phrase, one whole tree.
const ARP = [
  "e2", "b2", "e3", "g3", // seed + trunk reach (low → the tree grows tall)
  "b3", "d4", "e4", "g4", // mid canopy branches open
  "b4", "g4", "e4", "d4", // high buds shimmer, then settle
  "b3", "g3", "e3", "b2", // fold back down toward the seed
];
const LOOP_LEN = ARP.length; // 16 generations per grown tree
const BASS = ["e1", "e1", "c2", "c2", "a1", "a1", "b1", "b1"]; // trunk, half-time
const ACCENT_STEPS = new Set([8, 9, 10]); // bell buds at the canopy's peak

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// hue 0..1, sat/light 0..1 → [r,g,b] 0..255. Wraps num.hslToRgb (wants hue in
// DEGREES + sat/light 0..100, and already returns 0..255 — do NOT ×255).
function hue2rgb(num, hue, sat, light) {
  return num.hslToRgb((((hue % 1) + 1) % 1) * 360, sat * 100, light * 100);
}

// --- vunn-specific visual state (the engine owns pump/bursts/rhythm) ---------
// The tree is a list of GENERATIONS. Each generation is a list of branches, and
// each branch is grown from a parent tip in the previous generation. Growing one
// generation per beat means the whole melody is legible as the tree's structure.
//
// A branch: { x0,y0,x1,y1, angle, len, gen, hue, born, bud, budHue }
// The frontier = the tips of the newest generation (where the next split grows).
let generations = []; // [[branch,...], ...] oldest → newest
let frontier = []; // [{ x,y, angle, len }] — tips awaiting the next split
let treeFade = 1; // 1 = solid, → 0 as the tree fades at the loop seam
let trunkThick = 0; // decays; extra trunk girth on the bass beat
let trunkSway = 0; // decays; sway target, swings the whole tree
let rootPulse = 0; // decays; earthy bloom at the seed on the bass beat
let beatCount = 0; // total beats (for the recently-born branch glow)
let baseX = 0, baseY = 0; // seed position (screen coords)

let buds = []; // tap-sprouted buds: { x,y, hue, grow, life, segs }
let flecks = []; // pollen/leaf flecks (accents + taps)

// The engine's pump lives in ctx; onBeat fires inside sim, so we stash it.
let curPump = 0;
const pump = () => curPump;
let maxDepth = 6; // adaptive: how many child branches per split (quality-gated)
let grounded = false;

// Reseed the tree — a single upward trunk tip, called at each loop seam.
function reseed(w, h) {
  baseX = w / 2;
  baseY = h * 0.9; // grow up from near the bottom
  const trunkLen = Math.min(w, h) * 0.16;
  const tipX = baseX;
  const tipY = baseY - trunkLen;
  // Generation 0 is the trunk itself (one branch straight up).
  generations = [[{
    x0: baseX, y0: baseY, x1: tipX, y1: tipY,
    angle: -Math.PI / 2, len: trunkLen, gen: 0,
    hue: 0.08, born: beatCount, bud: false, budHue: 0.1,
  }]];
  frontier = [{ x: tipX, y: tipY, angle: -Math.PI / 2, len: trunkLen }];
  treeFade = 1;
}

// Grow ONE generation: from each frontier tip, sprout a self-similar PAIR of
// child branches, split by ±spread and scaled by lenRatio (both ∝ pitch). This
// is the L-system subdivision — the recursion is unrolled across beats so each
// note literally adds one fractal level. Returns the new frontier.
function growGeneration(pn, gen, accent) {
  const spread = 0.32 + pn * 0.55; // high pitch → wider fan
  const lenRatio = 0.62 + pn * 0.16; // high pitch → longer children
  const sway = trunkSway * 0.4; // whole tree leans with the bass sway
  const next = [];
  // Cap branch count by quality so deep generations stay cheap (60fps).
  const cap = 2 << Math.min(9, maxDepth); // generous ceiling
  for (const t of frontier) {
    if (generations.reduce((n, g) => n + g.length, 0) > cap) break;
    const childLen = t.len * lenRatio;
    for (const side of [-1, 1]) {
      const a = t.angle + side * spread + sway
        + Math.sin((gen + side) * 1.3) * 0.06; // organic jitter
      const nx = t.x + Math.cos(a) * childLen;
      const ny = t.y + Math.sin(a) * childLen;
      const hue = 0.24 + pn * 0.2 + gen * 0.006; // green → chartreuse w/ height
      const branch = {
        x0: t.x, y0: t.y, x1: nx, y1: ny,
        angle: a, len: childLen, gen, hue, born: beatCount,
        bud: accent || pump() > 1.3,
        budHue: accent ? 0.02 + pn * 0.06 : 0.12 + pn * 0.06,
      };
      generations[gen] = generations[gen] || [];
      generations[gen].push(branch);
      next.push({ x: nx, y: ny, angle: a, len: childLen });
    }
  }
  // Keep the frontier bounded so growth cost never explodes.
  frontier = next.length > 96 ? next.slice(0, 96) : next;
}

const CONFIG = {
  bpm: 96, // patient, hypnotic koto tempo (one generation per beat)
  steps: LOOP_LEN,
  drawBursts: false, // vunn draws its own bud sprouts in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.24, feedback: 0.5 }); // airy hall
    },

    // A new UTC beat crossed — grow one branch generation + pluck the note. Use
    // idx to detect the loop seam (idx % LOOP_LEN === 0) → reseed the tree.
    onBeat({ idx, synth, screen }) {
      const w = screen.width, h = screen.height;
      const s = ((idx % LOOP_LEN) + LOOP_LEN) % LOOP_LEN;

      // At the seam (or first-ever beat) reseed a fresh trunk.
      if (s === 0 || generations.length === 0) reseed(w, h);

      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const accent = ACCENT_STEPS.has(s);
      const pan = -0.55 + (s / (LOOP_LEN - 1)) * 1.1;

      // --- SONIC: koto/harp plucked note (Karplus–Strong harp + sine body).
      voices.pluck(synth, note, { beats: 1.0, decay: 0.55, volume: 0.55, pan });
      synth({ tone: note, type: "sine", beats: 0.7, attack: 0.003, decay: 0.5,
        volume: 0.24, pan }); // rounded koto body
      if (accent) {
        // Bright bell shimmer when the canopy buds.
        voices.bell(synth, note, { beats: 1.1, volume: 0.26, pan });
        for (let i = 0; i < 6 && flecks.length < 220; i++) {
          const ang = Math.random() * Math.PI * 2;
          const sp = 1 + Math.random() * 2.5;
          flecks.push({ x: baseX, y: baseY - h * 0.4, vx: Math.cos(ang) * sp,
            vy: Math.sin(ang) * sp - 0.5, life: 1, hue: 0.13 });
        }
      }

      // --- TRUNK BASS on the half-beat → sub, root pulse, trunk thickens+sways.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.7, decay: 0.5, volume: 0.52 });
        rootPulse = 1;
        trunkThick = 1;
        trunkSway = ((idx / 2) % 2 === 0 ? 1 : -1) * (0.12 + pn * 0.1);
      }

      // Soft koto-hall tick on off-beats → a leaf-rustle fleck.
      if (s % 2 === 1) {
        voices.hat(synth, { tone: 6500, beats: 0.1, volume: 0.12 });
      }

      // --- GRAPHIC: grow the NEXT fractal generation from the frontier tips.
      // gen index = how many generations already exist (trunk = gen 0).
      const gen = generations.length;
      growGeneration(pn, gen, accent);
      beatCount++;
    },

    onSim({ pump: p, step, beatProgress, screen }) {
      const w = screen.width, h = screen.height;
      if (generations.length === 0) reseed(w, h);
      curPump = p; // stash engine pump for onBeat geometry (fires inside sim)

      // Fade the tree across the last slice of the loop → seamless regrow.
      const phase = (step + beatProgress) / LOOP_LEN; // 0..1 through this tree
      treeFade = phase > 0.88 ? Math.max(0, 1 - (phase - 0.88) / 0.12) : 1;

      // Advance flecks (pollen / leaves).
      for (const f of flecks) {
        f.x += f.vx; f.y += f.vy; f.vy += 0.06; f.vx *= 0.98; f.life -= 0.03;
      }
      flecks = flecks.filter((f) => f.life > 0);

      // Advance tap-sprouted buds — each grows a tiny 2-branch tuft over time.
      for (const b of buds) {
        b.grow += 0.05 + p * 0.04;
        while (b.segs.length < Math.floor(b.grow) * 2 && b.segs.length < 6) {
          const parent = b.segs.length < 2
            ? { x: b.x, y: b.y, a: -Math.PI / 2 }
            : b.segs[b.segs.length - 2];
          const side = b.segs.length % 2 === 0 ? -1 : 1;
          const a = parent.a + side * 0.5;
          const len = Math.min(w, h) * 0.035 * (1 + p * 0.3);
          const nx = parent.x + Math.cos(a) * len;
          const ny = parent.y + Math.sin(a) * len;
          b.segs.push({ x0: parent.x, y0: parent.y, x1: nx, y1: ny, x: nx, y: ny, a,
            bud: b.segs.length >= 4 });
        }
        b.life -= 0.006;
      }
      buds = buds.filter((b) => b.life > 0);

      // Decay pulses (the engine decays pump itself).
      rootPulse *= 0.9;
      trunkThick *= 0.92;
      trunkSway *= 0.9;
    },

    // Tap = a NEW BUD sprouts from the touch point + a plucked boost (engine
    // already bumped pump + pushed the burst; X→pan/hue, Y→pitch, top = high).
    onTap({ x, y, ex, ey, synth, isDraw }) {
      const drag = isDraw;
      const hue = 0.2 + (1 - y) * 0.22; // higher tap → golder bud
      buds.push({ x: ex, y: ey, hue, grow: 0, life: 1, segs: [] });
      const n = drag ? 3 : 8;
      for (let i = 0; i < n && flecks.length < 220; i++) {
        const ang = Math.random() * Math.PI * 2;
        const sp = 1 + Math.random() * (drag ? 2 : 4);
        flecks.push({ x: ex, y: ey, vx: Math.cos(ang) * sp,
          vy: Math.sin(ang) * sp - 1, life: 1, hue });
      }

      // SONIC pluck — X→pan/hue, Y→pitch (top = high). Koto harp + sine body.
      const scale = ["e", "g", "a", "b", "d"]; // E minor pentatonic
      const oct = 2 + Math.floor((1 - y) * 3); // 2..4, higher tap = higher
      const note = scale[Math.min(4, Math.floor(x * 5))] + oct;
      voices.pluck(synth, note, { beats: drag ? 0.5 : 1.0, decay: 0.55,
        volume: drag ? 0.42 : 0.7, pan: x * 2 - 1 });
      if (!drag) voices.bell(synth, note, { beats: 0.6, volume: 0.22, pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, box, screen, num, paintCount } = api;
      const { pump, band, quality } = s;
      const w = screen.width, h = screen.height;

      // Quality → recursion cap. More quality = deeper/bushier fractal.
      maxDepth = quality < 0.6 ? 4 : quality < 0.85 ? 5 : 6;
      const bx = baseX || w / 2;
      const by = baseY || h * 0.9;

      // Lay a SOLID dusk-botanical ground on the first frames (a translucent veil
      // alone converges to gray). Then a soft veil each frame so growth persists.
      if (!grounded || Number(paintCount) < 3) {
        ink("fade:#0a1420-#04070c:vertical").box(0, 0, w, h);
        grounded = true;
      }
      ink(6, 16, 20, Math.max(70, 100 - pump * 8)).box(0, 0, w, h);
      ink("fade:#0a1420-#04070c:vertical", 32).box(0, 0, w, h);

      const bass = band("subBass");
      const air = band("air");

      // === ROOT PULSE — earthy bloom at the seed on the trunk/bass beat ===
      const bloom = Math.max(rootPulse, 0) * (1 + pump * 0.3);
      if (bloom > 0.02) {
        const R = Math.min(w, h) * (0.09 + bass * 0.35) * bloom;
        const [r0, g0, b0] = hue2rgb(num, 0.1, 0.65, 0.32);
        for (let i = 4; i > 0; i--) {
          ink(r0, g0, b0, 30 * bloom).circle(bx, by, R * (i / 4), true);
        }
      }

      // === THE TREE — draw oldest→newest so tips sit on top. Recently-born
      // branches glow. Deeper generations render thinner (self-similar taper). ===
      const now = beatCount;
      // How many generations to render (quality-gated for perf).
      const genLimit = generations.length; // grow-cap already bounds branch count
      for (let g = 0; g < genLimit; g++) {
        const genBranches = generations[g];
        if (!genBranches) continue;
        for (const br of genBranches) {
          const age = now - br.born;
          const fresh = Math.max(0, 1 - age * 0.14);
          const light = 0.3 + (1 - br.gen / (LOOP_LEN + 1)) * 0.18 + fresh * 0.24;
          const [r, gc, b] = hue2rgb(num, br.hue, 0.8, Math.min(0.85, light));
          const a = 235 * treeFade;
          // Thickness tapers with generation depth (trunk thick → twigs thin).
          const baseTh = Math.max(1, (7 - br.gen * 0.9)) * (1 + trunkThick * 0.4);
          const th = baseTh * (1 + fresh * 0.5 + bass * 0.4);
          ink(r, gc, b, a).line(br.x0, br.y0, br.x1, br.y1, Math.max(1, th));
          // Highlight core down thicker limbs.
          if (th > 2.5) {
            const [hr, hg, hb] = hue2rgb(num, br.hue, 0.45, 0.72);
            ink(hr, hg, hb, 110 * treeFade)
              .line(br.x0, br.y0, br.x1, br.y1, Math.max(1, th * 0.4));
          }
          // === Bud / flower at accented (or pumped) branch tips ===
          if (br.bud) {
            const budGrow = Math.min(1, 0.3 + fresh);
            const rad = (2.5 + budGrow * 4) * (1 + pump * 0.3);
            const [pr, pg, pb] = hue2rgb(num, br.budHue, 0.9, 0.6);
            ink(pr, pg, pb, 220 * treeFade).circle(br.x1, br.y1, rad, true);
            ink(255, 240, 150, 200 * treeFade * fresh)
              .circle(br.x1, br.y1, rad * 0.4, true); // pollen glint
          }
        }
      }

      // === Growing TIPS — the frontier buds swell within each beat ===
      if (treeFade > 0.02 && frontier.length) {
        const tipR = (2 + s.beatProgress * 3 + bass * 6) * (1 + pump * 0.35);
        for (const t of frontier) {
          ink(210, 255, 180, 200 * treeFade).circle(t.x, t.y, tipR, true);
          ink(255, 255, 255, 150 * treeFade).circle(t.x, t.y, tipR * 0.45, true);
        }
      }

      // === TAP BUDS — the whole-piece "button" made visible as new tufts ===
      for (const b of buds) {
        const [r, gc, bl] = hue2rgb(num, b.hue, 0.85, 0.55);
        for (const sg of b.segs) {
          ink(r, gc, bl, 235 * b.life).line(sg.x0, sg.y0, sg.x1, sg.y1,
            Math.max(1, (2 + pump) * b.life));
          if (sg.bud) {
            const [br2, bg2, bb2] = hue2rgb(num, b.hue + 0.05, 0.9, 0.62);
            ink(br2, bg2, bb2, 230 * b.life).circle(sg.x1, sg.y1, 3 + pump, true);
          }
        }
        ink(220, 255, 180, 200 * b.life).circle(b.x, b.y, 3 + pump, true);
      }

      // === Flecks = pollen / leaf flickers ===
      for (const f of flecks) {
        const [r, gc, b] = hue2rgb(num, f.hue, 0.7, 0.6);
        ink(r, gc, b, 220 * f.life).circle(f.x, f.y, 1 + f.life * 2, true);
      }

      // Air-band dew shimmer over the canopy (garnish when audio is live).
      if (air > 0.05) {
        ink(200, 255, 220, air * 80)
          .circle(bx, by - Math.min(w, h) * 0.35, Math.min(w, h) * 0.22, false, 1);
      }
    },
  },
};

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared singleton
// — each entry must re-assert THIS pad's config before the engine boot/sim/paint/
// act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
