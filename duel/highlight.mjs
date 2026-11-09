import { glyphs as chunky } from './chunky-font.mjs';
import { smallWidth } from './paint.mjs';
// Coloured runs of pixel text: syntax colours for Aesel's code, controller
// face buttons as coloured badges, and verbs in the dev strip.

export const THEME = {
  text: 0xf2f2f2,
  dim: 0x8a8f9e,
  number: 0xffb86c,
  string: 0x8be28b,
  head: 0x7cc4ff,
  key: 0xd7a6ff,
  verb: 0xffe066,
};

// Xbox layout; the standard Gamepad mapping puts A at the bottom.
export const BUTTONS = {
  A: { fill: 0x3fb950, ink: 0xffffff },
  B: { fill: 0xe5534b, ink: 0xffffff },
  X: { fill: 0x4a90e2, ink: 0xffffff },
  Y: { fill: 0xf5c518, ink: 0x181818 },
  LB: { fill: 0x5a5f70, ink: 0xffffff },
  RB: { fill: 0x5a5f70, ink: 0xffffff },
  RT: { fill: 0x5a5f70, ink: 0xffffff },
  LT: { fill: 0x5a5f70, ink: 0xffffff },
  LS: { fill: 0x5a5f70, ink: 0xffffff },
  RS: { fill: 0x5a5f70, ink: 0xffffff },
  START: { fill: 0x5a5f70, ink: 0xffffff },
  BACK: { fill: 0x5a5f70, ink: 0xffffff },
};
// A badge lights white while its button is held.
const LIT = { fill: 0xffffff, ink: 0x181818 };
const badgeWidth = (b) => (b.length * 6 + 4);

const TOKEN =
  /("(?:[^"\\]|\\.)*"?)|(-?\d+(?:\.\d+)?)|([()[\]{}])|([A-Za-z_][\w-]*)(?=\s*")?|(\s+)|([^\s])/g;

export function codeSpans(line, base = THEME.text) {
  const spans = [];
  let previous = '';
  for (const m of line.matchAll(TOKEN)) {
    const [all, str, num, bracket, word] = m;
    let color = base;
    if (str) color = /:\s*$/.test(line.slice(m.index + all.length, m.index + all.length + 2)) ? THEME.key : THEME.string;
    else if (num) color = THEME.number;
    else if (bracket) color = THEME.dim;
    else if (word) color = previous === '(' ? THEME.head : base;
    else if (all === ':' || all === ',' || all === ';') color = THEME.dim;
    if (all.trim()) previous = all;
    spans.push({ text: all, color });
  }
  return merge(spans);
}

// Live source: timers flash as their period comes round (1s, 2s..., 0.5s!),
// and random choices (?) shimmer, the way AC's editor animates KidLisp.
export function animateSpans(spans, now) {
  const out = [];
  for (const s of spans) {
    if (s.badge || !s.text) {
      out.push(s);
      continue;
    }
    for (const m of s.text.matchAll(/(\d*\.?\d+s(?:\.\.\.|!)?)|(\?)|([^?\d]+|\d+(?!\.?\d*s))/g)) {
      const [all, timer, chance] = m;
      if (timer) {
        const period = parseFloat(timer) || 1;
        const phase = ((now / 1000) % period) / period;
        out.push({ text: all, color: phase < 0.12 ? 0xffffff : phase < 0.3 ? 0xffd28a : s.color });
      } else if (chance) {
        const hues = [0xff6b6b, 0xffe066, 0x8be28b, 0x7cc4ff, 0xd7a6ff];
        out.push({ text: all, color: hues[Math.floor(now / 120) % hues.length] });
      } else out.push({ text: all, color: s.color });
    }
  }
  return out;
}

// "[A] CHOOSE / [X] BUILD" → text runs and button badges; "{enter} build"
// → an outlined key cap (2026-11-03), so every control reads as a key.
export function hintSpans(line, color = THEME.text) {
  const spans = [];
  let at = 0;
  for (const m of line.matchAll(/\[(LB|LT|RB|RT|LS|RS|START|BACK|[ABXY])\]|\{([^}]+)\}/g)) {
    if (m.index > at) spans.push({ text: line.slice(at, m.index), color });
    if (m[1]) spans.push({ badge: m[1] });
    else spans.push({ cap: m[2] });
    at = m.index + m[0].length;
  }
  if (at < line.length) spans.push({ text: line.slice(at), color });
  return spans;
}
// A key cap: the key's name in a thin outline, lit while it is held.
const capWidth = (k) => k.length * 6 + 4;

// A running step's verb is yellow with ticking dots; a finished one is green.
export function devSpans({ kind, text, done, age = 0 }, color = THEME.text, live = false) {
  const when = live && age >= 5 ? ` ${age < 60 ? Math.floor(age) + 's' : Math.floor(age / 60) + 'm'}` : '';
  if (kind !== 'do') return [{ text, color }, { text: when, color: THEME.dim }].filter((s) => s.text);
  const [verb, ...rest] = text.split(' ');
  const dots = live && !done ? ' ' + '.'.repeat(1 + (Math.floor(age * 3) % 3)) : '';
  return [
    { text: verb.toLowerCase(), color: done ? 0x3fb950 : THEME.verb },
    { text: rest.length ? ' ' + rest.join(' ') : '', color },
    { text: dots, color: THEME.verb },
    { text: done ? when : '', color: THEME.dim },
  ].filter((s) => s.text);
}

const merge = (spans) =>
  spans.reduce((out, s) => {
    const last = out[out.length - 1];
    if (last && last.color === s.color && !last.badge && !s.badge) last.text += s.text;
    else out.push({ ...s });
    return out;
  }, []);

export const spanWidth = (spans, unit) =>
  spans.reduce((w, s) => w + (s.badge ? badgeWidth(s.badge) + 1 : s.cap ? capWidth(s.cap) + 1 : s.block ? (s.wire !== undefined ? 11 : 7) : s.text.length * 6) * unit, 0);

// Compact (MatrixChunky8) widths: caps and badges are the name plus a
// two-pixel frame each side.
// MatrixChunky8 has no arrows: the arrow keys get five-pixel drawings.
const SMALL_ARROWS = {
  '←': ['..#..', '.#...', '#####', '.#...', '..#..'],
  '→': ['..#..', '...#.', '#####', '...#.', '..#..'],
  '↑': ['..#..', '.###.', '#.#.#', '..#..', '..#..'],
  '↓': ['..#..', '..#..', '#.#.#', '.###.', '..#..'],
};
const smallBoxWidth = (k) => (SMALL_ARROWS[k] ? 5 : smallWidth(k)) + 4;
// The rows a label's ink actually covers, counted down from small()'s y
// (row k is the band from y - k to y - k + 1): tall letters start at 0,
// x-height at 2; the baseline ends at 6, descenders at 7. A cap's outline sits
// one clear row outside these, so every label has the same 1px gap
// (2026-11-08).
function inkRows(name) {
  if (SMALL_ARROWS[name]) return [2, 6];
  let top = Infinity, bottom = -Infinity;
  for (const ch of String(name)) {
    const g = chunky[ch] || chunky['?'];
    const [, h, , yoff] = g;
    if (ch === ' ') continue;
    top = Math.min(top, 8 - yoff - h);
    bottom = Math.max(bottom, 8 - yoff - 1);
  }
  return Number.isFinite(top) ? [top, bottom] : [2, 6];
}
export const smallSpanWidth = (spans, unit) =>
  spans.reduce(
    (w, s) =>
      w + (s.badge ? smallBoxWidth(s.badge) + 1 : s.cap ? smallBoxWidth(s.cap) + 1 : s.text ? smallWidth(s.text) + 1 : 0) * unit,
    0,
  );

export const plain = (spans) =>
  spans.map((s) => (s.badge ? `[${s.badge}]` : s.cap ? s.cap : s.block ? '' : s.text)).join('');

// Clip runs to a column budget; a badge counts as two columns.
export function clipSpans(spans, columns) {
  const out = [];
  let left = columns;
  for (const s of spans) {
    const cost = s.badge ? s.badge.length + 1 : s.cap ? s.cap.length + 1 : s.block ? 1 : s.text.length;
    if (cost <= left) {
      out.push(s);
      left -= cost;
    } else {
      if (!s.badge && !s.cap && !s.block && left > 0) out.push({ ...s, text: s.text.slice(0, left) });
      break;
    }
  }
  return out;
}

// Draw centred at cx (screen-right), top row at y. rect must be the
// overlay-flipped rect so faces and glyphs share one x axis.
export function drawSpans({ rect, text, small }, spans, cx, y, unit, { z = 0, shadow = 0x181818, step = 0.000001, tiny = false } = {}) {
  // tiny: MatrixChunky8 (proportional) instead of font_1.
  if (tiny && small) {
    let left = cx - smallSpanWidth(spans, unit) / 2;
    for (const s of spans) {
      if (s.badge || s.cap) {
        const name = s.badge || s.cap;
        const bw = smallBoxWidth(name) * unit;
        // Sized to the label's own ink: outline, one clear pixel, the ink,
        // one clear pixel, outline — the same on every side.
        const [top, low] = inkRows(name);
        const bottom = y - (low + 2) * unit, bh = (low - top + 5) * unit;
        const textX = left + 2 * unit + (smallWidth(name) * unit) / 2; // ink starts two in.
        if (s.badge) {
          const { fill, ink } = s.lit ? LIT : BUTTONS[s.badge];
          if (shadow !== null) rect(left + unit, bottom - unit, z + 2 * step, bw, bh, shadow);
          rect(left, bottom, z + step, bw, bh, fill);
          small(name, textX, y, z, unit, ink);
        } else {
          // A coloured cap (the legend's action colour) outlines in its colour;
          // held, any cap lights white.
          const line = s.lit ? 0xffffff : s.color ?? THEME.dim;
          if (shadow !== null) rect(left + unit, bottom - unit, z + 3 * step, bw, bh, shadow);
          rect(left, bottom, z + 2 * step, bw, bh, line);
          rect(left + unit, bottom + unit, z + step, bw - 2 * unit, bh - 2 * unit, 0x15171e);
          const ink = s.lit ? 0xffffff : s.color ?? THEME.text;
          const arrow = SMALL_ARROWS[name];
          if (arrow)
            arrow.forEach((row, r) =>
              [...row].forEach((ch, c) => ch === '#' && rect(left + (2 + c) * unit, y - (2 + r) * unit, z, unit, unit, ink)),
            );
          else small(name, textX, y, z, unit, ink);
        }
        left += bw + unit;
        continue;
      }
      if (!s.text) continue;
      const w = (smallWidth(s.text) + 1) * unit;
      const centre = left + (w - unit) / 2;
      if (shadow !== null && s.text.trim()) small(s.text, centre + unit, y - unit, z + 2 * step, unit, shadow);
      small(s.text, centre, y, z, unit, s.color);
      left += w;
    }
    return;
  }
  let left = cx - spanWidth(spans, unit) / 2;
  for (const s of spans) {
    if (s.badge) {
      const { fill, ink } = s.lit ? LIT : BUTTONS[s.badge];
      const bw = badgeWidth(s.badge) * unit;
      if (shadow !== null) rect(left + unit, y - 8 * unit, z + 2 * step, bw, 9 * unit, shadow);
      rect(left, y - 7 * unit, z + step, bw, 9 * unit, fill);
      text(s.badge, left + bw / 2, y - unit, z, unit, ink, true);
      left += bw + unit;
      continue;
    }
    if (s.block && s.wire !== undefined) {
      // The build prompt's cursor: a wireframe cube turning in the block's
      // cell (wider than a letter, so it reads at one pixel a unit), green
      // while a builder listens, red when none does (2026-11-08).
      const cx = left + 5 * unit, cy = y - 2 * unit, k = 3 * unit;
      const a = s.turn, b = 0.55 + 0.2 * Math.sin(a * 0.7);
      const ca = Math.cos(a), sa = Math.sin(a), cb = Math.cos(b), sb = Math.sin(b);
      const P = [];
      for (const vx of [-1, 1]) for (const vy of [-1, 1]) for (const vz of [-1, 1]) {
        const x1 = vx * ca + vz * sa, z1 = -vx * sa + vz * ca;
        const y1 = vy * cb - z1 * sb, z2 = vy * sb + z1 * cb;
        const p = 1 / (1 - z2 * 0.18);
        P.push([cx + x1 * k * p, cy + y1 * k * p]);
      }
      const edges = [[0, 1], [2, 3], [4, 5], [6, 7], [0, 2], [1, 3], [4, 6], [5, 7], [0, 4], [1, 5], [2, 6], [3, 7]];
      const dot = (px, py, color, zz) =>
        rect(Math.round(px / unit) * unit, Math.round(py / unit) * unit, zz, unit, unit, color);
      for (const [i, j] of edges) {
        const [[x0, y0], [x1, y1]] = [P[i], P[j]];
        const n = Math.max(1, Math.ceil(Math.max(Math.abs(x1 - x0), Math.abs(y1 - y0)) / unit));
        for (let t = 0; t <= n; t++) {
          const px = x0 + ((x1 - x0) * t) / n, py = y0 + ((y1 - y0) * t) / n;
          if (shadow !== null) dot(px + unit, py - unit, shadow, z + 2 * step);
          dot(px, py, s.wire, z + step);
        }
      }
      left += 11 * unit;
      continue;
    }
    if (s.block) {
      // The prompt's pink cursor block: output is being spat out here.
      if (shadow !== null) rect(left + unit, y - 8 * unit, z + 2 * step, 6 * unit, 10 * unit, shadow);
      rect(left, y - 7 * unit, z + step, 6 * unit, 10 * unit, 0xff3296);
      left += 7 * unit;
      continue;
    }
    if (s.cap) {
      // An outline a unit thick on a dark ground; the name in the middle.
      const bw = capWidth(s.cap) * unit, bh = 9 * unit, top = y - 7 * unit;
      const line = s.lit ? THEME.verb : THEME.dim;
      if (shadow !== null) rect(left + unit, top - unit, z + 3 * step, bw, bh, shadow);
      rect(left, top, z + 2 * step, bw, bh, line);
      rect(left + unit, top + unit, z + step, bw - 2 * unit, bh - 2 * unit, 0x15171e);
      text(s.cap, left + bw / 2, y - unit, z, unit, s.lit ? THEME.verb : THEME.text, true);
      left += bw + unit;
      continue;
    }
    const w = s.text.length * 6 * unit;
    const centre = left + (w - unit) / 2;
    if (shadow !== null && s.text.trim())
      text(s.text, centre + unit, y - unit, z + 2 * step, unit, shadow);
    text(s.text, centre, y, z, unit, s.color);
    left += w;
  }
}

// The ring's words and the prompt they build: colours by role, and colour
// words in their own colour.
const WORDS = {
  red: 0xff6b6b,
  blue: 0x6bb6ff,
  striped: 0xffe066,
  big: THEME.number,
  bigger: THEME.number,
  tiny: THEME.number,
  smaller: THEME.number,
  tall: THEME.number,
  taller: THEME.number,
  spinning: THEME.key,
  spin: THEME.key,
  bouncing: THEME.key,
  bounce: THEME.key,
  walking: THEME.key,
  flying: THEME.key,
  fluffy: THEME.number,
  humming: 0x8be28b,
  hum: 0x8be28b,
  low: 0x8be28b,
  high: 0x8be28b,
  deep: 0x8be28b,
  bright: 0x8be28b,
  ringing: 0x8be28b,
  buzzing: 0x8be28b,
  pulsing: 0x8be28b,
  chiming: 0x8be28b,
  sing: 0x8be28b,
  singing: 0x8be28b,
  make: THEME.dim,
  paint: THEME.dim,
  add: THEME.dim,
  it: THEME.dim,
  a: THEME.dim,
  an: THEME.dim,
};
export function phraseSpans(line, base = THEME.head) {
  return merge(
    [...String(line).matchAll(/([A-Za-z]+)|([^A-Za-z]+)/g)].map(([all, word]) => ({
      text: all,
      color: word ? (WORDS[word.toLowerCase()] ?? base) : THEME.dim,
    })),
  );
}
