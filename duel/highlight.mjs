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
  for (const m of line.matchAll(/\[(LB|LT|RB|RT|LS|RS|[ABXY])\]|\{([^}]+)\}/g)) {
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
  spans.reduce((w, s) => w + (s.badge ? badgeWidth(s.badge) + 1 : s.cap ? capWidth(s.cap) + 1 : s.block ? 7 : s.text.length * 6) * unit, 0);

export const smallSpanWidth = (spans, unit) =>
  spans.reduce((w, s) => w + (s.text ? smallWidth(s.text) + 1 : 0) * unit, 0);

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
