#!/usr/bin/env node
// build-score.mjs — render the momabobasheep graphic score as a single wide
// SVG inside an HTML page, ready to screenshot with chrome-shot.mjs.
//
// Reads:  pop/momboba/out/momabobasheep.events.json (from score-extract.mjs)
// Writes: pop/momboba/out/momabobasheep-score.html
//
// Then:   node toolchain/macos/chrome-shot.mjs file://…/momabobasheep-score.html \
//           pop/momboba/out/momabobasheep-score.png --size 3200x1400

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
const D = JSON.parse(readFileSync(resolve(OUT, "momabobasheep.events.json"), "utf8"));

// ── canvas ────────────────────────────────────────────────────────────────
const W = 3200, H = 1400;
const ML = 190, MR = 56;            // left gutter (lane labels) / right margin
const PW = W - ML - MR;             // plot width
const T = D.masterSec;              // 600 s
const x = (t) => ML + (t / T) * PW;

const INK = "#1E1E1E", CREAM = "#FBFAFF";
const FAINT = "rgba(30,30,30,0.14)", FAINTER = "rgba(30,30,30,0.07)";

// restrained accents, one per lane
const C = {
  whistle: "#2A6F77",   // deep teal
  bells:   "#6E5A8E",   // dusty violet
  sparkle: "#C08A2D",   // amber
  arp:     "#A65A3A",   // terracotta
  alto:    "#7A7B45",   // olive
  pad:     "#5B7FA6",   // soft slate blue
  bass:    "#6B4A36",   // warm brown
  drone:   "#8A8378",   // warm grey
  sub:     "#57514A",   // deeper warm grey
  golden:  "#B3402E",   // climax marker
  band:    "#3E4470",   // movement band indigo
};

// chord-root pitch-class hues (subtle pastel fills)
const PC_HUE = { C: 205, Db: 230, D: 255, Eb: 280, E: 305, F: 35, Gb: 330, G: 80, Ab: 105, A: 130, Bb: 160, B: 185 };
const chordFill = (name) => {
  const pc = name.replace(/m$/, "");
  const minor = /m$/.test(name);
  return `hsla(${PC_HUE[pc] ?? 0}, ${minor ? 30 : 45}%, ${minor ? 72 : 66}%, 0.55)`;
};

const esc = (s) => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;");
const svg = [];

// ── layout rows ───────────────────────────────────────────────────────────
const TITLE_Y = 64;
const BAND_Y = 108, BAND_H = 74;
const CHORD_Y = 200, CHORD_H = 46;
const LANES = [
  { key: "whistle", label: "whistle melody",    h: 112 },
  { key: "bells",   label: "reverso sine bells", h: 100 },
  { key: "sparkle", label: "kalimba sparkle",   h: 70 },
  { key: "arp",     label: "rosewood arpeggio", h: 132 },
  { key: "alto",    label: "rosewood alto",     h: 70 },
  { key: "pad",     label: "vibraphone pad",    h: 112 },
  { key: "bass",    label: "walking bass",      h: 100 },
  { key: "drone",   label: "drone bed + sub",   h: 106 },
];
let yCur = CHORD_Y + CHORD_H + 18;
for (const l of LANES) { l.y = yCur; yCur += l.h + 13; }
const ARC_Y = yCur + 8, ARC_H = 148;
const AXIS_Y = ARC_Y + ARC_H + 34;

// ── background ────────────────────────────────────────────────────────────
svg.push(`<rect width="${W}" height="${H}" fill="${CREAM}"/>`);

// ── title ─────────────────────────────────────────────────────────────────
svg.push(`<text x="${ML}" y="${TITLE_Y}" class="ywft" font-size="42" fill="${INK}">momabobasheep — graphic score</text>`);
svg.push(`<text x="${ML}" y="${TITLE_Y + 28}" class="mono" font-size="16" fill="${INK}" opacity="0.72">10:00 generative sleep mix · one night in nine fibonacci chapters · F major home · seed “momabobasheep” · ${D.totalBars} bars of ${D.bar}s</text>`);

// ── movement bands ────────────────────────────────────────────────────────
for (let i = 0; i < D.movements.length; i++) {
  const m = D.movements[i];
  const x0 = x(m.startSec), x1 = x(i === D.movements.length - 1 ? T : m.endSec);
  const a = 0.10 + 0.42 * m.level;
  svg.push(`<rect x="${x0.toFixed(1)}" y="${BAND_Y}" width="${(x1 - x0).toFixed(1)}" height="${BAND_H}" fill="${C.band}" opacity="${a.toFixed(2)}"/>`);
  // movement separators through the whole plot
  if (i > 0) svg.push(`<line x1="${x0.toFixed(1)}" y1="${BAND_Y}" x2="${x0.toFixed(1)}" y2="${AXIS_Y - 18}" stroke="${FAINT}" stroke-width="1"/>`);
  const cx = (x0 + x1) / 2, wpx = x1 - x0;
  const big = wpx > 120;
  svg.push(`<text x="${cx.toFixed(1)}" y="${BAND_Y + (big ? 30 : 26)}" text-anchor="middle" class="ywft" font-size="${big ? 23 : 15}" fill="${m.level > 0.6 ? CREAM : INK}">${esc(m.name)}</text>`);
  if (big) {
    svg.push(`<text x="${cx.toFixed(1)}" y="${BAND_Y + 50}" text-anchor="middle" class="mono" font-size="13" fill="${m.level > 0.6 ? CREAM : INK}" opacity="0.85">${esc(m.sub)} · ${m.bars} bars</text>`);
    svg.push(`<text x="${cx.toFixed(1)}" y="${BAND_Y + 66}" text-anchor="middle" class="mono" font-size="11.5" fill="${m.level > 0.6 ? CREAM : INK}" opacity="0.7">${esc(m.prog.join(" "))}</text>`);
  } else {
    svg.push(`<text x="${cx.toFixed(1)}" y="${BAND_Y + 44}" text-anchor="middle" class="mono" font-size="11" fill="${INK}" opacity="0.8">${m.bars}</text>`);
  }
}

// ── chord lane (merged runs of the same chord) ────────────────────────────
svg.push(`<text x="${ML - 14}" y="${CHORD_Y + CHORD_H / 2 + 4}" text-anchor="end" class="mono" font-size="15" fill="${INK}">chord walk</text>`);
{
  const runs = [];
  for (const c of D.chords) {
    const last = runs[runs.length - 1];
    if (last && last.name === c.name && Math.abs(last.t + last.dur - c.t) < 1e-6) last.dur += c.dur;
    else runs.push({ name: c.name, t: c.t, dur: c.dur });
  }
  for (const r of runs) {
    const x0 = x(r.t), wpx = x(r.t + r.dur) - x0;
    svg.push(`<rect x="${x0.toFixed(1)}" y="${CHORD_Y}" width="${(wpx - 1).toFixed(1)}" height="${CHORD_H}" fill="${chordFill(r.name)}" stroke="rgba(30,30,30,0.18)" stroke-width="0.5"/>`);
    if (wpx >= 13.5) svg.push(`<text x="${(x0 + wpx / 2).toFixed(1)}" y="${CHORD_Y + CHORD_H / 2 + 4.5}" text-anchor="middle" class="mono" font-size="${wpx > 24 ? 13 : 10.5}" fill="${INK}">${esc(r.name)}</text>`);
  }
  // pad-only tail to the seam (F home)
  const tail0 = x(D.chords[D.chords.length - 1].t + D.chords[D.chords.length - 1].dur);
  svg.push(`<rect x="${tail0.toFixed(1)}" y="${CHORD_Y}" width="${(x(T) - tail0 - 1).toFixed(1)}" height="${CHORD_H}" fill="${chordFill("F")}" opacity="0.45" stroke="rgba(30,30,30,0.18)" stroke-width="0.5" stroke-dasharray="3 3"/>`);
  svg.push(`<text x="${((tail0 + x(T)) / 2).toFixed(1)}" y="${CHORD_Y + CHORD_H / 2 + 4.5}" text-anchor="middle" class="mono" font-size="10.5" fill="${INK}" opacity="0.7">F…</text>`);
}

// ── lane scaffolding ──────────────────────────────────────────────────────
for (const l of LANES) {
  svg.push(`<text x="${ML - 14}" y="${l.y + l.h / 2 + 5}" text-anchor="end" class="mono" font-size="15" fill="${INK}">${esc(l.label)}</text>`);
  svg.push(`<line x1="${ML}" y1="${l.y + l.h}" x2="${ML + PW}" y2="${l.y + l.h}" stroke="${FAINTER}" stroke-width="1"/>`);
}

const pitchY = (lane, midi, lo, hi, pad = 6) =>
  lane.y + lane.h - pad - ((midi - lo) / Math.max(1, hi - lo)) * (lane.h - pad * 2);
const minmax = (evs) => {
  let lo = 1e9, hi = -1e9;
  for (const e of evs) { if (e.midi < lo) lo = e.midi; if (e.midi > hi) hi = e.midi; }
  return [lo, hi];
};

// whistle — line segments, pitch-placed, length = dur, weight = gain
{
  const L = LANES[0], [lo, hi] = minmax(D.whistle);
  for (const e of D.whistle) {
    const y = pitchY(L, e.midi, lo, hi);
    svg.push(`<rect x="${x(e.t).toFixed(1)}" y="${(y - 2).toFixed(1)}" width="${Math.max(2, (e.dur / T) * PW).toFixed(1)}" height="4" rx="2" fill="${C.whistle}" opacity="${(0.35 + e.g * 3).toFixed(2)}"/>`);
  }
}

// reverso bells — swell wedges rising to the chord-arrival peak, then melting
{
  const L = LANES[1];
  for (const b of D.bells) {
    const x0 = x(b.t), xp = x(b.peakT), x1 = x(b.t + b.dur);
    const hgt = (b.gain / 0.18) * (L.h - 14);
    const yb = L.y + L.h - 4;
    svg.push(`<path d="M ${x0.toFixed(1)} ${yb} Q ${((x0 + xp) / 2).toFixed(1)} ${yb}, ${xp.toFixed(1)} ${(yb - hgt).toFixed(1)} Q ${((xp + x1) / 2).toFixed(1)} ${(yb - hgt * 0.4).toFixed(1)}, ${x1.toFixed(1)} ${yb} Z" fill="${C.bells}" opacity="0.42"/>`);
    svg.push(`<line x1="${xp.toFixed(1)}" y1="${(yb - hgt).toFixed(1)}" x2="${xp.toFixed(1)}" y2="${yb}" stroke="${C.bells}" stroke-width="1" opacity="0.5"/>`);
  }
}

// kalimba sparkle — small diamonds on the euclidean onsets
{
  const L = LANES[2], [lo, hi] = minmax(D.sparkle);
  for (const e of D.sparkle) {
    const cx = x(e.t), cy = pitchY(L, e.midi, lo, hi, 10), r = 3 + e.g * 40;
    svg.push(`<path d="M ${cx.toFixed(1)} ${(cy - r).toFixed(1)} L ${(cx + r).toFixed(1)} ${cy.toFixed(1)} L ${cx.toFixed(1)} ${(cy + r).toFixed(1)} L ${(cx - r).toFixed(1)} ${cy.toFixed(1)} Z" fill="${C.sparkle}" opacity="0.8"/>`);
  }
}

// arpeggio — dense pitch-placed ticks, height/opacity by gain
{
  const L = LANES[3], [lo, hi] = minmax(D.arp);
  for (const e of D.arp) {
    const y = pitchY(L, e.midi, lo, hi, 4);
    const hgt = 5 + e.g * 30;
    svg.push(`<rect x="${x(e.t).toFixed(1)}" y="${(y - hgt / 2).toFixed(1)}" width="1.8" height="${hgt.toFixed(1)}" fill="${C.arp}" opacity="${(0.30 + e.g * 1.8).toFixed(2)}"/>`);
  }
}

// alto — short held dashes (dream chapters only)
{
  const L = LANES[4], [lo, hi] = minmax(D.alto);
  for (const e of D.alto) {
    const y = pitchY(L, e.midi, lo, hi, 8);
    svg.push(`<rect x="${x(e.t).toFixed(1)}" y="${(y - 2.5).toFixed(1)}" width="${((e.dur / T) * PW).toFixed(1)}" height="5" rx="2.5" fill="${C.alto}" opacity="0.7"/>`);
  }
}

// pad — translucent held blooms (always on)
{
  const L = LANES[5], [lo, hi] = minmax(D.pad);
  for (const e of D.pad) {
    const y = pitchY(L, e.midi, lo, hi, 7);
    svg.push(`<rect x="${x(e.t).toFixed(1)}" y="${(y - 4).toFixed(1)}" width="${((Math.min(e.dur, T - e.t) / T) * PW).toFixed(1)}" height="8" rx="4" fill="${C.pad}" opacity="${(e.g * 3.4).toFixed(2)}"/>`);
  }
}

// walking bass — root/approach ticks from the lane floor
{
  const L = LANES[6], [lo, hi] = minmax(D.bass);
  for (const e of D.bass) {
    const y = pitchY(L, e.midi, lo, hi, 6);
    const hgt = 8 + e.g * 60;
    svg.push(`<rect x="${x(e.t).toFixed(1)}" y="${(y - hgt / 2).toFixed(1)}" width="2.4" height="${hgt.toFixed(1)}" fill="${C.bass}" opacity="${(0.35 + e.g * 1.6).toFixed(2)}"/>`);
  }
}

// drone bed + sub — continuous ribbons, thickness = envelope × dynamic arc
{
  const L = LANES[7];
  const mid = L.y + L.h * 0.42, midSub = L.y + L.h * 0.80;
  const top = [], bot = [], stop = [], sbot = [];
  for (let t = 0; t <= T; t += 2) {
    const e = D.droneEnv[Math.min(t, D.droneEnv.length - 1)];
    const a = D.arc[Math.min(t, D.arc.length - 1)];
    const th = (3 + 30 * e * (0.45 + 0.55 * a)) / 2;
    const ts = (1.5 + 12 * e * (0.45 + 0.55 * a)) / 2;
    top.push(`${x(t).toFixed(1)},${(mid - th).toFixed(1)}`);
    bot.push(`${x(t).toFixed(1)},${(mid + th).toFixed(1)}`);
    stop.push(`${x(t).toFixed(1)},${(midSub - ts).toFixed(1)}`);
    sbot.push(`${x(t).toFixed(1)},${(midSub + ts).toFixed(1)}`);
  }
  svg.push(`<polygon points="${top.join(" ")} ${bot.reverse().join(" ")}" fill="${C.drone}" opacity="0.55"/>`);
  svg.push(`<polygon points="${stop.join(" ")} ${sbot.reverse().join(" ")}" fill="${C.sub}" opacity="0.6"/>`);
  svg.push(`<text x="${ML + 8}" y="${midSub + 4}" class="mono" font-size="10.5" fill="${CREAM}" opacity="0.9">sub</text>`);
}

// ── dynamic arc ───────────────────────────────────────────────────────────
{
  svg.push(`<text x="${ML - 14}" y="${ARC_Y + ARC_H / 2 + 5}" text-anchor="end" class="mono" font-size="15" fill="${INK}">dynamic arc</text>`);
  const y0 = ARC_Y + ARC_H - 6;
  const pts = [];
  for (let t = 0; t <= T; t += 2) {
    const a = D.arc[Math.min(t, D.arc.length - 1)];
    pts.push(`${x(t).toFixed(1)},${(y0 - a * (ARC_H - 18)).toFixed(1)}`);
  }
  svg.push(`<polygon points="${x(0).toFixed(1)},${y0} ${pts.join(" ")} ${x(T).toFixed(1)},${y0}" fill="${C.band}" opacity="0.14"/>`);
  svg.push(`<polyline points="${pts.join(" ")}" fill="none" stroke="${INK}" stroke-width="2"/>`);
  svg.push(`<line x1="${ML}" y1="${y0}" x2="${ML + PW}" y2="${y0}" stroke="${FAINT}" stroke-width="1"/>`);
  // movement level dots
  for (const m of D.movements) {
    const cx = x((m.startSec + m.endSec) / 2), cy = y0 - m.level * (ARC_H - 18);
    svg.push(`<circle cx="${cx.toFixed(1)}" cy="${cy.toFixed(1)}" r="3.4" fill="${C.band}"/>`);
    svg.push(`<text x="${cx.toFixed(1)}" y="${(cy - 8).toFixed(1)}" text-anchor="middle" class="mono" font-size="10.5" fill="${INK}" opacity="0.7">${m.level.toFixed(2)}</text>`);
  }
}

// ── golden-section climax marker (full height) ────────────────────────────
{
  const gx = x(D.goldenSec);
  svg.push(`<line x1="${gx.toFixed(1)}" y1="${BAND_Y - 8}" x2="${gx.toFixed(1)}" y2="${AXIS_Y - 18}" stroke="${C.golden}" stroke-width="1.6" stroke-dasharray="7 5"/>`);
  const mm = Math.floor(D.goldenSec / 60), ss = Math.round(D.goldenSec % 60);
  svg.push(`<text x="${(gx + 9).toFixed(1)}" y="${BAND_Y - 14}" class="mono" font-size="14" fill="${C.golden}">golden section · ${mm}:${String(ss).padStart(2, "0")} · the dream crests</text>`);
}

// ── loop-seam note ────────────────────────────────────────────────────────
svg.push(`<text x="${x(T).toFixed(1)}" y="${BAND_Y - 14}" text-anchor="end" class="mono" font-size="13" fill="${INK}" opacity="0.6">10:00 meets 0:00 — the same F haze (loop seam)</text>`);

// ── time axis ─────────────────────────────────────────────────────────────
{
  svg.push(`<line x1="${ML}" y1="${AXIS_Y - 18}" x2="${ML + PW}" y2="${AXIS_Y - 18}" stroke="${INK}" stroke-width="1.4"/>`);
  for (let s = 0; s <= T; s += 30) {
    const tx = x(s), minor = s % 60 !== 0;
    svg.push(`<line x1="${tx.toFixed(1)}" y1="${AXIS_Y - 18}" x2="${tx.toFixed(1)}" y2="${AXIS_Y - 18 + (minor ? 7 : 12)}" stroke="${INK}" stroke-width="${minor ? 1 : 1.4}"/>`);
    if (!minor) svg.push(`<text x="${tx.toFixed(1)}" y="${AXIS_Y + 16}" text-anchor="middle" class="mono" font-size="15" fill="${INK}">${s / 60}:00</text>`);
  }
}

// ── html shell ────────────────────────────────────────────────────────────
const ywftPath = resolve(homedir(), "Library", "Fonts", "ywft-processing-bold.ttf");
const ywftFace = existsSync(ywftPath)
  ? `@font-face { font-family: "YWFT"; src: url(data:font/ttf;base64,${readFileSync(ywftPath).toString("base64")}) format("truetype"); }`
  : "";
const html = `<!doctype html><html><head><meta charset="utf-8"><style>
${ywftFace}
html, body { margin: 0; padding: 0; background: ${CREAM}; }
.ywft { font-family: "YWFT", "Menlo", monospace; }
.mono { font-family: "Menlo", "SF Mono", monospace; }
</style></head><body>
<svg width="${W}" height="${H}" viewBox="0 0 ${W} ${H}" xmlns="http://www.w3.org/2000/svg">
${svg.join("\n")}
</svg>
</body></html>`;

const outPath = resolve(OUT, "momabobasheep-score.html");
writeFileSync(outPath, html);
console.log(`✓ score page → ${outPath} (${(html.length / 1024).toFixed(0)} KB)`);
