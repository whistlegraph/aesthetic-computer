// keystroke-timing.mjs — how a message was typed, carried with what it says.
//
// A chat interface hands the model text and throws the typing away: a reply
// typed in four seconds and the same words typed over four minutes, with two
// long stops and a deleted sentence, arrive identical. Keystroke research has
// treated that timing as signal for forty years — interstroke intervals in
// skilled typing (Gentner 1983; Salthouse 1984), 136 million keystrokes of
// modern typing (Dhakal et al., CHI 2018), the 2-second pause that splits
// writing into bursts (Chenoweth & Hayes 2001; Inputlog) — and Vyping
// (Encinas & Jensenius, NIME 2026) names the stripped timing a design gap in
// LLM interfaces. This module closes it for Aesel.
//
// What is kept: when keys arrived, how many characters each produced, and
// erases. Never which key — the message text already goes to the model, and
// the deleted text must not. A terminal reports presses only, so there is no
// hold (dwell) time; the intervals here are press-to-press (DD), the measure
// Dhakal and Inputlog report as IKI.
//
// The TUI records (the clock); every engine accepts the summary as
// `startTurn(text, {cadence})` / `steer(text, {cadence})` and appends it with
// `withCadence`, so a caller other than the TUI can send timing too.

export const PAUSE_MS = 2000;      // P-burst boundary in writing research
export const IDLE_CAP_MS = 120000; // a gap this long is "away", not a pause
const SAME_CHUNK_MS = 3;           // keys this close came in one stdin read
const MAX_GAPS = 96;
const MAX_PAUSES = 8;

export class KeystrokeClock {
  constructor({ now = () => Date.now() } = {}) { this.now = now; this.reset(); this.readyAt = null; }
  reset() { this.events = []; }
  // The moment the person could start typing: a reply finished, or the prompt opened.
  ready(at = this.now()) { this.readyAt = at; }
  type(characters = 1, at = this.now()) {
    const count = Math.max(0, Math.trunc(characters));
    if (!count) return;
    const last = this.events.at(-1);
    // One read carrying several characters is a paste (or a bracketed paste
    // split into tokens), not a run of impossibly fast keystrokes.
    if (last && last.kind !== 'erase' && at - last.at < SAME_CHUNK_MS) { last.chars += count; last.kind = 'paste'; return; }
    this.events.push({ at, chars: count, kind: count > 1 ? 'paste' : 'key' });
  }
  erase(at = this.now()) { this.events.push({ at, chars: 0, kind: 'erase' }); }
  // Hand over what was typed since the last take and start fresh.
  take() {
    const summary = summarizeCadence(this.events, { readyAt: this.readyAt });
    this.reset(); this.readyAt = null;
    return summary;
  }
}

const round = (ms) => Math.round(ms / 10) * 10;
function quantile(sorted, q) {
  if (!sorted.length) return null;
  const at = (sorted.length - 1) * q, lo = Math.floor(at), hi = Math.ceil(at);
  return sorted[lo] + (sorted[hi] - sorted[lo]) * (at - lo);
}

export function summarizeCadence(events, { readyAt = null } = {}) {
  if (!events?.length) return null;
  const keys = events.filter(e => e.kind === 'key').length;
  const pasted = events.filter(e => e.kind === 'paste').reduce((n, e) => n + e.chars, 0);
  const erased = events.filter(e => e.kind === 'erase').length;
  const typed = events.reduce((n, e) => n + e.chars, 0);
  const gaps = [], pauses = [];
  let away = 0, produced = events[0].chars, burst = events[0].chars;
  const bursts = [];
  for (let i = 1; i < events.length; i++) {
    const gap = events[i].at - events[i - 1].at;
    if (gap >= IDLE_CAP_MS) { away++; bursts.push(burst); burst = 0; }
    else if (gap >= PAUSE_MS) { pauses.push({ afterChars: produced, ms: round(gap) }); bursts.push(burst); burst = 0; }
    else if (events[i].kind === 'key' && events[i - 1].kind === 'key') gaps.push(gap);
    produced += events[i].chars; burst += events[i].chars;
  }
  bursts.push(burst);
  const sorted = [...gaps].sort((a, b) => a - b);
  const logs = gaps.filter(g => g > 0).map(Math.log);
  const mean = logs.reduce((a, b) => a + b, 0) / (logs.length || 1);
  const logSd = logs.length > 1 ? Math.sqrt(logs.reduce((a, b) => a + (b - mean) ** 2, 0) / (logs.length - 1)) : null;
  const longest = [...pauses].sort((a, b) => b.ms - a.ms).slice(0, MAX_PAUSES).sort((a, b) => a.afterChars - b.afterChars);
  const summary = {
    keys, typedChars: typed, erases: erased, pastedChars: pasted,
    spanMs: round(events.at(-1).at - events[0].at),
    ...(readyAt != null && events[0].at >= readyAt ? { firstKeyMs: round(events[0].at - readyAt) } : {}),
    ...(sorted.length ? { ikiMs: { p10: round(quantile(sorted, .1)), median: round(quantile(sorted, .5)), p90: round(quantile(sorted, .9)) } } : {}),
    ...(logSd != null ? { ikiLogSd: Math.round(logSd * 100) / 100 } : {}),
    pauses: pauses.length, longestPauses: longest,
    bursts: bursts.filter(Boolean).length,
    ...(away ? { awayGaps: away } : {}),
    // The rhythm itself, newest last, in 10 ms steps: the inter-onset intervals
    // a musician would read. Pauses and paste boundaries are left out.
    gapsMs: gaps.slice(-MAX_GAPS).map(round),
  };
  return summary;
}

export function cadenceContext(cadence) {
  const list = (Array.isArray(cadence) ? cadence : [cadence]).filter(Boolean);
  if (!list.length) return '';
  return '\n\n[Typing cadence — how the person typed this message, measured by the harness from key timing only. No key identities or deleted text were recorded. '
    + `Intervals are press-to-press in ms; a pause is a gap of ${PAUSE_MS / 1000} s or more, placed after the Nth character produced. `
    + 'Treat it as weak context, like tone of voice: it can say a message was hurried, pasted, hesitated over or heavily revised, never why. Do not diagnose mood from it or remark on it unless it bears on the reply or you are asked.]\n'
    + JSON.stringify(list.length === 1 ? list[0] : { messages: list });
}

export function withCadence(text, cadence) {
  return String(text ?? '') + cadenceContext(cadence);
}

// AESEL_CADENCE=off keeps timing out of every prompt for the session.
export function cadenceEnabled(env = process.env) {
  return !/^(0|off|false|no)$/i.test(String(env.AESEL_CADENCE ?? ''));
}
