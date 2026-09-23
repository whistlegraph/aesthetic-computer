#!/usr/bin/env node
// wordmeta.mjs — the metadata layer for a spoken stem that is about to be
// sung: WHERE every word is (whisper, guided by the lyric), WHAT it is made
// of (IPA phonemes, syllabified, vowel / consonant classes), and WHERE the
// vowels actually are in the audio (measured nuclei, one per syllable).
//
//   node spinging/lib/wordmeta.mjs <stem.wav|mp3> --lyrics "<tokens>" [--witness alignment.json] [--out meta.json] [--allow-fuzzy]
//
// lyrics: one token per note, syllables of a word joined by "-" (the lane's
// grammar). The lyric is the truth; whisper is the witness that places it.
//
// Correctness gate (the 99.99% rule): every lyric word must be matched to a
// whisper word EXACTLY (after normalisation). A fuzzy match or a fallback
// window fails the run (exit 2) unless --allow-fuzzy. A second witness
// (the say endpoint's per-character alignment, when the stem came from
// ElevenLabs) is compared and the disagreement per word is reported.
//
// Output: { stem, fs, durationMs, lyrics, report, words: [ { text, fromMs,
//   toMs, whisper:{fromMs,toMs}, witness?:{fromMs,toMs,deltaMs}, ipa,
//   ipaSource, syllables: [ { text, onset:[{ipa,cls,voiced}], nucleus:[…],
//   coda:[…], fromMs, toMs, nucleusFromMs, nucleusToMs, measured } ] } ] }

import { execFileSync } from "node:child_process";
const spawnSyncSafe = (cmd, args) => { try { return execFileSync(cmd, args, { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }).trim(); } catch { return ""; } };
import { existsSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { alignWords } from "./align-words.mjs";
import { pronounce, phoneClass } from "./pronounce.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");
const MODEL = resolve(REPO, "recap/models/ggml-base.en.bin");
const PY = resolve(REPO, "pop/.venv/bin/python");
const NUCLEI = resolve(HERE, "nuclei.py");

// ---- args -------------------------------------------------------------------
const argv = process.argv.slice(2);
const flags = {};
const words0 = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) { const k = a.slice(2); const n = argv[i + 1]; if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true; }
  else words0.push(a);
}
const stem = words0[0] && resolve(words0[0]);
if (!stem || !flags.lyrics) {
  console.error("usage: wordmeta.mjs <stem> --lyrics \"<tokens>\" [--witness alignment.json] [--out meta.json] [--allow-fuzzy]");
  process.exit(1);
}
if (!existsSync(MODEL)) { console.error(`✗ whisper model missing: ${MODEL}`); process.exit(1); }

const tokens = String(flags.lyrics).trim().split(/\s+/);
const lyricWords = tokens.map((t) => t.split("-").join(""));
const lyricSyl = tokens.map((t) => t.split("-"));
const prompt = lyricWords.join(" ");

// ---- 1. whisper — two passes ---------------------------------------------------
// Pass A, unprompted: honest timestamps, but numerals come back as digits
// ("two hundred twelve" → "212"), so digits are expanded back into words
// that share the numeral's span. Pass B, prompted with the lyric: whisper
// hears the exact words, but on short clips its timestamps collapse (every
// word at one instant, the last running to 30 s) — so B is only used when A
// fails identity, and its timing only when it is valid. The audio's real
// length referees.
const tmp = mkdtempSync(resolve(tmpdir(), "wordmeta-"));
const probe = spawnSyncSafe("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", stem]);
const durationMs = Math.round(parseFloat(probe || "0") * 1000);
const timingValid = (ws) => ws.length > 0 && ws.every((w) => w.fromMs <= durationMs * 1.05 && w.toMs <= durationMs * 1.15);

function runWhisper(tag, extra) {
  const outBase = resolve(tmp, tag);
  execFileSync("whisper-cli", ["-m", MODEL, "-f", stem, "-ojf", "-of", outBase, "--max-len", "1", "-ml", "1", "-sow", ...extra],
    { stdio: ["ignore", "ignore", "ignore"] });
  const raw = JSON.parse(readFileSync(`${outBase}.json`, "utf8"));
  return raw.transcription.map((s) => ({ text: s.text.trim(), fromMs: s.offsets.from, toMs: s.offsets.to })).filter((w) => w.text);
}

// digits → words, sharing the numeral's span evenly
const ONES = ["zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"];
const TENS = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"];
const ORD = { one:"first", two:"second", three:"third", five:"fifth", eight:"eighth", nine:"ninth", twelve:"twelfth" };
function numWords(n) {
  if (n < 20) return [ONES[n]];
  if (n < 100) return n % 10 ? [TENS[Math.floor(n / 10)], ONES[n % 10]] : [TENS[Math.floor(n / 10)]];
  if (n < 1000) return [ONES[Math.floor(n / 100)], "hundred", ...(n % 100 ? numWords(n % 100) : [])];
  return [String(n)];
}
function expandNumerals(ws) {
  const out = [];
  for (const w of ws) {
    const m = w.text.match(/^(\d+)(st|nd|rd|th)?[.,;:!?]*$/);
    if (!m) { out.push(w); continue; }
    let parts = numWords(parseInt(m[1], 10));
    if (m[2]) { const last = parts[parts.length - 1]; parts[parts.length - 1] = ORD[last] || (last.endsWith("y") ? last.slice(0, -1) + "ieth" : last + "th"); }
    const span = (w.toMs - w.fromMs) / parts.length;
    parts.forEach((t, k) => out.push({ text: t, fromMs: Math.round(w.fromMs + k * span), toMs: Math.round(w.fromMs + (k + 1) * span), expanded: true }));
  }
  return out;
}

const normW = (s) => String(s).toLowerCase().replace(/[^a-z0-9']/g, "");
const identityOK = (ws) => {
  const al = alignWords(lyricWords, ws);
  return lyricWords.every((lw, i) => ws.some((w) => w.fromMs === al[i]?.fromMs && normW(w.text) === normW(lw)));
};

let whisper = expandNumerals(runWhisper("a", []));
let whisperPass = "unprompted";
if (!identityOK(whisper)) {
  const b = runWhisper("b", ["--prompt", lyricWords.join(" ")]);
  if (identityOK(b)) { whisper = b; whisperPass = "prompted"; }
}
const whisperTimingValid = timingValid(whisper);

// ---- 2. reconcile against the lyric (the lyric is the truth) ------------------
const norm = normW;
const aligned = alignWords(lyricWords, whisper);   // one window per lyric word
// Exact = some whisper word STARTING at this window's onset normalises to the
// lyric word (whisper gives consecutive short words identical onsets, so a
// lookup keyed on onset alone would collide — "I am" → "am").
let exact = 0, inexact = 0, fallback = 0;
const wordsOut = lyricWords.map((lw, i) => {
  const a = aligned[i] || {};
  const cands = whisper.filter((w) => w.fromMs === a.fromMs);
  const hit = cands.find((w) => norm(w.text) === norm(lw));
  let kind = "fallback";
  if (hit) kind = "exact";
  else if (a.fromMs != null && a.toMs != null) kind = "inexact";
  if (kind === "exact") exact++; else if (kind === "inexact") inexact++; else fallback++;
  return { text: lw, fromMs: a.fromMs ?? 0, toMs: a.toMs ?? 0,
    whisper: { fromMs: a.fromMs ?? null, toMs: a.toMs ?? null, heard: (hit || cands[0])?.text ?? null, kind } };
});
// Whisper's word boundaries are coarse: runs of short words share one onset
// and zero-width windows. Spread such a run across [onset, next distinct
// onset) proportionally to syllable count so every word owns some time.
for (let i = 0; i < wordsOut.length;) {
  let j = i; while (j + 1 < wordsOut.length && wordsOut[j + 1].fromMs === wordsOut[i].fromMs) j++;
  if (j > i) {
    const start = wordsOut[i].fromMs;
    const end = j + 1 < wordsOut.length ? wordsOut[j + 1].fromMs : Math.max(wordsOut[j].toMs, start + 200 * (j - i + 1));
    const weights = []; for (let k = i; k <= j; k++) weights.push(lyricSyl[k].length);
    const tot = weights.reduce((x, y) => x + y, 0);
    let cur = start;
    for (let k = i; k <= j; k++) { const len = (end - start) * weights[k - i] / tot; wordsOut[k].fromMs = Math.round(cur); wordsOut[k].toMs = Math.round(cur + len); cur += len; }
  }
  i = j + 1;
}

// ---- 3. second witness → timing fusion ------------------------------------------
// When the stem came with an exact per-character alignment (the say endpoint's
// ElevenLabs route), THAT gives the word windows; whisper's job is to prove
// the words are the ones we think (identity), not to place them. Without a
// witness, whisper's windows stand. --timing whisper|witness|fuse (default fuse).
const timing = flags.timing || "fuse";
let witnessAgree = null;
if (flags.witness && existsSync(flags.witness)) {
  const doc = JSON.parse(readFileSync(flags.witness, "utf8"));
  const ww = (doc.words || doc).map((w) => ({ text: w.text, fromMs: w.fromMs, toMs: w.toMs }));
  if (ww.length === wordsOut.length) {
    let agree = 0; const deltas = [];
    wordsOut.forEach((w, i) => {
      const d = (w.fromMs - ww[i].fromMs);
      w.witness = { fromMs: ww[i].fromMs, toMs: ww[i].toMs, deltaMs: d, text: ww[i].text };
      deltas.push(Math.abs(d));
      if (Math.abs(d) <= 120) agree++;
    });
    const sorted = [...deltas].sort((a, b) => a - b);
    const median = sorted[Math.floor(sorted.length / 2)];
    // Which timing to believe: the witness unless it disagrees with a VALID
    // whisper by more than a syllable (then it is a stale/mismatched
    // alignment). When whisper's own timestamps run past the audio (it does
    // this on short clips), whisper only proves identity and the witness
    // places the words.
    const witnessValid = ww.every((w) => w.fromMs <= durationMs * 1.05 && w.toMs <= durationMs * 1.15);
    const trust = timing !== "whisper" && witnessValid && (!whisperTimingValid || (median <= 300 && agree >= Math.ceil(wordsOut.length * 0.4)));
    if (trust) wordsOut.forEach((w, i) => { w.fromMs = ww[i].fromMs; w.toMs = ww[i].toMs; });
    witnessAgree = { within120ms: agree, of: wordsOut.length, medianAbsDeltaMs: median, timingFrom: trust ? "witness" : "whisper", distrusted: !trust && timing !== "whisper", whisperTimingValid, witnessValid };
  } else {
    witnessAgree = { error: `witness has ${ww.length} words, lyric has ${wordsOut.length}`, timingFrom: "whisper" };
  }
}
// A word's window ends no later than the next word begins (the witness's
// per-word ends can leave gaps; the gap belongs to the coda).
for (let i = 0; i + 1 < wordsOut.length; i++) if (wordsOut[i].toMs > wordsOut[i + 1].fromMs) wordsOut[i].toMs = wordsOut[i + 1].fromMs;

// ---- 4. phonemes: IPA → syllables → onset / nucleus / coda ------------------------
for (let i = 0; i < wordsOut.length; i++) {
  const w = wordsOut[i];
  const pr = await pronounce(w.text);
  w.ipa = pr.ipa; w.ipaSource = pr.source;
  const want = lyricSyl[i].length;
  // pronounce() syllabifies from the IPA; reconcile its count with the
  // lyric's: merge the weakest or split the longest until they agree.
  // pronounce() gives structured syllables {onset[], nucleus, coda[]}; flatten
  // to phone lists (already classed) so the count can be reconciled below.
  const asPhone = (p) => (typeof p === "string" ? { ipa: p, ...phoneClass(p) } : p);
  let sylls = (pr.syllables || []).map((sy) => sy.phones
    ? sy.phones.map(asPhone)
    : [...(sy.onset || []).map(asPhone), ...(sy.nucleus ? [asPhone(sy.nucleus)] : []), ...(sy.coda || []).map(asPhone)]);
  while (sylls.length > want && sylls.length > 1) {          // merge the shortest into its neighbour
    let k = 0; for (let j = 1; j < sylls.length; j++) if (sylls[j].length < sylls[k].length) k = j;
    const into = k === 0 ? 1 : k - 1;
    sylls[into] = into < k ? [...sylls[into], ...sylls[k]] : [...sylls[k], ...sylls[into]];
    sylls.splice(k, 1);
  }
  while (sylls.length < want) {                              // split the longest at its first vowel boundary
    let k = 0; for (let j = 1; j < sylls.length; j++) if (sylls[j].length > sylls[k].length) k = j;
    const s = sylls[k]; const vi = s.findIndex((p) => p.cls === "vowel");
    const cut = vi >= 0 && vi + 1 < s.length ? vi + 1 : Math.ceil(s.length / 2);
    sylls.splice(k, 1, s.slice(0, cut), s.slice(cut));
    if (sylls.some((x) => !x.length)) { sylls = sylls.filter((x) => x.length); break; }
  }
  w.syllables = sylls.map((phones, si) => {
    const vi = phones.findIndex((p) => p.cls === "vowel");
    let vj = vi; while (vj + 1 < phones.length && phones[vj + 1].cls === "vowel") vj++;
    return {
      text: lyricSyl[i][si] ?? lyricSyl[i][lyricSyl[i].length - 1],
      onset: vi < 0 ? phones : phones.slice(0, vi),
      nucleus: vi < 0 ? [] : phones.slice(vi, vj + 1),
      coda: vi < 0 ? [] : phones.slice(vj + 1),
    };
  });
}

// ---- 5. measured nuclei in the audio (one per syllable) ----------------------------
const req = wordsOut.map((w) => ({ fromMs: w.fromMs, toMs: w.toMs, nsyl: w.syllables.length }));
const reqPath = resolve(tmp, "windows.json");
writeFileSync(reqPath, JSON.stringify(req));
const measured = JSON.parse(execFileSync(PY, [NUCLEI, stem, reqPath], { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }));
let nucleiFound = 0, nucleiWanted = 0;
wordsOut.forEach((w, i) => {
  const m = measured.words[i];
  w.syllables.forEach((s, si) => {
    const span = m.syllables[si];
    nucleiWanted++;
    if (span) {
      s.fromMs = span.fromMs; s.toMs = span.toMs;
      s.nucleusFromMs = span.nucleusFromMs; s.nucleusToMs = span.nucleusToMs; s.measured = span.measured;
      if (span.measured) nucleiFound++;
    }
  });
});

// ---- report + gate ----------------------------------------------------------------
const report = {
  lyricWords: lyricWords.length, whisperWords: whisper.length,
  exact, inexact, fallback, coverage: exact / lyricWords.length,
  witness: witnessAgree,
  nuclei: { measured: nucleiFound, wanted: nucleiWanted },
  ipaSources: wordsOut.reduce((a, w) => { a[w.ipaSource] = (a[w.ipaSource] || 0) + 1; return a; }, {}),
};
const meta = { stem, fs: measured.fs, durationMs, lyrics: flags.lyrics, report, words: wordsOut };
const outPath = flags.out ? resolve(flags.out) : stem.replace(/\.[^.]+$/, "") + ".meta.json";
writeFileSync(outPath, JSON.stringify(meta, null, 2));

report.whisperTimingValid = whisperTimingValid;
report.whisperPass = whisperPass;
report.audioMs = durationMs;
const pct = (x) => `${(x * 100).toFixed(2)}%`;
console.log(`♪ wordmeta · ${basename(stem)} · ${lyricWords.length} lyric words · whisper (${whisperPass}) heard ${whisper.length} · audio ${durationMs} ms`);
console.log(`  words placed: ${exact} exact · ${inexact} inexact · ${fallback} fallback → coverage ${pct(report.coverage)}`);
if (witnessAgree && !witnessAgree.error) console.log(`  second witness: ${witnessAgree.within120ms}/${witnessAgree.of} onsets within 120 ms · median |Δ| ${witnessAgree.medianAbsDeltaMs} ms · timing from ${witnessAgree.timingFrom}${witnessAgree.distrusted ? "  ⚠ witness DISTRUSTED (does not describe this audio)" : ""}`);
if (witnessAgree?.error) console.log(`  second witness: ${witnessAgree.error}`);
console.log(`  nuclei measured: ${nucleiFound}/${nucleiWanted} · IPA from ${Object.entries(report.ipaSources).map(([k, v]) => `${k}×${v}`).join(", ")}`);
for (const w of wordsOut) if (w.whisper.kind !== "exact") console.log(`    ⚠ "${w.text}" ${w.whisper.kind}${w.whisper.heard ? ` (heard "${w.whisper.heard}")` : ""} @${w.fromMs} ms`);
console.log(`  → ${outPath}`);
if (!whisperTimingValid) console.log(`  ⚠ whisper timestamps run past the audio (${durationMs} ms) — whisper used for identity only`);
if (!whisperTimingValid && !(witnessAgree && witnessAgree.timingFrom === "witness") && !flags["allow-fuzzy"]) { console.log("  ✗ no trustworthy word timing (whisper invalid, no valid witness) — refusing"); process.exit(3); }
if ((inexact || fallback) && !flags["allow-fuzzy"]) { console.log("  ✗ not every word placed exactly — refusing (pass --allow-fuzzy to override)"); process.exit(2); }
