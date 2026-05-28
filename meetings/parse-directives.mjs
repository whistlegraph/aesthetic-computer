#!/usr/bin/env node
// parse-directives.mjs — whistlepops.json + transcript.json → directives.json
//
// Implements the two-pass grammar from meetings/dsl.md:
//   1. Pair pass — group whistlepop events within pairWindowSec; bracketed
//      pairs become directives, unmatched events become lone punctuation.
//   2. Verb pass — tokenize the bracketed speech against the verb table
//      and resolve anchors against the transcript segments.
//
// Usage:
//   parse-directives.mjs <meeting-dir> [--out <path>]
//
// Reads:
//   <dir>/whistlepops.json   { events: [{ t_start, t_end, kind, ... }] }
//   <dir>/transcript.json    { segments: [{ start, end, speaker, text }] }
//
// Writes:
//   <dir>/directives.json    { directives: [{ type, t, text?, ... }] }

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { join, resolve } from "node:path";

// ─── tuning constants ────────────────────────────────────────────────

const PAIR_WINDOW_SEC = 8;     // max gap between open + close whistles
const ANCHOR_WINDOW_SEC = 3;   // how far back to look for "prev sentence"

// ─── verb table ──────────────────────────────────────────────────────
//
// Each verb knows: how many head tokens it consumes, where to anchor,
// and how to construct the directive record. `head` is matched
// case-insensitive against the first token(s) of the bracketed speech.

const VERBS = [
  { head: ["highlight"], anchor: "prev",     build: (_rest, ctx) => ({ type: "highlight", text: ctx.prevText }) },
  { head: ["decision"],  anchor: "prev",     build: (rest, ctx)  => ({ type: "decision", text: ctx.prevText, tag: rest.join(" ").trim() || undefined }) },
  { head: ["action"],    anchor: "prev",     build: (rest, ctx)  => ({ type: "action", person: rest[0] || "", text: ctx.prevText }) },
  { head: ["quote"],     anchor: "prev",     build: (_rest, ctx) => ({ type: "quote", text: ctx.prevText }) },
  { head: ["section"],   anchor: "here",     build: (rest)      => ({ type: "section", name: rest.join(" ").trim() || "Section" }) },
  { head: ["subsection"],anchor: "here",     build: (rest)      => ({ type: "subsection", name: rest.join(" ").trim() || "Subsection" }) },
  { head: ["note"],      anchor: "margin",   build: (rest)      => ({ type: "note", text: rest.join(" ").trim() }) },
  { head: ["skip"],      anchor: "bracketed",build: (_rest, ctx) => ({ type: "skip", duration: ctx.bracketDuration }) },
  { head: ["redact"],    anchor: "bracketed",build: (rest, ctx)  => ({ type: "redact", text: rest.join(" ").trim(), duration: ctx.bracketDuration }) },
  { head: ["start"],     anchor: "open",     build: ()          => ({ type: "highlight-start" }) },
  { head: ["end"],       anchor: "close",    build: ()          => ({ type: "highlight-end" }) },
];

// ─── helpers ─────────────────────────────────────────────────────────

function tokenize(s) {
  return String(s || "").toLowerCase().trim()
    .split(/\s+/).filter(Boolean);
}

// Pick the segment that the directive should anchor to as "prev sentence".
// Preference order:
//   1. A segment that contains t — the speaker was mid-utterance when they
//      whistled, so the directive attaches to that in-progress sentence.
//   2. The most-recently-ended segment before t, if within ANCHOR_WINDOW_SEC.
//   3. Fallback: the first segment starting after t (whistle came in a gap).
function findPrevSegment(segments, t) {
  for (const s of segments) {
    if (s.start <= t && t <= s.end) return s;
  }
  let best = null;
  for (const s of segments) {
    if (s.end <= t) {
      if (!best || s.end > best.end) best = s;
    }
  }
  if (best && (t - best.end) <= ANCHOR_WINDOW_SEC) return best;
  for (const s of segments) {
    if (s.start >= t) return s;
  }
  return best;
}

// Extract the transcript text covering [t0, t1]. Concatenates segments
// whose timestamps overlap the bracket window.
function bracketedText(segments, t0, t1) {
  const parts = [];
  for (const s of segments) {
    if (s.end <= t0) continue;
    if (s.start >= t1) break;
    parts.push(s.text);
  }
  return parts.join(" ").trim();
}

function formatDuration(seconds) {
  const s = Math.max(0, Math.round(Number(seconds) || 0));
  if (s < 60) return `${s} sec`;
  const m = Math.floor(s / 60);
  const r = s % 60;
  return r === 0 ? `${m} min` : `${m} min ${r} sec`;
}

// ─── main passes ─────────────────────────────────────────────────────

function pairPass(events) {
  // Sort by t_start. Greedy walk: when an event has no pair within window,
  // it's lone; otherwise consume the next event and emit a pair record.
  const sorted = [...events].sort((a, b) => a.t_start - b.t_start);
  const out = [];
  const used = new Set();
  for (let i = 0; i < sorted.length; i++) {
    if (used.has(i)) continue;
    const open = sorted[i];
    let pair = null;
    for (let j = i + 1; j < sorted.length; j++) {
      if (used.has(j)) continue;
      if (sorted[j].t_start - open.t_end > PAIR_WINDOW_SEC) break;
      pair = j;
      break;
    }
    if (pair !== null) {
      used.add(i); used.add(pair);
      out.push({ kind: "pair", open, close: sorted[pair] });
    } else {
      out.push({ kind: "lone", event: open });
    }
  }
  return out;
}

// Segments are "conversational" if they don't fall inside any whistle bracket.
// Excluding bracketed segments from anchor lookups means "decision" doesn't
// pull "highlight" as its prev sentence — it pulls the previous real speech.
function filterConversational(segments, grouped) {
  const brackets = grouped
    .filter((g) => g.kind === "pair")
    .map((g) => [g.open.t_start, g.close.t_end]);
  return segments.filter((s) => {
    const mid = (s.start + s.end) / 2;
    return !brackets.some(([t0, t1]) => mid >= t0 && mid <= t1);
  });
}

function verbPass(grouped, segments) {
  const conversational = filterConversational(segments, grouped);
  const directives = [];
  for (const g of grouped) {
    if (g.kind === "lone") {
      const e = g.event;
      // Map acoustic class → structural punctuation.
      let type = "break";
      if (e.kind === "medium") type = "subsection-break";
      if (e.kind === "long")   type = "break";
      directives.push({
        type,
        t: e.t_start,
        kind: e.kind,
      });
      continue;
    }
    // Pair → directive.
    const { open, close } = g;
    const text = bracketedText(segments, open.t_end, close.t_start);
    const tokens = tokenize(text);
    const verb = VERBS.find((v) =>
      v.head.length <= tokens.length
      && v.head.every((h, idx) => tokens[idx] === h));

    const ctx = {
      prevText: findPrevSegment(conversational, open.t_start)?.text || "",
      bracketDuration: formatDuration(close.t_start - open.t_end),
    };

    if (!verb) {
      // Unmatched directive — preserve as freeform margin note so the
      // reader sees what the parser couldn't classify rather than
      // silently dropping a deliberate whistle.
      directives.push({
        type: "freeform",
        t: open.t_start,
        text: text || "(empty whistlepop)",
      });
      continue;
    }
    const rest = tokens.slice(verb.head.length);
    const built = verb.build(rest, ctx);
    directives.push({ t: open.t_start, ...built });
  }
  return directives.sort((a, b) => a.t - b.t);
}

// ─── range pass: stitch start ... end pairs into a highlight range ───
//
// `start` opens an unbounded highlight; the next matching `end` closes
// it. Renderer turns the [t_start, t_end] range into a shaded region.
function rangePass(directives) {
  const out = [];
  let open = null;
  for (const d of directives) {
    if (d.type === "highlight-start") {
      if (open) {
        // Unclosed earlier start — flush as a freeform note and reopen.
        out.push({ type: "freeform", t: open.t, text: "unclosed highlight start" });
      }
      open = d;
      continue;
    }
    if (d.type === "highlight-end") {
      if (open) {
        out.push({ type: "highlight-range", t: open.t, t_end: d.t });
        open = null;
      } else {
        out.push({ type: "freeform", t: d.t, text: "unmatched highlight end" });
      }
      continue;
    }
    out.push(d);
  }
  if (open) {
    out.push({ type: "freeform", t: open.t, text: "unclosed highlight start" });
  }
  return out;
}

// ─── entry ───────────────────────────────────────────────────────────

const args = process.argv.slice(2);
const dirArg = args[0];
let outPath = null;
for (let i = 1; i < args.length; i++) {
  if (args[i] === "--out") outPath = args[++i];
}
if (!dirArg) {
  console.error("usage: parse-directives.mjs <meeting-dir> [--out <path>]");
  process.exit(2);
}
const dir = resolve(dirArg);
const popsPath = join(dir, "whistlepops.json");
const txPath = join(dir, "transcript.json");
if (!existsSync(popsPath) || !existsSync(txPath)) {
  console.error(`[parse] needs whistlepops.json + transcript.json in ${dir}`);
  process.exit(2);
}

const pops = JSON.parse(readFileSync(popsPath, "utf8"));
const transcript = JSON.parse(readFileSync(txPath, "utf8"));
const segments = (transcript.segments || []).slice();

const events = pops.events || [];
const grouped = pairPass(events);
const verbed = verbPass(grouped, segments);
const ranged = rangePass(verbed);

const result = {
  source: { whistlepops: popsPath, transcript: txPath },
  generatedAt: new Date().toISOString(),
  counts: {
    events: events.length,
    pairs: grouped.filter((g) => g.kind === "pair").length,
    lone: grouped.filter((g) => g.kind === "lone").length,
    directives: ranged.length,
  },
  directives: ranged,
};

const outFile = outPath || join(dir, "directives.json");
writeFileSync(outFile, JSON.stringify(result, null, 2) + "\n");
console.error(`[parse] ${ranged.length} directives → ${outFile}`);
