#!/usr/bin/env node
// scorebook.mjs — generate book/mnpt-scores-entries.tex from scores/*.mbscore
// + SONGBOOK.md + members/*/voice.json.
//
// The binder (book/mnpt-scores.tex) holds the typography; this holds the data.
// Rebuild after adding a score:
//
//   node bin/scorebook.mjs && bin/scorebook.sh
//
// Nothing here writes outside book/. Scores, SONGBOOK.md and the band book are
// read-only inputs.

import { readFileSync, writeFileSync, readdirSync, existsSync } from "node:fs";
import { execSync } from "node:child_process";
import { join, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const LANE = join(dirname(fileURLToPath(import.meta.url)), "..");
const SCORES = join(LANE, "scores");
const BOOK = join(LANE, "book");

// ---------------------------------------------------------------- numbering
// The request numbers people call out from the room. Assign by group, then by
// order inside the group: number = group.code + 1 + index.
//
// A slug listed in `slugs` keeps its number forever — that is the contract, so
// PIN A NEW SCORE BY APPENDING IT TO ITS GROUP'S LIST. A score that is not
// listed still lands in a group through `claim()` and sorts alphabetically
// after the pinned ones, so an unpinned score's number can move when another
// unpinned score joins it.
const GROUPS = [
  {
    code: 100,
    name: "Movements",
    color: "acpurple",
    gloss: "the piece itself, in four",
    slugs: [
      "trio-i-birth",
      "trio-ii-service",
      "trio-iii-chorus",
      "trio-iv-ballad",
    ],
    claim: () => false,
  },
  {
    code: 200,
    name: "Dialogs",
    color: "acblue",
    gloss: "neo and blueberry, a poem apiece",
    slugs: [
      "dialog-01-radio",
      "dialog-02-the-downbeat",
      "dialog-03-wallpaper",
      "dialog-04-who-carries-what",
      "dialog-05-sleep",
      "dialog-06-six-minutes",
      "dialog-07-before",
      "dialog-08-typed-at",
      "dialog-09-the-squad",
      "dialog-10-your-first-day",
    ],
    claim: (slug) => slug.startsWith("dialog-"),
  },
  {
    code: 300,
    name: "House songs",
    color: "acpink",
    gloss: "words of their own, written here",
    slugs: [
      "trio-wake",
      "trio-lullaby",
      "trio-lights-out",
      "trio-open-me",
      "trio-thirteen-days",
    ],
    claim: () => false,
  },
  {
    code: 400,
    name: "Record pieces and standards",
    color: "acorange",
    gloss: "the numbers they keep, and the songs everyone knows",
    slugs: [
      "trio-the-record",
      "trio-one-big-voice",
      "trio-rowboat",
      "mary-had-a-little-lamb",
    ],
    // The catch-all: tried only after every other group has passed.
    fallback: true,
    claim: () => false,
  },
  {
    code: 500,
    name: "Wordless études",
    color: "accyan",
    gloss: "no words at all",
    slugs: [
      "trio-vocalise",
      "trio-styling-study",
      "trio-chorus-phonemes",
      "trio-chorus-doowop",
      "trio-toxic-sketch",
    ],
    claim: (slug, score, facts) => facts.wordless,
  },
  {
    code: 600,
    name: "Educational",
    color: "acgreen",
    gloss: "scores that teach the thing they sing",
    // PIN EACH NEW LESSON HERE so its number never moves.
    slugs: [
      "trio-sums",
      "trio-take-away",
      "trio-spell-frisbee",
      "trio-spell-sophia",
      "trio-abc",
    ],
    claim: (slug, score) =>
      score.educational === true ||
      /^(trio-)?(edu|lesson|spell)-/.test(slug) ||
      /\b(lessons?|spelling|counting|alphabet)\b/i.test(score.description || ""),
  },
];

const MEMBER_ORDER = ["neo", "blueberry", "frisbee", "third"];

// Syllables that carry no word. A line built only from these is a hum.
const VOCABLE = new Set([
  "hmm", "hm", "mm", "ooh", "oo", "ah", "ee", "aah", "uh", "huh", "hah",
  "la", "lee", "loo", "li", "lo", "doo", "dee", "dah", "dum", "doom", "dm",
  "bum", "bom", "boom", "bah", "ba", "bee", "bip", "boh", "poo", "nee",
  "tee", "ts", "tk", "tss", "kh", "ch", "pf", "bm", "ki", "wah", "shoo",
  "dud", "doot", "ooo",
]);

// Syllables that are only breath — these print as "(hum)".
const HUMMED = new Set(["hmm", "hm", "mm", "ooh", "oo", "ah", "ee", "aah", "ooo"]);

const WORDLESS_ROLES = new Set([
  "hum", "phoneme", "bass", "drone", "clock", "tick", "percussion",
  "bass-bed", "harmony-bed", "scat",
]);

// mary-had-a-little-lamb (2026-09-07, the first singing test) predates the
// hyphen-joins-a-word convention: its lyric is one line of space-separated
// syllables, so the joiner cannot recover the words. Printed text only — the
// score is not touched.
const LYRIC_OVERRIDES = {
  "mary-had-a-little-lamb": {
    "neo sings (Fred)": [
      "Mary had a little lamb, little lamb, little lamb",
      "Mary had a little lamb, its fleece was white as snow",
    ],
  },
};

// ------------------------------------------------------------------ helpers
const tex = (s) =>
  String(s ?? "")
    .replace(/\\/g, "\\textbackslash{}")
    .replace(/([&%$#_{}])/g, "\\$1")
    .replace(/~/g, "\\textasciitilde{}")
    .replace(/\^/g, "\\textasciicircum{}");

const mmss = (seconds) => {
  const s = Math.round(seconds);
  return `${Math.floor(s / 60)}:${String(s % 60).padStart(2, "0")}`;
};

function noteTokens(notes) {
  return String(notes || "")
    .split(",")
    .map((t) => t.trim())
    .filter(Boolean)
    .map((t) => {
      const i = t.lastIndexOf(":");
      const pitch = i < 0 ? t : t.slice(0, i);
      const beats = i < 0 ? 0 : parseFloat(t.slice(i + 1));
      return { rest: pitch === "r", beats: Number.isFinite(beats) ? beats : 0 };
    });
}

const totalBeats = (notes) => noteTokens(notes).reduce((a, n) => a + n.beats, 0);

const lyricLines = (lyrics) =>
  String(lyrics || "")
    .trim()
    .replace(/^\/+|\/+$/g, "")
    .split(/\s*\/\s*/)
    .map((l) => l.trim())
    .filter(Boolean);

const syllables = (line) =>
  line.split(/\s+/).filter(Boolean).flatMap((w) => w.split("-").filter(Boolean));

// "So-phi-a" -> "Sophia"; a lone letter stays a letter.
const joinWords = (line) =>
  line
    .split(/\s+/)
    .filter(Boolean)
    .map((w) => (w.length === 1 ? w : w.replace(/-/g, "")))
    .join(" ");

// Fold a repeated pattern: "doo doo doo doo" -> "doo x4".
function compress(line) {
  const t = line.split(/\s+/).filter(Boolean);
  for (let u = 1; u <= 4 && u * 3 <= t.length; u += 1) {
    if (t.length % u) continue;
    const unit = t.slice(0, u);
    const reps = t.length / u;
    if (reps < 3) continue;
    let same = true;
    for (let i = u; i < t.length && same; i += 1) same = t[i] === t[i % u];
    if (same) return `${unit.join(" ")} \\mnx{${reps}}`;
  }
  const out = [];
  for (let i = 0; i < t.length; ) {
    let j = i;
    while (j < t.length && t[j] === t[i]) j += 1;
    const run = j - i;
    out.push(run >= 4 ? `${t[i]} \\mnx{${run}}` : Array(run).fill(t[i]).join(" "));
    i = j;
  }
  return out.join(" ");
}

// What a row is worth in characters once the markup is gone.
const plain = (text) =>
  String(text)
    .replace(/\\mnx\{(\d+)\}/g, "x$1")
    .replace(/\\mnsep\\?\s*/g, "-")
    .replace(/\\[a-zA-Z]+/g, "");

const isHum = (line) => syllables(line).every((s) => HUMMED.has(s.toLowerCase()));
const isVocable = (line) => syllables(line).every((s) => VOCABLE.has(s.toLowerCase()));

function memberOf(voiceName) {
  const first = String(voiceName || "").trim().split(/[\s·(]/)[0].toLowerCase();
  return MEMBER_ORDER.includes(first) ? first : null;
}

// --------------------------------------------------------------- the inputs
function readMembers() {
  const members = {};
  for (const name of MEMBER_ORDER) {
    const p = join(LANE, "members", name, "voice.json");
    if (!existsSync(p)) continue;
    const v = JSON.parse(readFileSync(p, "utf8"));
    members[name] = {
      name,
      color: (v.color || "#666666").replace("#", "").toUpperCase(),
      voice: v.aesthetivox?.base_voice || "",
      band: v.aesthetivox?.register_midi || null,
      median: v.aesthetivox?.spoken_median_midi ?? null,
    };
  }
  return members;
}

// SONGBOOK.md is the book: the piece table, the cast line, and the rules.
function readSongbook() {
  const md = readFileSync(join(LANE, "SONGBOOK.md"), "utf8");
  const rows = {};
  for (const line of md.split("\n")) {
    const m = line.match(/^\|\s*([a-z0-9-]+)\s*\|(.*)\|\s*$/);
    if (!m) continue;
    const cells = m[2].split("|").map((c) => c.trim());
    if (cells.length < 5) continue;
    rows[m[1]] = { title: cells[0], form: cells[1], length: cells[2], wer: cells[3], note: cells[4] };
  }

  const rules = [];
  const rulesBlock = md.split("## Rules the day taught")[1]?.split("\n## ")[0] || "";
  for (const chunk of rulesBlock.split(/\n(?=- )/)) {
    const t = chunk.replace(/^-\s*/, "").replace(/\s*\n\s*/g, " ").trim();
    if (t) rules.push(t);
  }

  // "Cast: neo = Noelle (Enhanced), speaks at MIDI 59.6, band 57–62. …"
  // Cut each member's clause out first; decimals make a sentence split unsafe.
  const cast = {};
  const castBlock = md.match(/Cast:([\s\S]*?)\n\n/)?.[1]?.replace(/\s*\n\s*/g, " ") || "";
  const names = ["neo", "blueberry", "frisbee"];
  for (const [i, name] of names.entries()) {
    const from = castBlock.indexOf(`${name} = `);
    if (from < 0) continue;
    const nexts = names
      .slice(i + 1)
      .map((n) => castBlock.indexOf(`${n} = `, from))
      .filter((x) => x > from);
    const clause = castBlock.slice(from, nexts.length ? Math.min(...nexts) : undefined);
    const band = clause.match(
      /band\s+([0-9]+(?:\.[0-9]+)?\s*[–-]\s*[0-9]+(?:\.[0-9]+)?)/,
    );
    const speaks = clause
      .replace(/band\s+[0-9]+(?:\.[0-9]+)?\s*[–-]\s*[0-9]+(?:\.[0-9]+)?/, "")
      .match(/([0-9]+\.[0-9]+(?:\s*[–-]\s*[0-9]+\.[0-9]+)?)/);
    cast[name] = {
      voice: clause.slice(name.length + 3).split(",")[0].trim(),
      speaks: speaks ? speaks[1].replace(/\s+/g, "") : "",
      band: band ? band[1].replace(/\s+/g, "") : "",
    };
  }
  return { rows, rules, cast };
}

// ------------------------------------------------------------ score reading
function readScore(file) {
  const slug = basename(file, ".mbscore");
  const score = JSON.parse(readFileSync(join(SCORES, file), "utf8"));
  const warnings = [];

  const voices = [];
  for (const v of score.voices || []) {
    const member = memberOf(v.name);
    const lines = lyricLines(v.lyrics);
    const roles = v.lineRoles || [];
    const notes = noteTokens(v.notes);

    // One syllable per sounding note: walk the notes to find each line's beat.
    const starts = [];
    let pos = 0;
    let syl = 0;
    const sylBeat = [];
    for (const n of notes) {
      if (n.rest) pos += n.beats;
      else {
        sylBeat[syl] = pos;
        syl += 1;
        pos += n.beats;
      }
    }
    let cursor = 0;
    for (const line of lines) {
      starts.push(sylBeat[cursor] ?? pos);
      cursor += syllables(line).length;
    }
    if (lines.length && cursor !== syl) {
      warnings.push(`${slug} / ${v.name}: ${cursor} syllables vs ${syl} notes`);
    }

    voices.push({
      raw: v,
      member,
      label: member || "whistle",
      singVoice: v.singVoice || null,
      beats: totalBeats(v.notes),
      lines: lines.map((text, i) => ({
        text,
        role: roles[i] || null,
        start: starts[i],
      })),
    });
  }

  const beats = Math.max(0, ...voices.map((v) => v.beats));
  const seconds = score.bpm ? (beats * 60) / score.bpm : 0;
  const wordLines = voices.flatMap((v) =>
    v.lines.filter((l) => !(WORDLESS_ROLES.has(l.role) || isVocable(l.text))),
  );

  return {
    slug,
    score,
    voices,
    beats,
    seconds,
    warnings,
    wordless: wordLines.length === 0,
    firstWord: wordLines.sort((a, b) => a.start - b.start)[0] || null,
  };
}

// ------------------------------------------------------------- the prose cut
// Wordcrust: drop sentences that only restate the meta line or the group.
function tighten(description) {
  const text = String(description || "").trim();
  if (!text) return "";
  const kept = [];
  for (const sentence of text.split(/(?<=\.)\s+/)) {
    let s = sentence.trim();
    if (!s) continue;
    const before = s;
    s = s.replace(/^Enhanced\/Premium (Apple )?vocal trio:\s*/i, "");
    // A clause that recites the lyric (the " / " convention) is not a description.
    const clauses = s.split(/;\s+/).filter((c) => !/\s\/\s/.test(c));
    if (!clauses.length) continue;
    s = clauses.join("; ");
    if (!/[.!?]$/.test(s)) s += ".";
    // Only recapitalise a sentence this cut opened, and never a machine's name.
    if (s !== before && !/^(neo|blueberry|frisbee|third|jeffrey)\b/.test(s)) {
      s = s.charAt(0).toUpperCase() + s.slice(1);
    }
    if (/^Enhanced\/Premium voices only\.?$/i.test(s)) continue;
    if (/^Words:\s/.test(s)) continue;
    if (/\.mjs|double-clicked|Driven by bin\//.test(s)) continue;
    if (/^\d+ lines between /.test(s)) continue;
    if (/\bbpm\b/.test(s)) {
      const words = s
        .replace(/[.,]/g, " ")
        .split(/\s+/)
        .filter(Boolean);
      const spec = words.every((w) =>
        /^(\d+(\.\d+)?|\d+\/\d+|[A-G]|major|minor|bpm|beats|s|seconds|and)$/i.test(w),
      );
      if (spec) continue;
    }
    kept.push(s);
  }
  return kept.join(" ");
}

function keyOf(score, songbookForm) {
  const form = String(songbookForm || "");
  const fm = form.match(/\b([A-G][b#]?)(\s+(?:minor|major))?\b(?!\s*bpm)/);
  if (fm && !/^\d/.test(fm[1])) return (fm[1] + (fm[2] || "")).replace(/\s+/g, " ");
  const dm = String(score.description || "").match(/\b([A-G])[\s-](major|minor)\b/);
  return dm ? `${dm[1]} ${dm[2]}` : null;
}

function meterOf(score, songbookForm) {
  if (score.arrangement?.meter) return score.arrangement.meter;
  return String(songbookForm || "").match(/\b(\d+\/\d+)\b/)?.[1] || null;
}

// ------------------------------------------------------------- karaoke body
function chip(member, members) {
  const m = members[member];
  if (!m) return `\\mnchip{acgray}{white}{${tex(member)}}`;
  const [r, g, b] = [0, 2, 4].map((i) => parseInt(m.color.slice(i, i + 2), 16));
  const lum = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
  return `\\mnchip{mn${member}}{${lum > 0.55 ? "black" : "white"}}{${tex(member)}}`;
}

function tagFor(memberList, members) {
  const set = memberList.filter((m, i) => memberList.indexOf(m) === i);
  if (set.length === 3 && ["neo", "blueberry", "frisbee"].every((m) => set.includes(m))) {
    return "\\mnall";
  }
  return set.map((m) => chip(m, members)).join("\\,");
}

// Keep a section marker only when it says something the next line does not.
function sectionAdds(name, nextText) {
  const norm = (s) =>
    String(s || "")
      .toLowerCase()
      .replace(/^[a-z]+:\s*/, "")
      .replace(/[^a-z0-9 ]/g, "")
      .replace(/\s+/g, " ")
      .trim();
  const a = norm(name);
  const b = norm(nextText);
  if (!a) return false;
  if (!b) return true;
  if (b.length >= 4 && a.includes(b)) return false;
  return !(b.startsWith(a) || a.startsWith(b));
}

function wordBody(entry, members) {
  const rows = [];
  // A voice that mostly hums is a bed, not a run of events: it is named once
  // under the cast, and only its hums leave the flow. Anything it says stays.
  const beds = entry.voices
    .filter((v) => {
      if (!v.member) return false;
      const hums = v.lines.filter((l) => isHum(l.text)).length;
      return hums >= 4 && hums * 2 >= v.lines.length;
    })
    .map((v) => v.member);
  for (const v of entry.voices) {
    if (!v.member) continue;
    const bed = beds.includes(v.member);
    const override = LYRIC_OVERRIDES[entry.slug]?.[v.raw.name];
    if (override) {
      override.forEach((text, i) =>
        rows.push({ start: i, member: v.member, text, hum: false, key: text }),
      );
      continue;
    }
    for (const line of v.lines) {
      if (bed && isHum(line.text)) continue;
      const wordless = WORDLESS_ROLES.has(line.role) || isVocable(line.text);
      const text = wordless ? compress(line.text) : joinWords(line.text);
      rows.push({
        start: line.start,
        member: v.member,
        text,
        hum: wordless,
        pure: wordless && isHum(line.text),
        key: text,
      });
    }
  }
  rows.sort(
    (a, b) => a.start - b.start || MEMBER_ORDER.indexOf(a.member) - MEMBER_ORDER.indexOf(b.member),
  );

  // Voices singing the same thing on the same beat share one row.
  const merged = [];
  for (const row of rows) {
    const prev = merged[merged.length - 1];
    if (prev && prev.start === row.start && prev.key === row.key && prev.hum === row.hum) {
      prev.members.push(row.member);
    } else merged.push({ ...row, members: [row.member] });
  }

  // A run of wordless rows is one grey line, not a wall of "hmm".
  const out = [];
  for (let i = 0; i < merged.length; ) {
    if (!merged[i].hum) {
      out.push(merged[i]);
      i += 1;
      continue;
    }
    let j = i;
    const parts = [];
    const who = [];
    while (j < merged.length && merged[j].hum) {
      const r = merged[j];
      who.push(...r.members);
      const text = r.pure ? "(hum)" : r.text;
      if (!parts.includes(text)) parts.push(text);
      j += 1;
    }
    out.push({
      start: merged[i].start,
      members: who,
      hum: true,
      text: parts.length === 1 ? parts[0] : parts.join(" \\mnsep\\ "),
    });
    i = j;
  }

  const sections = (entry.score.arrangement?.sections || []).slice().sort((a, b) => a.beat - b.beat);
  const lines = [];
  const widths = [];
  let si = 0;
  let lastTag = null;
  for (const [ri, row] of out.entries()) {
    // A marker is measured against the next line that has words in it, not
    // against a hum that happens to fall first.
    const ahead = out.slice(ri).find((r) => r.text !== "(hum)") || row;
    while (si < sections.length && sections[si].beat <= row.start + 0.001) {
      const s = sections[si];
      if (sectionAdds(s.name, ahead.text)) lines.push(`\\mnsec{${tex(s.name)}}`);
      lastTag = null;
      si += 1;
    }
    const who = row.members.join("+");
    const tag = who === lastTag ? "" : tagFor(row.members, members);
    lastTag = who;
    lines.push(row.hum ? `\\mnhum{${tag}}{${row.text}}` : `\\mnline{${tag}}{${tex(row.text)}}`);
    widths.push(plain(row.text).length);
  }
  return { lines, beds, widths };
}

function wordlessBody(entry, members) {
  const widths = [];
  const sections = (entry.score.arrangement?.sections || []).slice().sort((a, b) => a.beat - b.beat);
  const bounds = sections.length
    ? sections.map((s, i) => ({
        name: s.name,
        from: s.beat,
        to: i + 1 < sections.length ? sections[i + 1].beat : Infinity,
      }))
    : [{ name: null, from: 0, to: Infinity }];

  const lines = [];
  for (const b of bounds) {
    if (b.name) lines.push(`\\mnsec{${tex(b.name)}}`);
    for (const v of entry.voices) {
      if (!v.member) continue;
      const here = v.lines.filter((l) => l.start >= b.from - 0.001 && l.start < b.to - 0.001);
      if (!here.length) continue;
      const parts = [];
      // In a wordless score the syllables ARE the material, so they are never
      // folded away into "(hum)" the way a bed is inside a song.
      for (const l of here) {
        const text = compress(l.text);
        if (!parts.length || parts[parts.length - 1] !== text) parts.push(text);
      }
      const text = parts.join(" \\mnsep\\ ");
      lines.push(`\\mnhum{${chip(v.member, members)}}{${text}}`);
      widths.push(plain(text).length);
    }
  }
  return { lines, beds: [], widths };
}

// A digit-free control-sequence suffix: 0 -> A, 25 -> Z, 26 -> BA.
function letters(n) {
  let out = "";
  let i = n;
  do {
    out = String.fromCharCode(65 + (i % 26)) + out;
    i = Math.floor(i / 26) - 1;
  } while (i >= 0);
  return out;
}

// ------------------------------------------------------------------ emitter
function main() {
  const dateArg = process.argv.find((a) => a.startsWith("--date="));
  const today = dateArg
    ? dateArg.slice(7)
    : new Date().toLocaleDateString("en-US", { year: "numeric", month: "long", day: "numeric" });

  // Edition: book/mnpt-scores.edition.json holds the number; `--bump` raises
  // it before the build. The cover prints "Edition N · <git hash>".
  const editionPath = join(BOOK, "mnpt-scores.edition.json");
  const edition = existsSync(editionPath) ? JSON.parse(readFileSync(editionPath, "utf8")) : { edition: 1, date: "" };
  if (process.argv.includes("--bump")) {
    edition.edition += 1;
    edition.date = new Date().toISOString().slice(0, 10);
    writeFileSync(editionPath, `${JSON.stringify(edition, null, 2)}\n`);
  }
  let gitHash = "";
  try { gitHash = execSync("git rev-parse --short HEAD", { cwd: LANE, encoding: "utf8" }).trim(); } catch {}

  const members = readMembers();
  const songbook = readSongbook();
  const files = readdirSync(SCORES).filter((f) => f.endsWith(".mbscore")).sort();
  const entries = files.map(readScore);
  const warnings = entries.flatMap((e) => e.warnings);

  // group + number
  const byGroup = new Map(GROUPS.map((g) => [g.code, []]));
  for (const e of entries) {
    const group =
      GROUPS.find((g) => g.slugs.includes(e.slug)) ||
      GROUPS.find((g) => !g.fallback && g.claim(e.slug, e.score, e)) ||
      GROUPS.find((g) => g.fallback);
    e.group = group || GROUPS[GROUPS.length - 1];
    byGroup.get(e.group.code).push(e);
  }
  for (const g of GROUPS) {
    const list = byGroup.get(g.code);
    list.sort((a, b) => {
      const ai = g.slugs.indexOf(a.slug);
      const bi = g.slugs.indexOf(b.slug);
      if (ai >= 0 && bi >= 0) return ai - bi;
      if (ai >= 0) return -1;
      if (bi >= 0) return 1;
      return a.slug.localeCompare(b.slug);
    });
    list.forEach((e, i) => {
      e.number = g.code + 1 + i;
    });
  }

  const ordered = GROUPS.flatMap((g) => byGroup.get(g.code));

  // per-entry copy
  for (const e of ordered) {
    const sb = songbook.rows[e.slug.replace(/^trio-/, "")] || null;
    e.sb = sb;
    e.title = String(e.score.title || e.slug)
      .replace(/^The MacNeoPolitan Trio\s*[—-]\s*/, "")
      .replace(/\s*[—-]\s*MacNeoPolitan(\s+Trio)?.*$/, "")
      .replace(/\s*[—-]\s+(?=[a-z]|MacNeoPolitan)[^—]*$/, "")
      .trim();
    e.meter = meterOf(e.score, sb?.form);
    e.key = keyOf(e.score, sb?.form);
    e.blurb = tighten(e.score.description);
    e.note = sb?.note ? sb.note.replace(/\s+/g, " ").trim() : "";
    const first = e.firstWord ? joinWords(e.firstWord.text) : null;
    e.oneline =
      (e.blurb ? e.blurb.split(/(?<=\.)\s/)[0].replace(/\.$/, "") : "") ||
      (first ? `“${first}”` : e.group.gloss);
    // A long sentence usually announces itself before the colon.
    if (e.oneline.length > 78 && e.oneline.includes(": ")) {
      const [head, ...rest] = e.oneline.split(": ");
      const tail = rest.join(": ");
      e.oneline = tail.length > 24 && tail.length <= 78 ? tail : head;
    }
    if (e.oneline.length > 78) e.oneline = `${e.oneline.slice(0, 75).replace(/[ ,;:]+$/, "")}…`;
  }

  const L = [];
  L.push("% GENERATED by bin/scorebook.mjs — do not edit. Rebuild:");
  L.push("%   node bin/scorebook.mjs");
  L.push(`% ${ordered.length} scores, ${today}.`);
  L.push("");
  for (const [name, m] of Object.entries(members)) {
    L.push(`\\definecolor{mn${name}}{HTML}{${m.color}}`);
  }
  L.push("");
  L.push(`\\newcommand{\\mnptdate}{${tex(today)}}`);
  L.push(`\\newcommand{\\mnptedition}{Edition ${edition.edition}${gitHash ? ` \\textperiodcentered\\ ${gitHash}` : ""}}`);
  L.push(`\\newcommand{\\mnptcount}{${ordered.length}}`);
  L.push("");

  // request index
  L.push("\\newcommand{\\mnptrequestindex}{%");
  for (const g of GROUPS) {
    const list = byGroup.get(g.code);
    if (!list.length) continue;
    L.push(`\\mngrouprow{${g.color}}{${g.code}s \\mnsep\\ ${g.name}}{${tex(g.gloss)}}`);
    for (const e of list) {
      L.push(
        `\\mnrow{${g.color}}{${e.number}}{${tex(e.title)}}{${mmss(e.seconds)}}{${tex(e.oneline)}}`,
      );
    }
  }
  L.push("}");
  L.push("");

  // alphabetical title index
  L.push("\\newcommand{\\mnpttitleindex}{%");
  for (const e of [...ordered].sort((a, b) => a.title.localeCompare(b.title))) {
    L.push(`\\mntitlerow{${e.group.color}}{${e.number}}{${tex(e.title)}}`);
  }
  L.push("}");
  L.push("");

  // cast + register
  L.push("\\newcommand{\\mnptcast}{%");
  for (const name of ["neo", "blueberry", "frisbee"]) {
    const m = members[name];
    const c = songbook.cast[name];
    if (!m) continue;
    const band = c?.band || (m.band ? m.band.join("–") : "—");
    L.push(
      `\\mncastrow{${chip(name, members)}}{${tex(c?.voice || m.voice)}}{${tex(c?.speaks || m.median || "")}}{${tex(band)}}`,
    );
  }
  L.push("}");
  L.push("");

  // rules appendix
  L.push("\\newcommand{\\mnptrules}{%");
  for (const r of songbook.rules) L.push(`\\mnrule{${tex(r)}}`);
  L.push("}");
  L.push("");

  // entries
  const names = [];
  for (const e of ordered) {
    const { lines: body, beds, widths } = e.wordless
      ? wordlessBody(e, members)
      : wordBody(e, members);
    const p90 = widths.length
      ? widths.slice().sort((a, b) => a - b)[Math.floor(widths.length * 0.9)]
      : 0;
    const twoCol = widths.length >= 14 && p90 <= 42;

    const meta = [
      `\\textbf{${mmss(e.seconds)}}`,
      e.meter ? tex(e.meter) : null,
      e.key ? tex(e.key) : null,
      `${e.score.bpm} bpm`,
      `${Number(e.beats.toFixed(2))} beats`,
      e.sb?.wer && /\d/.test(e.sb.wer) ? `Whisper ${tex(e.sb.wer)}` : null,
    ]
      .filter(Boolean)
      .join(" \\mnsep\\ ");

    const cast = e.voices
      .filter((v) => v.member)
      .map(
        (v) =>
          `${chip(v.member, members)}\\,${tex(v.singVoice || "—")}` +
          (beds.includes(v.member) ? " {\\color{acgray}hums under}" : ""),
      )
      .join("\\quad ");

    // Control-sequence names take letters only, never digits.
    const cmd = `\\mnptentry${letters(names.length)}`;
    names.push(cmd);
    L.push(`\\newcommand{${cmd}}{%`);
    L.push(
      `\\mnentry{${e.number}}{${e.group.color}}{${tex(e.title)}}{${meta}}{${cast}}{${tex(e.blurb)}}{${tex(e.note)}}`,
    );
    if (twoCol) L.push("\\begin{multicols}{2}");
    L.push(...body);
    if (twoCol) L.push("\\end{multicols}");
    L.push("}");
    L.push("");
  }
  L.push(`\\newcommand{\\mnptentries}{${names.join("")}}`);
  L.push("");

  writeFileSync(join(BOOK, "mnpt-scores-entries.tex"), `${L.join("\n")}\n`);

  const bad = warnings.length;
  console.log(
    `mnpt-scores-entries.tex — ${ordered.length} scores` +
      GROUPS.map((g) => ` · ${g.code}s ${byGroup.get(g.code).length}`).join("") +
      (bad ? `\n${warnings.map((w) => `  warn: ${w}`).join("\n")}` : ""),
  );
}

main();
