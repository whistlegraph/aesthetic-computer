#!/usr/bin/env node
// ste-check.mjs — measure Markdown against the AC subset of ASD-STE100.
//
// It checks the rules that a program can count: sentence length, paragraph
// length, -ing forms, passive voice, modal verbs, contractions and words that
// STE.md does not approve. It cannot check meaning, so a clean report is not
// proof that a text is STE. A person still reads the text.
//
//   node toolchain/ste/ste-check.mjs FILE.md [FILE.md ...]
//   node toolchain/ste/ste-check.mjs --strict FILE.md   (exit 1 on any finding)
//   node toolchain/ste/ste-check.mjs --json FILE.md

import { readFileSync } from "node:fs";
import { relative } from "node:path";

// --- Limits (STE.md, "Sentences") -------------------------------------------

const LIMIT = {
  procedural: 20, // words in one instruction
  descriptive: 25, // words in one descriptive sentence
  paragraph: 6, // sentences in one paragraph
};

// --- Word lists --------------------------------------------------------------

// Words that STE does not approve, with the approved word to use.
// This list is for AC text. It is not the ASD dictionary.
const UNAPPROVED = new Map(
  Object.entries({
    may: "can (ability) or must (obligation)",
    might: "can",
    could: "can",
    would: "(a direct statement)",
    should: "must, or a direct instruction",
    shall: "must",
    please: "(remove it)",
    just: "(remove it)",
    simply: "(remove it)",
    easily: "(remove it)",
    basically: "(remove it)",
    actually: "(remove it)",
    really: "(remove it)",
    very: "(remove it)",
    quite: "(remove it)",
    utilize: "use",
    utilise: "use",
    leverage: "use",
    via: "through, or with",
    etc: "(give the full list)",
    "e.g": "for example",
    "i.e": "that is",
    obtain: "get",
    commence: "start",
    terminate: "stop",
    perform: "do",
    ensure: "make sure",
    allow: "let",
    allows: "lets",
    enable: "let, or start",
    enables: "lets",
    approximately: "about",
    numerous: "many",
    prior: "before",
    subsequent: "after",
    additional: "more",
    sufficient: "enough",
    desire: "want",
    desired: "necessary, or that you want",
    assist: "help",
    whilst: "while",
    upon: "on, or when",
    therefore: "thus, or (two sentences)",
    however: "but",
  }),
);

// -ing words that are technical names in AC text, or that STE approves as
// nouns or adjectives. Everything else that ends in -ing is a finding.
const ING_OK = new Set([
  "thing", "things", "something", "nothing", "anything", "everything",
  "string", "strings", "ring", "king", "wing", "spring", "sing", "bring",
  "during", "morning", "evening", "ceiling", "ping", "pong", "bling",
  "painting", "paintings", "recording", "recordings", "setting", "settings",
  "warning", "warnings", "drawing", "drawings", "building", "rendering",
  "kidlisp", "laklok", "notepat", "bleep", "lisp", "landing",
  "meaning", "meanings", "writing", "processing", "heading", "headings",
]);

// Technical words that look passive or -ing but are names in AC.
const TECHNICAL = new Set([
  "aesthetic", "piece", "pieces", "prompt", "handle", "handles", "channel",
  "publish", "source", "tape", "share", "smear", "wipe", "kidlisp", "notepat",
]);

const BE = new Set(["is", "are", "was", "were", "be", "been", "being", "am"]);
const IRREGULAR_PARTICIPLES = new Set([
  "made", "done", "given", "shown", "known", "seen", "written", "built",
  "sent", "kept", "found", "held", "set", "put", "run", "read", "taken",
  "brought", "bought", "left", "lost", "cut", "hidden", "chosen", "drawn",
  "broken", "spoken", "stored", "saved", "loaded", "used", "called",
]);
// Words in -ed that are adjectives after "be" in normal STE text.
const ED_ADJECTIVES = new Set([
  "red", "bed", "need", "speed", "feed", "seed", "shed", "bred", "hundred",
  "registered", "connected", "necessary", "closed", "open", "signed",
  "related", "interested", "tired", "advanced", "detailed", "fixed",
]);

const CONTRACTION = /\b\w+'(t|s|re|ve|ll|d|m)\b/i;
const POSSESSIVE_OK = /\b(\w+)'s\b/i; // possessive is not a contraction

// --- Markdown to sentences ---------------------------------------------------

function blocksOf(markdown) {
  const lines = markdown.split(/\r?\n/);
  const blocks = [];
  let fence = false;
  let current = null;

  const flush = () => {
    if (current && current.text.trim()) blocks.push(current);
    current = null;
  };

  lines.forEach((raw, i) => {
    const line = raw.replace(/\s+$/, "");
    if (/^\s*(```|~~~)/.test(line)) {
      flush();
      fence = !fence;
      return;
    }
    if (fence) return;
    if (/^\s*$/.test(line)) return flush();
    if (/^\s*(<[^>]+>\s*)+$/.test(line)) return flush(); // html lines
    if (/^\s*(---|\*\*\*|___)\s*$/.test(line)) return flush();
    if (/^\s*\|/.test(line)) return flush(); // tables: cells are not sentences
    if (/^\s*>/.test(line) && !current) {
      current = { kind: "quote", line: i + 1, text: "" };
    }

    const heading = line.match(/^#{1,6}\s+(.*)$/);
    if (heading) {
      flush();
      blocks.push({ kind: "heading", line: i + 1, text: heading[1] });
      return;
    }

    const step = line.match(/^\s*(\d+)[.)]\s+(.*)$/);
    const bullet = line.match(/^\s*[-*+]\s+(.*)$/);
    if (step || bullet) {
      flush();
      current = {
        kind: step ? "step" : "item",
        line: i + 1,
        text: (step ? step[2] : bullet[1]) + " ",
      };
      return;
    }

    if (!current) current = { kind: "paragraph", line: i + 1, text: "" };
    current.text += line.replace(/^\s*>\s?/, "").trim() + " ";
  });
  flush();
  return blocks;
}

// Replace inline code, links and URLs with one token each, so that
// `notepat` or a long URL counts as one word.
function plain(text) {
  return text
    .replace(/`[^`]*`/g, " CODE ")
    .replace(/"[^"]*"|“[^”]*”/g, " QUOTE ") // quoted examples are not prose
    .replace(/!\[[^\]]*\]\([^)]*\)/g, " ")
    .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1")
    .replace(/https?:\/\/\S+/g, " URL ")
    .replace(/<[^>]+>/g, " ")
    .replace(/\[\d+\]/g, " ")
    .replace(/[*_]{1,3}([^*_]+)[*_]{1,3}/g, "$1")
    .replace(/\s+/g, " ")
    .trim();
}

function sentencesOf(text) {
  const out = [];
  const re = /[^.!?:;]+[.!?:;]?/g;
  let m;
  const t = plain(text).replace(/\b(e\.g|i\.e|etc)\./gi, "$1");
  while ((m = re.exec(t))) {
    const s = m[0].trim();
    if (s && /[a-z]/i.test(s)) out.push(s);
  }
  // A colon ends a lead-in, not a sentence, unless text follows on the same
  // line. Join a lead-in that is very short to the next sentence.
  return out;
}

const wordsOf = (s) => s.match(/[A-Za-z][A-Za-z'’-]*|CODE|QUOTE|URL|\d+/g) || [];

// --- Checks ------------------------------------------------------------------

function isImperative(sentence) {
  const first = wordsOf(sentence)[0] || "";
  return /^(enter|type|press|push|open|close|start|stop|save|drag|drop|go|do|use|make|put|set|get|read|write|run|find|select|pick|visit|return|record|play|add|remove|keep|turn|move|hold|touch|tap|click|draw|paint|write|copy|change|install|look|see|let|give|send|show|wait|try|make|examine|connect|disconnect|download|upload|publish|share|sign)$/i.test(
    first,
  );
}

function checkSentence(s, block, findings) {
  const words = wordsOf(s);
  const lower = words.map((w) => w.toLowerCase().replace(/[’']/g, "'"));
  const procedural =
    block.kind === "step" || (block.kind !== "heading" && isImperative(s));
  const limit = procedural ? LIMIT.procedural : LIMIT.descriptive;

  if (block.kind !== "heading" && words.length > limit) {
    findings.push({
      line: block.line,
      rule: procedural ? "S1 instruction length" : "S2 sentence length",
      detail: `${words.length} words (limit ${limit})`,
      text: s,
    });
  }

  lower.forEach((w, i) => {
    const bare = w.replace(/^'+|'+$/g, "");
    if (UNAPPROVED.has(bare)) {
      findings.push({
        line: block.line,
        rule: "W1 unapproved word",
        detail: `"${bare}" → ${UNAPPROVED.get(bare)}`,
        text: s,
      });
    }

    if (
      /ing$/.test(bare) &&
      bare.length > 4 &&
      !ING_OK.has(bare) &&
      !TECHNICAL.has(bare) &&
      words[i] !== "CODE"
    ) {
      findings.push({
        line: block.line,
        rule: "V1 -ing form",
        detail: `"${bare}"`,
        text: s,
      });
    }

    if (CONTRACTION.test(w) && !POSSESSIVE_OK.test(w)) {
      findings.push({
        line: block.line,
        rule: "W2 contraction",
        detail: `"${w}"`,
        text: s,
      });
    }

    if (BE.has(bare) && i + 1 < lower.length) {
      // be + (adverb)? + participle
      let j = i + 1;
      if (/ly$/.test(lower[j]) || lower[j] === "not") j++;
      const next = (lower[j] || "").replace(/'/g, "");
      const participle =
        (/ed$/.test(next) && !ED_ADJECTIVES.has(next)) ||
        IRREGULAR_PARTICIPLES.has(next);
      if (participle) {
        findings.push({
          line: block.line,
          rule: procedural ? "V2 passive in instruction" : "V3 passive voice",
          detail: `"${bare} ${next}"`,
          text: s,
          soft: !procedural, // STE permits some passive in description
        });
      }
    }
  });

  if (block.kind === "step") {
    // One instruction per sentence: "Enter X and press Y".
    if (
      /\b(and|then)\s+(enter|type|press|open|close|save|drag|drop|go|use|make|put|run|select|pick|return|play|record|add|remove|turn|move|hold|touch|tap|click|draw|paint|download|publish|share)\b/i.test(
        s,
      )
    ) {
      findings.push({
        line: block.line,
        rule: "P1 two instructions",
        detail: "write one instruction in each sentence",
        text: s,
      });
    }
  }
}

function check(markdown) {
  const findings = [];
  let count = 0;
  for (const block of blocksOf(markdown)) {
    const sentences = sentencesOf(block.text);
    count += sentences.length;
    if (block.kind === "paragraph" && sentences.length > LIMIT.paragraph) {
      findings.push({
        line: block.line,
        rule: "S3 paragraph length",
        detail: `${sentences.length} sentences (limit ${LIMIT.paragraph})`,
        text: sentences[0],
      });
    }
    for (const s of sentences) checkSentence(s, block, findings);
  }
  return { sentences: count, findings };
}

// --- CLI ---------------------------------------------------------------------

const args = process.argv.slice(2);
const strict = args.includes("--strict");
const json = args.includes("--json");
const files = args.filter((a) => !a.startsWith("--"));

if (!files.length) {
  console.error("usage: ste-check.mjs [--strict] [--json] FILE.md ...");
  process.exit(2);
}

let hard = 0;
const report = [];
for (const file of files) {
  const { sentences, findings } = check(readFileSync(file, "utf8"));
  hard += findings.filter((f) => !f.soft).length;
  report.push({ file: relative(process.cwd(), file), sentences, findings });
}

if (json) {
  console.log(JSON.stringify(report, null, 2));
} else {
  for (const r of report) {
    const h = r.findings.filter((f) => !f.soft).length;
    const s = r.findings.length - h;
    console.log(
      `\n${r.file}  ${r.sentences} sentences · ${h} findings · ${s} notes`,
    );
    for (const f of r.findings) {
      const mark = f.soft ? "·" : "✗";
      const clip = f.text.length > 72 ? f.text.slice(0, 69) + "..." : f.text;
      console.log(`  ${mark} ${String(f.line).padStart(4)}  ${f.rule}: ${f.detail}`);
      console.log(`          ${clip}`);
    }
  }
  console.log(hard ? `\n${hard} findings.` : "\nNo findings.");
}

process.exit(strict && hard ? 1 : 0);
