#!/usr/bin/env node
// verify-speech.mjs — Whisper round-trip QA for a finished podcast reading.
//
// This is an intelligibility check, not a phonetics oracle: it catches words
// that a second speech model cannot recover from the rendered audio and leaves
// a short review list. It cannot certify expressive delivery or prosody.

import { execFileSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createHash } from "node:crypto";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";

function sha256(data) {
  return createHash("sha256").update(data).digest("hex");
}

function findModel() {
  const candidates = [
    process.env.WHISPER_MODEL,
    join(homedir(), ".whisper-models", "ggml-small.bin"),
    join(homedir(), ".whisper-models", "ggml-base.en.bin"),
  ].filter(Boolean);
  return candidates.find((candidate) => existsSync(candidate));
}

function words(text) {
  return text
    .normalize("NFKD")
    .replace(/\p{M}/gu, "")
    .toLowerCase()
    .replace(/[’']/g, "")
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    // Do not send harmless typography and conventional spellings to review.
    // These pairs are acoustically equivalent in this series.
    .replace(/\bkey maps\b/g, "keymaps")
    .replace(/\bkey map\b/g, "keymap")
    .replace(/\bmenu band\b/g, "menuband")
    .replace(/\bnote pad\b/g, "notepat")
    .replace(/\bnotepad\b/g, "notepat")
    .replace(/\bgeoffrey\b/g, "jeffrey")
    .replace(/\bevery one\b/g, "everyone")
    .replace(/\bsixty four\b/g, "64")
    .replace(/\btwenty\b/g, "20")
    .replace(/\bw q\b/g, "wq")
    .trim()
    .split(/\s+/)
    .filter(Boolean);
}

// Word-error alignment. Uint arrays keep an essay-sized comparison small.
function align(expected, heard) {
  const width = heard.length + 1;
  const ops = new Uint8Array((expected.length + 1) * width);
  let previous = new Uint16Array(width);
  let current = new Uint16Array(width);
  for (let j = 1; j < width; j++) { previous[j] = j; ops[j] = 3; }
  for (let i = 1; i <= expected.length; i++) {
    current[0] = i;
    ops[i * width] = 2;
    for (let j = 1; j <= heard.length; j++) {
      if (expected[i - 1] === heard[j - 1]) {
        current[j] = previous[j - 1];
        ops[i * width + j] = 0;
        continue;
      }
      const substitution = previous[j - 1] + 1;
      const deletion = previous[j] + 1;
      const insertion = current[j - 1] + 1;
      const best = Math.min(substitution, deletion, insertion);
      current[j] = best;
      ops[i * width + j] = best === substitution ? 1 : best === deletion ? 2 : 3;
    }
    [previous, current] = [current, previous];
  }

  const path = [];
  let i = expected.length;
  let j = heard.length;
  while (i || j) {
    const op = ops[i * width + j];
    if (op === 0 || op === 1) {
      path.push({ op: op === 0 ? "same" : "substitute", expected: expected[--i], heard: heard[--j], expectedIndex: i });
    } else if (op === 2) {
      path.push({ op: "delete", expected: expected[--i], heard: null, expectedIndex: i });
    } else {
      path.push({ op: "insert", expected: null, heard: heard[--j], expectedIndex: i });
    }
  }
  return { distance: previous[heard.length], path: path.reverse() };
}

function reviewIssues(path, units) {
  const boundaries = [];
  let cursor = 0;
  for (let i = 0; i < units.length; i++) {
    const count = words(units[i].text).length;
    boundaries.push({ from: cursor, to: cursor + count, unit: i + 1, start: units[i].start, end: units[i].end });
    cursor += count;
  }
  const locate = (index) => boundaries.find((boundary) => index >= boundary.from && index < boundary.to) || boundaries.at(-1);

  const issues = [];
  let run = [];
  const flush = () => {
    if (!run.length) return;
    const expectedIndex = run.find((step) => step.expected !== null)?.expectedIndex ?? run[0].expectedIndex;
    const where = locate(expectedIndex);
    issues.push({
      unit: where?.unit,
      startSec: where ? Number(where.start.toFixed(3)) : undefined,
      expected: run.map((step) => step.expected).filter(Boolean).join(" "),
      heard: run.map((step) => step.heard).filter(Boolean).join(" "),
      edits: run.length,
    });
    run = [];
  };
  for (const step of path) {
    if (step.op === "same") flush();
    else run.push(step);
  }
  flush();
  return issues
    .filter((issue) => issue.expected || issue.heard)
    .sort((a, b) => b.edits - a.edits || (a.startSec ?? 0) - (b.startSec ?? 0));
}

export function verifySpeech({ audioPath, units, outPath, workDir, force = false }) {
  const model = findModel();
  const audioHash = sha256(readFileSync(audioPath));
  const expectedText = units.map((unit) => unit.text).join(" ");
  const expectedHash = sha256(expectedText);
  if (!model) {
    const report = { status: "unavailable", reason: "No local Whisper model found.", audioHash, expectedHash };
    writeFileSync(outPath, JSON.stringify(report, null, 2) + "\n");
    return report;
  }

  if (!force && existsSync(outPath)) {
    try {
      const cached = JSON.parse(readFileSync(outPath, "utf8"));
      if (cached.audioHash === audioHash && cached.expectedHash === expectedHash && cached.model === model) {
        return { ...cached, cached: true };
      }
    } catch { /* recalculate a malformed or stale report */ }
  }

  mkdirSync(workDir, { recursive: true });
  const wav = resolve(workDir, "speech-qa.wav");
  const prefix = resolve(workDir, "speech-qa");
  // Check only the essay body. This excludes framing and musical stings while
  // retaining the mastered voice and the delivered background score.
  const start = Math.max(0, units[0].start);
  const end = units.at(-1).end;
  execFileSync("ffmpeg", [
    "-y", "-ss", String(start), "-to", String(end), "-i", audioPath,
    "-vn", "-ac", "1", "-ar", "16000", "-c:a", "pcm_s16le", wav,
  ], { stdio: "ignore" });
  execFileSync("whisper-cli", [
    "-m", model, "-oj", "-np", "-t", "8", "-l", "en", "-of", prefix, wav,
  ], { stdio: ["ignore", "ignore", "inherit"] });

  const raw = JSON.parse(readFileSync(`${prefix}.json`, "utf8"));
  const transcript = (raw.transcription || []).map((segment) => segment.text?.trim()).filter(Boolean).join(" ");
  const expectedWords = words(expectedText);
  const heardWords = words(transcript);
  const aligned = align(expectedWords, heardWords);
  const wer = expectedWords.length ? aligned.distance / expectedWords.length : 1;
  const report = {
    status: wer <= 0.15 ? "pass" : "review",
    generatedAt: new Date().toISOString(),
    audio: audioPath,
    audioHash,
    expectedHash,
    model,
    scope: { startSec: Number(start.toFixed(3)), endSec: Number(end.toFixed(3)) },
    expectedWordCount: expectedWords.length,
    heardWordCount: heardWords.length,
    edits: aligned.distance,
    wordErrorRate: Number(wer.toFixed(4)),
    transcript,
    issues: reviewIssues(aligned.path, units),
    note: "Whisper round-trip checks intelligibility; listen to flagged phrases to judge pronunciation and delivery.",
  };
  writeFileSync(outPath, JSON.stringify(report, null, 2) + "\n");
  rmSync(wav, { force: true });
  rmSync(`${prefix}.json`, { force: true });
  return report;
}
