// words.mjs — whisper tokens in, real words out.
//
// whisper.cpp with `-ml 1` gives one segment per *token*, not per word, so
// "Y Combinator" arrives as ["Y", " Com", "bin", "ator"] and a naive caption
// renderer would flash "ator" at you. The tell is the leading space: a token
// that starts one begins a new word, and everything after it — subword pieces,
// clitics like "'m", trailing commas — belongs to the word before it.
//
//   whisper-cli -m <model> -f audio.wav -oj -ojf -ml 1 -of out
//   import { wordsFromWhisper } from "../lib/words.mjs";
//   const words = wordsFromWhisper("out.json");   // [{ text, fromMs, toMs }]
//
// The {text, fromMs, toMs} shape is the repo's caption schema — the same one
// marketing/bin/align-captions.mjs writes and captions-train.mjs consumes.

import { readFileSync } from "node:fs";

export function wordsFromWhisper(jsonPath) {
  const raw = JSON.parse(readFileSync(jsonPath, "utf8"));
  const words = [];

  for (const seg of raw.transcription) {
    const piece = seg.text;
    if (!piece || !piece.trim()) continue;

    const startsWord = piece.startsWith(" ");
    const text = piece.trim();
    const from = seg.offsets.from;
    const to = seg.offsets.to;

    // A token with no leading space continues the word we're already building —
    // unless there is no such word yet, at which point it starts one.
    if (startsWord || words.length === 0) {
      words.push({ text, fromMs: from, toMs: to });
    } else {
      const prev = words[words.length - 1];
      prev.text += text;
      prev.toMs = to; // the word isn't finished until its last piece is
    }
  }

  // Bare punctuation ("," on its own) is not a word; glue it back on so it
  // doesn't get its own karaoke beat.
  const merged = [];
  for (const w of words) {
    if (/^[^\w'"“”‘’(]+$/.test(w.text) && merged.length > 0) {
      const prev = merged[merged.length - 1];
      prev.text += w.text;
      prev.toMs = Math.max(prev.toMs, w.toMs);
    } else {
      merged.push(w);
    }
  }
  return merged;
}
