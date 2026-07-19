// align-words.mjs — reconcile whisper word timestamps against the
// canonical score word list. (Canonical home: spinging/lib — moved from
// pop/bin/align-words.mjs, which now re-exports from here.)
//
// Whisper occasionally splits/merges/substitutes words ("twas" → "to as",
// "tis" → "to his", "am" → "I'm") which cascades the entire downstream
// pitch alignment. This module walks both sequences and produces, for
// each score word, a (fromMs, toMs) window from the source audio.
//
// Strategy: greedy walk with three operations:
//   match   — current pair are the same word (token-equal or fuzzy)
//   merge   — score word == whisper[j] + whisper[j+1] concatenated
//             (covers "twas" = "to" + "as")
//   skip-w  — whisper has an inserted/spurious word; skip it
//             (covers whisper hearing "to his" where score expects "tis")
//
// Falls back to taking the current whisper time as a best-effort guess
// when no rule fires, so we never lose sync entirely.
//
// export: alignWords(scoreWords: string[], whisperWords: {text,fromMs,toMs}[])
//   → { fromMs, toMs }[] of length scoreWords.length

const norm = (s) => (s || "").toLowerCase().replace(/[^a-z']/g, "");

// Levenshtein with early reject. Returns true iff edit distance > max.
function editsExceed(a, b, max) {
  if (Math.abs(a.length - b.length) > max) return true;
  const m = a.length, n = b.length;
  let prev = new Array(n + 1);
  let curr = new Array(n + 1);
  for (let j = 0; j <= n; j++) prev[j] = j;
  for (let i = 1; i <= m; i++) {
    curr[0] = i;
    let row_min = i;
    for (let j = 1; j <= n; j++) {
      curr[j] = a[i - 1] === b[j - 1]
        ? prev[j - 1]
        : 1 + Math.min(prev[j - 1], prev[j], curr[j - 1]);
      if (curr[j] < row_min) row_min = curr[j];
    }
    if (row_min > max) return true;
    [prev, curr] = [curr, prev];
  }
  return prev[n] > max;
}

function fuzzyMatch(a, b) {
  if (!a || !b) return false;
  if (a === b) return true;
  const ax = a.replace(/'/g, ""), bx = b.replace(/'/g, "");
  if (ax === bx) return true;
  // Short words (≤ 4 chars): allow 1 edit. Longer: allow up to 2.
  const maxLen = Math.max(ax.length, bx.length);
  const tol = maxLen <= 4 ? 1 : 2;
  if (!editsExceed(ax, bx, tol)) return true;
  if (a.length >= 3 && b.length >= 3 && (a.startsWith(b) || b.startsWith(a))) return true;
  return false;
}

// Merge tolerance — score word vs concatenated whisper pair. More
// permissive (covers "twas" ≈ "toas", "tis" ≈ "tohis").
function fuzzyMerge(scoreW, mergedW) {
  if (scoreW === mergedW) return true;
  const a = scoreW.replace(/'/g, ""), b = mergedW.replace(/'/g, "");
  if (a === b) return true;
  // Allow edits up to half the merged length (e.g. tohis→tis = 2 edits, length 5)
  const tol = Math.min(3, Math.ceil(Math.max(a.length, b.length) / 2));
  return !editsExceed(a, b, tol);
}

export function alignWords(scoreWords, whisperWords) {
  const out = new Array(scoreWords.length);
  let i = 0, j = 0;
  let lastEnd = 0;
  while (i < scoreWords.length) {
    if (j >= whisperWords.length) {
      // Out of whisper data — extrapolate from last
      out[i] = { fromMs: lastEnd, toMs: lastEnd + 200 };
      lastEnd += 200; i++; continue;
    }
    const s = norm(scoreWords[i]);
    const w = norm(whisperWords[j].text);
    // 1. direct match
    if (fuzzyMatch(s, w)) {
      out[i] = { fromMs: whisperWords[j].fromMs, toMs: whisperWords[j].toMs };
      lastEnd = whisperWords[j].toMs;
      i++; j++; continue;
    }
    // 2. merge: score == whisper[j] + whisper[j+1] (with fuzzy tolerance)
    if (j + 1 < whisperWords.length) {
      const merged = w + norm(whisperWords[j + 1].text);
      if (fuzzyMerge(s, merged)) {
        out[i] = { fromMs: whisperWords[j].fromMs, toMs: whisperWords[j + 1].toMs };
        lastEnd = whisperWords[j + 1].toMs;
        i++; j += 2; continue;
      }
    }
    // 3. skip whisper if its NEXT word matches the current score word
    if (j + 1 < whisperWords.length) {
      const next = norm(whisperWords[j + 1].text);
      if (fuzzyMatch(s, next)) {
        j++; continue;        // skip the spurious whisper word
      }
      // Or score[i] + score[i+1] == whisper[j] (rare 1-to-2 split)
      if (i + 1 < scoreWords.length) {
        const merged_score = s + norm(scoreWords[i + 1]);
        if (fuzzyMatch(merged_score, w)) {
          // share whisper window across two score words
          const mid = Math.floor((whisperWords[j].fromMs + whisperWords[j].toMs) / 2);
          out[i] = { fromMs: whisperWords[j].fromMs, toMs: mid };
          out[i + 1] = { fromMs: mid, toMs: whisperWords[j].toMs };
          lastEnd = whisperWords[j].toMs;
          i += 2; j++; continue;
        }
      }
    }
    // 4. fallback — accept the current whisper as best guess
    out[i] = { fromMs: whisperWords[j].fromMs, toMs: whisperWords[j].toMs };
    lastEnd = whisperWords[j].toMs;
    i++; j++;
  }
  return out;
}
