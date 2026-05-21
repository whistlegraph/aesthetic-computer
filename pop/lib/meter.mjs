// meter.mjs — rhythm-structure helpers for /pop.
//
//   fibSequence(count)        -> [1, 2, 3, 5, 8, …]
//   fibPartition(n)           -> Fibonacci addends that sum to n
//   fibMeter(totalBars, opts) -> section bar-lengths from the sequence
//   subdivideCell(cell, n)    -> one .np cell split into n equal cells
//   subdivideNp(text, opts)   -> subdivide notes across a whole .np score
//
// "Fibonacci division and addition for measures/bars" — divide a bar of
// N beats into Fibonacci groups, or build a form whose section lengths
// climb the sequence. "Note subdivision" — the Ableton move of cutting a
// long note into 1/2 · 1/4 · 1/8 · 1/16 notes.

// ── Fibonacci ─────────────────────────────────────────────────────────
// The sequence with no leading 1,1 duplicate — useful as distinct
// rhythmic group sizes: 1, 2, 3, 5, 8, 13, 21 …
export function fibSequence(count) {
  const seq = [1, 2];
  while (seq.length < count) seq.push(seq[seq.length - 1] + seq[seq.length - 2]);
  return seq.slice(0, Math.max(0, count));
}

// The "golden division" of a bar of `n` beats into two Fibonacci-leaning
// parts: the larger is the biggest Fibonacci number strictly less than
// n, the smaller is the remainder. 8 -> [5,3], 13 -> [8,5], 7 -> [5,2].
// Recurse on the parts for nested groupings. n <= 2 bottoms out.
export function fibPartition(n) {
  const v = Math.max(0, Math.round(n));
  if (v === 0) return [];
  if (v <= 2) return v === 2 ? [1, 1] : [1];
  const fibs = [1, 2];
  while (fibs[fibs.length - 1] < v)
    fibs.push(fibs[fibs.length - 1] + fibs[fibs.length - 2]);
  let larger = 1;
  for (const f of fibs) if (f < v) larger = f;
  return [larger, v - larger];
}

// Build a form: lay section lengths along the Fibonacci sequence until
// they sum to `totalBars`. mode "additive" climbs (1,2,3,5,8…), mode
// "descending" falls from the largest fitting value. The final section
// is clamped so the lengths sum exactly to totalBars.
export function fibMeter(totalBars, opts = {}) {
  const mode = opts.mode ?? "additive";
  const total = Math.max(1, Math.round(totalBars));
  let pool = fibSequence(16).filter((v) => v <= total);
  if (mode === "descending") pool = pool.slice().reverse();

  const sections = [];
  let used = 0, idx = 0;
  while (used < total) {
    let len = pool[Math.min(idx, pool.length - 1)];
    if (used + len > total) len = total - used;
    sections.push(len);
    used += len;
    idx++;
  }
  return sections;
}

// ── .np note subdivision ──────────────────────────────────────────────
// A .np cell is `NOTE:syllable*weight` — e.g. `E5:_*2`, or a chord
// `A4+C5+E5:_*2`. splitting keeps the note(s) and syllable, dividing the
// weight into `n` equal cells. `E5:_*2` ÷ 3 -> `E5:_*0.667 ×3`.
export function subdivideCell(cell, n) {
  const parts = Math.max(1, Math.floor(n));
  const m = cell.match(/^(.*?):([^*]*)\*([\d.]+)$/);
  if (!m) return [cell]; // not a weighted cell — leave it
  const [, notes, syll, weight] = m;
  const w = parseFloat(weight) / parts;
  const wStr = Number(w.toFixed(3)).toString();
  return Array.from({ length: parts }, () => `${notes}:${syll}*${wStr}`);
}

// Walk a whole .np score and subdivide cells. By default every cell is
// split by `parts`; set `minBeats` to only cut notes at least that long
// (the Ableton "divide the big note" behaviour). Comments and `# section`
// headers pass through untouched.
//
// opts: { parts (default 2), minBeats (default 0) }
export function subdivideNp(text, opts = {}) {
  const parts    = Math.max(1, Math.floor(opts.parts ?? 2));
  const minBeats = opts.minBeats ?? 0;
  return text
    .split("\n")
    .map((line) => {
      const trimmed = line.trim();
      if (trimmed === "" || trimmed.startsWith("#")) return line;
      return line.replace(/\S+/g, (tok) => {
        const m = tok.match(/\*([\d.]+)$/);
        if (!m || parseFloat(m[1]) < minBeats) return tok;
        return subdivideCell(tok, parts).join(" ");
      });
    })
    .join("\n");
}
