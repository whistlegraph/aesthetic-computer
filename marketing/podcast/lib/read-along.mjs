// Align measured ASR word boundaries to canonical prose; missing words stay untimed.
export function alignWords(body, transcription, offsetMs = 0) {
  if (!Number.isFinite(offsetMs) || offsetMs < 0) throw new Error("Expected a nonnegative narration offset");
  const canonical = [...body.matchAll(/[\p{L}\p{N}]+(?:['’\-][\p{L}\p{N}]+)*/gu)].map(m => ({ text: m[0], start: m.index, end: m.index + m[0].length }));
  const heard = transcription.filter(w => w.text.trim() && w.offsets.to > w.offsets.from);
  const normalize = s => s.toLowerCase().replace(/[^\p{L}\p{N}]/gu, '');
  const n = canonical.length, m = heard.length;
  const costs = Array.from({ length: n + 1 }, () => new Float64Array(m + 1));
  const moves = Array.from({ length: n + 1 }, () => []);
  for (let i = 0; i <= n; i++) costs[i][0] = i;
  for (let j = 0; j <= m; j++) costs[0][j] = j;
  for (let i = 1; i <= n; i++) for (let j = 1; j <= m; j++) {
    const choices = [
      [costs[i - 1][j - 1] + (normalize(canonical[i - 1].text) === normalize(heard[j - 1].text) ? 0 : 1), 'pair'],
      [costs[i - 1][j] + 1, 'missing'], [costs[i][j - 1] + 1, 'extra'],
    ];
    if (j > 1 && normalize(canonical[i - 1].text) === normalize(heard[j - 2].text + heard[j - 1].text)) choices.push([costs[i - 1][j - 2], 'merge']);
    choices.sort((a, b) => a[0] - b[0]);
    [costs[i][j], moves[i][j]] = choices[0];
  }
  const words = [];
  let i = n, j = m;
  while (i || j) {
    const move = i && j ? moves[i][j] : i ? 'missing' : 'extra';
    if (move === 'pair' || move === 'merge') {
      const count = move === 'merge' ? 2 : 1;
      const original = canonical[i - 1], first = heard[j - count], last = heard[j - 1];
      words.push({ ...original, fromMs: offsetMs + first.offsets.from, toMs: offsetMs + last.offsets.to,
        match: normalize(original.text) === normalize(heard.slice(j - count, j).map(w => w.text).join('')) ? 'exact' : 'substitution' });
      i--; j -= count;
    } else if (move === 'missing') i--;
    else j--;
  }
  words.reverse();
  return { words, alignment: { method: 'whisper-word-boundaries + text edit alignment', canonicalWords: n, timedWords: words.length, missingWords: n - words.length, substitutions: words.filter(w => w.match === 'substitution').length } };
}
