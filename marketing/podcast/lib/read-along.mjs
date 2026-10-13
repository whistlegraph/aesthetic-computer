// Assemble all spoken sections without replacing the author's body with ASR.
export function spokenTranscript(body, narration) {
  if (!Array.isArray(narration?.cues) || !narration.cues.length) throw new Error('Expected a complete spoken timeline');
  const cues = narration.cues;
  let end = 0, role = -1;
  for (const cue of cues) {
    const order = ['intro', 'body', 'outro'].indexOf(cue.role);
    if (order < 0 || order < role || !cue.text?.trim() || !Number.isFinite(cue.start) ||
        !Number.isFinite(cue.end) || cue.start < end || cue.end <= cue.start) throw new Error('Expected ordered spoken sections');
    end = cue.end; role = order;
  }
  if (!cues.some(cue => cue.role === 'body')) throw new Error('Spoken timeline has no body');
  const sections = [], parts = [];
  let offset = 0;
  for (const role of ['intro', 'body', 'outro']) {
    const group = cues.filter(cue => cue.role === role);
    if (!group.length) continue;
    const text = role === 'body' ? body : group.map(cue => cue.text).join(' ');
    if (parts.length) offset += 2;
    sections.push({ role, start: offset, end: offset + text.length,
      fromMs: Math.round(group[0].start * 1000), toMs: Math.round(group.at(-1).end * 1000) });
    parts.push(text); offset += text.length;
  }
  return { body: parts.join('\n\n'), sections,
    sentences: cues.map(cue => ({ text: cue.text, from: Math.round(cue.start * 1000), to: Math.round(cue.end * 1000) })) };
}

// Align measured ASR word boundaries to canonical prose; missing words stay untimed.
export function alignWords(body, transcription, offsetMs = 0) {
  if (!Number.isFinite(offsetMs) || offsetMs < 0) throw new Error("Expected a nonnegative narration offset");
  const canonical = [...body.matchAll(/[\p{L}\p{N}]+(?:['’\-][\p{L}\p{N}]+)*/gu)].map(m => ({ text: m[0], start: m.index, end: m.index + m[0].length }));
  const heard = transcription.filter(w => /[\p{L}\p{N}]/u.test(w.text) && w.offsets.to > w.offsets.from);
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

export function sentenceCues(srt) {
  const milliseconds = stamp => {
    const [hours, minutes, seconds, ms] = stamp.split(/[:,]/).map(Number);
    return ((hours * 60 + minutes) * 60 + seconds) * 1000 + ms;
  };
  return srt.trim().split(/\r?\n\s*\r?\n/).map(block => {
    const match = block.match(/^\d+\r?\n(\d\d:\d\d:\d\d,\d{3}) --> (\d\d:\d\d:\d\d,\d{3})\r?\n([\s\S]+)$/);
    if (!match) throw new Error("Expected sentence SRT from the mastered timeline");
    return { from: milliseconds(match[1]), to: milliseconds(match[2]), text: match[3] };
  });
}

// Whisper's short-segment offsets are heuristic. With -dtw MODEL -ojf,
// t_dtw is the cross-attention token onset, in 10 ms units. End a word at
// its next token (including punctuation), then bound it by the exact edit
// timeline when available. Never stretch a word across a sentence's silence.
// https://github.com/ggml-org/whisper.cpp/blob/master/src/whisper.cpp
export function alignWhisperWords(body, transcription, offsetMs = 0, sentences = []) {
  const hasDtw = transcription.some(segment => segment.tokens?.some(token => token.t_dtw >= 0));
  let heard = transcription;
  if (hasDtw) {
    const tokens = transcription.flatMap(segment => segment.tokens || []).filter(token => token.t_dtw >= 0 && !token.text.startsWith('[_'));
    const nextTime = new Map();
    for (let i = 0; i < tokens.length; i++) {
      const next = tokens.slice(i + 1).find(token => token.t_dtw > tokens[i].t_dtw);
      if (next) nextTime.set(tokens[i], next.t_dtw * 10);
    }
    heard = transcription.flatMap(segment => {
      // A spoken domain is often printed as one ASR word. Its internal dot
      // still has a measured token onset: retain name / dot / domain cues.
      const printedDomain = segment.text.trim().replace(/[.,!?]+$/, '');
      if (/^[\p{L}]+(?:\.[\p{L}]+)+$/u.test(printedDomain) &&
          body.toLowerCase().includes(printedDomain.replace(/\./g, ' dot ').toLowerCase())) {
        const measured = (segment.tokens || []).filter(token => token.t_dtw >= 0 && !token.text.startsWith('[_'));
        const groups = []; let group = [];
        for (let i = 0; i < measured.length; i++) {
          const token = measured[i];
          if (token.text === '.' && group.length && /[\p{L}]/u.test(measured[i + 1]?.text || '')) {
            groups.push(group, [token]); group = [];
          } else if (/[\p{L}\p{N}]/u.test(token.text)) group.push(token);
        }
        if (group.length) groups.push(group);
        if (groups.length > 1) return groups.flatMap(group => {
          const from = group[0].t_dtw * 10, to = nextTime.get(group.at(-1)) ?? segment.offsets.to;
          const text = group.length === 1 && group[0].text === '.' ? 'dot' : group.map(token => token.text).join('');
          return to > from ? [{ text, offsets: { from, to } }] : [];
        });
      }
      const lexical = (segment.tokens || []).filter(token => token.t_dtw >= 0 && !token.text.startsWith('[_') && /[\p{L}\p{N}]/u.test(token.text));
      if (!lexical.length) return [];
      const from = lexical[0].t_dtw * 10;
      const to = nextTime.get(lexical.at(-1)) ?? segment.offsets.to;
      // ASR sometimes prints a spoken quantity as “8GB”. Preserve the two
      // separately measured tokens so canonical “eight gigabytes” can align.
      if (/^\s*\d+[\p{L}]+[.,!?]?$/u.test(segment.text) && /^\s*\d+$/.test(lexical[0].text) && lexical.length > 1) {
        const split = lexical[1].t_dtw * 10;
        if (from < split && split < to) return [
          { text: lexical[0].text, offsets: { from, to: split } },
          { text: lexical.slice(1).map(token => token.text).join(''), offsets: { from: split, to } },
        ];
      }
      return to > from ? [{ text: segment.text, offsets: { from, to } }] : [];
    });
  }
  const result = alignWords(body, heard, offsetMs);
  if (sentences.length) {
    const bounds = alignWords(body, sentences.flatMap(cue => [...cue.text.matchAll(/[\p{L}\p{N}]+(?:['’\-][\p{L}\p{N}]+)*/gu)].map(match => ({ text: match[0], offsets: { from: cue.from, to: cue.to } }))));
    const byStart = new Map(bounds.words.map(word => [word.start, word]));
    result.words = result.words.flatMap(word => {
      const cue = byStart.get(word.start);
      if (!cue) return [word];
      const fromMs = Math.max(word.fromMs, cue.fromMs), toMs = Math.min(word.toMs, cue.toMs);
      return toMs > fromMs ? [{ ...word, fromMs, toMs }] : [];
    });
  }
  result.alignment = { ...result.alignment,
    method: `${hasDtw ? 'whisper-dtw-token-onsets' : 'whisper-word-boundaries'} + text edit alignment${sentences.length ? ' + sentence timeline bounds' : ''}`,
    timedWords: result.words.length, missingWords: result.alignment.canonicalWords - result.words.length,
    substitutions: result.words.filter(word => word.match === 'substitution').length,
  };
  return result;
}
