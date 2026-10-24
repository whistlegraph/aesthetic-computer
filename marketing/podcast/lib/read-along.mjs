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

// ASR prints a spoken number as digits: "270", "250,000", "$1.50". The
// script spells it out, so one printed token would leave several canonical
// words untimed. Spell the digits the way the narrator reads them, so each
// word can take a share of the measured span. Null when it is not a number.
const ONES = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
const TENS = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
function spellInteger(n) {
  if (n < 20) return [ONES[n]];
  if (n < 100) return n % 10 ? [`${TENS[Math.floor(n / 10)]}-${ONES[n % 10]}`] : [TENS[n / 10]];
  for (const [unit, name] of [[1e9, 'billion'], [1e6, 'million'], [1e3, 'thousand'], [100, 'hundred']]) {
    if (n >= unit) return [...spellInteger(Math.floor(n / unit)), name, ...(n % unit ? spellInteger(n % unit) : [])];
  }
  return [String(n)];
}
export function spellNumber(printed) {
  const match = printed.trim().match(/^(\$)?(\d{1,3}(?:,\d{3})+|\d+)(?:\.(\d+))?$/);
  if (!match) return null;
  const [, dollars, whole, fraction] = match;
  const integer = Number(whole.replace(/,/g, ''));
  if (!Number.isSafeInteger(integer) || integer >= 1e12) return null;
  const words = spellInteger(integer);
  if (dollars) {
    words.push(integer === 1 ? 'dollar' : 'dollars');
    if (fraction && Number(fraction)) words.push(...spellInteger(Number(fraction.padEnd(2, '0').slice(0, 2))));
  } else if (fraction) words.push('point', ...[...fraction].map(digit => ONES[Number(digit)]));
  return words;
}

// ASR prints what it hears as one token where the script reads several
// words: "24.22.1" for "twenty-four point twenty-two point one",
// "mail@aesthetic.computer" for "mail at aesthetic dot computer", "9x16" for
// "nine by sixteen", "patchercron" for "patcher cron". Say each script word
// the way ASR would print it, so a run of script words can be matched against
// one printed token and each word can take its share of the measured span.
const SPOKEN_MARKS = { dot: '.', point: '.', at: '@', by: 'x', slash: '/', dash: '-', hyphen: '-', colon: ':', comma: ',', underscore: '_' };
const SMALL = Object.fromEntries([...ONES.map((w, n) => [w, n]), ...TENS.map((w, n) => [w, n * 10]).filter(([w]) => w)]);
const SCALE = { hundred: 100, thousand: 1e3, million: 1e6, billion: 1e9 };
// Read a spelled number from words[i]; null when words[i] does not start one.
function readNumber(words, i) {
  let total = 0, current = 0, j = i, seen = false;
  for (; j < words.length; j++) {
    const word = words[j].toLowerCase();
    if (word === 'and' && seen && current && words[j + 1]?.toLowerCase() in SMALL) continue;
    const parts = word.split('-');
    if (parts.length === 2 && parts[0] in SMALL && SMALL[parts[0]] >= 20 && parts[1] in SMALL && SMALL[parts[1]] < 10) { current += SMALL[parts[0]] + SMALL[parts[1]]; seen = true; continue; }
    if (word in SMALL) { current += SMALL[word]; seen = true; continue; }
    if (word in SCALE) {
      if (!seen) break;
      if (SCALE[word] === 100) current *= 100; else { total += current * SCALE[word]; current = 0; }
      continue;
    }
    break;
  }
  return seen ? { digits: String(total + current), count: j - i } : null;
}
// Chunks of a run of script words: a spelled number (several words, one
// printed digit string), a spoken mark (one word, one printed sign) or a
// plain word. Each chunk carries the characters ASR would print for it.
function chunkSpoken(words) {
  const chunks = [];
  for (let i = 0; i < words.length;) {
    const number = readNumber(words, i);
    if (number) { chunks.push({ words: words.slice(i, i + number.count), printed: number.digits, kind: 'word' }); i += number.count; continue; }
    const mark = SPOKEN_MARKS[words[i].toLowerCase()];
    chunks.push(mark ? { words: [words[i]], printed: mark, kind: 'mark' } : { words: [words[i]], printed: words[i].toLowerCase(), kind: 'word' });
    i++;
  }
  return chunks;
}
const alnum = s => s.toLowerCase().replace(/[^\p{L}\p{N}]/gu, '');
// One printed ASR token that reads as several consecutive script words:
// heard words for each of them, timed by the token's measured characters.
export function splitPrintedRun(segment, scriptWords, nextTime) {
  const tokens = (segment.tokens || []).filter(token => token.t_dtw >= 0 && !token.text.startsWith('[_'));
  if (tokens.length < 2) return null;
  const chars = tokens.flatMap(token => [...token.text].map(ch => ({ ch, at: token.t_dtw * 10, token })));
  const printed = alnum(chars.map(c => c.ch).join(''));
  if (printed.length < 2) return null;
  const isAlnum = ch => /[\p{L}\p{N}]/u.test(ch);
  for (let start = 0; start < scriptWords.length; start++) {
    if (!printed.startsWith(alnum(chunkSpoken([scriptWords[start]])[0].printed).slice(0, 1))) continue;
    for (let count = 2; count <= 8 && start + count <= scriptWords.length; count++) {
      const chunks = chunkSpoken(scriptWords.slice(start, start + count));
      const spoken = alnum(chunks.map(chunk => chunk.printed).join(''));
      if (spoken !== printed) continue;
      // Hand each chunk its printed characters: alnum for words, signs for marks.
      let pointer = 0; const spans = [];
      for (const chunk of chunks) {
        if (chunk.kind === 'word') {
          while (pointer < chars.length && !isAlnum(chars[pointer].ch)) pointer++;
          const from = pointer; let need = alnum(chunk.printed).length;
          while (pointer < chars.length && need) { if (isAlnum(chars[pointer].ch)) need--; pointer++; }
          spans.push({ chunk, from, to: pointer });
        } else {
          while (pointer < chars.length && /\s/.test(chars[pointer].ch)) pointer++;
          const from = pointer;
          if (chars[pointer]?.ch.toLowerCase() === chunk.printed) pointer++;
          else while (pointer < chars.length && !isAlnum(chars[pointer].ch) && !/\s/.test(chars[pointer].ch)) pointer++;
          spans.push({ chunk, from, to: pointer });
        }
      }
      const end = nextTime.get(tokens.at(-1)) ?? segment.offsets.to;
      const timeAt = index => chars[index]?.at ?? end;
      return spans.flatMap(({ chunk, from, to }) => {
        if (to === from) return [];
        const fromMs = timeAt(from);
        let next = to; while (next < chars.length && chars[next].at <= fromMs) next++;
        const toMs = next < chars.length ? chars[next].at : end;
        if (toMs <= fromMs) return [];
        const step = (toMs - fromMs) / chunk.words.length;
        return chunk.words.map((text, k) => ({ text, offsets: { from: Math.round(fromMs + k * step), to: Math.round(fromMs + (k + 1) * step) } }));
      });
    }
  }
  return null;
}

// Whisper's short-segment offsets are heuristic. With -dtw MODEL -ojf,
// t_dtw is the cross-attention token onset, in 10 ms units. End a word at
// its next token (including punctuation), then bound it by the exact edit
// timeline when available. Never stretch a word across a sentence's silence.
// https://github.com/ggml-org/whisper.cpp/blob/master/src/whisper.cpp
export function alignWhisperWords(body, transcription, offsetMs = 0, sentences = []) {
  const hasDtw = transcription.some(segment => segment.tokens?.some(token => token.t_dtw >= 0));
  const scriptWords = [...body.matchAll(/[\p{L}\p{N}]+(?:['’\-][\p{L}\p{N}]+)*/gu)].map(match => match[0]);
  let heard = transcription;
  if (hasDtw) {
    const tokens = transcription.flatMap(segment => segment.tokens || []).filter(token => token.t_dtw >= 0 && !token.text.startsWith('[_'));
    const nextTime = new Map();
    for (let i = 0; i < tokens.length; i++) {
      const next = tokens.slice(i + 1).find(token => token.t_dtw > tokens[i].t_dtw);
      if (next) nextTime.set(tokens[i], next.t_dtw * 10);
    }
    heard = transcription.flatMap(segment => {
      const run = splitPrintedRun(segment, scriptWords, nextTime);
      if (run) return run;
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
      // A printed number the script spells out ("270" for "two hundred
      // seventy") becomes one heard word per spoken word, sharing the span.
      const spelled = spellNumber(segment.text.trim().replace(/[.,!?]+$/, ''));
      if (spelled && spelled.length > 1 && to > from) {
        const lower = body.toLowerCase();
        const phrase = spelled.join(' '), article = spelled[0] === 'one' ? ['a', ...spelled.slice(1)].join(' ') : null;
        const words = lower.includes(phrase) ? spelled : article && lower.includes(article) ? ['a', ...spelled.slice(1)] : null;
        if (words) {
          const step = (to - from) / words.length;
          return words.map((text, k) => ({ text, offsets: { from: Math.round(from + k * step), to: Math.round(from + (k + 1) * step) } }));
        }
      }
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
