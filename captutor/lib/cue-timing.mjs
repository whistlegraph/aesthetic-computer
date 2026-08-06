// Word-aligned stage directions for narrated beats.

const normalize = (value) => String(value || "")
  .toLowerCase()
  .replace(/[^a-z0-9]+/g, "");

const normalizedWords = (value) => String(value || "")
  .trim()
  .split(/\s+/)
  .map(normalize)
  .filter(Boolean);

function localizedNarration(value, locale) {
  if (typeof value === "string") return value;
  return value?.[locale] || "";
}

function cueDeclaration(value) {
  if (typeof value === "string") return { phrase:value };
  if (value && typeof value === "object" && typeof value.phrase === "string") return value;
  throw new Error(`invalid action cue ${JSON.stringify(value)}`);
}

export function validateActionCuePlan(screenplay, { locale = "en" } = {}) {
  if (screenplay.actionCuePolicy !== "required") return screenplay;
  for (const [index, beat] of screenplay.beats.entries()) {
    if (typeof beat.do !== "function" || beat.actionCues === false) continue;
    if (!Array.isArray(beat.cues) || beat.cues.length === 0) {
      throw new Error(`${screenplay.slug} beat ${index + 1} performs an action without a word cue`);
    }
    const spoken = normalizedWords(localizedNarration(beat.say, locale));
    for (const raw of beat.cues) {
      const declaration = cueDeclaration(raw);
      const wanted = normalizedWords(declaration.phrase);
      const found = wanted.length > 0 && spoken.some((_, start) =>
        wanted.every((word, offset) => spoken[start + offset] === word));
      if (!found) {
        throw new Error(
          `${screenplay.slug} beat ${index + 1} cue ${JSON.stringify(declaration.phrase)} ` +
          `is not present in the ${locale} narration`,
        );
      }
    }
  }
  return screenplay;
}

export function cueTimeMs(words, phrase, {
  occurrence = 1,
  anchor = "start",
} = {}) {
  const wanted = normalizedWords(phrase);
  if (!wanted.length) throw new Error("cue phrase must contain a word");
  if (!Number.isInteger(occurrence) || occurrence < 1) {
    throw new Error("cue occurrence must be a positive integer");
  }
  if (anchor !== "start" && anchor !== "end") {
    throw new Error(`unsupported cue anchor ${JSON.stringify(anchor)}`);
  }

  const spoken = (words || []).map((word) => normalize(word.text));
  let seen = 0;
  for (let i = 0; i <= spoken.length - wanted.length; i += 1) {
    if (!wanted.every((word, offset) => spoken[i + offset] === word)) continue;
    seen += 1;
    if (seen !== occurrence) continue;
    const matched = anchor === "end" ? words[i + wanted.length - 1] : words[i];
    const value = anchor === "end" ? matched.toMs : matched.fromMs;
    if (!Number.isFinite(value)) throw new Error(`cue ${JSON.stringify(phrase)} has no ${anchor} time`);
    return value;
  }
  throw new Error(`cue phrase ${JSON.stringify(phrase)} was not found in the narration`);
}

export function createBeatCue({ beat, startedAt, now, sleep, onCue = () => {} }) {
  return async (phrase, options = {}) => {
    const targetMs = cueTimeMs(beat.words, phrase, options);
    const leadMs = Number(options.leadMs || 0);
    if (!Number.isFinite(leadMs) || leadMs < 0) {
      throw new Error("cue leadMs must be a non-negative number");
    }
    const elapsedMs = Math.max(0, (now() - startedAt) * 1000);
    const waitMs = Math.max(0, targetMs - leadMs - elapsedMs);
    if (waitMs > 0) await sleep(waitMs);
    const result = {
      phrase,
      anchor:options.anchor || "start",
      occurrence:options.occurrence || 1,
      targetMs,
      leadMs,
      waitMs:+waitMs.toFixed(3),
    };
    onCue(result);
    return result;
  };
}

export function createBeatCuePlan({ beat, cue, required = false }) {
  const declarations = (beat.cues || []).map(cueDeclaration);
  let consumed = 0;
  return {
    async next() {
      const declaration = declarations[consumed];
      if (!declaration) {
        if (required) throw new Error(`beat ${beat.index + 1} has no remaining action cue`);
        return null;
      }
      consumed += 1;
      const { phrase, ...options } = declaration;
      return cue(phrase, options);
    },
    assertActionReady(kind) {
      if (required && consumed === 0) {
        throw new Error(`beat ${beat.index + 1} attempted ${kind} before nextCue()`);
      }
    },
    assertComplete() {
      if (required && consumed !== declarations.length) {
        throw new Error(
          `beat ${beat.index + 1} consumed ${consumed} of ${declarations.length} action cues`,
        );
      }
    },
  };
}
