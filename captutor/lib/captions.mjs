// Shared caption segmentation for burned pixels, embedded subtitles, and VTT.
// One timeline must drive every representation; two slightly different split
// rules can otherwise put a soft cue over the next burned phrase.

const HAS_WORD = /[\p{L}\p{N}]/u;

// Karaoke highlights need a visible letter or number. Languages such as French
// can emit punctuation as its own timed token (for example the space before
// ":"); keep that token in the base phrase without rasterizing an empty-looking
// highlight layer for it.
export function isHighlightableCaptionToken(text) {
  return HAS_WORD.test(text);
}

export function captionPhrases(beats, { maxWords = 6, pauseMs = 380 } = {}) {
  const out = [];
  for (const beat of beats) {
    let current = [];
    const flush = () => {
      if (!current.length) return;
      const words = current.map((word) => ({
        text: word.text,
        from: beat.offsetSec + word.fromMs / 1000,
        to: beat.offsetSec + word.toMs / 1000,
      }));
      const text = words.map((word) => word.text).join(" ");
      if (!HAS_WORD.test(text)) {
        const previous = out[out.length - 1];
        if (previous) {
          previous.text += ` ${text}`;
          previous.to = words.at(-1).to;
          previous.words.push(...words);
        }
        current = [];
        return;
      }
      out.push({
        from:words[0].from, to:words.at(-1).to, text, words,
        color:beat.captionColor || null,
      });
      current = [];
    };

    for (const [index, word] of beat.words.entries()) {
      current.push(word);
      const next = beat.words[index + 1];
      const punctuation = /[.!?,—]$/.test(word.text);
      const pause = next && next.fromMs - word.toMs >= pauseMs;
      if (punctuation || pause || current.length >= maxWords) flush();
    }
    flush();
  }

  // Clamp any rounding-level collision defensively. Two phrases must never
  // paint at once, even if a future voice provider returns overlapping words.
  for (let index = 0; index + 1 < out.length; index += 1) {
    if (out[index].to > out[index + 1].from) {
      const boundary = (out[index].to + out[index + 1].from) / 2;
      out[index].to = boundary;
      out[index + 1].from = boundary;
    }
  }
  return out;
}
