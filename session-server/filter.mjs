import {
  RegExpMatcher,
  englishDataset,
  englishRecommendedTransformers,
} from "obscenity";

const matcher = new RegExpMatcher({
  ...englishDataset.build(),
  ...englishRecommendedTransformers,
});

// Filter text for profanities by replacing them with underscores.
export function filter(text) {
  let out = text;
  if (matcher.hasMatch(text)) {
    console.log("🫢 Profanities found in:", text);
    const matches = matcher.getAllMatches(text, true);
    for (const match of matches) {
      const { startIndex: start, endIndex: end } =
        englishDataset.getPayloadWithPhraseMetadata(match);
      const len = end - start + 1; // Calc the matched string length.
      // Replace the corresponding portion in the target string.
      out = out.substring(0, start) + "_".repeat(len) + out.substring(end + 1);
    }
  }
  return out;
}