// filter, 24.04.10.19.23
// This module filters words for obscenities.

/* #region 🏁 TODO 
  - [🟡] Populate the list of replacement "niceties".
#endregion */

const niceties = ["apple", "banana"];

import {
  RegExpMatcher,
  englishDataset,
  englishRecommendedTransformers,
  DataSet,
  pattern,
} from "obscenity";

const aestheticDataset = new DataSet()
  .addAll(englishDataset)
  .addPhrase((phrase) =>
    phrase
      .setMetadata({ originalWord: "beaner" })
      .addPattern(pattern`bean er`)
      .addPattern(pattern`beaner`),
  )
  .addPhrase((phrase) =>
    phrase
      .setMetadata({ originalWord: "nigger" })
      .addPattern(pattern`n!gger`)
      .addPattern(pattern`n*gger`),
  )
  .addPhrase((phrase) =>
    phrase.setMetadata({ originalWord: "shit" }).addPattern(pattern`sh*t`),
  );

const matcher = new RegExpMatcher({
  ...aestheticDataset.build(),
  ...englishRecommendedTransformers,
});

const whitelist = ["rapper"];

// Filter text for profanities by replacing them with underscores and log matches.
export function filter(text) {
  let out = text;
  if (matcher.hasMatch(text)) {
    console.log("🫢 Profanities found in:", text);
    const matches = matcher.getAllMatches(text, true);
    for (const match of matches) {
      // console.log("🧨 Match:", match);
      const phraseMetadata =
        aestheticDataset.getPayloadWithPhraseMetadata(match);
      // console.log("🧨 Match details:", phraseMetadata);
      let { startIndex: start, endIndex: end } = phraseMetadata;
      // Extend end index to the next space or the end of the string
      const nextSpaceIndex = text.indexOf(" ", end);
      if (nextSpaceIndex !== -1) {
        end = nextSpaceIndex;
      } else {
        end = text.length;
      }
      const len = end - start;
      const matchedSubstring = text.substring(start, end);
      // console.log("🧨 Matched substring:", matchedSubstring);

      if (!whitelist.includes(matchedSubstring)) {
        out = out.substring(0, start) + "_".repeat(len) + out.substring(end);
      }
    }
  }
  return out;
}
