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
    phrase.setMetadata({ originalWord: "nig" }).addPattern(pattern`nlg`),
  )
  .addPhrase((phrase) =>
    phrase.setMetadata({ originalWord: "n i g a" }).addPattern(pattern`n ig a`),
  )
  .addPhrase((phrase) =>
    phrase.setMetadata({ originalWord: "fuck" }).addPattern(pattern`f u ck`),
  )
  .addPhrase((phrase) =>
    phrase.setMetadata({ originalWord: "pegging" }).addPattern(pattern`pegging`),
  )
  .addPhrase((phrase) =>
    phrase
      .setMetadata({ originalWord: "nigger" })
      .addPattern(pattern`n!gger`)
      .addPattern(pattern`nig hair`)
      .addPattern(pattern`n ig a`)
      .addPattern(pattern`n_?_g_g_e_r`)
      .addPattern(pattern`n?66er`)
      .addPattern(pattern`n***er`)
      .addPattern(pattern`reggin`)
      .addPattern(pattern`n*gger`),
  );

const matcher = new RegExpMatcher({
  ...aestheticDataset.build(),
  ...englishRecommendedTransformers,
});

const whitelist = ["rapper", "arse"];

// Regex to match URLs (http://, https://, or www.)
const urlRegex = /(https?:\/\/[^\s]+|www\.[^\s]+)/gi;

// Filter text for profanities by replacing them with underscores and optionally log matches.
// URLs are protected from filtering.
export function filter(text, debug = false) {
  // Extract and protect URLs before filtering
  const urls = [];
  const placeholder = "\u0000URL\u0000"; // Use null chars as unlikely placeholder
  let protectedText = text.replace(urlRegex, (match) => {
    urls.push(match);
    return placeholder + (urls.length - 1) + placeholder;
  });

  let out = protectedText.replaceAll("n166er", "n!gger");
  out = out.replace(new RegExp("n i g g e r", "gi"), "n_i_g_g_e_r");

  if (matcher.hasMatch(out)) {
    if (debug) {
      console.log("🫢 Profanities found in:", text);
    }
    const matches = matcher.getAllMatches(out, true);
    for (const match of matches) {
      if (debug) {
        console.log("🧨 Match:", match);
      }
      const phraseMetadata =
        aestheticDataset.getPayloadWithPhraseMetadata(match);
      if (debug) {
        console.log("🧨 Match details:", phraseMetadata);
      }
      let { startIndex: start, endIndex: end } = phraseMetadata;
      // Extend end index to the next space or the end of the string
      const nextSpaceIndex = out.indexOf(" ", end);
      if (nextSpaceIndex !== -1) {
        end = nextSpaceIndex;
      } else {
        end = out.length;
      }
      const len = end - start;
      const matchedSubstring = out.substring(start, end);
      if (debug) {
        console.log("🧨 Matched substring:", matchedSubstring);
      }

      // Skip if the match is inside a URL placeholder
      if (!matchedSubstring.includes(placeholder) && !whitelist.includes(matchedSubstring)) {
        out = out.substring(0, start) + "_".repeat(len) + out.substring(end);
      }
    }
  }

  // Restore URLs
  out = out.replace(new RegExp(placeholder + "(\\d+)" + placeholder, "g"), (_, index) => {
    return urls[parseInt(index, 10)];
  });

  // console.log("🧹 Post-filtered message:", out);
  return out;
}