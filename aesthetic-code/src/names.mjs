// names.mjs — pronounceable random piece names.
//
// A session's blank piece needs a name before anyone has decided what the
// piece is. These are the same shape as Slab's prompt-rock pet names:
// alternating consonant and vowel syllables, easy to say out loud and short
// enough to keep the QR code small.
import { randomBytes } from "node:crypto";

const CONSONANTS = "bdfgklmnprstvz";
const VOWELS = "aeiou";

function pick(alphabet, byte) {
  return alphabet[byte % alphabet.length];
}

// Two or three syllables: movika, tesu, blorenda-free and always typeable.
export function randomSlug({ syllables = 0, bytes = randomBytes(8) } = {}) {
  const count = syllables || 2 + (bytes[0] % 2);
  let name = "";
  for (let index = 0; index < count; index += 1) {
    name += pick(CONSONANTS, bytes[(index * 2 + 1) % bytes.length]);
    name += pick(VOWELS, bytes[(index * 2 + 2) % bytes.length]);
  }
  return name;
}

// The code channel is a shared secret: anyone who knows it can push source to
// whatever is watching. It is never shown as a name, so make it unguessable.
// Eight base64url characters is 48 bits of entropy, and keeps the scan URL
// under the 53 bytes that fit a version-3 QR code — the largest code that
// still fits a terminal corner.
export function randomChannel(bytes = randomBytes(8)) {
  return Buffer.from(bytes).toString("base64url").slice(0, 8);
}
