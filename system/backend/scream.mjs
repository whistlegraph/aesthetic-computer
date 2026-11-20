// Scream, 26.11.20
// How a line becomes a scream in jeffrey-pvc (eleven_v4): which delivery it
// gets, what is sent to ElevenLabs, and how the returned character timing
// becomes word timing. The deliveries are the ones `pop/frothyscreama`
// measured: a descriptive tag, stability 0, style 1, the words left in
// capitals with a held vowel.

import crypto from "crypto";

export const VOICE = "ZXoQQp5X0PKHGwyZpVIT"; // jeffrey-pvc
export const MODEL = "eleven_v4";
export const MAX_CHARS = 120;

const WILD = { stability: 0, similarity_boost: 0.9, style: 1, use_speaker_boost: true, speed: 1 };
const HOT = { ...WILD, stability: 0.3, style: 0.8 };

// Tags for a line with words in it.
export const TAGS = [
  "[throat-shredding hardcore scream]",
  "[screaming, losing his mind]",
  "[frantic, screaming, out of control]",
  "[voice cracking, screaming]",
  "[screaming in rage, breaking down]",
  "[screaming until his voice gives out]",
  "[chaotic screaming, several outbursts]",
  "[war cry, screaming at the top of his lungs]",
];

// A scream with no words: the chaos bank's keepers, most chaotic first.
// [tag, sound, settings]
export const WORDLESS = [
  ["[throat-shredding hardcore scream]", "BLEEEEEEEEGGGHHHHH! YAAAAAAAAAAAGGHH!", WILD],
  ["[screaming at the top of his lungs]", "YYYEEEEEAAAAAAAAAAAAAAAHHHH!", HOT],
  ["[voice cracking, screaming]", "AAAAaaaAAAAHHhhAAAA!", HOT],
  ["[frantic gibbering, screaming, out of control]", "AH AH AAH YAH GAH AAAGH RAH AH AH AAAAAH!", WILD],
  ["[screaming until his voice gives out]", "AAAAAAAAAAAAAAAAAAAAAAAAAH— aaaaaaaaaaahhhhh... hhhhh.", { ...WILD, speed: 0.7 }],
  ["[pig squeal, grindcore vocal]", "REEEEEEEEEEEEEEEEEEEEEEE!", WILD],
  ["[screaming, losing his mind]", "AAAAAAH! AAAAAAAAAAAH!! AAAAAAAAAAAAAAAAAAAAAH!!!", WILD],
  ["[shrieking in terror, piercing]", "EEEEEEEEEEEEEEEEEEEEEEEEEEEK! EEEEEEEEEEEEEEEEEEE!", WILD],
  ["[chaotic screaming, several outbursts]", "RAAAGH! AAAH! EEEYAAAH! GRRRAAAH! AAAAAAAAAAAAAH!", WILD],
  ["[hysterical screaming, voice breaking]", "aaaAAAAaaaAAAAHHH HAAAA AAAAAH!", HOT],
];

// What the caller typed, made safe to send: one line, no tags of their own.
export function clean(text) {
  return String(text ?? "").replace(/[\[\]]/g, " ").replace(/\s+/g, " ").trim().slice(0, MAX_CHARS);
}

// The last word's last vowel is held, the way the lyric lines were respelt.
function hold(word) {
  const at = word.search(/[AEIOU](?=[^AEIOU]*$)/);
  return at < 0 ? word : word.slice(0, at) + word[at].repeat(4) + word.slice(at + 1);
}

// The request for a line. An empty line asks for a wordless take; `take`
// steps through the deliveries, so the same words can be screamed again
// another way. The same line and take always make the same request.
export function shape(text, take = 0) {
  const line = clean(text);
  const pick = (list) => list[(parseInt(crypto.createHash("sha1").update(line).digest("hex").slice(0, 8), 16) + take) % list.length];
  let tag, said, settings = WILD;
  if (line) {
    tag = pick(TAGS);
    const words = line.toUpperCase().split(" ");
    words.push(hold(words.pop()));
    said = words.join(" ");
    if (!/[!?]$/.test(said)) said += "!";
  } else {
    [tag, said, settings] = WORDLESS[take % WORDLESS.length];
  }
  const body = { text: `${tag} ${said}`, model_id: MODEL, voice_settings: settings };
  const id = crypto.createHash("sha256").update(`scream:${VOICE}:${JSON.stringify(body)}`).digest("hex");
  return { id, body, tag, said, text: line ? line.toUpperCase() : said };
}

// Word times from `/with-timestamps`. The tag is not a word, whether or not
// the model reports its characters. Each word carries what to show for it:
// the caller's own word when the counts agree, else what was sent.
export function wordTimes(alignment, text) {
  const { characters: chars, character_start_times_seconds: starts, character_end_times_seconds: ends } = alignment;
  const words = [];
  let current = null, depth = 0;
  for (let i = 0; i < chars.length; i += 1) {
    if (chars[i] === "[") depth += 1;
    if (depth > 0 || /\s/.test(chars[i])) current = null;
    else if (current) { current.w += chars[i]; current.e = ends[i]; }
    else words.push((current = { w: chars[i], s: starts[i], e: ends[i] }));
    if (chars[i] === "]") depth = Math.max(0, depth - 1);
  }
  const shown = text.split(" ");
  return words.map((w, i) => ({
    w: shown.length === words.length ? shown[i] : w.w,
    s: Number(w.s.toFixed(3)),
    e: Number(w.e.toFixed(3)),
  }));
}
