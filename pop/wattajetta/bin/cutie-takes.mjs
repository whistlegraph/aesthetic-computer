#!/usr/bin/env node
// cutie-takes.mjs — the two sung lines of wattajetta cutie as whole spoken
// takes from ElevenLabs (jeffrey-pvc, eleven_v4, with timestamps), in eleven
// inflections. Same files as pop/aesthetina/bin/phrases.mjs writes, in the same
// place (vault voice/jeffrey-phonemes/phrases/), so aesthetina's sing_phrase
// can stretch them onto the tune. Whole words keep the diction that stitched
// syllables lose. Costs a few cents; takes on disk are kept unless --force.
//
//   node pop/wattajetta/bin/cutie-takes.mjs [--force] [--line weirdest --cadence cheeky]
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const VAULT = process.env.AC_VAULT || join(ROOT, "aesthetic-computer-vault");
const FORCE = process.argv.includes("--force");
const env = readFileSync(join(VAULT, "lith", ".env"), "utf8");
const KEY = process.env.ELEVENLABS_API_KEY || env.match(/^ELEVENLABS_API_KEY=(.*)$/m)?.[1].replace(/^"|"$/g, "");

// syllables are marked with spaces and hyphens; lyrics/cutie.json is the one copy
export const LINES = JSON.parse(readFileSync(join(dirname(fileURLToPath(import.meta.url)), "..", "lyrics", "cutie.json"), "utf8"));
// inflections: a v4 audio tag and the end punctuation. sing-lines.py keeps
// the ones whisper can read and the song takes turns through them.
export const CADENCES = {
  plain: ["", ""], excited: ["[excited] ", "!"], deadpan: ["[deadpan] ", "."],
  happy: ["[happily] ", "!"], curious: ["[curious] ", "?"], whisper: ["[whispers] ", "."], cheeky: ["[mischievously] ", "!"],
  amazed: ["[amazed] ", "!"], soft: ["[softly] ", "."], proud: ["[proudly] ", "."], laughing: ["[laughing] ", "!"],
  // soft ones that keep enough voice to carry a note (plain [softly] and [whispers] do not)
  softcheeky: ["[mischievously] [softly] ", "."], gentle: ["[gently] ", "."], warm: ["[warmly] ", "."],
  // more takes of the two the song uses: the model gives a different reading each
  // time, and sing-lines.py measures which one sits on the notes best, phrase by phrase
  cheeky2: ["[mischievously] ", "!"], cheeky3: ["[mischievously] ", "!"], cheeky4: ["[mischievously] ", "."],
  warm2: ["[warmly] ", "."], warm3: ["[warmly] ", "."],
  // the solo: whispered (several, they vary a lot), then loud
  whisper2: ["[whispers] ", "."], whisper3: ["[whispering softly] ", "."],
  shout: ["[shouting] ", "!"], loud: ["[loudly, proudly] ", "!"], loud2: ["[shouting] ", "!"],
};

const lib = JSON.parse(readFileSync(join(VAULT, "voice", "jeffrey-phonemes", "library.json"), "utf8"));
const dir = join(VAULT, "voice", "jeffrey-phonemes", "phrases");
mkdirSync(dir, { recursive: true });

async function take(line, cadence) {
  const slug = `cutie-${line}-${cadence}`, meta = join(dir, `${slug}.json`);
  if (!FORCE && existsSync(meta)) return `${slug} (kept)`;
  const lyric = LINES[line], spoken = lyric.replace(/-/g, "");
  const [tag, end] = CADENCES[cadence], text = tag + spoken + end;
  let res;
  for (let a = 0; a < 5; a++) {
    res = await fetch(`https://api.elevenlabs.io/v1/text-to-speech/${lib.voice_id}/with-timestamps?output_format=mp3_44100_128`, {
      method: "POST", headers: { "xi-api-key": KEY, "content-type": "application/json" },
      body: JSON.stringify({ text, model_id: "eleven_v4", voice_settings: lib.voice_settings }),
    });
    if (res.ok) break;
    if (res.status !== 429 && res.status < 500) throw new Error(`${slug}: ${res.status} ${await res.text()}`);
    await new Promise((r) => setTimeout(r, 1500 * (a + 1)));
  }
  const j = await res.json();
  writeFileSync(join(dir, `${slug}.mp3`), Buffer.from(j.audio_base64, "base64"));
  const al = j.alignment, at = al.characters.join("").toLowerCase().indexOf(spoken.toLowerCase());
  if (at < 0) throw new Error(`${slug}: lyric not found in the alignment`);
  const spans = []; let pos = 0;
  for (const word of lyric.split(" ")) {
    for (const part of word.split("-")) { spans.push([pos, pos + part.length - 1]); pos += part.length; }
    pos += 1;
  }
  const syllables = spans.map(([a, b]) => [al.character_start_times_seconds[at + a], al.character_end_times_seconds[at + b]]);
  writeFileSync(meta, JSON.stringify({ text, lyric, cadence, voice: "jeffrey", syllables }, null, 1) + "\n");
  return `${slug} ${syllables.length} syllables · ${syllables.at(-1)[1].toFixed(2)} s`;
}

// the colour lines (lip-red … lip-black) are only ever sung mischievous, warm or whispered
const wanted = (line, c) => {
  if (line === "weirdest") return c === "cheeky";
  if (line === "weird") return c === "warm";
  return !line.startsWith("lip-") || /^(cheeky|warm|whisper)\d?$/.test(c);
};
function selected(flag, choices) {
  const i = process.argv.indexOf(flag);
  if (i < 0) return Object.keys(choices);
  const key = process.argv[i + 1];
  if (!Object.hasOwn(choices, key)) throw new Error(`Unknown ${flag}: ${key}`);
  return [key];
}
const cadences = selected("--cadence", CADENCES);
for (const line of selected("--line", LINES)) {
  for (const c of cadences) if (wanted(line, c)) console.log(await take(line, c));
}
