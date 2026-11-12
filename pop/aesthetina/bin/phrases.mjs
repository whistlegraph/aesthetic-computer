#!/usr/bin/env node
// phrases.mjs — whole spoken lyric lines from ElevenLabs (eleven_v4, with-timestamps),
// in a few cadences, for voice.py's sing_phrase to stretch onto the melody. Natural
// cadence and full words carry the diction that stitched syllables lose.
//
// Each take writes <slug>.mp3 + <slug>.json ({ text, syllables: [[start, end] s], cadence })
// into the vault: voice/<voice>-phonemes/phrases/. Syllables are marked in the lyric with
// spaces and hyphens ("com-pu-ter"); their times come from the character alignment.
//
//   node pop/aesthetina/bin/phrases.mjs [--force]
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const VAULT = process.env.AC_VAULT || join(ROOT, "aesthetic-computer-vault");
const FORCE = process.argv.includes("--force");
const env = readFileSync(join(VAULT, "lith", ".env"), "utf8");
const envv = (k) => process.env[k] || env.match(new RegExp(`^${k}=(.*)$`, "m"))?.[1].replace(/^"|"$/g, "");

export const LINES = {
  hand: "will you please hand me that com-pu-ter there?",
  buy: "i want to buy an-oth-er com-pu-ter",
  get: "i need to get an-oth-er com-pu-ter",
  chubby: "pass a chub-by ca-ble for my char-ging please",
};
// cadence: v4 audio tag + end punctuation
const CADENCES = { plain: ["", ""], excited: ["[excited] ", "!"], deadpan: ["[deadpan] ", "."] };
const TAKES = [
  ...["hand", "buy", "get", "chubby"].flatMap((l) => Object.keys(CADENCES).map((c) => ["jeffrey", l, c])),
  // Prutti (Pruttivox IVC) — DRAFT ONLY until he approves clone + performance (Klokkentales release gate)
  ...Object.keys(CADENCES).map((c) => ["prutti", "chubby", c]),
];

function voiceOf(v) {
  const lib = JSON.parse(readFileSync(join(VAULT, "voice", `${v}-phonemes`, "library.json"), "utf8"));
  return { id: lib.voice_id, settings: lib.voice_settings };
}

async function take(voice, line, cadence) {
  const dir = join(VAULT, "voice", `${voice}-phonemes`, "phrases"); mkdirSync(dir, { recursive: true });
  const slug = `${line}-${cadence}`, mp3 = join(dir, `${slug}.mp3`), meta = join(dir, `${slug}.json`);
  if (!FORCE && existsSync(meta)) return `${voice}/${slug} (kept)`;
  const lyric = LINES[line], spoken = lyric.replace(/-/g, "").replace(/[?.!]$/, "");
  const [tag, end] = CADENCES[cadence];
  const text = tag + spoken + (end || (lyric.endsWith("?") ? "?" : ""));
  const { id, settings } = voiceOf(voice);
  let res;
  for (let a = 0; a < 5; a++) {
    res = await fetch(`https://api.elevenlabs.io/v1/text-to-speech/${id}/with-timestamps?output_format=mp3_44100_128`, {
      method: "POST", headers: { "xi-api-key": envv("ELEVENLABS_API_KEY"), "content-type": "application/json" },
      body: JSON.stringify({ text, model_id: "eleven_v4", voice_settings: settings }),
    });
    if (res.ok) break;
    if (res.status !== 429 && res.status < 500) throw new Error(`${slug}: ${res.status} ${await res.text()}`);
    await new Promise((r) => setTimeout(r, 1500 * (a + 1)));
  }
  const j = await res.json();
  writeFileSync(mp3, Buffer.from(j.audio_base64, "base64"));
  // locate the lyric inside the aligned characters (the tag may or may not be aligned)
  const al = j.alignment, chars = al.characters.join("").toLowerCase();
  const at = chars.indexOf(spoken.toLowerCase());
  if (at < 0) throw new Error(`${slug}: lyric not found in alignment "${chars}"`);
  // syllable spans in the spoken string
  const syl = []; let pos = 0;
  for (const word of lyric.replace(/[?.!]$/, "").split(" ")) {
    for (const part of word.split("-")) { syl.push([pos, pos + part.length - 1]); pos += part.length; }
    pos += 1; // the space
  }
  const syllables = syl.map(([a, b]) => [al.character_start_times_seconds[at + a], al.character_end_times_seconds[at + b]]);
  writeFileSync(meta, JSON.stringify({ text, lyric, cadence, voice, syllables }, null, 1) + "\n");
  return `${voice}/${slug} ${syllables.length} syllables · ${syllables.at(-1)[1].toFixed(2)} s`;
}

for (const t of TAKES) console.log(await take(...t));
