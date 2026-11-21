#!/usr/bin/env node
// cutie-rap.mjs — the commit rap (@jeffrey 2026-11-20: "around 2:00 would be better to have some rap …
// super fast commit rap … from jeffrey"). The words are jeffrey's own commit subjects from this repo,
// as written (a subject may stop at a clause boundary; hashes dropped; no subject that names another
// person, a client, a host or a key). Each phrase is one jeffrey-pvc render (eleven_v4, the cutie voice
// settings at the model's top speed) made with ElevenLabs' alignment, so every word's time is known:
// the renderer cuts the take into words and puts each on the sixteenths.
// Kept in the vault as phrases/cutie-rap-<n>.{mp3,json}. Billable; cached.
//
//   node pop/wattajetta/bin/cutie-rap.mjs [--list]
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";
import { key } from "../../frothyscreama/bin/scream.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const VAULT = process.env.AC_VAULT ?? resolve(HERE, "../../../aesthetic-computer-vault");
const PHRASES = join(VAULT, "voice/jeffrey-phonemes/phrases");
const lib = JSON.parse(readFileSync(join(VAULT, "voice/jeffrey-phonemes/library.json"), "utf8"));
// [commit, the words as rapped (the subject, or its first clauses)]
export const RAP = [
  [
    ["d0d0f01a93", "scream: the voice travels as its own message."],
    ["c75ba387ff", "C engines that match the JS renders sample for sample, and more screaming."],
    ["eae7665a86", "voices: every ElevenLabs caller moves to eleven v4."],
  ],
  [
    ["a3c9e7be39", "aesthetina: the reveal cut."],
    ["6f4b732571", "eight gigabytes: the dance cut, staged four-on-the-floor, sidechain, pop polish."],
    ["5b22b7669c", "eight gigabytes: submitted to DistroKid."],
    ["c5dd7cc4f7", "pop: refresh the library index."],
  ],
];
const TAG = "[rapping very fast, rhythmic and percussive, one breath] ";
export const rapPath = (n) => join(PHRASES, `cutie-rap-${n}`);
// syllables in a word, near enough to give each one a sixteenth
export const syllables = (w) => {
  const s = w.toLowerCase().replace(/[^a-z0-9]/g, "");
  if (/^\d+$/.test(s)) return s.length;                       // "4": four
  if (s === "js") return 2; if (s === "c") return 1; if (s === "v4") return 2;
  const m = s.replace(/e$/, "").match(/[aeiouy]+/g);
  return Math.max(1, m ? m.length : 1);
};

if (import.meta.url === `file://${process.argv[1]}`) {
  let made = 0, chars = 0;
  for (const [n, lines] of RAP.entries()) {
    const spoken = lines.map(([, t]) => t).join(" "), text = TAG + spoken;
    if (process.argv.includes("--list")) { console.log(n, existsSync(rapPath(n) + ".json") ? "cached" : "missing", text); continue; }
    if (existsSync(rapPath(n) + ".json") && !process.argv.includes("--force")) { console.log(`cached ${n}`); continue; }
    const res = await fetch(`https://api.elevenlabs.io/v1/text-to-speech/${lib.voice_id}/with-timestamps?output_format=mp3_44100_128`, {
      method: "POST", headers: { "xi-api-key": key(), "content-type": "application/json" },
      body: JSON.stringify({ text, model_id: "eleven_v4", voice_settings: { ...lib.voice_settings, speed: 1.2 } }) });
    if (!res.ok) { console.error(`REFUSED ${n}: ${res.status} ${(await res.text()).slice(0, 300)}`); continue; }
    const j = await res.json(), al = j.alignment, all = al.characters.join("");
    const at = all.toLowerCase().indexOf(spoken.slice(0, 12).toLowerCase());
    if (at < 0) { console.error(`${n}: the words were not found in the alignment`); continue; }
    const words = []; let pos = at;
    for (const w of spoken.split(" ")) {
      const i = all.toLowerCase().indexOf(w.toLowerCase(), pos);
      if (i < 0 || i > pos + 12) { console.error(`${n}: lost the alignment at "${w}"`); break; }
      words.push({ w, a: al.character_start_times_seconds[i], b: al.character_end_times_seconds[i + w.length - 1], syl: syllables(w) });
      pos = i + w.length;
    }
    writeFileSync(rapPath(n) + ".mp3", Buffer.from(j.audio_base64, "base64"));
    writeFileSync(rapPath(n) + ".json", JSON.stringify({ text, lines, words }, null, 1) + "\n");
    made++; chars += text.length;
    console.log(`rendered ${n}: ${words.length} words, ${words.reduce((s, x) => s + x.syl, 0)} syllables, ${words.at(-1).b.toFixed(2)} s`);
  }
  console.log(`${made} takes, ${chars} characters billed`);
}
