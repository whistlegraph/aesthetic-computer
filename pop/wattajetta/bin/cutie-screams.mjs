#!/usr/bin/env node
// cutie-screams.mjs — the song's own words, screamed (@jeffrey 2026-11-20: "and lets add some screaming
// lyrics?"). The frothyscreama chaos delivery (pop/frothyscreama/bin/scream.mjs: jeffrey-pvc on
// eleven_v4, stability 0, style 1, a descriptive tag, the words in bursts), one render per line, kept
// in the vault beside the other cutie takes as phrases/cutie-scream-<id>.mp3. Billable; cached.
//
//   node pop/wattajetta/bin/cutie-screams.mjs          render the missing takes
//   node pop/wattajetta/bin/cutie-screams.mjs --list   what would be sent
import { writeFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { WILD_SETTINGS, key } from "../../frothyscreama/bin/scream.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const PHRASES = resolve(process.env.AC_VAULT ?? resolve(HERE, "../../../aesthetic-computer-vault"), "voice/jeffrey-phonemes/phrases");
const VOICE = "ZXoQQp5X0PKHGwyZpVIT", MODEL = "eleven_v4";
export const SCREAMS = {
  "title-a": ["[throat-shredding hardcore scream] {text}", "WHAT A JETTER! CUTIE! YOU AAARE!"],
  "title-b": ["[screaming, losing his mind] {text}", "WHAT A JETTER CUTIE YOU AAAAARE!"],
  "tax-a": ["[screaming in rage, voice cracking] {text}", "BREAK! A TAX! BRAAACKET!"],
  "nothing-a": ["[screaming until his voice gives out] {text}", "AND THEN DO NOOOTHING! NOOOOTHING!"],
  "nothing-b": ["[throat-shredding hardcore scream] {text}", "DO NOTHING! DO NOOOOTHING!"],
  "better-a": ["[war cry, screaming at the top of his lungs] {text}", "NOTHING BETTER! THAN MY JETTER!"],
  "cutie-a": ["[voice cracking, screaming] {text}", "CUTIE! CUTIEEEE!"],
  "lip-a": ["[frantic, screaming, out of control] {text}", "PUT THE PINKEST! LIP LINER OOON!"],
};
export const screamPath = (id) => resolve(PHRASES, `cutie-scream-${id}.mp3`);

if (import.meta.url === `file://${process.argv[1]}`) {
  let made = 0, chars = 0;
  for (const [id, [tag, text]] of Object.entries(SCREAMS)) {
    const body = { text: tag.replace("{text}", text), model_id: MODEL, voice_settings: WILD_SETTINGS };
    if (process.argv.includes("--list")) { console.log(id, existsSync(screamPath(id)) ? "cached" : "missing", body.text); continue; }
    if (existsSync(screamPath(id))) { console.log(`cached ${id}`); continue; }
    const res = await fetch(`https://api.elevenlabs.io/v1/text-to-speech/${VOICE}?output_format=mp3_44100_128`, {
      method: "POST", headers: { "xi-api-key": key(), "content-type": "application/json" }, body: JSON.stringify(body) });
    if (!res.ok) { console.error(`REFUSED ${id}: ${res.status} ${(await res.text()).slice(0, 300)}`); continue; }
    writeFileSync(screamPath(id), Buffer.from(await res.arrayBuffer()));
    writeFileSync(screamPath(id).replace(/\.mp3$/, ".sent.json"), JSON.stringify(body, null, 1));
    made++; chars += body.text.length;
    console.log(`rendered ${id}: ${body.text}`);
  }
  console.log(`${made} takes, ${chars} characters billed`);
}
