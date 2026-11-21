#!/usr/bin/env node
// scream.mjs — the takes. Every lyric line is one ElevenLabs render in
// jeffrey-pvc (eleven_v4): a whole line screamed in one breath, never
// syllables glued together. Raw returns are kept in out/takes/ keyed by a
// hash of text + settings, so a re-render costs nothing.
//
//   node pop/frothyscreama/bin/scream.mjs              render every missing take
//   node pop/frothyscreama/bin/scream.mjs --only s1    one line
//   node pop/frothyscreama/bin/scream.mjs --only s1 --tag "[screaming] {text}!!" --stability 0.5 --variant b
//   node pop/frothyscreama/bin/scream.mjs --only s9 --text "NOW?! NOWWW?!"     an audition: the words respelt for the take
//   node pop/frothyscreama/bin/scream.mjs --wild              the chaos-delivery candidates (WILD), billable, cached
//   node pop/frothyscreama/bin/scream.mjs --list              each scream line's take in use and its candidates, as JSON
// A line whose audition won is written into PICKS; the renderer then uses that take.
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { createHash } from "node:crypto";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const TAKES = resolve(HERE, "../out/takes");
mkdirSync(TAKES, { recursive: true });
const VOICE = "ZXoQQp5X0PKHGwyZpVIT", MODEL = "eleven_v4";
const flag = (n, d) => { const i = process.argv.indexOf(n); return i > 0 ? process.argv[i + 1] : d; };
export const TAGS = { scream: "[screaming] {text}!", whisper: "[whispers] {text}" };
export const SETTINGS = {
  scream: { stability: 0.5, similarity_boost: 0.9, style: 0.6, use_speaker_boost: true, speed: 1 },
  whisper: { stability: 0.65, similarity_boost: 0.9, style: 0.15, use_speaker_boost: true, speed: 1 },
};

export function lyric() {
  return readFileSync(resolve(HERE, "../lyrics/frothyscreama.txt"), "utf8").split("\n")
    .filter((l) => l.trim() && !l.startsWith("#"))
    .map((l) => { const [, id, kind, text] = l.match(/^(\S+)\s+(\S+)\s+(.+)$/); return { id, kind, text }; });
}
// The chaos delivery (2026-11-20): the lyric lines screamed the way the chaos
// bank's best takes were — stability 0, style 1, a descriptive tag, the words
// in bursts. Candidates per line; `--wild` renders the missing ones,
// bin/wild-probe.py measures them against the take in use, and a winner goes
// into PICKS with `wild: true`.
export const WILD_SETTINGS = { stability: 0, similarity_boost: 0.9, style: 1, use_speaker_boost: true, speed: 1 };
export const WILD = {
  s1: [["[throat-shredding hardcore scream] {text}", "TRY NOW! NOOOW?! AND NOOOOOW?!"], ["[frantic, screaming, out of control] {text}", "TRY NOW! NOW?! AND NOW?! AND NOOOOW?!"]],
  s2: [["[screaming, losing his mind] {text}", "I DON'T SEE IT! CAN'T! HEAR IT!!"], ["[voice cracking, screaming] {text}", "I DON'T SEEEE IT! CAN'T HEEEAR IT!"]],
  s3: [["[screaming in rage, breaking down] {text}", "DAAAMN! STILL! NOT! WORKING!!"], ["[throat-shredding hardcore scream] {text}", "DAMN! STILL NOT WOOORKING! AAAGH!"]],
  s4: [["[screaming until his voice gives out] {text}", "THEY ARE WAAAAY! TOO! LOOOOUD!"], ["[shrieking in pain, voice cracking] {text}", "THEY ARE WAY! TOO! LOUD! AAAH!"]],
  s5: [["[screaming, losing his mind] {text}", "YIKES! WHAT I JUST HEARD! WAS SEEEEARING!"], ["[shrieking in terror, voice cracking] {text}", "YIKES! WHAT I JUST HEARD WAS SEARING! AAAH!"]],
  s6: [["[screaming in rage, breaking down, sobbing] {text}", "NOOO! IT CRASHED! AGAAAIN!"], ["[throat-shredding hardcore scream] {text}", "NO! NO! IT CRASHED AGAIN! AGAAAIN!"]],
  s7: [["[frantic, screaming, out of control] {text}", "WRITE! ALL! THIS! CODE! NOOOW!"], ["[throat-shredding hardcore scream] {text}", "WRITE ALL THIS CODE! NOW! NOOOOW!"]],
  s8: [["[screaming until his voice gives out] {text}", "THIS MACHINE! IS ABOUT! TO DIIIIIE! aaaah..."], ["[screaming in terror, voice cracking] {text}", "THIS MACHINE IS ABOUT TO DIE! TO DIIIIE!"]],
  s9: [["[chaotic screaming, several outbursts] {text}", "NOW?! NOW?! NOOOW?! NOOOOOOW?!"], ["[voice cracking, screaming] {text}", "NOW?! NOOOOOOW?!"]],
  s10: [["[screaming, losing his mind] {text}", "I WANT MORE! SCREAMING! MOOORE SCREEEAMING!"], ["[throat-shredding hardcore scream] {text}", "I WANT! MORE! SCREEEEAMING!"]],
  s11: [["[frantic, panicked, screaming] {text}", "IT'S KILLING MY RAM! STOP IT! STOP IIIIT!"], ["[screaming in rage, voice cracking] {text}", "IT'S KILLING! MY RAM! STOOOP IT!"]],
  s12: [["[screaming in rage, breaking down] {text}", "THAT! SHOULD NOT! HAVE HAPPENED!!"], ["[throat-shredding hardcore scream] {text}", "THAT SHOULD NOOOT HAVE HAPPENED! AAAGH!"]],
  s13: [["[war cry, screaming at the top of his lungs] {text}", "EVERYTHING! BLAST! THEEERE!"], ["[throat-shredding hardcore scream] {text}", "EVERYTHING BLAAAST THERE! RAAAGH!"]],
  s14: [["[pig squeal, grindcore vocal, shrieking] {text}", "SHRIIIIIIILL! SHRILL!"]],
  s15: [["[voice cracking, screaming] {text}", "NEVER! LOOOOPS! NEVER LOOPS!"]],
  s16: [["[frantic, screaming, out of control] {text}", "DO IT AGAIN! AGAIN! AGAAAAIN!"]],
  s17: [["[throat-shredding hardcore scream] {text}", "SHOW ME! NOW! SHOW ME NOOOW!"]],
};
const wild = (id, n) => ({ tag: WILD[id][n][0], text: WILD[id][n][1], wild: true });
// The delivery picked for a line when the plain one could not be understood
// once pitched and in the mix (whisper.cpp read every candidate back,
// 2026-11-20). `text` is the line respelt for the take; the lyric is unchanged.
export const PICKS = {
  // 2026-11-20, the chaos delivery: the owner heard the chaos reel and wanted the
  // words screamed like that. Each is the WILD candidate (index n) that measured
  // wildest by bin/wild-probe.py with every word still read back by whisper.
  // s14, s15 and s16 keep the plain take: it measured wilder than its candidate
  // (and the s14 candidate was heard as other words).
  // Before this: s4 "[screaming in pain] THEY ARE WAY, TOO, LOOOUD", s7 "[screaming furiously]
  // WRITE AAALL THIS CODE! NOOOW", s9 "[screaming] NOW?! NOWWWWW?!", s13 "[screaming
  // triumphantly] EVERYTHING BLAAAST THERE" — picked for readability once pitched.
  s1: wild("s1", 0),
  s2: wild("s2", 1),
  s3: wild("s3", 1),
  s4: wild("s4", 1),
  s5: wild("s5", 1),
  s6: wild("s6", 0),
  s7: wild("s7", 1),
  s8: wild("s8", 1),
  s9: wild("s9", 1),
  s10: wild("s10", 0),
  s11: wild("s11", 1),
  s12: wild("s12", 1),
  s13: wild("s13", 1),
  s17: wild("s17", 0),
};
export function takePath(line, o = {}) {
  const pick = o.tag || o.text || o.variant ? {} : PICKS[line.id] ?? {};
  const { tag = pick.tag ?? TAGS[line.kind], text = pick.text ?? line.text, settings = pick.wild ? WILD_SETTINGS : SETTINGS[line.kind], variant = "" } = o;
  const body = { text: tag.replace("{text}", text), model_id: MODEL, voice_settings: settings };
  const hash = createHash("sha1").update(JSON.stringify(body) + variant).digest("hex").slice(0, 10);
  return { body, path: resolve(TAKES, `${line.id}-${hash}.mp3`), hash, picked: Object.keys(pick).length > 0 };
}
export function key() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  const env = resolve(process.env.AC_VAULT ?? resolve(HERE, "../../../aesthetic-computer-vault"), "lith/.env");
  const line = existsSync(env) && readFileSync(env, "utf8").split(/\r?\n/).find((l) => l.startsWith("ELEVENLABS_API_KEY="));
  if (!line) throw new Error("no ELEVENLABS_API_KEY in the environment or the vault's lith/.env");
  return line.slice("ELEVENLABS_API_KEY=".length).replace(/^["']|["']$/g, "");
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const only = flag("--only"), variant = flag("--variant", "");
  let chars = 0, made = 0;
  const wilds = () => lyric().flatMap((line) => (WILD[line.id] ?? []).map(([tag, text], n) => ({ line, n, tag, text, ...takePath(line, { tag, text, settings: WILD_SETTINGS }) })));
  if (process.argv.includes("--list")) {     // every scream line: the take in use, and its wild candidates
    console.log(JSON.stringify(lyric().filter((l) => l.kind === "scream").map((line) => ({ id: line.id, text: line.text, inUse: { ...takePath(line), sent: takePath(line).body.text },
      wild: wilds().filter((w) => w.line.id === line.id).map((w) => ({ n: w.n, path: w.path, hash: w.hash, sent: w.body.text })) }))));
    process.exit(0);
  }
  if (process.argv.includes("--wild")) {
    for (const w of wilds()) {
      if (only && w.line.id !== only) continue;
      if (existsSync(w.path)) continue;
      const res = await fetch(`https://api.elevenlabs.io/v1/text-to-speech/${VOICE}?output_format=mp3_44100_128`, {
        method: "POST", headers: { "xi-api-key": key(), "content-type": "application/json" }, body: JSON.stringify(w.body) });
      if (!res.ok) { console.error(`REFUSED ${w.line.id}: ${res.status} ${(await res.text()).slice(0, 300)}`); continue; }
      writeFileSync(w.path, Buffer.from(await res.arrayBuffer()));
      chars += w.body.text.length; made++;
      console.log(`rendered ${w.line.id}.${w.n} ${w.body.text}`);
    }
    console.log(`${made} takes, ${chars} characters billed`);
    process.exit(0);
  }
  for (const line of lyric()) {
    if (only && line.id !== only) continue;
    const settings = { ...SETTINGS[line.kind], ...(flag("--stability") ? { stability: Number(flag("--stability")) } : {}), ...(flag("--style") ? { style: Number(flag("--style")) } : {}) };
    const { body, path } = takePath(line, { tag: flag("--tag"), text: flag("--text"), settings, variant });
    if (existsSync(path)) { console.log(`cached ${line.id} ${path}`); continue; }
    const res = await fetch(`https://api.elevenlabs.io/v1/text-to-speech/${VOICE}?output_format=mp3_44100_128`, {
      method: "POST", headers: { "xi-api-key": key(), "content-type": "application/json" }, body: JSON.stringify(body) });
    if (!res.ok) { console.error(`REFUSED ${line.id}: ${res.status} ${(await res.text()).slice(0, 300)}`); process.exit(1); }
    writeFileSync(path, Buffer.from(await res.arrayBuffer()));
    chars += body.text.length; made++;
    console.log(`rendered ${line.id} (${body.text.length} chars) ${path}`);
  }
  console.log(`${made} takes, ${chars} characters billed`);
}
