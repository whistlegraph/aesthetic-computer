#!/usr/bin/env node
// chaos.mjs — the chaos bank: wordless roars, shrieks, howls and growls in
// jeffrey-pvc (eleven_v4). Not lyric lines: the tag asks for the sound and the
// text is only something to scream it on. Raw returns are cached in
// out/takes/ beside the lines (x<nn>-<hash>.mp3), so a re-run costs nothing.
// bin/chaos-probe.py measures them and writes the bank (out/chaos/).
//
//   node pop/frothyscreama/bin/chaos.mjs             render every missing take (billable)
//   node pop/frothyscreama/bin/chaos.mjs --list      print id, path, tag + text as JSON; renders nothing
import { writeFileSync, existsSync, mkdirSync } from "node:fs";
import { createHash } from "node:crypto";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { key } from "./scream.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const TAKES = resolve(HERE, "../out/takes");
const VOICE = "ZXoQQp5X0PKHGwyZpVIT", MODEL = "eleven_v4";
// [id, text sent (tags and all), stability, style, speed = 1]
export const CHAOS = [
  ["x01", "[roaring] RRRAAAAAAAGH!", 0.3, 0.8],
  ["x02", "[guttural roar] GRRRRROOOOAAAAAHHH!", 0.3, 0.8],
  ["x03", "[shrieking] EEEEEEEEEEEEEEAAAAAHHH!", 0.3, 0.8],
  ["x04", "[screams chaotically] AAAAAAAAAAAAAAAAAAAAH!!", 0.3, 0.8],
  ["x05", "[howling in rage] AAAOOOOOOOOOOOOOOOOOHHH!", 0.3, 0.8],
  ["x06", "[voice cracking, screaming] AAAAaaaAAAAHHhhAAAA!", 0.3, 0.8],
  ["x07", "[frantic, panicked yelp] AH! AH! AAAH!", 0.3, 0.8],
  ["x08", "[long sustained scream] AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAH!", 0.3, 0.8],
  ["x09", "[death metal growl] UUURRRRRRGGGHHHH.", 0.3, 0.8],
  ["x10", "[screaming] [roaring] RAAAH! RRRAAAAAAH! RRRRAAAAAAAAAAAGH!", 0.3, 0.8],
  ["x11", "[blood-curdling scream] AAAAAAAAAAAIIIIIEEEEEEE!", 0.3, 0.8],
  ["x12", "[screams] Aah. [screams louder] AAAH! [roars] RAAAAAH!", 0.3, 0.8],     // tags alone are refused: "empty text"
  ["x13", "[primal scream] HHHRRRAAAAAAAAAAAAAAAA!", 0.3, 0.8],
  ["x14", "[screaming in agony] NNNNGGGAAAAAAAAAHHH!", 0.3, 0.8],
  ["x15", "[short sharp scream] AGH!", 0.3, 0.8],
  ["x16", "[feral snarl, growling] GRRRRRRRR... RRRAGH!", 0.3, 0.8],
  ["x17", "[hysterical screaming, voice breaking] aaaAAAAaaaAAAAHHH HAAAA AAAAAH!", 0.3, 0.8],
  ["x18", "[screaming at the top of his lungs] YYYEEEEEAAAAAAAAAAAAAAAHHHH!", 0.3, 0.8],
  ["x19", "[war cry] HHHHYYYAAAAAAAAAAAAAAAHHH!", 0.3, 0.8],
  ["x20", "[wailing, sobbing scream] WWWAAAAAAAAAAAHHHH HAAAAAAH!", 0.3, 0.8],
  // batch two: the first twenty all screamed but were short (1.3–3 s) and high (380–830 Hz).
  // Longer texts in several bursts, lower and rougher directions, stability at zero.
  ["x21", "[screaming, losing his mind] AAAAAAH! AAAAAAAAAAAH!! AAAAAAAAAAAAAAAAAAAAAH!!!", 0, 1],
  ["x22", "[deep demonic roar, very low pitched] ROOOOOOOAAAAAAAARRRRRR!", 0, 1],
  ["x23", "[low guttural growl, from the chest] GRRRRRRRROOOOOWWWLLL... GRRRRAAAAAHHH.", 0, 1],
  ["x24", "[roaring like a lion] RRRRROOOOAAAAAARRRRR! RRRROOOOAAAARRR!", 0, 1],
  ["x25", "[screaming until his voice gives out] AAAAAAAAAAAAAAAAAAAAAAAAAH— aaaaaaaaaaahhhhh... hhhhh.", 0, 1, 0.7],
  ["x26", "[frantic gibbering, screaming, out of control] AH AH AAH YAH GAH AAAGH RAH AH AH AAAAAH!", 0, 1],
  ["x27", "[shrieking in terror, piercing] EEEEEEEEEEEEEEEEEEEEEEEEEEEK! EEEEEEEEEEEEEEEEEEE!", 0, 1],
  ["x28", "[furious, roaring through clenched teeth] NNNNRRRRRRRRRGGGGHHHHH! RRRRAAAAAAAAGH!", 0, 1],
  ["x29", "[screaming] [gasps] [screaming again, louder] AAAAAAAAH! ... AAAAAAAAAAAAAAAAAAH!", 0, 1],
  ["x30", "[howling like a wolf, then screaming] AAAWOOOOOOOOOOOOOOOOOO! AAAAAAAAAH!", 0, 1],
  ["x31", "[throat-shredding hardcore scream] BLEEEEEEEEGGGHHHHH! YAAAAAAAAAAAGGHH!", 0, 1],
  ["x32", "[maniacal laughter turning into screaming] HAHAHAHAHA HAAAAAAH AAAAAAAAAAAAH!", 0, 1],
  ["x33", "[short sharp scream] GAH!", 0.3, 0.8],
  ["x34", "[barks, short roar] RAH!", 0.3, 0.8],
  ["x35", "[yelps] YAH!", 0.3, 0.8],
  ["x36", "[long sustained scream, never stopping, one breath] AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAH!", 0.3, 1, 0.7],
  ["x37", "[pig squeal, grindcore vocal] REEEEEEEEEEEEEEEEEEEEEEE!", 0, 1],
  ["x38", "[screaming in rage, breaking down, sobbing] AAAAAAAAAH! Aaaah... AAAAAAAAAAAAAH!", 0, 1],
  ["x39", "[tortured wail rising to a scream] ooooooooOOOOOOOOAAAAAAAAAAAAAAAAAHHHH!", 0, 1],
  ["x40", "[chaotic screaming, several outbursts] RAAAGH! AAAH! EEEYAAAH! GRRRAAAH! AAAAAAAAAAAAAH!", 0, 1],
];
export function chaosPath([id, text, stability, style, speed = 1]) {
  const body = { text, model_id: MODEL, voice_settings: { stability, similarity_boost: 0.9, style, use_speaker_boost: true, speed } };
  return { id, body, path: resolve(TAKES, `${id}-${createHash("sha1").update(JSON.stringify(body)).digest("hex").slice(0, 10)}.mp3`) };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  if (process.argv.includes("--list")) { console.log(JSON.stringify(CHAOS.map((c) => { const t = chaosPath(c); return { id: t.id, path: t.path, text: t.body.text, stability: c[2], style: c[3] }; }))); process.exit(0); }
  mkdirSync(TAKES, { recursive: true });
  let made = 0, chars = 0;
  for (const c of CHAOS) {
    const { id, body, path } = chaosPath(c);
    if (existsSync(path)) continue;
    const res = await fetch(`https://api.elevenlabs.io/v1/text-to-speech/${VOICE}?output_format=mp3_44100_128`, {
      method: "POST", headers: { "xi-api-key": key(), "content-type": "application/json" }, body: JSON.stringify(body) });
    if (!res.ok) { console.error(`REFUSED ${id}: ${res.status} ${(await res.text()).slice(0, 300)}`); process.exit(1); }
    writeFileSync(path, Buffer.from(await res.arrayBuffer()));
    made++; chars += body.text.length;
    console.log(`rendered ${id} ${body.text}`);
  }
  console.log(`${made} takes, ${chars} characters billed`);
}
