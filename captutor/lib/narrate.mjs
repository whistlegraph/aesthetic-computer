// narrate — turn a screenplay's spoken lines into audio + word timings.
//
// This runs BEFORE anything is recorded, and that inversion is the whole design.
// The usual way to make a tutorial is to film it and then fight the narration
// into sync. Here the voice exists first, so we know exactly how long every beat
// lasts before the camera rolls — the UI performs to a click track.
//
// /api/say (jeffrey PVC) with `withTimestamps` returns ElevenLabs' per-CHARACTER
// alignment, so captions are exact and free: no whisper pass, no forced aligner,
// and none of the transcription-fixup ("notepat" → "Notepad") that every other
// pipeline in this repo needs, because we already know the words — we wrote them.
//
// NOTE the request field is `from`, not `text`. say.js does
// `body.from || "aesthetic.computer"` — pass the wrong key and it cheerfully
// returns 200 with a perfectly-aligned recording of the words "aesthetic
// computer". Verify alignment.characters matches your line.

import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const SAY_URL = process.env.SAY_ENDPOINT || "https://aesthetic.computer/api/say";

/// Fold ElevenLabs' per-character alignment into words. A word is a run of
/// non-space characters; its span is [first char start, last char end].
/// The shape ({text, fromMs, toMs}) is deliberately recap's words.json — so
/// recap's subtitle machinery can read it unchanged.
function wordsFromAlignment(a) {
  const chars = a.characters || [];
  const starts = a.character_start_times_seconds || [];
  const ends = a.character_end_times_seconds || [];
  const words = [];
  let i = 0;
  while (i < chars.length) {
    if (/\s/.test(chars[i])) { i++; continue; }
    const from = starts[i];
    let text = "";
    let to = ends[i];
    while (i < chars.length && !/\s/.test(chars[i])) {
      text += chars[i];
      to = ends[i];
      i++;
    }
    words.push({ text, fromMs: Math.round(from * 1000), toMs: Math.round(to * 1000) });
  }
  return words;
}

/// Speak one line. Content-hash cached — ElevenLabs bills per character, and a
/// screenplay gets re-rendered many times while its blocking is tuned.
async function speak(line, { voice = "jeffrey", dir }) {
  const key = createHash("sha256").update(`${voice}:${line}`).digest("hex").slice(0, 16);
  const mp3 = join(dir, `${key}.mp3`);
  const meta = join(dir, `${key}.json`);

  if (existsSync(mp3) && existsSync(meta)) {
    return { mp3, ...JSON.parse(readFileSync(meta, "utf8")), cached: true };
  }

  const res = await fetch(SAY_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ from: line, voice, withTimestamps: true }),
    redirect: "follow",
  });
  if (!res.ok) throw new Error(`/api/say ${res.status}: ${await res.text()}`);

  const json = await res.json();
  const align = json.alignment || {};
  const spoken = (align.characters || []).join("");
  // The silent-default trap, caught loudly. If say.js fell back we'd otherwise
  // ship a tutorial narrated as "aesthetic computer" over the right pictures.
  if (spoken.trim() !== line.trim()) {
    throw new Error(
      `/api/say returned alignment for a DIFFERENT line.\n` +
      `  asked: ${JSON.stringify(line)}\n` +
      `  got:   ${JSON.stringify(spoken)}\n` +
      `  (is the request field still \`from\`?)`);
  }

  writeFileSync(mp3, Buffer.from(json.audio, "base64"));
  const words = wordsFromAlignment(align);
  const durationSec = words.length ? words[words.length - 1].toMs / 1000 : 0;
  const info = { line, voice, words, durationSec };
  writeFileSync(meta, JSON.stringify(info, null, 2));
  return { mp3, ...info, cached: false };
}

/// Narrate every beat. Returns the beats enriched with {mp3, words, durationSec}.
export async function narrate(beats, { voice = "jeffrey", dir }) {
  mkdirSync(dir, { recursive: true });
  const out = [];
  for (const [i, beat] of beats.entries()) {
    const spoken = await speak(beat.say, { voice, dir });
    process.stdout.write(
      `  ${String(i + 1).padStart(2)}. ${spoken.durationSec.toFixed(1)}s` +
      `${spoken.cached ? " (cached)" : ""}  ${beat.say.slice(0, 58)}\n`);
    out.push({ ...beat, ...spoken, index: i });
  }
  return out;
}
