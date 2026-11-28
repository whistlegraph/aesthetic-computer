#!/usr/bin/env node
// 🗣️ A status line spoken in @jeffrey's own voice, for rax development.
//   node rax/bin/say.mjs "the roof is fixed"
//   node rax/bin/say.mjs --no-play "..."     (print the audio file's path, play nothing)
//   node rax/bin/say.mjs --voice <id> "..."  (another ElevenLabs voice)
//
// The voice is the Professional Voice Clone /api/say calls `jeffrey`
// (system/netlify/functions/say.js), asked of ElevenLabs directly so nothing
// is cached on the way. The key comes from pop/bin/elevenlabs-key.mjs: an
// ELEVENLABS_API_KEY in the environment, else the vault's lith/.env. It is
// never printed or written anywhere.
//
// Whatever goes wrong (no key, no network, a refused request), macOS `say`
// speaks the same text and one line on stderr says why. Nothing is kept: the
// audio is a temp file, played with afplay and deleted.
import { spawnSync } from 'node:child_process';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const JEFFREY = 'ZXoQQp5X0PKHGwyZpVIT'; // (JEFFREY_VOICE_ID in say.js)
const MODEL = process.env.RAX_SAY_MODEL || 'eleven_v4'; // (ELEVEN_MODEL_ID in say.js)
// The settings say.js gives the calm jeffrey, so a status sounds like a reading.
const SETTINGS = { stability: 0.65, similarity_boost: 0.9, style: 0.15, use_speaker_boost: true, speed: 1 };

const args = process.argv.slice(2);
let play = true, voice = process.env.RAX_SAY_VOICE || JEFFREY;
const words = [];
for (let i = 0; i < args.length; i++) {
  if (args[i] === '--no-play') play = false;
  else if (args[i] === '--voice') voice = args[++i];
  else words.push(args[i]);
}
const text = words.join(' ').trim();
if (!text) { console.error('say.mjs [--no-play] [--voice <id>] "text"'); process.exit(1); }

const dir = mkdtempSync(join(tmpdir(), 'rax-say-'));

async function synthesize() {
  if (!voice) throw new Error('no voice id');
  const { elevenLabsKey } = await import('../../pop/bin/elevenlabs-key.mjs');
  const key = elevenLabsKey();
  // (A small mp3 is plenty for one sentence on a laptop speaker; the cost is by the letter either way.)
  const url = `https://api.elevenlabs.io/v1/text-to-speech/${encodeURIComponent(voice)}?output_format=mp3_22050_32`;
  const res = await fetch(url, {
    method: 'POST',
    headers: { 'xi-api-key': key, 'Content-Type': 'application/json' },
    body: JSON.stringify({ text, model_id: MODEL, voice_settings: SETTINGS }),
    signal: AbortSignal.timeout(30_000),
  });
  if (!res.ok) throw new Error(`ElevenLabs ${res.status}: ${(await res.text()).slice(0, 200).replace(/\s+/g, ' ')}`);
  const audio = Buffer.from(await res.arrayBuffer());
  if (audio.length < 256) throw new Error(`ElevenLabs sent ${audio.length} bytes`);
  const file = join(dir, 'say.mp3');
  writeFileSync(file, audio);
  return file;
}

let code = 0;
try {
  const file = await synthesize();
  if (!play) console.log(file); // (the caller's to delete)
  else {
    const played = spawnSync('afplay', [file], { stdio: 'ignore' });
    if (played.status !== 0) throw new Error('afplay could not play the audio');
    console.log('spoken');
  }
} catch (error) {
  // The fallback: the Mac's own voice, aloud, or to a file when nothing is to be played.
  console.error(`say.mjs: macOS say instead (${String(error.message || error)})`);
  const file = join(dir, 'say.aiff');
  const said = spawnSync('say', play ? [text] : ['-o', file, text], { stdio: 'ignore' });
  if (said.status !== 0) { console.error('say.mjs: macOS say failed too'); code = 1; }
  else console.log(play ? 'spoken (macOS say)' : file);
}
if (play) rmSync(dir, { recursive: true, force: true });
process.exit(code);
