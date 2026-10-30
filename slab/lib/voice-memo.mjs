// A voice memo in Jeffrey's own voice: text → ElevenLabs (through the
// `/api/say` endpoint, which holds the PVC voice id, its settings and a CDN
// cache) → `Audio Message.caf`, the shape Messages records itself (one
// channel, 24 kHz Opus in a CAF). Sent as an attachment it plays inline on
// iPhone and Mac; Messages' own red "audio message" bubble is only made by
// its microphone, so that flag is not ours to set. afconvert does the
// transcode — nothing to install.

import { createHash } from "node:crypto";
import { execFile } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";

const pexec = promisify(execFile);
const SAY_URL = process.env.SAY_ENDPOINT || "https://aesthetic.computer/api/say";
const SLAB_HOME = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab");
export const VOICE_MEMO_DIR = join(SLAB_HOME, "voice-memos");
export const VOICE_MEMO_NAME = "Audio Message.caf";
export const VOICE_MEMO_MAX_CHARS = 1200;
export const VOICES = ["jeffrey", "prutti"];

// Speech knobs go to the same endpoint the `say` piece uses: speed 0.7–1.2,
// style 0–1, stability 0–1 (jeffrey-pvc wants ≥0.5), similarity 0–1.
export async function fetchSpeech(text, { voice = "jeffrey", speed, style, stability, similarity } = {}) {
  const body = { from: text, voice, provider: voice };
  for (const [k, v] of Object.entries({ speed, style, stability, similarity })) if (typeof v === "number") body[k] = v;
  const res = await fetch(SAY_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json", "User-Agent": "Mozilla/5.0 slab-voice-memo" },
    body: JSON.stringify(body),
    redirect: "follow",
  });
  if (!res.ok) throw new Error(`say API ${res.status}: ${(await res.text()).slice(0, 200)}`);
  const type = res.headers.get("content-type") || "";
  if (type.includes("json")) {
    const info = await res.json();
    if (!info.url) throw new Error(`say API gave no audio url: ${JSON.stringify(info).slice(0, 200)}`);
    const clip = await fetch(info.url);
    if (!clip.ok) throw new Error(`speech CDN ${clip.status}`);
    return Buffer.from(await clip.arrayBuffer());
  }
  return Buffer.from(await res.arrayBuffer());
}

async function durationSeconds(path) {
  const { stdout } = await pexec("/usr/bin/afinfo", [path]).catch(() => ({ stdout: "" }));
  const m = stdout.match(/estimated duration:\s*([\d.]+)/);
  return m ? Number(m[1]) : null;
}

// Render (or reuse) the memo for `text`. Returns { path, bytes, seconds,
// cached, mp3 }. The file sits in its own folder so its basename can be the
// one Messages uses for its own recordings.
export async function renderVoiceMemo(text, { voice = "jeffrey", dir = VOICE_MEMO_DIR, ...knobs } = {}) {
  const words = String(text || "").trim();
  if (!words) throw new Error("`voice` needs words to speak");
  if (words.length > VOICE_MEMO_MAX_CHARS) throw new Error(`voice memo text is ${words.length} characters; keep it under ${VOICE_MEMO_MAX_CHARS}`);
  if (!VOICES.includes(voice)) throw new Error(`unknown voice "${voice}" (${VOICES.join(", ")})`);
  const key = createHash("sha256").update(JSON.stringify({ voice, words, ...knobs })).digest("hex").slice(0, 16);
  const folder = join(dir, key);
  const path = join(folder, VOICE_MEMO_NAME);
  const mp3 = join(folder, "speech.mp3");
  const cached = existsSync(path);
  if (!cached) {
    mkdirSync(folder, { recursive: true });
    if (!existsSync(mp3)) writeFileSync(mp3, await fetchSpeech(words, { voice, ...knobs }));
    // The format Messages records: mono, 24 kHz Opus, 480-frame packets.
    await pexec("/usr/bin/afconvert", ["-f", "caff", "-d", "opus@24000", "-c", "1", mp3, path]);
    writeFileSync(join(folder, "memo.json"), JSON.stringify({ voice, text: words, ...knobs, at: new Date().toISOString() }, null, 2));
  }
  return { path, bytes: statSync(path).size, seconds: await durationSeconds(path), cached, mp3, text: words, voice };
}

export function describeVoiceMemo(memo) {
  const secs = memo.seconds == null ? "" : ` ${memo.seconds.toFixed(1)}s,`;
  return `voice memo (${memo.voice}):${secs} ${memo.bytes} bytes${memo.cached ? ", cached" : ""} — ${memo.path}\n  says: ${memo.text}`;
}

export function readVoiceMemoText(path) {
  try {
    return JSON.parse(readFileSync(join(path, "..", "memo.json"), "utf8")).text || null;
  } catch {
    return null;
  }
}
