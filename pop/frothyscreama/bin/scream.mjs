#!/usr/bin/env node
// scream.mjs — the takes. Every lyric line is one ElevenLabs render in
// jeffrey-pvc (eleven_v4): a whole line screamed in one breath, never
// syllables glued together. Raw returns are kept in out/takes/ keyed by a
// hash of text + settings, so a re-render costs nothing.
//
//   node pop/frothyscreama/bin/scream.mjs              render every missing take
//   node pop/frothyscreama/bin/scream.mjs --only s1    one line
//   node pop/frothyscreama/bin/scream.mjs --only s1 --tag "[screaming] {text}!!" --stability 0.5 --variant b
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
export function takePath(line, { tag = TAGS[line.kind], settings = SETTINGS[line.kind], variant = "" } = {}) {
  const body = { text: tag.replace("{text}", line.text), model_id: MODEL, voice_settings: settings };
  const hash = createHash("sha1").update(JSON.stringify(body) + variant).digest("hex").slice(0, 10);
  return { body, path: resolve(TAKES, `${line.id}-${hash}.mp3`) };
}
function key() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  const env = resolve(process.env.AC_VAULT ?? resolve(HERE, "../../../aesthetic-computer-vault"), "lith/.env");
  const line = existsSync(env) && readFileSync(env, "utf8").split(/\r?\n/).find((l) => l.startsWith("ELEVENLABS_API_KEY="));
  if (!line) throw new Error("no ELEVENLABS_API_KEY in the environment or the vault's lith/.env");
  return line.slice("ELEVENLABS_API_KEY=".length).replace(/^["']|["']$/g, "");
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const only = flag("--only"), variant = flag("--variant", "");
  let chars = 0, made = 0;
  for (const line of lyric()) {
    if (only && line.id !== only) continue;
    const settings = { ...SETTINGS[line.kind], ...(flag("--stability") ? { stability: Number(flag("--stability")) } : {}), ...(flag("--style") ? { style: Number(flag("--style")) } : {}) };
    const { body, path } = takePath(line, { tag: flag("--tag", TAGS[line.kind]), settings, variant });
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
