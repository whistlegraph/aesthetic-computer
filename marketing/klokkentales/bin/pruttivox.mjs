#!/usr/bin/env node

// Pruttivox: read a community text aloud in the consented Prutti IVC.
// Every clip ends with a spoken synthetic-voice tag and is logged to the
// vault with its requester, so provenance survives the disposable out/ dir.
//
//   node bin/pruttivox.mjs "Hej klokken, god torsdag" --from @snakes
//   node bin/pruttivox.mjs "..." --slug god-torsdag --publish
//   node bin/pruttivox.mjs --file interview.txt --slug interview --from prutti

import { appendFileSync, existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { klokkentalesVault, loadKlokkentalesEnv } from "../lib/env.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const outRoot = resolve(HERE, "..", "out", "pruttivox");
const logPath = resolve(klokkentalesVault, "pruttivox", "log.jsonl");
const tagPath = resolve(outRoot, "tag.mp3");
const tagText = "Pruttivox. Syntetisk stemme.";
const publishPrefix = "klokkentales/pruttivox";

const env = loadKlokkentalesEnv();
const apiBase = env.ELEVENLABS_API_BASE || "https://api.elevenlabs.io";
const receiptPath = resolve(klokkentalesVault, "voices", "prutti", "voice.json");
const voiceId = env.PRUTTI_ELEVENLABS_VOICE_ID || (existsSync(receiptPath)
  ? JSON.parse(readFileSync(receiptPath, "utf8")).voiceId
  : null);

const argv = process.argv.slice(2);
const flag = (name) => {
  const index = argv.indexOf(`--${name}`);
  return index >= 0 ? argv[index + 1] : null;
};
const valued = new Set([flag("from"), flag("slug"), flag("file")].filter(Boolean));
const file = flag("file");
const text = (file
  ? readFileSync(resolve(file), "utf8")
  : argv.find((arg) => !arg.startsWith("--") && !valued.has(arg)) || "").trim();

if (!voiceId) throw new Error("Prutti voice has not been created; see voice.mjs");
if (!env.ELEVENLABS_API_KEY) throw new Error("ELEVENLABS_API_KEY is missing");
if (!text) {
  throw new Error('usage: pruttivox.mjs "the text" | --file text.txt [--from @handle] [--slug name] [--publish] [--force]');
}

// the voice gets brittle past a page, so a long text is spoken in pieces and
// stitched — seams land on paragraph or sentence ends, where a breath belongs.
function pieces(body, limit = 900) {
  const out = [];
  const fit = (parts, join) => {
    for (const part of parts) {
      const last = out[out.length - 1];
      if (last && last.length + join.length + part.length <= limit) out[out.length - 1] = last + join + part;
      else out.push(part);
    }
  };
  for (const paragraph of body.split(/\n\s*\n/).map((p) => p.replace(/\s+/g, " ").trim()).filter(Boolean)) {
    if (paragraph.length <= limit) fit([paragraph], "\n\n");
    else for (const sentence of paragraph.split(/(?<=[.!?])\s+/)) {
      if (sentence.length <= limit) fit([sentence], " ");
      else fit(sentence.match(new RegExp(`.{1,${limit}}(\\s|$)`, "g")).map((s) => s.trim()), " ");
    }
  }
  return out;
}

const slug = (flag("slug") || text.trim().toLowerCase()
  .replace(/[^\p{L}\p{N}\s-]/gu, "").split(/\s+/).slice(0, 5).join("-"))
  .replace(/[^a-z0-9æøå-]/g, "-").replace(/-+/g, "-").replace(/^-|-$/g, "");
if (!slug) throw new Error("could not derive a slug from the text; pass --slug");
const from = flag("from") || "";
const output = resolve(outRoot, `${slug}.mp3`);
const publish = argv.includes("--publish");

const log = existsSync(logPath)
  ? readFileSync(logPath, "utf8").trim().split("\n").filter(Boolean).map((line) => JSON.parse(line))
  : [];
if (publish && !argv.includes("--force") && log.some((entry) => entry.slug === slug && entry.published)) {
  throw new Error(`"${slug}" was already published; a republish sits stale on the CDN for ~1h — pick a new slug or pass --force`);
}

async function speak(line, destination) {
  const response = await fetch(`${apiBase}/v1/text-to-speech/${encodeURIComponent(voiceId)}`, {
    method: "POST",
    headers: { "xi-api-key": env.ELEVENLABS_API_KEY, "Content-Type": "application/json" },
    body: JSON.stringify({
      text: line,
      model_id: "eleven_multilingual_v2",
      voice_settings: { stability: 0.38, similarity_boost: 0.9, style: 0.48, use_speaker_boost: true, speed: 0.98 },
    }),
    signal: AbortSignal.timeout(120_000),
  });
  if (!response.ok) throw new Error(`ElevenLabs ${response.status}: ${(await response.text()).slice(0, 500)}`);
  writeFileSync(destination, Buffer.from(await response.arrayBuffer()), { mode: 0o600 });
}

mkdirSync(outRoot, { recursive: true });
if (!existsSync(tagPath)) await speak(tagText, tagPath);
const bodies = pieces(text);
const bodyPaths = [];
for (const [index, body] of bodies.entries()) {
  const bodyPath = resolve(outRoot, bodies.length > 1 ? `${slug}.body-${index + 1}.mp3` : `${slug}.body.mp3`);
  await speak(body, bodyPath);
  bodyPaths.push(bodyPath);
}

const pad = bodyPaths.map((_, i) => `[${i}:a]apad=pad_dur=0.4[a${i}]`).join(";");
const chain = bodyPaths.map((_, i) => `[a${i}]`).join("");
execFileSync("ffmpeg", [
  "-y", ...bodyPaths.flatMap((path) => ["-i", path]), "-i", tagPath,
  "-filter_complex", `${pad};${chain}[${bodyPaths.length}:a]concat=n=${bodyPaths.length + 1}:v=0:a=1`,
  "-ar", "44100", "-b:a", "192k",
  "-metadata", "artist=Pruttivox (syntetisk stemme / synthetic voice)",
  "-metadata", "album=Klokkentales",
  "-metadata", `title=${slug}`,
  "-metadata", "comment=Consented ElevenLabs IVC — aesthetic.computer/klokkentales",
  output,
], { stdio: "ignore" });

const url = `https://assets.aesthetic.computer/${publishPrefix}/${slug}.mp3`;
if (publish) {
  execFileSync("aws", [
    "s3", "cp", output, `s3://assets-aesthetic-computer/${publishPrefix}/${slug}.mp3`,
    "--endpoint-url", "https://sfo3.digitaloceanspaces.com", "--acl", "public-read",
  ], { stdio: "inherit" });
}

mkdirSync(dirname(logPath), { recursive: true });
appendFileSync(logPath, JSON.stringify({
  slug, text, from, published: publish, url: publish ? url : undefined,
  at: new Date().toISOString(),
}) + "\n", { mode: 0o600 });

console.log(output);
if (publish) console.log(url);
