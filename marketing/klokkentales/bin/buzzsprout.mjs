#!/usr/bin/env node

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { released } from "../lib/released.mjs";
import { loadKlokkentalesEnv } from "../lib/env.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
const vaultEnv = resolve(ROOT, "..", "..", "..", "aesthetic-computer-vault", "klokkentales", ".env");

function credentials() {
  const loaded = loadKlokkentalesEnv();
  let token = loaded.BUZZSPROUT_TOKEN;
  let podcast = loaded.KLOKKENTALES_BUZZSPROUT_PODCAST_ID;
  if ((!token || !podcast) && existsSync(vaultEnv)) {
    for (const line of readFileSync(vaultEnv, "utf8").split("\n")) {
      const match = line.match(/^\s*(BUZZSPROUT_TOKEN|KLOKKENTALES_BUZZSPROUT_PODCAST_ID)\s*=\s*(.+?)\s*$/);
      if (match?.[1] === "BUZZSPROUT_TOKEN") token ||= match[2];
      if (match?.[1] === "KLOKKENTALES_BUZZSPROUT_PODCAST_ID") podcast ||= match[2];
    }
  }
  if (!token || !podcast) throw new Error(`missing dedicated Klokkentales Buzzsprout credentials in env or ${vaultEnv}`);
  return { token, podcast };
}

const argv = process.argv.slice(2);
const command = argv[0] === "publish" ? "publish" : "upload";
const slug = command === "publish" ? argv[1] : argv.find((arg) => !arg.startsWith("--"));
if (!slug) throw new Error("usage: buzzsprout.mjs <slug> --private | buzzsprout.mjs publish <slug>");

const { token, podcast } = credentials();
const api = `https://www.buzzsprout.com/api/${podcast}`;
const auth = { Authorization: `Token token="${token}"` };
const receiptPath = resolve(OUT, `${slug}.buzzsprout.json`);

if (command === "publish") {
  if (!released(slug)) throw new Error(`${slug} is not in lib/released.mjs; both narrators must approve before public release`);
  if (!existsSync(receiptPath)) throw new Error(`missing private-stage receipt ${receiptPath}`);
  const receipt = JSON.parse(readFileSync(receiptPath, "utf8"));
  const form = new FormData();
  form.append("private", "false");
  form.append("published_at", new Date().toISOString());
  const response = await fetch(`${api}/episodes/${receipt.id}.json`, { method: "PUT", headers: auth, body: form });
  if (!response.ok) throw new Error(`Buzzsprout publish HTTP ${response.status}: ${(await response.text()).slice(0, 240)}`);
  const result = await response.json();
  writeFileSync(receiptPath, JSON.stringify(result, null, 2) + "\n");
  console.log(`published ${slug} on the Klokkentales show`);
  process.exit(0);
}

if (!argv.includes("--private")) throw new Error("first upload must use --private; public release is a separate approval-gated command");
const metaPath = resolve(OUT, `${slug}.json`);
const audioPath = resolve(OUT, `${slug}.mp3`);
const coverPath = resolve(OUT, `${slug}-cover-1400.png`);
for (const file of [metaPath, audioPath, coverPath]) if (!existsSync(file)) throw new Error(`missing ${file}`);
if (existsSync(receiptPath)) throw new Error(`${slug} already has a Buzzsprout receipt`);
const meta = JSON.parse(readFileSync(metaPath, "utf8"));
if (meta.pruttiVoice !== "consented-elevenlabs-voice") throw new Error("refusing to stage a generic Prutti casting placeholder");

const form = new FormData();
form.append("title", meta.title);
form.append("description", `${meta.description}\n\n${meta.disclosure}\n\nListen on Aesthetic Computer: https://aesthetic.computer/klokkentales`);
form.append("artist", "Jeffrey & Prutti");
form.append("private", "true");
form.append("audio_file", new Blob([readFileSync(audioPath)], { type: "audio/mpeg" }), basename(audioPath));
form.append("artwork_file", new Blob([readFileSync(coverPath)], { type: "image/png" }), basename(coverPath));
const response = await fetch(`${api}/episodes.json`, { method: "POST", headers: auth, body: form });
if (!response.ok) throw new Error(`Buzzsprout stage HTTP ${response.status}: ${(await response.text()).slice(0, 240)}`);
const result = await response.json();
writeFileSync(receiptPath, JSON.stringify(result, null, 2) + "\n");
console.log(`staged ${slug} privately on the Klokkentales show`);
