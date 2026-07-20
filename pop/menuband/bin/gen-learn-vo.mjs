#!/usr/bin/env node
// Generate Jeffrey ElevenLabs narration for menuband.app's 24 follow-along
// lessons. Existing takes are kept unless --force is passed.

import { mkdir, writeFile, access } from "node:fs/promises";
import { constants } from "node:fs";
import { resolve } from "node:path";

const OUT = resolve("system/public/menuband/audio/learn");
const FORCE = process.argv.includes("--force");
const ROOTS = [
  ["c", "C"], ["c-sharp", "C sharp"], ["d", "D"], ["e-flat", "E flat"],
  ["e", "E"], ["f", "F"], ["f-sharp", "F sharp"], ["g", "G"],
  ["a-flat", "A flat"], ["a", "A"], ["b-flat", "B flat"], ["b", "B"],
];

const jobs = ROOTS.flatMap(([slug, root]) => [
  { id: `chord-${slug}`, text: `Here’s ${root} major. Listen to the three notes, then play the glowing keys together.` },
  { id: `scale-${slug}`, text: root === "C"
    ? "Here’s how to type out the C scale. Watch it climb, then play it back."
    : `Here’s the ${root} major scale. Watch it climb, then play it back.` },
]);

async function exists(path) {
  try { await access(path, constants.R_OK); return true; } catch { return false; }
}

async function synth({ id, text }) {
  const file = resolve(OUT, `${id}.mp3`);
  if (!FORCE && await exists(file)) return { id, src: `/menuband/audio/learn/${id}.mp3`, text };
  const response = await fetch("https://aesthetic.computer/api/say", {
    method: "POST", redirect: "follow",
    headers: { "content-type": "application/json", origin: "https://aesthetic.computer" },
    body: JSON.stringify({ from: text, provider: "jeffrey", voice: "neutral:0", stability: 0.55 }),
  });
  if (!response.ok) throw new Error(`${id}: /api/say ${response.status} ${await response.text()}`);
  const type = response.headers.get("content-type") || "";
  let audio;
  if (type.includes("application/json")) {
    const json = await response.json();
    if (json.audio) audio = Buffer.from(json.audio, "base64");
    else if (json.url) audio = Buffer.from(await (await fetch(json.url)).arrayBuffer());
  } else audio = Buffer.from(await response.arrayBuffer());
  if (!audio || audio.length < 256) throw new Error(`${id}: empty audio response`);
  await writeFile(file, audio);
  console.log(`✓ ${id} · ${Math.round(audio.length / 1024)} KB`);
  return { id, src: `/menuband/audio/learn/${id}.mp3`, text };
}

await mkdir(OUT, { recursive: true });
const lessons = [];
for (const job of jobs) lessons.push(await synth(job));
await writeFile(resolve(OUT, "manifest.json"), `${JSON.stringify({ provider: "jeffrey", stability: 0.55, lessons }, null, 2)}\n`);
console.log(`✓ ${lessons.length} lesson voices → ${OUT}`);
