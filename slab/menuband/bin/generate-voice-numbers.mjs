#!/usr/bin/env node

// Generate the complete 0...128 Menu Band chooser bank through the cached
// Aesthetic Computer Jeffrey/ElevenLabs endpoint. These files ship inside the
// app; playback itself is offline and never waits on a network request.

import { mkdir, writeFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const output = resolve(
  here, "../Sources/MenuBand/Resources/voice-numbers"
);
const endpoint = "https://aesthetic.computer/api/say";
const concurrency = 5;

await mkdir(output, { recursive: true });
const pending = Array.from({ length: 129 }, (_, number) => number)
  .filter((number) => !existsSync(resolve(output, `${number}.mp3`)));

async function render(number) {
  const response = await fetch(endpoint, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ from: String(number), provider: "jeffrey" }),
    redirect: "follow",
  });
  if (!response.ok) {
    throw new Error(`${number}: HTTP ${response.status} ${await response.text()}`);
  }
  const bytes = Buffer.from(await response.arrayBuffer());
  if (bytes.length < 512) throw new Error(`${number}: short audio response`);
  await writeFile(resolve(output, `${number}.mp3`), bytes);
  process.stdout.write(` ${number}`);
}

async function worker() {
  while (pending.length) {
    const number = pending.shift();
    let lastError;
    for (let attempt = 0; attempt < 3; attempt += 1) {
      try {
        await render(number);
        lastError = null;
        break;
      } catch (error) {
        lastError = error;
        await new Promise((done) => setTimeout(done, 500 * (attempt + 1)));
      }
    }
    if (lastError) throw lastError;
  }
}

process.stdout.write("Jeffrey voice numbers:");
await Promise.all(Array.from({ length: concurrency }, worker));
process.stdout.write("\n");

const manifest = {
  provider: "Jeffrey ElevenLabs PVC via Aesthetic Computer /api/say cache",
  range: [0, 128],
  files: 129,
};
await writeFile(
  resolve(output, "manifest.json"),
  `${JSON.stringify(manifest, null, 2)}\n`
);
