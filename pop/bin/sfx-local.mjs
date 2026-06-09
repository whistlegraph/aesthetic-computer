#!/usr/bin/env node
// sfx-local.mjs — a LOCAL stand-in for /api/sfx (ElevenLabs SFX).
//
// Sibling of pop/chillwave/bin/say-local.mjs. Replicates the
// `generateSfx` branch of system/netlify/functions/sfx.js — hits the
// ElevenLabs text-to-sound-effects model directly and returns the audio
// bytes. No S3/Mongo cache layer (sfx.mjs already content-hash caches
// every cue locally), so this writes nothing to the prod CDN.
//
// Reads ELEVENLABS_API_KEY from the environment, then the vault
// (aesthetic-computer-vault/lith/.env or .devcontainer envs).
//
// Usage:
//   node pop/bin/sfx-local.mjs                       # listens :8898
//   SFX_ENDPOINT=http://127.0.0.1:8898/api/sfx \
//     node pop/bin/sfx.mjs --text "warm analog drone" --duration 20

import { createServer } from "node:http";
import { readFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const PORT = Number(process.env.SFX_LOCAL_PORT || 8898);

const SFX_MODEL = "eleven_text_to_sound_v2"; // same as sfx.js
const DEFAULT_OUTPUT_FORMAT = "mp3_44100_128";

function loadKey() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  const candidates = [
    `${REPO}/aesthetic-computer-vault/lith/.env`,
    `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`,
  ];
  for (const path of candidates) {
    if (!existsSync(path)) continue;
    for (const line of readFileSync(path, "utf8").split("\n")) {
      if (line.startsWith("ELEVENLABS_API_KEY=")) {
        return line.slice("ELEVENLABS_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("ELEVENLABS_API_KEY not in env or vault");
}
const KEY = loadKey();

function readBody(req) {
  return new Promise((res, rej) => {
    const chunks = [];
    req.on("data", (c) => chunks.push(c));
    req.on("end", () => {
      try { res(JSON.parse(Buffer.concat(chunks).toString("utf8") || "{}")); }
      catch (e) { rej(e); }
    });
    req.on("error", rej);
  });
}

const server = createServer(async (req, res) => {
  if (req.method !== "POST" || !req.url.startsWith("/api/sfx")) {
    res.writeHead(404).end("not found");
    return;
  }
  try {
    const b = await readBody(req);
    const text = (b.text || b.prompt || b.from || "").trim();
    if (!text) { res.writeHead(400).end("no text"); return; }

    const payload = { text, model_id: SFX_MODEL };
    if (typeof b.duration_seconds === "number") payload.duration_seconds = Math.max(0.5, Math.min(30, b.duration_seconds));
    if (typeof b.prompt_influence === "number") payload.prompt_influence = Math.max(0, Math.min(1, b.prompt_influence));
    if (b.loop === true) payload.loop = true;

    const fmt = b.output_format || DEFAULT_OUTPUT_FORMAT;
    const url = `https://api.elevenlabs.io/v1/sound-generation?output_format=${encodeURIComponent(fmt)}`;
    const r = await fetch(url, {
      method: "POST",
      headers: { "xi-api-key": KEY, "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    if (!r.ok) {
      const err = await r.text();
      console.error(`✗ ElevenLabs SFX ${r.status}: ${err.slice(0, 300)}`);
      res.writeHead(502, { "content-type": "text/plain" }).end(err.slice(0, 500));
      return;
    }
    const buf = Buffer.from(await r.arrayBuffer());
    res.writeHead(200, { "content-type": "audio/mpeg" });
    res.end(buf);
    console.log(`✓ sfx · "${text.slice(0, 48)}${text.length > 48 ? "…" : ""}" · ${(buf.length / 1024) | 0} KB`);
  } catch (e) {
    console.error("✗", e.message);
    res.writeHead(500, { "content-type": "text/plain" }).end(String(e.message));
  }
});

server.listen(PORT, "127.0.0.1", () => {
  console.log(`▸ sfx-local (${SFX_MODEL}) → http://127.0.0.1:${PORT}/api/sfx`);
});
