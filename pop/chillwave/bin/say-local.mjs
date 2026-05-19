#!/usr/bin/env node
// say-local.mjs — a LOCAL stand-in for /api/say (jeffrey-pvc only).
//
// The production host (aesthetic.computer) is intermittently
// unreachable, but ElevenLabs itself is fine. This serves the exact
// request/response contract pop/bin/say.mjs expects, by replicating
// the `generateJeffrey` branch of system/netlify/functions/say.js —
// no S3/Mongo cache layer (say.mjs already content-hash caches).
//
// Reads ELEVENLABS_API_KEY from the vault devcontainer.env.
//
// Usage:
//   node pop/chillwave/bin/say-local.mjs            # listens :8899
//   SAY_ENDPOINT=http://127.0.0.1:8899/api/say \
//     node pop/bin/say.mjs <lyric> --provider jeffrey --voice neutral:0 --timestamps

import { createServer } from "node:http";
import { readFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const PORT = Number(process.env.SAY_LOCAL_PORT || 8899);

const JEFFREY_VOICE_ID = "dYNGZ848Oo6DtNBoeqgh"; // same as say.js

function loadKey() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("ELEVENLABS_API_KEY=")) {
        return line.slice("ELEVENLABS_API_KEY=".length).trim()
          .replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("ELEVENLABS_API_KEY not in env or vault devcontainer.env");
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
  if (req.method !== "POST" || !req.url.startsWith("/api/say")) {
    res.writeHead(404).end("not found");
    return;
  }
  try {
    const b = await readBody(req);
    const text = b.from ?? b.text ?? "";
    if (!text.trim()) { res.writeHead(400).end("no text"); return; }
    // jeffrey-pvc, calm non-scream defaults (mirrors say.js), overridable
    const voiceSettings = {
      stability:        b.stability  ?? 0.65,
      similarity_boost: b.similarity ?? 0.9,
      style:            b.style      ?? 0.15,
      use_speaker_boost: true,
      speed:            b.speed      ?? 1.0,
    };
    const withTs = b.withTimestamps === true;
    const base = `https://api.elevenlabs.io/v1/text-to-speech/${JEFFREY_VOICE_ID}`;
    const url = withTs ? `${base}/with-timestamps` : base;
    const r = await fetch(url, {
      method: "POST",
      headers: { "xi-api-key": KEY, "Content-Type": "application/json" },
      body: JSON.stringify({
        text,
        model_id: "eleven_multilingual_v2",
        voice_settings: voiceSettings,
      }),
    });
    if (!r.ok) {
      const err = await r.text();
      console.error(`✗ ElevenLabs ${r.status}: ${err.slice(0, 300)}`);
      res.writeHead(502, { "content-type": "text/plain" }).end(err.slice(0, 500));
      return;
    }
    if (withTs) {
      const j = await r.json();
      res.writeHead(200, { "content-type": "application/json" });
      res.end(JSON.stringify({
        audio: j.audio_base64,
        alignment: j.alignment,
        normalizedAlignment: j.normalized_alignment,
        voiceId: "jeffrey-pvc",
      }));
      console.log(`✓ jeffrey-pvc +timestamps · ${text.length} chars`);
    } else {
      const buf = Buffer.from(await r.arrayBuffer());
      res.writeHead(200, { "content-type": "audio/mpeg" });
      res.end(buf);
      console.log(`✓ jeffrey-pvc · ${text.length} chars · ${(buf.length / 1024) | 0} KB`);
    }
  } catch (e) {
    console.error("✗", e.message);
    res.writeHead(500, { "content-type": "text/plain" }).end(String(e.message));
  }
});

server.listen(PORT, "127.0.0.1", () => {
  console.log(`▸ say-local (jeffrey-pvc) → http://127.0.0.1:${PORT}/api/say`);
});
