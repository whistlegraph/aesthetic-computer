// SFX - Sound-effects generation API via ElevenLabs.
// Sibling of `say.js` (TTS): same auth, DO Spaces cache, CDN redirect,
// and Mongo ledger patterns — but hits the text-to-sound-effects model
// instead of text-to-speech.
//
// ElevenLabs sound-generation (latest model: eleven_text_to_sound_v2):
//   POST https://api.elevenlabs.io/v1/sound-generation
//   body: { text, model_id, duration_seconds?, prompt_influence?, loop? }
//   returns: binary audio (mp3)
//
// Usage from a piece / pipeline:
//   POST /api/sfx { text: "distant thunder rolling over a city" }
//   POST /api/sfx { text: "8-bit coin pickup", duration_seconds: 1.5,
//                   prompt_influence: 0.6, loop: false }
//
// Costs real money per generation — cached by content-hash to the CDN
// (sfx-cache/). Pass { bust: true } to regenerate.

const crypto = require("crypto");
const { S3Client, HeadObjectCommand, PutObjectCommand } = require("@aws-sdk/client-s3");

const SFX_MODEL = "eleven_text_to_sound_v2"; // latest text-to-sound model
// Higher-fidelity than the API default (mp3_22050_32) — paid-tier key.
const DEFAULT_OUTPUT_FORMAT = "mp3_44100_128";

// Initialize S3 client for Digital Ocean Spaces (same creds as say.js).
const s3 = new S3Client({
  endpoint: `https://${process.env.ART_ENDPOINT}`,
  region: "us-east-1", // DO Spaces requires a region, but it's ignored
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

const BUCKET = process.env.ART_SPACE_NAME;
const CDN_URL = "https://art.aesthetic.computer";
const CACHE_PREFIX = "sfx-cache/";

// Cache key from every parameter that changes the generated audio.
function getCacheKey(spec, text) {
  const parts = `${SFX_MODEL}:${spec}:${text}`;
  const hash = crypto.createHash("sha256").update(parts).digest("hex");
  return `${CACHE_PREFIX}${hash}.mp3`;
}

async function checkCache(key) {
  try {
    await s3.send(new HeadObjectCommand({ Bucket: BUCKET, Key: key }));
    return `${CDN_URL}/${key}`;
  } catch (err) {
    if (err.name === "NotFound" || err.$metadata?.httpStatusCode === 404) {
      return null; // Not cached
    }
    console.error("Cache check error:", err);
    return null;
  }
}

// Ledger every generation into the `sfx` MongoDB collection — mirrors
// the `sayings` collection in say.js. Failures are swallowed.
async function recordSfx(entry) {
  let database;
  try {
    const { connect } = await import("../../backend/database.mjs");
    database = await connect();
    const collection = database.db.collection("sfx");
    await collection.createIndex({ when: -1 });
    await collection.createIndex({ cacheKey: 1 });
    await collection.insertOne({ ...entry, when: new Date() });
  } catch (err) {
    console.error("⚠️ sfx log failed:", err?.message || err);
  } finally {
    if (database) {
      try { await database.disconnect(); } catch (_) {}
    }
  }
}

async function saveToCache(key, audioBuffer, metadata = {}) {
  try {
    const cleanMeta = {};
    for (const [k, v] of Object.entries(metadata)) {
      if (v == null) continue;
      const str = String(v).slice(0, 1800);
      cleanMeta[k] = Buffer.from(str, "utf8").toString("ascii").replace(/[\r\n]/g, " ");
    }

    await s3.send(new PutObjectCommand({
      Bucket: BUCKET,
      Key: key,
      Body: audioBuffer,
      ContentType: "audio/mpeg",
      ACL: "public-read",
      CacheControl: "public, max-age=31536000", // 1 year (audio doesn't change)
      Metadata: cleanMeta,
    }));
    console.log(`✅ Cached SFX: ${CDN_URL}/${key}`);
    return `${CDN_URL}/${key}`;
  } catch (err) {
    console.error("Cache write error:", err);
    return null;
  }
}

// Generate a sound effect with ElevenLabs.
async function generateSfx(text, { durationSeconds, promptInfluence, loop, outputFormat }) {
  const payload = { text, model_id: SFX_MODEL };
  if (durationSeconds != null) payload.duration_seconds = durationSeconds;
  if (promptInfluence != null) payload.prompt_influence = promptInfluence;
  if (loop != null) payload.loop = loop;

  const url = `https://api.elevenlabs.io/v1/sound-generation?output_format=${encodeURIComponent(outputFormat)}`;

  const response = await fetch(url, {
    method: "POST",
    headers: {
      "xi-api-key": process.env.ELEVENLABS_API_KEY,
      "Content-Type": "application/json",
    },
    body: JSON.stringify(payload),
  });

  if (!response.ok) {
    const err = await response.text();
    throw new Error(`ElevenLabs SFX API error ${response.status}: ${err}`);
  }

  return Buffer.from(await response.arrayBuffer());
}

exports.handler = async (event) => {
  const method = event.httpMethod;
  const headers = corsHeaders(event);

  if (method === "OPTIONS") {
    return {
      statusCode: 200,
      headers,
      body: JSON.stringify({ message: "Success!" }),
    };
  } else if (method !== "POST") {
    return {
      statusCode: 405,
      headers,
      body: JSON.stringify({ message: "Method Not Allowed" }),
    };
  }

  const body = JSON.parse(event.body || "{}");

  // The prompt describing the desired sound. Accept `text` (canonical),
  // `prompt`, or `from` (parity with say.js).
  const text = (body.text || body.prompt || body.from || "").trim();
  if (!text) {
    return {
      statusCode: 400,
      headers,
      body: JSON.stringify({ message: "Missing `text` (sound description)." }),
    };
  }

  // duration_seconds: 0.5–30, or null to let the model auto-detect length.
  const durationSeconds = (typeof body.duration_seconds === "number" || typeof body.duration === "number")
    ? Math.max(0.5, Math.min(30, body.duration_seconds ?? body.duration))
    : null;
  // prompt_influence: 0–1 (default 0.3). Higher = closer to the prompt.
  const promptInfluence = (typeof body.prompt_influence === "number")
    ? Math.max(0, Math.min(1, body.prompt_influence))
    : null;
  // loop: produce a seamless loop (v2 only).
  const loop = body.loop === true ? true : (body.loop === false ? false : null);
  const outputFormat = body.output_format || DEFAULT_OUTPUT_FORMAT;
  const bustCache = body.bust === true;

  // Cache key spec — every knob that changes the audio.
  const durSuffix = durationSeconds != null ? `-d${durationSeconds}` : "";
  const piSuffix = promptInfluence != null ? `-pi${promptInfluence}` : "";
  const loopSuffix = loop === true ? "-loop" : "";
  const spec = `${outputFormat}${durSuffix}${piSuffix}${loopSuffix}`;
  const cacheKey = getCacheKey(spec, text);

  try {
    if (!bustCache) {
      const cachedUrl = await checkCache(cacheKey);
      if (cachedUrl) {
        console.log(`🎯 SFX cache hit: ${cachedUrl}`);
        await recordSfx({ text, spec, cacheKey, url: cachedUrl, cached: true });
        return {
          statusCode: 302,
          headers: { ...headers, Location: cachedUrl, "Cache-Control": "public, max-age=86400" },
          body: "",
        };
      }
    } else {
      console.log(`🧹 SFX cache bust requested for: ${text.substring(0, 50)}...`);
    }

    console.log(`🔄 SFX ${bustCache ? "regenerating" : "cache miss"}: ${text.substring(0, 50)}...`);

    const audioBuffer = await generateSfx(text, { durationSeconds, promptInfluence, loop, outputFormat });

    if (!audioBuffer || audioBuffer.length === 0) {
      return {
        statusCode: 500,
        headers,
        body: JSON.stringify({ message: "Failed to generate sound effect." }),
      };
    }

    console.log(`🔊 Generated SFX (${SFX_MODEL}): ${(audioBuffer.length / 1024).toFixed(0)} KB`);

    const cdnUrl = await saveToCache(cacheKey, audioBuffer, {
      text,
      model: SFX_MODEL,
      spec,
      ts: new Date().toISOString(),
    });

    if (cdnUrl) {
      await recordSfx({ text, spec, cacheKey, url: cdnUrl, cached: false });
      return {
        statusCode: 302,
        headers: { ...headers, Location: cdnUrl },
        body: "",
      };
    }

    // Fallback: return audio directly if caching failed.
    return {
      statusCode: 200,
      headers: {
        ...headers,
        "Content-Disposition": 'inline; filename="sfx.mp3"',
        "Content-Type": "audio/mpeg",
      },
      body: audioBuffer.toString("base64"),
      isBase64Encoded: true,
    };
  } catch (error) {
    console.error("SFX generation failed:", error);
    return {
      statusCode: 500,
      headers,
      body: JSON.stringify({ message: "An error has occurred.", error: error.message }),
    };
  }
};

function corsHeaders(event) {
  const dev = process.env.CONTEXT === "dev";
  const production = !dev;
  let allowedOrigin = production ? "https://aesthetic.computer" : "*";
  if (event.headers.origin === "null") allowedOrigin = "*";

  return {
    "Access-Control-Allow-Methods": "GET,OPTIONS,PATCH,DELETE,POST,PUT",
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Credentials": true,
    "Access-Control-Allow-Headers":
      "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version",
  };
}
