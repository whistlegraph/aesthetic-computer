// Scream — a line screamed in jeffrey-pvc (eleven_v4), the voice behind the
// `scream` takeover. Each take is kept in Spaces beside a sidecar that says
// what it is, and listed in the `screams` collection, so a scream made on the
// prompt can be pulled into a song or used as an asset later.
//
//   POST /api/scream { text, take? }   signed-in handle → the take (rendered once)
//   GET  /api/scream                   the library, newest first (?limit, ?by=@handle)
//   GET  /api/scream?id=<id>           one take
//
//   take: { id, audio, text, sent, tag, duration, words: [{ w, s, e }], voice, model, settings, by, when }
//
// The same text and take number is the same request, so it costs ElevenLabs
// once. An empty text is a wordless scream from the chaos bank's deliveries.

import { S3Client, GetObjectCommand, PutObjectCommand } from "@aws-sdk/client-s3";
import { connect } from "../../backend/database.mjs";
import { authorize, handleFor } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";
import { filter } from "../../../shared/filter.mjs";
import { shape, clean, wordTimes, VOICE, MODEL } from "../../backend/scream.mjs";

const BUCKET = "assets-aesthetic-computer";
const CDN = "https://assets.aesthetic.computer";
const PREFIX = "screams/jeffrey/";
const HOURLY = 30; // Fresh renders one handle may ask for in an hour.

const s3 = new S3Client({
  endpoint: `https://${process.env.ART_ENDPOINT}`,
  region: "us-east-1",
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

async function kept(id) {
  try {
    const got = await s3.send(new GetObjectCommand({ Bucket: BUCKET, Key: `${PREFIX}${id}.json` }));
    return JSON.parse(await got.Body.transformToString());
  } catch (err) {
    if (err.name !== "NoSuchKey" && err.$metadata?.httpStatusCode !== 404) console.error("😱 Scream cache read:", err);
    return null;
  }
}

const put = (key, Body, ContentType) =>
  s3.send(new PutObjectCommand({ Bucket: BUCKET, Key: key, Body, ContentType, ACL: "public-read", CacheControl: "public, max-age=31536000" }));

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);

  if (event.httpMethod === "GET") {
    const { id, by, limit } = event.queryStringParameters || {};
    const database = await connect();
    try {
      const screams = database.db.collection("screams");
      if (id) {
        const take = await screams.findOne({ _id: id }, { projection: { user: 0 } });
        return take ? respond(200, take) : respond(404, { message: "No such scream." });
      }
      const query = by ? { by: "@" + by.replace(/^@/, "").toLowerCase() } : {};
      const takes = await screams.find(query, { projection: { user: 0 } })
        .sort({ when: -1 }).limit(Math.min(parseInt(limit) || 50, 200)).toArray();
      return respond(200, { screams: takes });
    } finally {
      await database.disconnect();
    }
  }

  if (event.httpMethod !== "POST") return respond(405, { message: "Method Not Allowed" });
  if (!process.env.ELEVENLABS_API_KEY) return respond(503, { message: "Scream is not configured." });

  let input;
  try { input = JSON.parse(event.body || "{}"); } catch { return respond(400, { message: "Bad JSON." }); }

  const user = await authorize(event.headers);
  const handle = user?.sub && String((await handleFor(user.sub)) || "").toLowerCase();
  if (!handle) return respond(403, { message: "A scream needs a signed-in handle." });

  const line = clean(input.text);
  if (filter(line) !== line) return respond(422, { message: "That can't be screamed." });
  const take = Math.max(0, Math.min(99, parseInt(input.take) || 0));
  const { id, body, tag, text } = shape(line, take);

  const database = await connect();
  try {
    const screams = database.db.collection("screams");
    const cached = await kept(id);
    if (cached) {
      await screams.updateOne({ _id: id }, { $inc: { uses: 1 }, $set: { lastUsed: new Date() } });
      return respond(200, cached);
    }

    const recent = await screams.countDocuments({ user: user.sub, when: { $gt: new Date(Date.now() - 3600e3) } });
    if (recent >= HOURLY) return respond(429, { message: "Too many screams. Rest your voice." });

    const response = await fetch(
      `https://api.elevenlabs.io/v1/text-to-speech/${VOICE}/with-timestamps?output_format=mp3_44100_128`,
      {
        method: "POST",
        headers: { "xi-api-key": process.env.ELEVENLABS_API_KEY, "Content-Type": "application/json" },
        body: JSON.stringify(body),
      },
    );
    if (!response.ok) {
      console.error(`😱 Scream ElevenLabs ${response.status}:`, (await response.text()).slice(0, 300));
      return respond(502, { message: "The scream did not come out." });
    }
    const made = await response.json();
    const audio = Buffer.from(made.audio_base64, "base64");
    const words = wordTimes(made.alignment, text);

    const scream = {
      id,
      audio: `${CDN}/${PREFIX}${id}.mp3`,
      text,
      sent: body.text,
      tag,
      // 128 kbps: the length is in the byte count, and outlasts the last word.
      duration: Number(Math.max(audio.length / 16000, words.at(-1)?.e || 0).toFixed(3)),
      words,
      voice: "jeffrey-pvc",
      model: MODEL,
      settings: body.voice_settings,
      by: `@${handle}`,
      when: new Date().toISOString(),
    };

    await put(`${PREFIX}${id}.mp3`, audio, "audio/mpeg");
    await put(`${PREFIX}${id}.json`, JSON.stringify(scream), "application/json"); // Last: its presence is the cache hit.

    await screams.updateOne(
      { _id: id },
      { $set: { ...scream, when: new Date(), user: user.sub }, $inc: { uses: 1 } },
      { upsert: true },
    );
    // Same ledger `say` and Pruttivox write — one row per fresh utterance.
    await database.db.collection("sayings").insertOne({
      text: body.text, provider: "scream", voice: "jeffrey-pvc", requester: `@${handle}`,
      cacheKey: `${PREFIX}${id}.mp3`, url: scream.audio, scream: true, cached: false, when: new Date(),
    }).catch((err) => console.error("⚠️ sayings log failed:", err?.message || err));

    return respond(200, scream);
  } catch (error) {
    console.error("😱 Scream failed:", error);
    return respond(500, { message: "An error has occurred." });
  } finally {
    await database.disconnect();
  }
}
