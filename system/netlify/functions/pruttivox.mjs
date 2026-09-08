// Pruttivox — speak a @prutti chat message aloud in his consented voice
// clone, with per-word timing for the karaoke highlight in chat/laklok.
//
//   GET /api/pruttivox?id=<chat message id>
//   → { audio, duration, words: [{ i, s, e }], text }
//
// The message id must belong to @prutti — the text is read from the
// database, never from the caller, so this can't become an open TTS
// proxy. Renders cache to Spaces keyed by voice + spoken text, so each
// message costs ElevenLabs once. Voice consent + veto gates live in
// marketing/klokkentales/SCORE.md.

import crypto from "crypto";
import {
  S3Client,
  GetObjectCommand,
  PutObjectCommand,
} from "@aws-sdk/client-s3";
// ObjectId must come from the same mongodb copy the backend client uses —
// a second copy's BSON types fail serialization (BSONVersionError).
import { connect, ObjectId } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const BUCKET = "assets-aesthetic-computer";
const CDN = "https://assets.aesthetic.computer";
const PREFIX = "klokkentales/pruttivox/chat/";
const VOICE_HANDLE = "prutti";

const s3 = new S3Client({
  endpoint: `https://${process.env.ART_ENDPOINT}`,
  region: "us-east-1",
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

// Group the character alignment from `/with-timestamps` into word times.
// A word here is any non-whitespace run — the same split the client uses
// on the displayed text, so word i lines up with displayed token i.
function wordsFromAlignment(alignment) {
  const chars = alignment.characters;
  const starts = alignment.character_start_times_seconds;
  const ends = alignment.character_end_times_seconds;
  const words = [];
  let current = null;
  for (let i = 0; i < chars.length; i += 1) {
    if (/\s/.test(chars[i])) {
      current = null;
      continue;
    }
    if (!current) {
      current = { s: starts[i], e: ends[i] };
      words.push(current);
    } else {
      current.e = ends[i];
    }
  }
  return words.map((w, i) => ({
    i,
    s: Number(w.s.toFixed(3)),
    e: Number(w.e.toFixed(3)),
  }));
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);
  if (event.httpMethod !== "GET")
    return respond(405, { message: "Method Not Allowed" });

  const id = event.queryStringParameters?.id;
  if (!id || !ObjectId.isValid(id))
    return respond(400, { message: "Invalid message id." });

  const voiceId = process.env.PRUTTI_ELEVENLABS_VOICE_ID;
  if (!voiceId || !process.env.ELEVENLABS_API_KEY)
    return respond(503, { message: "Pruttivox is not configured." });

  const database = await connect();
  try {
    let msg = null;
    for (const collection of ["chat-clock", "chat-system"]) {
      msg = await database.db
        .collection(collection)
        .findOne({ _id: new ObjectId(id) });
      if (msg) break;
    }
    if (!msg || msg.deleted || !msg.user || !msg.text)
      return respond(404, { message: "No such message." });

    // One handle can carry several sister auth-provider ids — match them all.
    const speakers = await database.db
      .collection("@handles")
      .find({ handle: { $regex: `^${VOICE_HANDLE}$`, $options: "i" } })
      .toArray();
    if (!speakers.some((s) => s._id === msg.user))
      return respond(403, { message: `Only @${VOICE_HANDLE} can be voxed.` });

    // Spoken text: the displayed tokens with unreadable ones swapped out,
    // 1:1 by index so the client can highlight the displayed word.
    const tokens = [...msg.text.matchAll(/\S+/g)].map((m) => m[0]);
    if (!tokens.length) return respond(422, { message: "Nothing to say." });
    const spoken = tokens
      .map((t) => (/^(https?:\/\/|www\.)/i.test(t) ? "link" : t))
      .join(" ");

    const hash = crypto
      .createHash("sha256")
      .update(`pruttivox:${voiceId}:${spoken}`)
      .digest("hex");
    const audioKey = `${PREFIX}${hash}.mp3`;
    const wordsKey = `${PREFIX}${hash}.json`;

    try {
      const cached = await s3.send(
        new GetObjectCommand({ Bucket: BUCKET, Key: wordsKey }),
      );
      const payload = JSON.parse(await cached.Body.transformToString());
      return respond(200, payload);
    } catch (err) {
      if (err.name !== "NoSuchKey" && err.$metadata?.httpStatusCode !== 404) {
        console.error("Pruttivox cache read error:", err);
      }
    }

    const response = await fetch(
      `https://api.elevenlabs.io/v1/text-to-speech/${voiceId}/with-timestamps`,
      {
        method: "POST",
        headers: {
          "xi-api-key": process.env.ELEVENLABS_API_KEY,
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          text: spoken,
          model_id: "eleven_multilingual_v2",
          voice_settings: {
            stability: 0.38,
            similarity_boost: 0.9,
            style: 0.48,
            use_speaker_boost: true,
            speed: 0.98,
          },
        }),
      },
    );
    if (!response.ok) {
      const detail = (await response.text()).slice(0, 300);
      console.error(`Pruttivox ElevenLabs error ${response.status}:`, detail);
      return respond(502, { message: "Voice synthesis failed." });
    }
    const generated = await response.json();
    const audio = Buffer.from(generated.audio_base64, "base64");
    const words = wordsFromAlignment(generated.alignment);

    const payload = {
      audio: `${CDN}/${audioKey}`,
      duration: words.length ? words[words.length - 1].e : 0,
      words,
      text: msg.text,
      voice: "prutti-ivc",
    };

    await s3.send(
      new PutObjectCommand({
        Bucket: BUCKET,
        Key: audioKey,
        Body: audio,
        ContentType: "audio/mpeg",
        ACL: "public-read",
        CacheControl: "public, max-age=31536000",
      }),
    );
    await s3.send(
      new PutObjectCommand({
        Bucket: BUCKET,
        Key: wordsKey,
        Body: JSON.stringify(payload),
        ContentType: "application/json",
        ACL: "public-read",
        CacheControl: "public, max-age=31536000",
      }),
    );

    // Same ledger the `say` endpoint writes — one row per fresh utterance.
    try {
      await database.db.collection("sayings").insertOne({
        text: spoken,
        provider: "pruttivox",
        voice: "prutti-ivc",
        cacheKey: audioKey,
        url: payload.audio,
        messageId: id,
        cached: false,
        when: new Date(),
      });
    } catch (err) {
      console.error("⚠️ sayings log failed:", err?.message || err);
    }

    return respond(200, payload);
  } catch (error) {
    console.error("Pruttivox failed:", error);
    return respond(500, { message: "An error has occurred." });
  } finally {
    await database.disconnect();
  }
}
