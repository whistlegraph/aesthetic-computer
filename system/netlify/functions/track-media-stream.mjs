// track-media-stream.mjs
// Streaming SSE variant of /api/track-media for granular progress updates.
// Usage: POST /api/track-media-stream
// Emits events:
//   event: progress  data: { stage, message }
//   event: complete  data: { slug, code, paintingId? }
//   event: error     data: { error }

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { createMediaRecord, MediaTypes } from "../../backend/media-atproto.mjs";
import { S3Client, PutObjectAclCommand } from "@aws-sdk/client-s3";
import { stream } from "@netlify/functions";

const MAX_TAPE_DURATION = 30;
const dev = process.env.CONTEXT === "dev";

const headers = {
  "Content-Type": "text/event-stream",
  "Cache-Control": "no-cache",
  Connection: "keep-alive",
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
  "Access-Control-Allow-Methods": "POST, OPTIONS",
};

function sse(event, data) {
  return `event: ${event}\ndata: ${JSON.stringify(data)}\n\n`;
}

export const handler = stream(async (event) => {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers, body: "" };
  }

  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ error: "Method not allowed" }),
    };
  }

  const { readable, writable } = new TransformStream();
  const writer = writable.getWriter();
  const encoder = new TextEncoder();

  const send = async (eventType, data) => {
    await writer.write(encoder.encode(sse(eventType, data)));
  };

  let closed = false;
  const close = async () => {
    if (!closed) {
      closed = true;
      await writer.close();
    }
  };

  (async () => {
    let database = null;
    try {
      await send("progress", { stage: "init", message: "Parsing request..." });

      let body;
      try {
        body = JSON.parse(event.body || "{}");
      } catch {
        await send("error", { error: "Invalid JSON body" });
        return;
      }

      await send("progress", { stage: "auth", message: "Checking authorization..." });

      const authHeader = event.headers.authorization || event.headers.Authorization;
      let user = null;
      if (authHeader) {
        user = await authorize(event.headers);
      }

      await send("progress", { stage: "database", message: "Connecting to database..." });
      database = await connect();

      let type;
      let metadata;
      if (body.ext === "png") {
        type = "paintings";
      } else if (body.ext === "mjs" || body.ext === "lisp" || body.ext === "lua") {
        type = "pieces";
      } else if (body.ext === "zip") {
        type = "tapes";
        metadata = body.metadata || {};
        const duration = metadata.totalDuration || 0;
        if (duration > MAX_TAPE_DURATION) {
          await send("error", {
            error: `Tape duration ${duration}s exceeds maximum ${MAX_TAPE_DURATION}s`,
            code: "TAPE_TOO_LONG",
            maxDuration: MAX_TAPE_DURATION,
          });
          return;
        }
      } else {
        await send("error", { error: `Unsupported media type: ${body.ext}` });
        return;
      }

      const collection = database.db.collection(type);

      await send("progress", { stage: "indexes", message: "Preparing indexes..." });
      try {
        if (type !== "tapes") {
          await collection.createIndex({ code: 1 }, { unique: true, sparse: true });
        } else {
          await collection.createIndex({ code: 1 }, { unique: true });
        }
        await collection.createIndex({ user: 1 });
        await collection.createIndex({ when: 1 });
        await collection.createIndex({ slug: 1 });
        if (user) {
          await collection.createIndex({ slug: 1, user: 1 }, { unique: true });
        }
      } catch (indexError) {
        console.log(`ℹ️ track-media-stream index setup: ${indexError.message}`);
      }

      await send("progress", { stage: "code", message: "Generating short code..." });
      const code = await generateUniqueCode(collection, {
        mode: type === "tapes" ? "random" : undefined,
        type: type === "tapes" ? "tape" : undefined,
      });

      const slug = body.slug;
      const when = new Date();
      const record = {
        code,
        slug,
        when,
        bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
      };

      if (type === "tapes") {
        record.nuked = false;
        record.mp4Status = "pending";
      }

      if (user) {
        record.user = user.sub;
      }

      await send("progress", { stage: "insert", message: "Saving media record..." });
      const insertResult = await collection.insertOne(record);
      const mediaId = insertResult.insertedId;

      if (type === "tapes") {
        await send("progress", { stage: "acl", message: "Setting tape ACL..." });
        try {
          const s3Client = new S3Client({
            endpoint: `https://sfo3.digitaloceanspaces.com`,
            region: "us-east-1",
            credentials: {
              accessKeyId: process.env.ART_KEY || process.env.DO_SPACES_KEY,
              secretAccessKey: process.env.ART_SECRET || process.env.DO_SPACES_SECRET,
            },
          });

          const aclKey = user ? `${user.sub}/${slug}.zip` : `${slug}.zip`;
          await s3Client.send(
            new PutObjectAclCommand({
              Bucket: record.bucket,
              Key: aclKey,
              ACL: "public-read",
            }),
          );
        } catch (aclError) {
          console.error("track-media-stream ACL warning:", aclError);
        }

        await send("progress", { stage: "oven", message: "Dispatching to oven..." });
        try {
          const isDev = process.env.CONTEXT === "dev" || process.env.NODE_ENV === "development";
          const baseUrl = isDev ? "https://localhost:8888" : (process.env.URL || "https://aesthetic.computer");
          const key = user ? `${user.sub}/${slug}.zip` : `${slug}.zip`;
          const zipUrl = `https://${record.bucket}.sfo3.digitaloceanspaces.com/${key}`;
          const ovenUrl = process.env.OVEN_URL || "https://oven.aesthetic.computer";
          const callbackUrl = `${baseUrl}/api/oven-complete`;
          const callbackSecret = process.env.OVEN_CALLBACK_SECRET;

          const payload = {
            mongoId: mediaId.toString(),
            slug,
            code,
            zipUrl,
            callbackUrl,
            callbackSecret,
          };

          if (dev && ovenUrl.includes("localhost")) {
            const https = await import("https");
            const url = new URL(`${ovenUrl}/bake`);
            const bodyStr = JSON.stringify(payload);
            await new Promise((resolve, reject) => {
              const req = https.request({
                hostname: url.hostname,
                port: url.port,
                path: url.pathname,
                method: "POST",
                headers: {
                  "Content-Type": "application/json",
                  "Content-Length": Buffer.byteLength(bodyStr),
                },
                rejectUnauthorized: false,
              }, (res) => {
                if (res.statusCode >= 200 && res.statusCode < 300) resolve();
                else reject(new Error(`Oven request failed: ${res.statusCode}`));
              });
              req.on("error", reject);
              req.write(bodyStr);
              req.end();
            });
          } else {
            const ovenResponse = await fetch(`${ovenUrl}/bake`, {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify(payload),
            });
            if (!ovenResponse.ok) {
              throw new Error(`Oven request failed: ${ovenResponse.status}`);
            }
          }
        } catch (ovenError) {
          console.error("track-media-stream oven warning:", ovenError);
        }

        await send("complete", {
          slug,
          code,
          maxDuration: MAX_TAPE_DURATION,
          processing: "oven",
        });
        return;
      }

      await send("progress", { stage: "sync", message: "Syncing ATProto..." });
      try {
        const mediaType = type === "paintings" ? MediaTypes.PAINTING : MediaTypes.PIECE;
        const savedRecord = await collection.findOne({ _id: mediaId });
        if (savedRecord) {
          const atprotoResult = await createMediaRecord(database, mediaType, savedRecord, {
            userSub: user?.sub || null,
          });
          if (atprotoResult?.rkey) {
            await collection.updateOne(
              { _id: mediaId },
              { $set: { "atproto.rkey": atprotoResult.rkey } },
            );
          }
        }
      } catch (atprotoError) {
        console.error("track-media-stream ATProto warning:", atprotoError);
      }

      await send("complete", {
        slug,
        code,
        paintingId: mediaId.toString(),
      });
    } catch (err) {
      console.error("track-media-stream error:", err);
      await send("error", { error: err?.message || String(err) });
    } finally {
      if (database) {
        await database.disconnect().catch(() => {});
      }
      await close();
    }
  })();

  return {
    statusCode: 200,
    headers,
    body: readable,
  };
});
