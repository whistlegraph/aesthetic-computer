// Private, disposable tape pre-uploads.
// POST creates a signed private PUT; PUT finalizes through track-media;
// DELETE removes an abandoned object. Stale drafts are reclaimed after 24
// hours as subsequent draft traffic passes through this endpoint.

import crypto from "node:crypto";
import { customAlphabet } from "nanoid";
import {
  DeleteObjectCommand,
  PutObjectCommand,
  S3Client,
} from "@aws-sdk/client-s3";
import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { handler as trackMedia } from "./track-media.mjs";

const nanoid = customAlphabet(
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
  16,
);
const TTL_SECONDS = 24 * 60 * 60;

function s3() {
  return new S3Client({
    endpoint: `https://${process.env.ART_ENDPOINT || "sfo3.digitaloceanspaces.com"}`,
    region: "us-east-1",
    credentials: {
      accessKeyId: process.env.ART_KEY || process.env.DO_SPACES_KEY,
      secretAccessKey: process.env.ART_SECRET || process.env.DO_SPACES_SECRET,
    },
  });
}

function digest(value) {
  return crypto.createHash("sha256").update(value).digest("hex");
}

async function requestUser(event) {
  try {
    return await authorize(event.headers || {});
  } catch {
    return null;
  }
}

async function parseBody(event) {
  try {
    return JSON.parse(event.body || "{}");
  } catch {
    return {};
  }
}

async function cleanupExpired(drafts) {
  const expired = await drafts
    .find({ createdAt: { $lt: new Date(Date.now() - TTL_SECONDS * 1000) } })
    .limit(100)
    .toArray();
  for (const draft of expired) {
    try {
      await s3().send(new DeleteObjectCommand({ Bucket: draft.bucket, Key: draft.key }));
      await drafts.deleteOne({ _id: draft._id });
    } catch (error) {
      console.warn("📌 Could not expire tape draft", draft.id, error.message);
    }
  }
}

export async function handler(event) {
  const user = await requestUser(event);
  const database = await connect();
  const drafts = database.db.collection("tape-drafts");
  await drafts.createIndex({ createdAt: 1 });

  if (event.httpMethod === "POST") {
    await cleanupExpired(drafts);
    const id = nanoid();
    const token = nanoid() + nanoid();
    const slug = user ? `${user.sub}/${id}` : id;
    const key = `${slug}.zip`;
    const bucket = user
      ? process.env.USER_SPACE_NAME || "user-aesthetic-computer"
      : process.env.ART_SPACE_NAME || "art-aesthetic-computer";
    const command = new PutObjectCommand({
      Bucket: bucket,
      Key: key,
      ContentType: "application/zip",
      ContentDisposition: "inline",
    });
    const uploadURL = await getSignedUrl(s3(), command, { expiresIn: 3600 });
    await drafts.insertOne({
      id,
      tokenHash: digest(token),
      user: user?.sub || null,
      slug,
      key,
      bucket,
      createdAt: new Date(),
    });
    return respond(200, { id, token, slug, uploadURL, expiresIn: TTL_SECONDS });
  }

  const body = await parseBody(event);
  const draft = await drafts.findOne({ id: body.id, tokenHash: digest(body.token || "") });
  if (!draft || draft.user !== (user?.sub || null)) {
    return respond(404, { error: "Draft not found" });
  }

  if (event.httpMethod === "DELETE") {
    await s3().send(new DeleteObjectCommand({ Bucket: draft.bucket, Key: draft.key }));
    await drafts.deleteOne({ _id: draft._id });
    return respond(200, { deleted: true });
  }

  if (event.httpMethod === "PUT") {
    const forwarded = {
      ...event,
      httpMethod: "POST",
      body: JSON.stringify({ slug: draft.slug, ext: "zip", metadata: body.metadata || {} }),
    };
    const result = await trackMedia(forwarded);
    if (result.statusCode >= 200 && result.statusCode < 300) {
      await drafts.deleteOne({ _id: draft._id });
    }
    return result;
  }

  return respond(405, { error: "Method not allowed" });
}
