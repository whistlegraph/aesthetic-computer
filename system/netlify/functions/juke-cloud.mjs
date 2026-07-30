// Authenticated cloud storage for JukeWizard audio.

import { randomUUID } from "node:crypto";
import {
  GetObjectCommand,
  ListObjectsV2Command,
  PutObjectCommand,
  S3Client,
} from "@aws-sdk/client-s3";
import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import { authorize } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";

const AUDIO_TYPES = new Map([
  ["mp3", "audio/mpeg"],
  ["wav", "audio/wav"],
  ["flac", "audio/flac"],
  ["ogg", "audio/ogg"],
  ["m4a", "audio/mp4"],
  ["aac", "audio/aac"],
  ["aif", "audio/aiff"],
  ["aiff", "audio/aiff"],
  ["caf", "audio/x-caf"],
]);
const MAX_TRACK_BYTES = 2 * 1024 * 1024 * 1024;
let s3;

export function safeTrackName(value) {
  const leaf = String(value || "").split(/[\\/]/).pop() || "";
  return leaf.normalize("NFKC")
    .replace(/[\u0000-\u001f\u007f]/g, "")
    .replace(/[^\p{L}\p{N} ._()\[\]-]+/gu, "-")
    .replace(/\s+/g, " ")
    .replace(/^\.+/, "")
    .trim()
    .slice(0, 180);
}

export function audioType(filename) {
  const extension = safeTrackName(filename).split(".").pop()?.toLowerCase();
  return extension ? AUDIO_TYPES.get(extension) || null : null;
}

export function userPrefix(sub) {
  return `${sub}/jukewizard/`;
}

export function ownsKey(sub, key) {
  return typeof key === "string" && key.startsWith(userPrefix(sub)) && !key.includes("../");
}

function storageConfig() {
  const accessKeyId = process.env.ART_KEY || process.env.DO_SPACES_KEY;
  const secretAccessKey = process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
  const endpointName = process.env.USER_ENDPOINT || process.env.ART_ENDPOINT || "sfo3.digitaloceanspaces.com";
  const endpoint = endpointName.startsWith("http") ? endpointName : `https://${endpointName}`;
  const bucket = process.env.USER_SPACE_NAME || "user-aesthetic-computer";
  if (!accessKeyId || !secretAccessKey) throw new Error("Juke cloud storage is unavailable");
  return { accessKeyId, secretAccessKey, endpoint, bucket };
}

function client() {
  if (s3) return s3;
  const config = storageConfig();
  s3 = new S3Client({
    endpoint: config.endpoint,
    region: "us-east-1",
    credentials: {
      accessKeyId: config.accessKeyId,
      secretAccessKey: config.secretAccessKey,
    },
    requestChecksumCalculation: "WHEN_REQUIRED",
    responseChecksumValidation: "WHEN_REQUIRED",
  });
  return s3;
}

function publicURL(key) {
  const { endpoint, bucket } = storageConfig();
  const host = new URL(endpoint).host;
  const encoded = key.split("/").map(encodeURIComponent).join("/");
  return `https://${bucket}.${host}/${encoded}`;
}

function trackFromObject(object) {
  const key = object.Key || "";
  const storedName = key.slice(key.lastIndexOf("/") + 1);
  const name = storedName.replace(/^[0-9a-f-]{36}-/, "");
  const url = publicURL(key);
  return {
    key,
    name,
    bytes: object.Size || 0,
    updatedAt: object.LastModified?.toISOString?.() || null,
    etag: object.ETag?.replaceAll('"', "") || null,
    contentType: audioType(name),
    url,
    command: `play ${url}`,
  };
}

function body(event) {
  if (!event.body) return {};
  try { return JSON.parse(event.body); }
  catch { return null; }
}

async function authenticated(event) {
  const user = await authorize(event.headers || {});
  return user?.sub ? user : null;
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");
  try {
    const user = await authenticated(event);
    if (!user) return respond(401, { error: "Sign in to use Juke cloud." });
    const { bucket } = storageConfig();

    if (event.httpMethod === "GET") {
      const listed = await client().send(new ListObjectsV2Command({
        Bucket: bucket,
        Prefix: userPrefix(user.sub),
        MaxKeys: 1000,
      }));
      const tracks = (listed.Contents || [])
        .filter((object) => object.Key && audioType(object.Key))
        .map(trackFromObject)
        .sort((a, b) => String(b.updatedAt).localeCompare(String(a.updatedAt)));
      return respond(200, { tracks, truncated: !!listed.IsTruncated });
    }

    if (event.httpMethod !== "POST") {
      return respond(405, { error: "Method not allowed." });
    }

    const input = body(event);
    if (!input) return respond(400, { error: "Invalid JSON." });

    if (input.action === "download") {
      if (!ownsKey(user.sub, input.key)) return respond(403, { error: "Track is outside your cloud library." });
      const url = await getSignedUrl(client(), new GetObjectCommand({
        Bucket: bucket,
        Key: input.key,
        ResponseContentDisposition: `attachment; filename="${safeTrackName(input.key)}"`,
      }), { expiresIn: 15 * 60 });
      return respond(200, { url });
    }

    if (input.action !== "upload") return respond(400, { error: "Unknown action." });
    const filename = safeTrackName(input.filename);
    const contentType = audioType(filename);
    const bytes = Number(input.bytes);
    if (!filename || !contentType) return respond(400, { error: "Choose an MP3, WAV, FLAC, OGG, M4A, AAC, AIFF, or CAF file." });
    if (!Number.isSafeInteger(bytes) || bytes <= 0 || bytes > MAX_TRACK_BYTES) {
      return respond(400, { error: "Track size must be between 1 byte and 2 GB." });
    }
    const key = `${userPrefix(user.sub)}${randomUUID()}-${filename}`;
    const uploadURL = await getSignedUrl(client(), new PutObjectCommand({
      Bucket: bucket,
      Key: key,
      ContentType: contentType,
      ContentDisposition: "inline",
      ACL: "public-read",
    }), { expiresIn: 15 * 60 });
    const track = trackFromObject({ Key: key, Size: bytes, LastModified: new Date() });
    return respond(200, {
      uploadURL,
      headers: { "Content-Type": contentType, "Content-Disposition": "inline" },
      track,
    });
  } catch (error) {
    console.error("juke-cloud failed", error?.message || error);
    return respond(503, { error: "Juke cloud is temporarily unavailable." });
  }
}
