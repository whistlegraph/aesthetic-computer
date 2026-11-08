import { createHash } from "node:crypto";
import { S3Client, PutObjectCommand, GetObjectCommand, DeleteObjectCommand } from "@aws-sdk/client-s3";
import { validateGame, MAX_BYTES } from "../public/oskie/format.mjs";

export const OSKIE_TYPE = "application/vnd.aesthetic.oskie+json";
const digest = body => createHash("sha256").update(body).digest("hex");
// JSON may encode a source character as six bytes (for example \u0000).
const maxObjectBytes = MAX_BYTES * 6 + 128 * 1024;

// The account prefix lets the existing account-erasure job remove these too.
// Object keys and credentials never appear in the public catalog or player API.
export function oskieObjectKey(revision) {
  if (!/^[a-f0-9]{32}$/.test(revision.gameId) || !/^[a-f0-9]{32}$/.test(revision._id) ||
      typeof revision.owner !== "string" || !revision.owner || /[\/\\\x00-\x1f]/.test(revision.owner))
    throw Error("Invalid game object identity.");
  return `${revision.owner}/oskie/${revision.gameId}/${revision._id}.oskie`;
}

export function createGameObjects({ client, bucket }) {
  if (!client || !bucket) throw Error("Game object storage is not configured.");
  const keyFor = revision => {
    const key = oskieObjectKey(revision);
    if (revision.object && revision.object.key !== key) throw Error("Game object identity mismatch.");
    return key;
  };
  return {
    async put(revision, bundle) {
      const body = Buffer.from(JSON.stringify(validateGame(bundle)));
      if (body.length > maxObjectBytes) throw Error("Game object is too large.");
      const key = keyFor(revision), sha256 = digest(body);
      await client.send(new PutObjectCommand({
        Bucket: bucket, Key: key, Body: body, ACL: "private",
        ContentType: OSKIE_TYPE, CacheControl: "private, no-store",
        ContentDisposition: `attachment; filename="${bundle.manifest.slug}.oskie"`,
        Metadata: { sha256 },
      }));
      return { key, sha256, bytes: body.length, type: OSKIE_TYPE };
    },
    async get(revision) {
      const key = keyFor(revision);
      if (!revision.object || !/^[a-f0-9]{64}$/.test(revision.object.sha256))
        throw Error("Missing game object checksum.");
      const result = await client.send(new GetObjectCommand({ Bucket: bucket, Key: key }));
      if (result.ContentLength > maxObjectBytes) {
        result.Body?.destroy?.();
        throw Error("Game object is too large.");
      }
      const chunks = []; let size = 0;
      for await (const chunk of result.Body) {
        size += chunk.length;
        if (size > maxObjectBytes) { result.Body?.destroy?.(); throw Error("Game object is too large."); }
        chunks.push(Buffer.from(chunk));
      }
      const body = Buffer.concat(chunks);
      if (body.length !== revision.object.bytes || digest(body) !== revision.object.sha256)
        throw Error("Game object checksum mismatch.");
      return validateGame(JSON.parse(body.toString("utf8")));
    },
    async remove(revision) {
      await client.send(new DeleteObjectCommand({ Bucket: bucket, Key: keyFor(revision) }));
    },
  };
}

let shared;
export function gameObjects() {
  if (shared) return shared;
  const key = process.env.ART_KEY || process.env.DO_SPACES_KEY;
  const secret = process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
  const bucket = process.env.USER_SPACE_NAME;
  if (!key || !secret || !bucket) throw Error("Game object storage is not configured.");
  const host = (process.env.USER_ENDPOINT || "sfo3.digitaloceanspaces.com").replace(/^https?:\/\//, "").replace(/\/$/, "");
  return shared = createGameObjects({ bucket, client: new S3Client({
    endpoint: `https://${host}`, region: "us-east-1",
    credentials: { accessKeyId: key, secretAccessKey: secret },
  }) });
}

// Old Mongo-only revisions remain readable until explicitly migrated.
export async function revisionBundle(revision, objects) {
  if (revision.object) return (objects || gameObjects()).get(revision);
  return validateGame(revision.bundle);
}
