// native-piece.mjs — a piece written on an AC OS machine joins its author's account.
//
// POST { handle, token, slug, ext, source }
//   An AC OS machine sends a piece that was pushed to it (see
//   fedac/native/initramfs-scripts/piece-keep.sh). The source lands where any
//   @handle piece lives — <sub>/piece/<slug>.<ext> in the user bucket — and
//   gets the same `pieces` row, marked platform "ac-native": it is written
//   against the OS's sound and input, so the web shows a notice in its place
//   (lith's /media route asks GET here).
//   `token` is the machine's own credential: the device token minted by
//   /api/device-token at flash time ("hmac.timestamp" over "sub:timestamp"),
//   or the sign-in token the `link` piece stores.
//
// GET ?sub=<sub>&slug=<slug>
//   { platform, ext, name } for that piece, or { platform: null }.

import { createHmac, timingSafeEqual } from "node:crypto";
import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { registerPiece, PIECE_BUCKET } from "../../backend/piece-registry.mjs";
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";

export const NATIVE = "ac-native";
const MAX_BYTES = 400000; // as /api/register-piece
const MIME = {
  mjs: "application/javascript; charset=utf-8",
  lisp: "text/x-lisp; charset=utf-8",
};

// The device token check the chat servers run (session-server/chat-manager.mjs).
export function deviceTokenMatches(sub, token, secret) {
  const parts = /^([0-9a-f]{32})\.([0-9a-z]+)$/.exec(token || "");
  if (!secret || !parts || !sub) return false;
  const expected = createHmac("sha256", secret).update(`${sub}:${parts[2]}`).digest("hex").slice(0, 32);
  return timingSafeEqual(Buffer.from(expected), Buffer.from(parts[1]));
}

async function putSource({ key, bytes, ext }) {
  const s3 = new S3Client({
    endpoint: "https://sfo3.digitaloceanspaces.com",
    region: "us-east-1",
    credentials: {
      accessKeyId: process.env.ART_KEY || process.env.DO_SPACES_KEY,
      secretAccessKey: process.env.ART_SECRET || process.env.DO_SPACES_SECRET,
    },
  });
  await s3.send(new PutObjectCommand({
    Bucket: PIECE_BUCKET, Key: key, Body: bytes, ACL: "public-read",
    ContentType: MIME[ext], ContentDisposition: "inline",
  }));
}

export function createHandler({
  database = connect,
  authenticate = authorize,
  store = putSource,
  makeCode = generateUniqueCode,
  secret = () => process.env.JWT_SECRET || process.env.AC_DEVICE_SECRET,
} = {}) {
  return async (event) => {
    if (event.httpMethod === "OPTIONS") return respond(204, "");
    let db;
    try {
      if (event.httpMethod === "GET") {
        const { sub, slug } = event.queryStringParameters || {};
        if (!sub || !/^[a-zA-Z0-9_-]{1,100}$/.test(slug || "")) return respond(400, { error: "sub and slug required" });
        db = await database();
        const row = await db.db.collection("pieces").findOne({ user: sub, slug: { $in: [`piece/${slug}`, `${sub}/piece/${slug}`] } });
        return respond(200, { platform: row?.platform || null, ext: row?.ext || null, name: row?.name || null });
      }
      if (event.httpMethod !== "POST") return respond(405, { error: "GET or POST" });

      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return respond(400, { error: "Invalid JSON" }); }
      const { handle, token, slug, ext, source } = body;
      if (typeof slug !== "string" || !/^[a-zA-Z0-9_-]{1,100}$/.test(slug) || !MIME[ext]) return respond(400, { error: "Invalid piece identity" });
      if (typeof source !== "string" || !source.trim()) return respond(400, { error: "Missing source" });
      const bytes = Buffer.from(source, "utf8");
      if (bytes.length > MAX_BYTES) return respond(413, { error: "Piece is too large" });
      if (typeof handle !== "string" || typeof token !== "string") return respond(401, { error: "handle and token required" });

      db = await database();
      const clean = handle.replace(/^@/, "");
      const owner = await db.db.collection("@handles").findOne({ handle: clean }, { projection: { _id: 1 } });
      if (!owner?._id) return respond(401, { error: "Unknown handle" });
      const sub = owner._id;
      let allowed = deviceTokenMatches(sub, token, secret());
      if (!allowed) {
        // A linked machine holds the owner's sign-in token instead.
        const user = await authenticate({ authorization: `Bearer ${token}` }).catch(() => null);
        allowed = !!user?.sub && user.sub === sub;
      }
      if (!allowed) return respond(401, { error: "This machine is not signed in as that handle" });

      await store({ key: `${sub}/piece/${slug}.${ext}`, bytes, ext });
      const saved = await registerPiece(db.db.collection("pieces"), {
        user: sub, slug, ext, bytes, origin: NATIVE, platform: NATIVE, makeCode,
      });
      return respond(200, { ...saved, platform: NATIVE, route: `https://aesthetic.computer/@${clean}/${slug}` });
    } catch (error) {
      console.error("native-piece:", error?.message || error);
      return respond(500, { error: "Native piece publishing unavailable" });
    } finally {
      await db?.disconnect?.();
    }
  };
}

export const handler = createHandler();
