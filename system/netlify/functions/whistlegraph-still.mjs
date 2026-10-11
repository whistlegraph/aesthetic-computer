// Whistlegraph Still, 2026.10.11
// The picture a turn worker saw when it saved a version: the last frame of
// its paint check, kept so the "is ready" push can show the piece itself.
//
// GET /api/whistlegraph-still?id=<token>   image/png
//     The token is random and only travels inside the owner's push, so the
//     URL is the capability; a still never changes once written.
import { connect } from "../../backend/database.mjs";

export const COLLECTION = "whistlegraph-stills";
const TOKEN = /^[a-f0-9]{32}$/;

const miss = (statusCode, message) => ({
  statusCode,
  headers: { "Content-Type": "application/json", "Cache-Control": "no-store" },
  body: JSON.stringify({ error: message }),
});

export async function handler(event) {
  if (event.httpMethod !== "GET") return miss(405, "Use GET");
  const id = String(event.queryStringParameters?.id || "");
  if (!TOKEN.test(id)) return miss(400, "Specify a still id");
  try {
    const { db } = await connect();
    const still = await db.collection(COLLECTION).findOne({ _id: id }, { projection: { png: 1 } });
    if (!still?.png) return miss(404, "No such still");
    const bytes = Buffer.from(still.png.buffer ?? still.png);
    return {
      statusCode: 200,
      headers: { "Content-Type": "image/png", "Cache-Control": "public, max-age=31536000, immutable" },
      body: bytes.toString("base64"),
      isBase64Encoded: true,
    };
  } catch (error) {
    return miss(503, "Could not read the still: " + error.message);
  }
}
