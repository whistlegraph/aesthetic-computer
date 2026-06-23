// Get Tape by Code, 25.10.17
// Query MongoDB for tape by code

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  const code = event.queryStringParameters?.code;
  const slug = event.queryStringParameters?.slug;

  if (!code && !slug) {
    return respond(400, { message: "Missing required parameter: code or slug" });
  }

  try {
    const database = await connect();
    const tapes = database.db.collection("tapes");

    // Query by code or slug
    const query = code ? { code } : { slug };
    const tape = await tapes.findOne(query);

    // Don't disconnect - let the connection be reused across serverless invocations
    // await database.disconnect();

    if (!tape) {
      return respond(404, { message: "Tape not found" });
    }

    // Return tape info. `kind` distinguishes the backing store:
    // "zip" = frame-based recording (default for legacy tapes),
    // "mp4" = video-based tape (e.g. a posted camera-roll clip).
    return respond(200, {
      slug: tape.slug,
      code: tape.code,
      when: tape.when,
      bucket: tape.bucket,
      user: tape.user,
      nuked: tape.nuked || false,
      kind: tape.kind || "zip",
      mp4Status: tape.mp4Status,
    });

  } catch (error) {
    console.error(`❌ Get tape error:`, error);
    return respond(500, { message: error.message || String(error) });
  }
}
