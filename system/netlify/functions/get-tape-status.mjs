// Get Tape Status, 25.10.17
// Query MongoDB for tape conversion status

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  const slug = event.queryStringParameters?.slug;

  if (!slug) {
    return respond(400, { message: "Missing required parameter: slug" });
  }

  try {
    const database = await connect();
    const tapes = database.db.collection("tapes");

    const tape = await tapes.findOne({ slug });

    await database.disconnect();

    if (!tape) {
      return respond(404, { message: "Tape not found" });
    }

    // Return minimal tape info with MP4 status
    return respond(200, {
      slug: tape.slug,
      code: tape.code,
      when: tape.when,
      mp4Status: tape.mp4Status || "pending",
      mp4: tape.mp4 || null,
    });

  } catch (error) {
    console.error(`❌ Get tape status error:`, error);
    return respond(500, { message: error.message || String(error) });
  }
}
