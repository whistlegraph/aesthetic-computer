// Get Painting by Code, 25.10.17
// Query MongoDB for painting by code

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { paintingWipExpired } from "../../backend/painting-wips.mjs";

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
    const paintings = database.db.collection("paintings");

    // Query by code or slug
    const escapedSlug = slug?.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    const query = code
      ? { code }
      : {
          $or: [
            { slug },
            // Anonymous paintings with recordings store IMAGE:RECORDING.
            // The image resolver addresses the IMAGE half independently.
            { slug: new RegExp(`^${escapedSlug}:`) },
          ],
        };
    const painting = await paintings.findOne(query);

    await database.disconnect();

    if (!painting || paintingWipExpired(painting)) {
      return respond(404, { message: "Painting not found" }, { "Cache-Control": "no-store" });
    }

    // Return painting info
    return respond(200, {
      slug: painting.slug,
      code: painting.code,
      discussion: `/mime/#/media/painting/${encodeURIComponent(painting.code || painting._id.toString())}`,
      when: painting.when,
      bucket: painting.bucket,
      user: painting.user,
      nuked: painting.nuked || false,
      ...(painting.status === "wip" ? {
        status: "wip", width: painting.wip.width, height: painting.wip.height,
      } : {}),
    }, painting.status === "wip" ? { "Cache-Control": "no-store" } : {});

  } catch (error) {
    console.error(`❌ Get painting error:`, error);
    return respond(500, { message: error.message || String(error) });
  }
}
