// Track Media, 23.10.01.15.29
// Formerly `Painting`

// POST: Create a new record in the database for a user uploaded painting or piece.

/* #region 🏁 TODO 
  - [] Eventually add metadata to paintings... like titles.
#endregion */

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  if (!["POST", "PUT"].includes(event.httpMethod))
    return respond(405, { message: "Method Not Allowed" });

  let body;
  try {
    body = JSON.parse(event.body);
    const user = await authorize(event.headers);

    const database = await connect();

    let type;
    if (body.ext === "png") {
      type = "paintings";
    } else if (body.ext === "mjs") {
      type = "pieces";
    } else {
      throw new Error(`Unsupported media type via extension: ${body.ext}`);
    }

    const collection = database.db.collection(type);

    if (event.httpMethod === "POST") {
      // POST logic for creating a new database record
      const slug = body.slug;
      if (user) {
        await collection.createIndex({ user: 1 });
        await collection.createIndex({ when: 1 });
        await collection.createIndex({ slug: 1 });
        await collection.createIndex({ slug: 1, user: 1 }, { unique: true });

        try {
          await collection.insertOne({
            slug,
            user: user.sub,
            when: new Date(),
          });
          return respond(200, { slug });
        } catch (error) {
          return respond(500, { message: error });
        } finally {
          await database.disconnect();
        }
      } else {
        return respond(401, { message: "Unauthorized" });
      }
    } else if (event.httpMethod === "PUT") {
      // PUT logic for updating an existing painting record
      const { slug, nuke } = body;
      if (!slug || nuke === undefined || nuke === null) {
        return respond(400, { message: "Slug & nuke must be set for update." });
      }

      try {
        const result = await collection.updateOne(
          { slug, user: user.sub },
          { $set: { nuked: nuke } },
        );

        if (result.matchedCount === 0) {
          return respond(404, { message: "Media not found." });
        } else if (result.modifiedCount === 1) {
          return respond(200, { message: "Media nuked successfully." });
        }
      } catch (error) {
        return respond(500, { message: error });
      } finally {
        await database.disconnect();
        return respond(200, { message: "No effect." });
      }
    }
  } catch (error) {
    return respond(400, { message: "Cannot parse input body." });
  }
}
