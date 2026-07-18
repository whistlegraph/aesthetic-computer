// Media Collection, 23.05.02.01.11
// Returns collections of user data from S3 given an open path.
// Example: https://aesthetic.computer/media-collection?for={path}

import { getHandleOrEmail, userIDFromHandleOrEmail } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

export function mediaCollectionPath({ userId, mediaType, slug, extension }) {
  const normalized = `${slug}`.replace(/^\/+/, "");
  const storagePath = normalized.startsWith(`${userId}/`)
    ? normalized
    : `${mediaType}/${normalized}`;
  return `${storagePath}.${extension}`;
}

// GET `/media/{@userHandleOrEmail}` will list files.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET")
    return respond(405, { error: "Wrong request type!" });

  const path = decodeURIComponent(event.queryStringParameters.for);
  const splitPath = path.split("/"); // Chop up the path.
  const sub = splitPath[0];
  const mediaType = splitPath[1];

  let files;
  try {
    const { db, disconnect } = await connect();

    // Convert handle/email to actual Auth0 sub for database query
    const userSub = await userIDFromHandleOrEmail(sub, { db });
    
    // Get human readable id (handle or email) for the response URLs
    const userId = await getHandleOrEmail(userSub);

    console.log("📕 Media collection query:", path, splitPath, sub, "->", userSub, "->", userId);

    // const mediaCollection = db.collection(`${mediaType}s`);
    const mediaCollection = db.collection(
      mediaType.endsWith("s") ? mediaType : `${mediaType}s`,
    );

    // Query the media collection for the specific user.
    // (Ignoring the `nuked` flag.)
    const media = await mediaCollection
      .find({ user: userSub, nuked: { $ne: true } })
      .toArray();

    // Only expect `painting` and `piece` for now. 23.10.12.22.32
    const extension = mediaType === "painting" ? "png" : "mjs";

    // Determine the base URL from the request headers
    const protocol = event.headers["x-forwarded-proto"] || "https";
    const host = event.headers.host || "aesthetic.computer";
    const baseUrl = `${protocol}://${host}`;

    // Format the response
    files = media.map((file) => {
      const path = mediaCollectionPath({
        userId: userSub,
        mediaType,
        slug: file.slug,
        extension,
      });
      return `${baseUrl}/media/${userId}/${path}`;
    });

    disconnect();
  } catch (err) {
    console.log("Error", err);
    return respond(500, {
      error: "Failed to fetch media from the database 😩",
    });
  }

  return respond(200, { files }); // Return a list of all the files.
}
