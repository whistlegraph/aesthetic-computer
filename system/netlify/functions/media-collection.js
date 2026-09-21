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

  // This endpoint is also linked without parameters by the public sitemap.
  // Validate before opening Mongo/Redis: decodeURIComponent(undefined) becomes
  // the string "undefined", rather than rejecting a missing query parameter.
  const requestedPath = event.queryStringParameters?.for;
  if (typeof requestedPath !== "string" || !requestedPath.trim()) {
    return respond(400, { error: "Expected for=<handle>/<media-type>." });
  }
  let path;
  try {
    path = decodeURIComponent(requestedPath);
  } catch {
    return respond(400, { error: "Invalid media collection path encoding." });
  }
  const splitPath = path.split("/");
  const [sub, mediaType] = splitPath;
  if (splitPath.length !== 2 || !sub.trim() || !/^[a-z]+$/.test(mediaType || "")) {
    return respond(400, { error: "Expected for=<handle>/<media-type>." });
  }

  let files;
  let disconnect;
  try {
    const connection = await connect();
    const { db } = connection;
    disconnect = connection.disconnect;

    // Convert handle/email to actual Auth0 sub for database query
    const userSub = await userIDFromHandleOrEmail(sub, { db });
    if (!userSub) return respond(404, { error: "User not found.", files: [] });

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
      .find({ user: userSub, nuked: { $ne: true }, status: { $ne: "wip" } })
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

  } catch (err) {
    console.log("Error", err);
    return respond(500, {
      error: "Failed to fetch media from the database 😩",
    });
  } finally {
    if (disconnect) await disconnect();
  }

  return respond(200, { files }); // Return a list of all the files.
}
