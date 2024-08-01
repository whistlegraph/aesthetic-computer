// Media Collection, 23.05.02.01.11
// Returns collections of user data from S3 given an open path.
// Example: https://aesthetic.computer/media-collection?for={path}

import { getHandleOrEmail } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

// GET `/media/{@userHandleOrEmail}` will list files.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET")
    return respond(405, { error: "Wrong request type!" });

  const path = decodeURIComponent(event.queryStringParameters.for);
  const splitPath = path.split("/"); // Chop up the path.
  const sub = splitPath[0];
  const mediaType = splitPath[1];
  const userId = await getHandleOrEmail(sub); // Get human readable id.

  console.log("📕 Media collection query:", path, splitPath, userId);

  let files;
  try {
    const { db, disconnect } = await connect();

    // const mediaCollection = db.collection(`${mediaType}s`);
    const mediaCollection = db.collection(
      mediaType.endsWith("s") ? mediaType : `${mediaType}s`,
    );

    // Query the media collection for the specific user.
    // (Ignoring the `nuked` flag.)
    const media = await mediaCollection
      .find({ user: sub, nuked: { $ne: true } })
      .toArray();

    // Only expect `painting` and `piece` for now. 23.10.12.22.32
    const extension = mediaType === "painting" ? "png" : "mjs";

    // Format the response
    files = media.map((file) => {
      return `https://aesthetic.computer/media/${userId}/${mediaType}/${file.slug}.${extension}`;
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
