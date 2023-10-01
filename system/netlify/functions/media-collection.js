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
  const userId = await getHandleOrEmail(sub); // Get human readable id.

  // TODO: Eventually adapt to colletions other than `paintings`. 23.10.01.17.26
  console.log(path, splitPath, userId);

  let files;
  try {
    const { db, disconnect } = await connect();
    const paintingsCollection = db.collection("paintings");

    // Query the paintings collection for the specific user
    const paintings = await paintingsCollection.find({ user: sub }).toArray();

    // Format the response
    files = paintings.map((painting) => {
      return `https://aesthetic.computer/media/${userId}/painting/${painting.slug}.png`;
    });

    disconnect();
  } catch (err) {
    console.log("Error", err);
    return respond(500, {
      error: "Failed to fetch paintings from the database 😩",
    });
  }

  return respond(200, { files }); // Return a list of all the files.
}
