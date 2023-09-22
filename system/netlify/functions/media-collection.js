// Media Collection, 23.05.02.01.11
// Returns collections of user data from S3 given an open path.
// Example: https://aesthetic.computer/media-collection?for={path}

import { getHandleOrEmail } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";
import { S3Client, ListObjectsV2Command } from "@aws-sdk/client-s3";

const s3User = new S3Client({
  endpoint: "https://" + process.env.USER_ENDPOINT,
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

// GET `/media/{@userHandleOrEmail}` will list files.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET")
    return respond(405, { error: "Wrong request type!" });

  const client = { s3: s3User, bucket: process.env.USER_SPACE_NAME };
  const path = decodeURIComponent(event.queryStringParameters.for);

  // List all objects prefixed with `path` in the bucket.
  let params = { Bucket: client.bucket, Prefix: path };

  let files;
  try {
    // TODO: Resort zip/png files / sort files by extension by making new
    //       directories in the buckets and migrating everything.
    //       https://chat.openai.com/c/c77b1bca-223b-49ec-bf93-f64bf7cccf05
    const data = await client.s3.send(new ListObjectsV2Command(params));
    const splitPath = path.split("/"); // Chop up the path.
    const id = await getHandleOrEmail(splitPath[0]); // Get human readable id.
    const route = splitPath.slice(1).join("/"); // Get collection route.
    files = data.Contents.map((file) => {
      // return `https://${process.env.USER_SPACE_NAME}/${file.Key}`; // Original bucket path.
      // Canonical url for this resource, routing through Cloudflare worker.
      const filename = file.Key.split("/").pop();
      return `https://aesthetic.computer/media/${id}/${route}/${filename}`;
    });
  } catch (err) {
    console.log("Error", err);
    return respond(500, { error: "Failed to list files 😩" });
  }

  return respond(200, { files }); // Return a list of all the files.
}
