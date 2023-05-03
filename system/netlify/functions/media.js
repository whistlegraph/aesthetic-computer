// Media, 23.05.02.01.11
// Parse clean urls and return uploaded file urls (for downloading) or lists
// of files from users.

/*
import {
  userIDFromHandle,
  userIDFromEmail,
} from "../../backend/authorization.mjs";

import { respond } from "../../backend/http.mjs";
import { S3Client, GetObjectCommand } from "@aws-sdk/client-s3";

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
  if (event.httpMethod !== "GET") {
    return {
      statusCode: 405,
      body: JSON.stringify({ error: "Wrong request type 😩" }),
    };
  }

  // Get a user handle or email and file key from the slug.
  const parsedPath = event.path.slice(1).split("/").slice(1);
  const handleOrEmail = parsedPath[0];
  const key = parsedPath.slice(1).join("/");

  if (!handleOrEmail) {
    return {
      statusCode: 400,
      body: JSON.stringify({ error: "Malformed request." }),
    };
  }

  //const client = { s3: s3User, bucket: process.env.USER_SPACE_NAME };

  // 1. Download / go to individual files.
  if (key) {
    let sub;
    if (handleOrEmail.startsWith("@")) {
      // Try and look up `sub` from `handle` in MongoDB.
      sub = await userIDFromHandle(handleOrEmail.slice(1));
    } else {
      // Assume email and try to look up sub from email via auth0.
      sub = await userIDFromEmail(handleOrEmail);
    }

    //const params = { Bucket: process.env.USER_SPACE_NAME, Key: `${sub}/${key}` };
    // Generate a pre-signed URL
    // const command = new GetObjectCommand(params);
    // const url = await getSignedUrl(client.s3, command, {
    //   expiresIn: 3600,
    // }); // expires in 1 hour

    // Construct a CDN url.
    const url = `https://${process.env.USER_SPACE_NAME}/${sub}/${key}`;

    // ❤️‍🔥
    // TODO: I don't really need a presigned URL here. I can just
    //       attempt to redirect to the CDN version of the file, based on
    //       the key?

    // Redirect to the signed download URL.
    return respond(302, null, { Location: url });
  } else {
    // 2. Listing.
    // List all objects prefixed with "${user_id}/painting"
  }
}
*/