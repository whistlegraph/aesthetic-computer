// 🖋️ Presigned Upload URL Generator
// This function generates a presigned URL for uploading to S3, and checks
// the current bucket to make sure we are not uploading a duplicate file, by
// autogenerating a uuid with nanoid.

// 🔏 Security
// Please ensure the bucket used to upload media has a CORS policy that
// matches the hosting domain.

// It should be... *.aesthetic.computer with
// allowed methods GET, PUT and HEAD.

// - [] Show the media file expiration date to the user.
// - [] Visiting aesthetic.computer/art~code will show the
//      file in a viewer.
// - [x] Upload the file using this presigned url, via the client.
//  - Read: https://stackoverflow.com/a/28699269/8146077
// - [x] Get presigned S3 URL from S3 bucket

// Next version?
// - [] Use a multi-part uploader.

import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import {
  S3Client,
  PutObjectCommand,
  GetObjectCommand,
} from "@aws-sdk/client-s3";
import {
  authorize,
  userIDFromHandleOrEmail,
} from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";

// Lazy-initialized S3 clients (created on first use to allow env vars to load)
let s3Guest, s3Wand, s3User;

function getS3Clients() {
  if (!s3Guest) {
    // Credentials must be provided via environment variables
    // Support both ART_KEY and DO_SPACES_KEY naming conventions
    const accessKeyId = process.env.ART_KEY || process.env.DO_SPACES_KEY;
    const secretAccessKey = process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
    const endpoint = "https://" + (process.env.ART_ENDPOINT || "sfo3.digitaloceanspaces.com");
    
    // Validate credentials are available
    if (!accessKeyId || !secretAccessKey) {
      console.error("❌ S3 Credentials missing:", {
        hasArtKey: !!process.env.ART_KEY,
        hasDoSpacesKey: !!process.env.DO_SPACES_KEY,
        hasArtSecret: !!process.env.ART_SECRET,
        hasDoSpacesSecret: !!process.env.DO_SPACES_SECRET,
        endpoint,
      });
      throw new Error(`Missing S3 credentials: ART_KEY=${!!process.env.ART_KEY}, DO_SPACES_KEY=${!!process.env.DO_SPACES_KEY}, ART_SECRET=${!!process.env.ART_SECRET}, DO_SPACES_SECRET=${!!process.env.DO_SPACES_SECRET}`);
    }
    
    console.log("🔑 Using credentials:", {
      keySource: process.env.ART_KEY ? 'ART_KEY' : 'DO_SPACES_KEY',
      secretSource: process.env.ART_SECRET ? 'ART_SECRET' : 'DO_SPACES_SECRET',
      keyLength: accessKeyId?.length,
      secretLength: secretAccessKey?.length,
      endpoint,
    });
    
    s3Guest = new S3Client({
      endpoint,
      credentials: { accessKeyId, secretAccessKey },
    });

    s3Wand = new S3Client({
      endpoint: "https://" + (process.env.WAND_ENDPOINT || "sfo3.digitaloceanspaces.com"),
      credentials: { accessKeyId, secretAccessKey },
    });

    s3User = new S3Client({
      endpoint: "https://" + (process.env.USER_ENDPOINT || "sfo3.digitaloceanspaces.com"),
      credentials: { accessKeyId, secretAccessKey },
    });
    
    console.log("✅ S3 Clients initialized successfully");
  }
  return { s3Guest, s3Wand, s3User };
}

export async function handler(event, context) {
  try {
    // Log environment variable availability for debugging
    console.log("🔍 Environment check:", {
      hasArtKey: !!process.env.ART_KEY,
      hasArtSecret: !!process.env.ART_SECRET,
      hasArtEndpoint: !!process.env.ART_ENDPOINT,
      hasUserEndpoint: !!process.env.USER_ENDPOINT,
      hasWandEndpoint: !!process.env.WAND_ENDPOINT,
      artKeyLength: process.env.ART_KEY?.length,
      artSecretLength: process.env.ART_SECRET?.length,
    });
    
    // Initialize S3 clients (lazy load to allow env vars to be set)
    const { s3Guest, s3Wand, s3User } = getS3Clients();
    
    // Only allow GET requests.
    if (event.httpMethod !== "GET")
      return respond(405, { error: "Wrong request type!" });

  if (event.path === "/presigned-download-url") {
    const queryParams = new URLSearchParams(event.queryStringParameters);
    const slug = queryParams.get("for");

    if (!slug) {
      return respond(400, { error: "Missing filename query parameter." });
    }

    //❤️‍🔥
    // TODO
    // Either choose s3 or s3User depending on the filename attribute...
    // https://aesthetic.computer/presigned-download-url

    // TODO: Need to parse out the path.

    let s3,
      bucket,
      filename = decodeURIComponent(slug);
    // Assume a guest / code lookup because emails or handles have an "@".
    if (slug.indexOf("@") === -1) {
      // Guest
      s3 = s3Guest;
      bucket = process.env.ART_SPACE_NAME;
    } else {
      // User
      s3 = s3User;
      bucket = process.env.USER_SPACE_NAME;
      // Replace `user` email or handle with their auth0 id.
      const path = filename.split("/");
      path[0] = await userIDFromHandleOrEmail(path[0]);
      filename = path.join("/");
    }

    const getObjectParams = {
      Bucket: bucket,
      Key: filename,
      ResponseContentDisposition: `attachment; filename="${filename}"`,
    };

    // Get download link that expires in an hour. (3600 seconds)
    const command = new GetObjectCommand(getObjectParams);
    const url = await getSignedUrl(s3, command, { expiresIn: 3600 });
    return respond(200, { url });
  }

  // ➡️ Assume the path === "presigned-upload-url"
  let client;
  const { customAlphabet } = await import("nanoid");
  const alphabet =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  const nanoid = customAlphabet(alphabet, 8);

  const extension = event.path.slice(1).split("/")[1];
  let name = event.path.slice(1).split("/")[2];
  const bucket = event.path.slice(1).split("/")[3];

  if (bucket !== "wand") {
    //           ^ Currently using a legacy name schema for wand. 23.05.01.21.59

    // 🧠 Replace first prefix- with a prefix/ for folder sorting by media.
    //    ⚠️ Only if name exists! Which it doesn't for the guest bucket.
    if (name?.indexOf("-") > -1) name = name.replace("-", "/");
  }

  let expiring = true;
  let subdirectory; // Sorted by user if necessary.

  // TODO: This switch a little janky right now because I need
  //       authentication. 22.11.15.07.16
  if (bucket === "wand") {
    const wandBucket = process.env.WAND_SPACE_NAME || "wand-aesthetic-computer";
    client = { s3: s3Wand, bucket: wandBucket };
  } else if (bucket === "user") {
    const userBucket = process.env.USER_SPACE_NAME || "user-aesthetic-computer";
    client = { s3: s3User, bucket: userBucket };

    const user = await authorize(event.headers);
    if (user) {
      expiring = false; // Set the file not to expire.
      subdirectory = user.sub; // Sort this object into a user directory.
    } else {
      // Fail if the user is not logged in but an upload is attempted from
      // the client as if they are.
      // TODO: 🔴 Should I just use the guest bucket here? 23.04.30.17.59
      return respond(401, { error: "Authorization failure..." });
    }
  } else {
    // Assume the unauthorized, temporary "art" bucket.
    const guestBucket = process.env.ART_SPACE_NAME || "art-aesthetic-computer";
    client = { s3: s3Guest, bucket: guestBucket };
    // tagging = `activity=${activity}`; // Add activity tag. (Assume anonymous.)
  }

  let mimeType;
  if (extension === "png") mimeType = "image/png";
  if (extension === "zip") mimeType = "application/zip";
  if (extension === "mjs") mimeType = "application/javascript; charset=utf-8";
  if (extension === "lisp") mimeType = "text/x-lisp; charset=utf-8";
  if (extension === "lua") mimeType = "text/x-lua; charset=utf-8";
  if (extension === "mp4") mimeType = "video/mp4";
  if (extension === "json") mimeType = "application/json";
  if (extension === "gltf") mimeType = "model/gltf+json";
  if (extension === "glb") mimeType = "model/gltf-binary";
  if (extension === "obj") mimeType = "application/object";

  if (!mimeType) {
    return respond(400, { message: "Invalid file extension." });
  }

  let loadCode = nanoid();
  let fileName = name || loadCode + "." + extension;

  // Check to see if this code has already been uploaded to blockStorage and if it has,
  // generate a new code. (This should almost never repeat.) See also: https://zelark.github.io/nano-id-cc
  while ((await fileExists(fileName, bucket)) === true) {
    if (name) {
      fileName = `${nanoid()}-${name}`;
    } else {
      loadCode = nanoid();
      fileName = loadCode + "." + extension;
    }
  }

  if (subdirectory) fileName = subdirectory + "/" + fileName;

  const ContentDisposition = "inline";

  const putObjectParams = {
    Bucket: client.bucket,
    Key: fileName,
    ContentType: mimeType,
    ContentDisposition,
    ACL: "public-read", // All media should be publicly readable
  };
  
  // if (tagging) putObjectParams.Tagging = tagging; // Add object tags if they exist.

  const command = new PutObjectCommand(putObjectParams);
  const uploadOptions = { ContentDisposition };

  if (expiring) uploadOptions.expiresIn = 3600; // Set to expiring if we are in an anonymous bucket / not a logged in user.
  const uploadURL = await getSignedUrl(client.s3, command, uploadOptions);

  return respond(200, {
    uploadURL: uploadURL,
    slug: fileName,
    code: loadCode,
  });
  } catch (error) {
    console.error("❌ Presigned URL error:", error);
    console.error("❌ Error details:", {
      message: error.message,
      name: error.name,
      code: error.code,
      statusCode: error.statusCode,
      hasArtKey: !!process.env.ART_KEY,
      hasArtSecret: !!process.env.ART_SECRET,
    });
    return respond(500, { 
      error: error.message || "Internal server error",
      details: error.stack 
    });
  }
}

// 📚 Library (Useful functions used throughout the file)

async function fileExists(filename) {
  try {
    const params = {
      Bucket: client.bucket,
      Key: filename,
    };

    const headCode = await client.s3.headObject(params).promise();
    console.error("File already exists:", filename);
    return true;
  } catch (headErr) {
    if (headErr.code === "NotFound") {
      // console.log("File doesn't exist yet:", filename);
      return false;
    }
  }
}
