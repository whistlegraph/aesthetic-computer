// 🖋️ Presigned Upload URL Generator
// This function generates a presigned URL for uploading to S3, and checks
// the current bucket to make sure we are not uploading a duplicate file, by
// autogenerating a uuid with nanoid.

// 🔏 Security
// Please ensure the bucket used to upload media has a CORS policy that
// matches the hosting domain.

// It should be... *.aesthetic.computer with
// allowed methods GET, PUT and HEAD.

// TODO

// - [] Show the media file experation date to the user.
// - [] Visiting aesthetic.computer/art~code will show the
//      file in a viewer.
// - [x] Upload the file using this presigned url, via the client.
//  - Read: https://stackoverflow.com/a/28699269/8146077
// - [x] Get presigned S3 URL from S3 bucket

// Next version?
// - [] Use a multi-part uploader.

import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";

const s3 = new S3Client({
  endpoint: "https://" + process.env.ART_ENDPOINT,
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

const s3Wand = new S3Client({
  endpoint: "https://" + process.env.WAND_ENDPOINT,
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

let client; // Will be assigned on each handler runs.

export async function handler(event, context) {
  const { customAlphabet } = await import("nanoid");

  const alphabet =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  const nanoid = customAlphabet(alphabet, 8);

  const extension = event.path.slice(1).split("/")[1];
  const name = event.path.slice(1).split("/")[2];
  const bucket = event.path.slice(1).split("/")[3];

  // TODO: This switch a little janky right now because I need
  //       authentication. 22.11.15.07.16
  if (bucket === "wand")
    client = { s3: s3Wand, bucket: process.env.WAND_SPACE_NAME };
  else client = { s3, bucket: process.env.ART_SPACE_NAME }; // Assume "art".

  let mimeType;

  if (extension === "png") {
    mimeType = "image/png";
  }

  if (extension === "mp4") {
    mimeType = "video/mp4";
  }

  if (extension === "json") {
    mimeType = "application/json";
  }

  if (extension === "gltf") {
    mimeType = "model/gltf+json";
  }

  if (extension === "glb") {
    mimeType = "model/gltf-binary";
  }

  if (extension === "obj") {
    mimeType = "application/object";
  }

  if (!mimeType) {
    return {
      statusCode: 400,
      body: JSON.stringify({
        message: "Invalid file extension.",
      }),
    };
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

  const putObjectParams = {
    Bucket: client.bucket,
    Key: fileName,
    ContentType: mimeType,
    ACL: "public-read",
  };

  const command = new PutObjectCommand(putObjectParams);
  const uploadURL = await getSignedUrl(client.s3, command, { expiresIn: 3600 });

  return {
    statusCode: 200,
    body: JSON.stringify({
      uploadURL: uploadURL,
    }),
  };
}

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
