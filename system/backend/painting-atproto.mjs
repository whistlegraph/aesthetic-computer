// painting-atproto.mjs
// Helper functions for syncing paintings with ATProto

import { AtpAgent } from "@atproto/api";
import { shell } from "./shell.mjs";
import { getThumbnailFromSlug } from "./thumbnail.mjs";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

/**
 * Sync a MongoDB painting to ATProto after it's been created
 * @param {Object} database - MongoDB connection
 * @param {string} sub - User sub (auth0|...) - undefined for guest paintings
 * @param {string} slug - Painting slug (timestamp)
 * @param {string} code - Short code (e.g., 'a3b')
 * @param {string} imageUrl - Full URL to painting PNG (/media or /api/pixel)
 * @param {string} recordingUrl - Optional URL to recording ZIP
 * @param {Date} paintingDate - Painting creation timestamp
 * @param {string} refId - Database record _id as string
 * @param {string} bucket - Bucket name where painting is stored
 * @returns {Promise<Object>} { rkey, uri } or { error }
 */
export async function createPaintingOnAtproto(
  database,
  sub,
  slug,
  code,
  imageUrl,
  recordingUrl,
  paintingDate,
  refId,
  bucket
) {
  const users = database.db.collection("users");
  const handles = database.db.collection("@handles");

  let atprotoDid, atprotoPassword, handle;

  // Guest paintings use the system art.at.aesthetic.computer account
  if (!sub) {
    atprotoDid = "did:plc:tliuubv7lyv2uiknsjbf4ppw"; // art.at.aesthetic.computer
    
    // Look up the art user account in MongoDB by DID
    const artUser = await users.findOne({ "atproto.did": atprotoDid });
    
    if (!artUser?.atproto?.password) {
      shell.log(`⚠️  Art user account not found in MongoDB: ${atprotoDid}`);
      return { error: "Art user account not configured" };
    }
    
    atprotoPassword = artUser.atproto.password;
    
    // Get handle from the art user account or from @handles collection
    const handleDoc = await handles.findOne({ _id: artUser._id });
    handle = handleDoc?.handle || "art";
    
    shell.log(`🎨 Syncing guest painting to art.at.aesthetic.computer (${handle}): ${slug}`);
  } else {
    // User paintings use their own ATProto account
    // 1. Check if user has ATProto account
    const user = await users.findOne({ _id: sub });

    if (!user?.atproto?.did || !user?.atproto?.password) {
      shell.log(`ℹ️  User ${sub} has no ATProto account, skipping painting sync`);
      return { error: "No ATProto account" };
    }
    
    atprotoDid = user.atproto.did;
    atprotoPassword = user.atproto.password;
    
    // 2. Determine handle (for thumbnail generation)
    const handleDoc = await handles.findOne({ _id: sub });
    handle = handleDoc?.handle;
  }

  try {

    let thumbnailBuffer = null;

    // Extract just the image slug for thumbnail generation
    // For combined slugs (imageSlug:recordingSlug), we only want the image part
    const imageSlug = slug.includes(':') ? slug.split(':')[0] : slug;

    if (handle && handle !== "art") {
      // User paintings: use /api/pixel with handle
      try {
        thumbnailBuffer = await getThumbnailFromSlug(imageSlug, handle, {
          bucket,
          userId: sub,
        });
      } catch (thumbError) {
        shell.warn(
          `⚠️  Failed to generate thumbnail via getThumbnailFromSlug: ${thumbError.message}`
        );
      }
    } else if (!sub) {
      // Guest paintings: use direct DigitalOcean URL (no handle)
      try {
        thumbnailBuffer = await getThumbnailFromSlug(imageSlug, null, {
          bucket,
        });
      } catch (thumbError) {
        shell.warn(
          `⚠️  Failed to generate thumbnail for guest painting: ${thumbError.message}`
        );
      }
    }

    // Fallback: fetch the original painting image if thumbnail generation failed
    if (!thumbnailBuffer) {
      try {
        const imageResponse = await fetch(imageUrl);
        if (!imageResponse.ok) {
          throw new Error(`Failed to fetch image: ${imageResponse.status}`);
        }

        const imageBuffer = await imageResponse.arrayBuffer();
        thumbnailBuffer = Buffer.from(imageBuffer);
      } catch (fallbackError) {
        shell.error(
          `⚠️  Failed to fetch original image for fallback thumbnail: ${fallbackError.message}`
        );
      }
    }

    // 3. Login to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: atprotoDid,
      password: atprotoPassword,
    });

    // 4. Upload thumbnail as blob (when available)
    let thumbnailBlob = null;

    if (thumbnailBuffer) {
      try {
        const imageUint8 = thumbnailBuffer instanceof Uint8Array
          ? thumbnailBuffer
          : new Uint8Array(thumbnailBuffer);

        const blobResponse = await agent.uploadBlob(imageUint8, {
          encoding: "image/png",
        });

        thumbnailBlob = blobResponse?.data?.blob || blobResponse?.blob || null;

        if (thumbnailBlob) {
          const blobLink = thumbnailBlob?.ref?.$link;
          const blobSize = typeof thumbnailBlob?.size === "number" ? `${thumbnailBlob.size} bytes` : "unknown size";
          const linkPart = blobLink ? `: ${blobLink}` : "";
          shell.log(`📸 Uploaded painting thumbnail blob${linkPart} (size: ${blobSize})`);
        } else {
          shell.warn("⚠️  uploadBlob returned no blob payload");
        }
      } catch (blobError) {
        shell.error(`⚠️  Failed to upload thumbnail blob: ${blobError.message}`);
        // Continue without thumbnail - record will still be created
      }
    } else {
      shell.warn("⚠️  No thumbnail buffer available; creating record without thumbnail");
    }

    // 5. Create ATProto record
    const record = {
      $type: "computer.aesthetic.painting",
      slug,
      code,
      imageUrl,
      when: paintingDate.toISOString(),
      ref: refId,
    };

    if (thumbnailBlob) {
      record.thumbnail = thumbnailBlob;
    }
    
    if (recordingUrl) {
      record.recordingUrl = recordingUrl;
    }

    const atprotoRecord = await agent.com.atproto.repo.createRecord({
      repo: atprotoDid,
      collection: "computer.aesthetic.painting",
      record,
    });

    const uri = atprotoRecord.uri || atprotoRecord.data?.uri;

    if (!uri) {
      const errMsg = `ATProto response missing URI: ${JSON.stringify(atprotoRecord)}`;
      shell.error(`⚠️  ${errMsg}`);
      return { error: errMsg };
    }

    const rkey = uri.split("/").pop();
    shell.log(`🎨 Created ATProto painting record: ${rkey}`);

    return { rkey, uri };
  } catch (error) {
    shell.error(`⚠️  Failed to sync painting to ATProto: ${error.message}`);
    return { error: error.message };
  }
}

/**
 * Delete a painting from ATProto
 * @param {Object} database - MongoDB connection
 * @param {string} sub - User sub
 * @param {string} rkey - ATProto record key
 * @returns {Promise<Object>} { deleted: true } or { error }
 */
export async function deletePaintingFromAtproto(database, sub, rkey) {
  const users = database.db.collection("users");
  const user = await users.findOne({ _id: sub });

  if (!user?.atproto?.did || !user?.atproto?.password) {
    return { error: "No ATProto account" };
  }

  try {
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });

    await agent.com.atproto.repo.deleteRecord({
      repo: user.atproto.did,
      collection: "computer.aesthetic.painting",
      rkey,
    });

    shell.log(`🗑️  Deleted ATProto painting record: ${rkey}`);
    return { deleted: true };
  } catch (error) {
    shell.error(`⚠️  Failed to delete painting from ATProto: ${error.message}`);
    return { error: error.message };
  }
}
