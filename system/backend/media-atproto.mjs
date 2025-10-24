// media-atproto.mjs
// Unified ATProto sync for all AC media types
// Single source of truth for creating/deleting/updating ATProto records

import { AtpAgent } from "@atproto/api";
import { shell } from "./shell.mjs";
import { getThumbnailFromSlug } from "./thumbnail.mjs";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

export const MediaTypes = {
  PAINTING: 'painting',
  MOOD: 'mood',
  PIECE: 'piece',
  KIDLISP: 'kidlisp',
  TAPE: 'tape'
};

// Lexicon configurations for each media type
const LEXICONS = {
  [MediaTypes.PAINTING]: {
    collection: "computer.aesthetic.painting",
    hasBlob: true,
    hasAnonymous: true,
    buildRecord: async (item, user, database) => {
      // Extract required fields
      const slug = item.slug;
      const code = item.code;
      const refId = item._id.toString();
      const paintingDate = item.when;
      const bucket = item.bucket || (user._id ? "user-aesthetic-computer" : "art-aesthetic-computer");
      
      // Determine user info
      const userSub = user._id;
      const atprotoDid = user.atproto.did;
      const atprotoPassword = user.atproto.password;
      
      // Get handle for thumbnail generation
      const handles = database.db.collection("@handles");
      const handleDoc = await handles.findOne({ _id: user._id || "art-guest" });
      const handle = handleDoc?.handle;
      
      // Generate image URL using short code routing
      // The /media/paintings/{code} edge function handles redirects to actual storage
      const imageUrl = `https://aesthetic.computer/media/paintings/${code}`;
      
      // Generate thumbnail blob
      let thumbnailBlob = null;
      let thumbnailBuffer = null;
      
      // Extract just the image slug for thumbnail generation
      const imageSlug = slug.includes(':') ? slug.split(':')[0] : slug;
      
      if (handle && handle !== "art") {
        // User paintings: use /api/pixel with handle
        try {
          thumbnailBuffer = await getThumbnailFromSlug(imageSlug, handle, { bucket, userId: userSub });
        } catch (thumbError) {
          shell.warn(`⚠️  Failed to generate thumbnail via getThumbnailFromSlug: ${thumbError.message}`);
        }
      } else if (!userSub) {
        // Guest paintings: use direct DigitalOcean URL (no handle)
        try {
          thumbnailBuffer = await getThumbnailFromSlug(imageSlug, null, { bucket });
        } catch (thumbError) {
          shell.warn(`⚠️  Failed to generate thumbnail for guest painting: ${thumbError.message}`);
        }
      }
      
      // Fallback: fetch the original painting image if thumbnail generation failed
      if (!thumbnailBuffer) {
        try {
          const imageResponse = await fetch(imageUrl);
          if (imageResponse.ok) {
            const imageBuffer = await imageResponse.arrayBuffer();
            thumbnailBuffer = Buffer.from(imageBuffer);
          }
        } catch (fallbackError) {
          shell.warn(`⚠️  Failed to fetch original image for fallback thumbnail: ${fallbackError.message}`);
        }
      }
      
      // Build the record
      const record = {
        $type: "computer.aesthetic.painting",
        slug,
        code,
        imageUrl,
        acUrl: `https://aesthetic.computer/#${code}`,
        when: paintingDate.toISOString(),
        ref: refId
      };
      
      // Check if recording exists by attempting to fetch it from storage
      // For user paintings: {userId}/{slug}.zip
      // For anonymous paintings with recordings: slug is combined as imageSlug:recordingSlug
      if (userSub || slug.includes(':')) {
        const recordingSlug = slug.includes(':') ? slug.split(':')[1] : slug;
        const zipKey = userSub ? `${userSub}/${recordingSlug}.zip` : `${recordingSlug}.zip`;
        const zipUrl = `https://${bucket}.sfo3.digitaloceanspaces.com/${zipKey}`;
        
        try {
          const zipCheck = await fetch(zipUrl, { method: 'HEAD' });
          if (zipCheck.ok) {
            record.recordingUrl = `https://aesthetic.computer/media/paintings/${code}.zip`;
            shell.log(`🎬 Recording found for ${code}`);
          }
        } catch (zipError) {
          // Recording doesn't exist, that's OK
          shell.log(`📹 No recording for ${code}`);
        }
      }
      
      return { record, blob: thumbnailBuffer };
    }
  },
  
  [MediaTypes.MOOD]: {
    collection: "computer.aesthetic.mood",
    hasBlob: false,
    hasAnonymous: false,
    buildRecord: async (item, user, database) => {
      return {
        record: {
          $type: "computer.aesthetic.mood",
          text: item.text,
          when: item.when.toISOString(),
          ref: item._id.toString()
        }
      };
    }
  },
  
  [MediaTypes.PIECE]: {
    collection: "computer.aesthetic.piece",
    hasBlob: false,
    hasAnonymous: true,
    buildRecord: async (item, user, database) => {
      return {
        record: {
          $type: "computer.aesthetic.piece",
          slug: item.slug,
          when: item.when.toISOString(),
          ref: item._id.toString()
        }
      };
    }
  },
  
  [MediaTypes.KIDLISP]: {
    collection: "computer.aesthetic.kidlisp",
    hasBlob: false,
    hasAnonymous: true,
    buildRecord: async (item, user, database) => {
      return {
        record: {
          $type: "computer.aesthetic.kidlisp",
          code: item.code,
          source: item.source,
          acUrl: `https://aesthetic.computer/$${item.code}`,
          when: item.when.toISOString(),
          ref: item._id.toString()
        }
      };
    }
  },
  
  [MediaTypes.TAPE]: {
    collection: "computer.aesthetic.tape",
    hasBlob: true, // MP4 video blob
    hasAnonymous: true,
    buildRecord: async (item, user, database) => {
      const code = item.code;
      const slug = item.slug;
      const refId = item._id.toString();
      const tapeDate = item.when;
      const bucket = item.bucket || (user._id ? "user-aesthetic-computer" : "art-aesthetic-computer");
      const userSub = user._id;
      
      // Construct ZIP URL
      const zipKey = userSub ? `${userSub}/${slug}.zip` : `${slug}.zip`;
      const zipUrl = `https://${bucket}.sfo3.digitaloceanspaces.com/${zipKey}`;
      
      shell.log(`📼 Tape ${code}: ZIP at ${zipUrl}`);
      
      // Convert ZIP to MP4 with thumbnail
      let mp4Buffer = null;
      let thumbnailBuffer = null;
      try {
        const { convertTapeToMp4, checkFfmpegAvailable } = await import('./tape-to-mp4.mjs');
        
        // Check if ffmpeg is available
        const ffmpegAvailable = await checkFfmpegAvailable();
        if (!ffmpegAvailable) {
          shell.warn(`⚠️  ffmpeg not available - tape will sync without video/thumbnail blobs`);
        } else {
          shell.log(`🎬 Converting to MP4 with thumbnail...`);
          const result = await convertTapeToMp4(zipUrl);
          mp4Buffer = result.video;
          thumbnailBuffer = result.thumbnail;
          
          const videoSizeKB = (mp4Buffer.length / 1024).toFixed(2);
          shell.log(`✅ MP4 ready: ${videoSizeKB} KB`);
          
          if (thumbnailBuffer) {
            const thumbSizeKB = (thumbnailBuffer.length / 1024).toFixed(2);
            shell.log(`✅ Thumbnail ready: ${thumbSizeKB} KB`);
          }
        }
      } catch (conversionError) {
        shell.warn(`⚠️  MP4 conversion failed: ${conversionError.message}`);
        shell.warn(`   Tape will sync without video/thumbnail blobs`);
      }
      
      const record = {
        $type: "computer.aesthetic.tape",
        slug,
        code,
        zipUrl, // Direct link to download the ZIP
        acUrl: `https://aesthetic.computer/!${code}`,
        when: tapeDate.toISOString(),
        ref: refId
      };
      
      return { record, videoBlob: mp4Buffer, thumbnailBlob: thumbnailBuffer };
    }
  }
};

/**
 * Get user ATProto credentials
 * Handles both authenticated users and anonymous/guest content
 */
async function getUserCredentials(database, userSub) {
  const users = database.db.collection("users");
  
  if (!userSub) {
    // Guest/anonymous content uses art.at.aesthetic.computer
    const artUser = await users.findOne({ _id: "art-guest" });
    
    if (!artUser?.atproto?.did || !artUser?.atproto?.password) {
      throw new Error("Art account not configured");
    }
    
    return {
      _id: null,
      atproto: artUser.atproto
    };
  }
  
  // Regular user
  const user = await users.findOne({ _id: userSub });
  
  if (!user?.atproto?.did || !user?.atproto?.password) {
    throw new Error(`User ${userSub} has no ATProto account`);
  }
  
  return user;
}

/**
 * Create a media record in ATProto
 * Works for all media types with proper lexicon support
 * 
 * @param {Object} database - MongoDB connection
 * @param {string} type - Media type (use MediaTypes constants)
 * @param {Object} item - MongoDB item document
 * @param {Object} options - { userSub: string|null, dryRun: boolean }
 * @returns {Promise<Object>} { rkey, uri } or { error }
 */
export async function createMediaRecord(database, type, item, options = {}) {
  const { userSub = null, dryRun = false } = options;
  
  const lexicon = LEXICONS[type];
  if (!lexicon) {
    throw new Error(`Unknown media type: ${type}`);
  }
  
  try {
    // Get user credentials
    const user = await getUserCredentials(database, userSub);
    
    // Build the record (and blobs if needed)
    const buildResult = await lexicon.buildRecord(item, user, database);
    const { record } = buildResult;
    
    // Handle different blob types
    const blob = buildResult.blob || null;
    const videoBlob = buildResult.videoBlob || null;
    const thumbnailBlobData = buildResult.thumbnailBlob || null;
    
    if (dryRun) {
      shell.log(`[DRY RUN] Would create ${type} record: ${JSON.stringify(record).substring(0, 100)}...`);
      return { rkey: 'dry-run-key', uri: 'dry-run-uri' };
    }
    
    // Login to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password
    });
    
    // Upload thumbnail blob (for paintings or tapes)
    let thumbnailBlob = null;
    const thumbnailData = blob || thumbnailBlobData;
    if (thumbnailData && lexicon.hasBlob) {
      try {
        const imageUint8 = thumbnailData instanceof Uint8Array ? thumbnailData : new Uint8Array(thumbnailData);
        const encoding = type === 'TAPE' ? 'image/jpeg' : 'image/png';
        const blobResponse = await agent.uploadBlob(imageUint8, { encoding });
        thumbnailBlob = blobResponse?.data?.blob || blobResponse?.blob || null;
        
        if (thumbnailBlob) {
          const blobSize = typeof thumbnailBlob?.size === "number" ? `${thumbnailBlob.size} bytes` : "unknown size";
          shell.log(`📸 Uploaded ${type} thumbnail blob (${blobSize})`);
        }
      } catch (blobError) {
        shell.warn(`⚠️  Failed to upload thumbnail blob: ${blobError.message}`);
        // Continue without blob
      }
    }
    
    // Upload video blob (for tapes only)
    let videoBlobRef = null;
    if (videoBlob && type === 'tape') {
      try {
        const videoUint8 = videoBlob instanceof Uint8Array ? videoBlob : new Uint8Array(videoBlob);
        const blobResponse = await agent.uploadBlob(videoUint8, { encoding: 'video/mp4' });
        videoBlobRef = blobResponse?.data?.blob || blobResponse?.blob || null;
        
        if (videoBlobRef) {
          const blobSize = typeof videoBlobRef?.size === "number" ? `${videoBlobRef.size} bytes` : "unknown size";
          shell.log(`📸 Uploaded tape video blob (${blobSize})`);
        }
      } catch (blobError) {
        shell.warn(`⚠️  Failed to upload video blob: ${blobError.message}`);
        // Continue without video
      }
    }
    
    // Add blobs to record
    if (thumbnailBlob) {
      record.thumbnail = thumbnailBlob;
    }
    if (videoBlobRef) {
      record.video = videoBlobRef;
    }
    
    // Create the record
    const atprotoRecord = await agent.com.atproto.repo.createRecord({
      repo: user.atproto.did,
      collection: lexicon.collection,
      record
    });
    
    const uri = atprotoRecord.uri || atprotoRecord.data?.uri;
    if (!uri) {
      throw new Error(`ATProto response missing URI`);
    }
    
    const rkey = uri.split("/").pop();
    shell.log(`✅ Created ${type} ATProto record: ${rkey}`);
    
    return { rkey, uri };
    
  } catch (error) {
    shell.error(`❌ Failed to create ${type} ATProto record: ${error.message}`);
    return { error: error.message };
  }
}

/**
 * Delete a media record from ATProto
 * 
 * @param {Object} database - MongoDB connection
 * @param {string} type - Media type (use MediaTypes constants)
 * @param {string} userSub - User sub (null for guest)
 * @param {string} rkey - ATProto record key
 * @returns {Promise<Object>} { deleted: true } or { error }
 */
export async function deleteMediaRecord(database, type, userSub, rkey) {
  const lexicon = LEXICONS[type];
  if (!lexicon) {
    throw new Error(`Unknown media type: ${type}`);
  }
  
  try {
    const user = await getUserCredentials(database, userSub);
    
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password
    });
    
    await agent.com.atproto.repo.deleteRecord({
      repo: user.atproto.did,
      collection: lexicon.collection,
      rkey
    });
    
    shell.log(`🗑️  Deleted ${type} ATProto record: ${rkey}`);
    return { deleted: true };
    
  } catch (error) {
    shell.error(`❌ Failed to delete ${type} ATProto record: ${error.message}`);
    return { error: error.message };
  }
}

/**
 * Get collection name for a media type
 */
export function getCollection(type) {
  const lexicon = LEXICONS[type];
  if (!lexicon) {
    throw new Error(`Unknown media type: ${type}`);
  }
  return lexicon.collection;
}

/**
 * Check if a media type supports anonymous content
 */
export function supportsAnonymous(type) {
  const lexicon = LEXICONS[type];
  if (!lexicon) {
    throw new Error(`Unknown media type: ${type}`);
  }
  return lexicon.hasAnonymous;
}
