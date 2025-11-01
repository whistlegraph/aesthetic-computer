// Oven Complete, 25.10.28
// Webhook handler called when oven.aesthetic.computer completes MP4 conversion
// Downloads MP4/thumbnail from Spaces and syncs to ATProto

import https from 'https';
import http from 'http';
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { createTapeOnAtproto } from "../../backend/tape-atproto.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  let body;
  try {
    // Check if body exists and is not empty
    if (!event.body || event.body.trim() === '') {
      console.error('❌ Empty request body received');
      return respond(400, { message: "Empty request body" });
    }
    
    body = JSON.parse(event.body);
  } catch (error) {
    console.error('❌ JSON parse error:', error.message);
    console.error('📦 Raw body:', event.body);
    return respond(400, { message: "Invalid JSON body: " + error.message });
  }

  const { mongoId, code, mp4Url, thumbnailUrl, secret } = body;

  // Verify callback secret
  const expectedSecret = process.env.OVEN_CALLBACK_SECRET;
  if (expectedSecret && secret !== expectedSecret) {
    console.error('❌ Invalid callback secret');
    return respond(401, { message: "Unauthorized" });
  }

  if (!mongoId || !code || !mp4Url) {
    console.error('❌ Missing required fields. Body:', JSON.stringify(body));
    return respond(400, { message: "Missing required fields: mongoId, code, mp4Url" });
  }

  console.log(`🎬 MP4 conversion complete for tape: ${code} (${mongoId})`);
  console.log(`📹 MP4 URL: ${mp4Url}`);
  if (thumbnailUrl) {
    console.log(`📸 Thumbnail URL: ${thumbnailUrl}`);
  }

  // Helper to send status updates to oven
  const ovenUrl = process.env.OVEN_URL || 'https://oven.aesthetic.computer';
  const sendStatus = async (status, details) => {
    try {
      await notifyOven(`${ovenUrl}/bake-status`, { code, status, details });
    } catch (err) {
      console.warn(`⚠️  Failed to send status update:`, err.message);
    }
  };

  try {
    await sendStatus('syncing', 'Updating database');
    
    const database = await connect();
    
    // Update MongoDB with MP4 URLs
    const { ObjectId } = await import("mongodb");
    const tapes = database.db.collection("tapes");
    
    await tapes.updateOne(
      { _id: new ObjectId(mongoId) },
      { 
        $set: { 
          mp4Url, 
          thumbnailUrl,
          mp4Status: "complete",
          mp4CompletedAt: new Date()
        } 
      }
    );
    
    console.log(`✅ Updated tape with MP4 URLs`);

    // Download MP4 and thumbnail for ATProto upload
    await sendStatus('downloading', 'Downloading MP4 for ATProto');
    console.log(`📥 Downloading MP4 from ${mp4Url}...`);
    const mp4Response = await fetch(mp4Url);
    if (!mp4Response.ok) {
      throw new Error(`Failed to download MP4: ${mp4Response.statusText}`);
    }
    const mp4Buffer = Buffer.from(await mp4Response.arrayBuffer());
    console.log(`✅ Downloaded MP4: ${(mp4Buffer.length / 1024).toFixed(2)} KB`);

    let thumbnailBuffer = null;
    if (thumbnailUrl) {
      await sendStatus('downloading', 'Downloading thumbnail');
      console.log(`📥 Downloading thumbnail from ${thumbnailUrl}...`);
      const thumbResponse = await fetch(thumbnailUrl);
      if (thumbResponse.ok) {
        thumbnailBuffer = Buffer.from(await thumbResponse.arrayBuffer());
        console.log(`✅ Downloaded thumbnail: ${(thumbnailBuffer.length / 1024).toFixed(2)} KB`);
      } else {
        console.warn(`⚠️  Failed to download thumbnail: ${thumbResponse.statusText}`);
      }
    }

    // Sync to ATProto (handles guest/no-atproto gracefully)
    await sendStatus('uploading-atproto', 'Syncing to ATProto');
    const result = await createTapeOnAtproto(database, mongoId, mp4Buffer, thumbnailBuffer);

    // Get the tape record for slug
    const tape = await tapes.findOne({ _id: new ObjectId(mongoId) });
    
    // Save to oven-bakes collection for history
    await sendStatus('finalizing', 'Saving to history');
    const ovenBakes = database.db.collection('oven-bakes');
    await ovenBakes.insertOne({
      mongoId,
      slug: tape?.slug,
      code,
      mp4Url,
      thumbnailUrl,
      success: true,
      completedAt: new Date(),
      atprotoRkey: result.rkey || null,
      atprotoError: result.error || null
    });

    await database.disconnect();
    
    // Notify oven of completion for live UI updates
    try {
      await notifyOven(`${ovenUrl}/bake-complete`, {
        slug: tape?.slug,
        code,
        success: true,
        mp4Url,
        thumbnailUrl,
        atprotoRkey: result.rkey || null
      });
      console.log(`🔥 Notified oven of completion`);
    } catch (notifyError) {
      console.warn(`⚠️  Failed to notify oven:`, notifyError.message);
    }

    if (result.error) {
      console.log(`ℹ️  ATProto sync skipped: ${result.error}`);
      return respond(200, { 
        message: "MP4 stored, ATProto sync skipped",
        reason: result.error 
      });
    }

    console.log(`🦋 ATProto sync complete: ${result.rkey}`);
    return respond(200, { 
      message: "MP4 stored and synced to ATProto",
      rkey: result.rkey 
    });

  } catch (error) {
    console.error(`❌ Webhook handler error:`, error);
    
    // Try to notify oven of failure
    const ovenUrl = process.env.OVEN_URL || 'https://oven.aesthetic.computer';
    try {
      await notifyOven(`${ovenUrl}/bake-complete`, {
        slug: code, // Fallback to code if we don't have slug
        code,
        success: false,
        error: error.message
      });
    } catch (notifyError) {
      console.warn(`⚠️  Failed to notify oven of error:`, notifyError.message);
    }
    
    return respond(500, { message: error.message || String(error) });
  }
}

/**
 * Notify oven server using https.request
 * Handles self-signed SSL certificates for localhost
 * @param {string} fullUrl - Full URL including path (e.g., https://localhost:3002/bake-complete)
 * @param {object} payload - JSON payload to send
 */
function notifyOven(fullUrl, payload) {
  return new Promise((resolve, reject) => {
    const url = new URL(fullUrl);
    const isHttps = url.protocol === 'https:';
    const client = isHttps ? https : http;
    const body = JSON.stringify(payload);
    
    const options = {
      hostname: url.hostname,
      port: url.port || (isHttps ? 443 : 80),
      path: url.pathname + url.search,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(body),
      },
      rejectUnauthorized: false, // Accept self-signed certs in dev
    };

    const req = client.request(options, (res) => {
      if (res.statusCode < 200 || res.statusCode >= 300) {
        reject(new Error(`Oven notification failed: ${res.statusCode} ${res.statusMessage}`));
        return;
      }
      resolve();
    });

    req.on('error', reject);
    req.write(body);
    req.end();
  });
}
