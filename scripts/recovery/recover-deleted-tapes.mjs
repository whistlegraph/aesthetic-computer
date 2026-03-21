#!/usr/bin/env node

/**
 * recover-deleted-tapes.mjs
 *
 * Attempts to recover deleted tapes by checking DigitalOcean Spaces for ZIP/MP4 files.
 * - If MP4 exists: update MongoDB tape doc with mp4Url and thumbnailUrl (if available)
 * - If ZIP exists but no MP4: optionally trigger oven rebake (POST to oven /bake)
 * - Optionally call recreate-tape-with-blobs.mjs to recreate ATProto records
 *
 * Usage:
 *   node system/backend/recover-deleted-tapes.mjs @handle [--codes a,b,c] [--dry-run] [--rebake] [--recreate] [--verbose]
 *
 * Requires DigitalOcean Spaces credentials in env (ART_SPACES_* and AT_BLOBS_SPACES_* env variables)
 */

import { S3Client, HeadObjectCommand, ListObjectsV2Command } from '@aws-sdk/client-s3';
import { connect } from '../system/backend/database.mjs';
import { userIDFromHandle } from '../system/backend/authorization.mjs';
import { createTapeOnAtproto } from '../system/backend/tape-atproto.mjs';
import fetch from 'node-fetch';
import { config } from 'dotenv';

// Load system/.env (scripts/ is at root level, system/ is sibling)
config({ path: './system/.env' });

const args = process.argv.slice(2);
const handleArg = args.find(a => a && a.startsWith('@'));
if (!handleArg) {
  console.error('Usage: node system/backend/recover-deleted-tapes.mjs @handle [--codes a,b] [--dry-run] [--rebake] [--recreate] [--verbose]');
  process.exit(1);
}
const handle = handleArg.replace('@', '');
const dryRun = args.includes('--dry-run');
const rebake = args.includes('--rebake');
const recreate = args.includes('--recreate');
const verbose = args.includes('--verbose');

const codesArgIndex = args.indexOf('--codes');
let codesList = null;
if (codesArgIndex !== -1 && args[codesArgIndex + 1]) {
  codesList = args[codesArgIndex + 1].split(',').map(s => s.trim());
}

// Support multiple env var naming conventions present in this repo
const ART_BUCKET = process.env.ART_SPACES_BUCKET || process.env.ART_SPACE_NAME || 'art-aesthetic-computer';
const AT_BLOBS_BUCKET = process.env.AT_BLOBS_SPACES_BUCKET || process.env.AT_BLOBS_BUCKET || 'at-blobs-aesthetic-computer';
const ART_ENDPOINT = process.env.ART_SPACES_ENDPOINT || process.env.DO_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com';
const AT_BLOBS_ENDPOINT = process.env.AT_BLOBS_SPACES_ENDPOINT || process.env.DO_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com';

const artAccessKey = process.env.ART_SPACES_KEY || process.env.ART_KEY || process.env.DO_SPACES_KEY;
const artSecret = process.env.ART_SPACES_SECRET || process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
const atAccessKey = process.env.AT_BLOBS_SPACES_KEY || process.env.AT_BLOBS_KEY || process.env.DO_SPACES_KEY;
const atSecret = process.env.AT_BLOBS_SPACES_SECRET || process.env.AT_BLOBS_SECRET || process.env.DO_SPACES_SECRET;

const artClient = new S3Client({
  endpoint: ART_ENDPOINT,
  region: 'us-east-1',
  credentials: {
    accessKeyId: artAccessKey,
    secretAccessKey: artSecret,
  },
});
const atBlobsClient = new S3Client({
  endpoint: AT_BLOBS_ENDPOINT,
  region: 'us-east-1',
  credentials: {
    accessKeyId: atAccessKey,
    secretAccessKey: atSecret,
  },
});
// Debug: ensure credentials are present
if (verbose) {
  console.log('ART access key present:', !!artAccessKey);
  console.log('ART secret present:', !!artSecret);
  console.log('AT_BLOBS access key present:', !!atAccessKey);
  console.log('AT_BLOBS secret present:', !!atSecret);
}

async function listKeys(client, bucket, prefix = 'tapes/') {
  const keys = [];
  let token;
  do {
    const res = await client.send(new ListObjectsV2Command({ Bucket: bucket, Prefix: prefix, ContinuationToken: token }));
    if (res.Contents) keys.push(...res.Contents.map(c => c.Key));
    token = res.NextContinuationToken;
  } while (token);
  return keys;
}

async function headKey(client, bucket, key) {
  try {
    await client.send(new HeadObjectCommand({ Bucket: bucket, Key: key }));
    return true;
  } catch (err) {
    return false;
  }
}

async function getObjectMeta(client, bucket, key) {
  try {
    const res = await client.send(new HeadObjectCommand({ Bucket: bucket, Key: key }));
    return { lastModified: res.LastModified, size: res.ContentLength };
  } catch (err) {
    return null;
  }
}

async function run() {
  const db = await connect();
  try {
    const userSub = await userIDFromHandle(handle, db);
    if (!userSub) {
      console.error(`Could not find user for @${handle}`);
      process.exit(1);
    }
    const tapes = db.db.collection('tapes');

    // If codes provided, fetch those and if missing create placeholder entries to recover into
    let targetTapes = [];
    if (codesList) {
      for (const code of codesList) {
        const t = await tapes.findOne({ code });
        if (t) {
          targetTapes.push(t);
        } else {
          // Placeholder; we'll fill more info after checking Spaces
          targetTapes.push({ code, _missingFromDb: true });
        }
      }
    } else {
      // default to known deleted set (attempt to discover in Spaces)
      const defaultDeleted = ['j75','egl','wk7','yi7','znz','nj9','pNf','val','79x'];
      for (const code of defaultDeleted) {
        const t = await tapes.findOne({ code });
        if (t) targetTapes.push(t);
        else targetTapes.push({ code, _missingFromDb: true });
      }
    }

    // Pre-list keys for both buckets (to avoid many head calls)
    let artKeys = [];
    let atKeys = [];
    try {
      artKeys = await listKeys(artClient, ART_BUCKET, 'tapes/');
      atKeys = await listKeys(atBlobsClient, AT_BLOBS_BUCKET, 'tapes/');
    } catch (err) {
      console.error('Failed to list bucket keys. Ensure Spaces credentials are set in env.');
      console.error(err.message || err);
      await db.disconnect();
      process.exit(1);
    }

    const results = [];
    for (const tape of targetTapes) {
      const code = tape.code;
      const zipKey = `tapes/${code}.zip`;
      const mp4Key = `tapes/${code}.mp4`;
      const thumbKey = `tapes/${code}-thumb.jpg`;

      const hasZipArt = artKeys.includes(zipKey);
      const hasZipAt = atKeys.includes(zipKey);
      const hasMp4Art = artKeys.includes(mp4Key);
      const hasMp4At = atKeys.includes(mp4Key);
      const hasThumbArt = artKeys.includes(thumbKey);
      const hasThumbAt = atKeys.includes(thumbKey);

      const mp4Url = hasMp4Art
        ? `https://art-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/${mp4Key}`
        : hasMp4At
          ? `https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/${mp4Key}`
          : null;
      const zipUrl = hasZipArt
        ? `https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/${zipKey}`
        : hasZipAt
          ? `https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/${zipKey}`
          : null;
      const thumbUrl = hasThumbArt
        ? `https://art-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/${thumbKey}`
        : hasThumbAt
          ? `https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/${thumbKey}`
          : null;

      console.log(`\nChecking code ${code}: zip=${!!zipUrl} mp4=${!!mp4Url} thumb=${!!thumbUrl}`);

      if (mp4Url) {
        console.log(` - MP4 found: ${mp4Url}`);
        if (tape._missingFromDb) {
          console.log(' - Tape missing from MongoDB, will create new record');
          if (!dryRun) {
            // try to get lastModified from object metadata to set when
            let meta = await getObjectMeta(hasMp4Art ? artClient : atBlobsClient, hasMp4Art ? ART_BUCKET : AT_BLOBS_BUCKET, mp4Key);
            const when = meta?.lastModified ? new Date(meta.lastModified) : new Date();
            const insert = {
              code,
              slug: tape.slug || when.toISOString(),
              when,
              user: userSub,
              mp4Url,
              thumbnailUrl: thumbUrl || null,
              mp4Status: 'complete',
              mp4CompletedAt: new Date()
            };
            const res = await tapes.insertOne(insert);
            tape._id = res.insertedId;
            console.log(' - Created MongoDB tape record with _id', tape._id.toString());
          } else {
            console.log(' - [DRY RUN] Would create MongoDB tape record');
          }
        } else {
          if (!dryRun) {
            console.log(' - Updating MongoDB with mp4Url/thumbnail...');
            await tapes.updateOne({ _id: tape._id }, { $set: { mp4Url, thumbnailUrl: thumbUrl || null, mp4Status: 'complete', mp4CompletedAt: new Date() } });
            console.log(' - Updated MongoDB');
          } else {
            console.log(' - [DRY RUN] Would update MongoDB');
          }
        }

        if (recreate) {
          console.log(' - Creating ATProto record with blobs using createTapeOnAtproto');
          if (!dryRun) {
            try {
              // Download MP4 and thumbnail
              console.log('   📥 Downloading MP4...');
              const mp4Response = await fetch(mp4Url);
              const mp4Buffer = Buffer.from(await mp4Response.arrayBuffer());
              console.log(`   ✅ MP4 downloaded: ${(mp4Buffer.length / 1024).toFixed(2)} KB`);
              
              let thumbnailBuffer = null;
              if (thumbUrl) {
                console.log('   📥 Downloading thumbnail...');
                const thumbResponse = await fetch(thumbUrl);
                if (thumbResponse.ok) {
                  thumbnailBuffer = Buffer.from(await thumbResponse.arrayBuffer());
                  console.log(`   ✅ Thumbnail downloaded: ${(thumbnailBuffer.length / 1024).toFixed(2)} KB`);
                }
              }
              
              // Create ATProto record with blobs
              await createTapeOnAtproto(db, tape._id, mp4Buffer, thumbnailBuffer);
              console.log('   ✅ ATProto record created');
            } catch (err) {
              console.error('   ❌ Failed to create ATProto record:', err.message);
            }
          } else {
            console.log(' - [DRY RUN] Would create ATProto record with createTapeOnAtproto');
          }
        }

      } else if (zipUrl) {
        console.log(` - ZIP found: ${zipUrl}`);
        if (rebake) {
          console.log(' - Triggering oven rebake via POST to /bake');
          const ovenUrl = process.env.OVEN_URL || 'https://oven.aesthetic.computer';
          const callbackUrl = process.env.OVEN_CALLBACK_URL || 'https://aesthetic.computer/api/oven-complete';
          const callbackSecret = process.env.CALLBACK_SECRET;
          const payload = { mongoId: tape._id ? tape._id.toString() : null, slug: tape.slug, code: tape.code, zipUrl, callbackUrl, callbackSecret };
          if (!dryRun) {
            try {
              const res = await fetch(`${ovenUrl}/bake`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(payload),
              });
              if (!res.ok) {
                console.error(` - Failed to trigger bake: ${res.status} ${res.statusText}`);
              } else {
                console.log(' - Bake triggered');
              }
            } catch (err) {
              console.error(' - Bake request failed:', err.message);
            }
          } else {
            console.log(' - [DRY RUN] Would trigger oven bake POST');
          }
        } else {
          console.log(' - ZIP available; run with --rebake to trigger oven to create MP4s');
        }
      } else {
        console.log(' - No source found in spaces for this tape');
      }

      results.push({ code: tape.code, mp4Url, zipUrl, thumbUrl });
    }

    console.log('\nRecovery summary:');
    console.log(JSON.stringify(results, null, 2));

    await db.disconnect();
    process.exit(0);
  } catch (err) {
    console.error('Fatal error:', err);
    await db.disconnect();
    process.exit(1);
  }
}

run().catch(err => { console.error(err); process.exit(1); });
