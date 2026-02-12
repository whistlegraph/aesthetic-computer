// Store Piece, 2026.02.12
// Accepts JavaScript source code and publishes it as a piece
// Combines the "accept source as string" UX of store-kidlisp with the "store as .mjs" backend of track-media

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { createMediaRecord, MediaTypes } from "../../backend/media-atproto.mjs";
import { S3Client, PutObjectCommand, PutObjectAclCommand } from "@aws-sdk/client-s3";
import crypto from 'crypto';

// Maximum source code length
const MAX_SOURCE_LENGTH = 100000;

// Validate that source code contains valid piece exports
function validatePieceSource(source) {
  const validExports = [
    'export function boot',
    'export function paint',
    'export function sim',
    'export function act',
    'export default'
  ];

  return validExports.some(exp => source.includes(exp));
}

// Ensure indexes exist (reentrant - safe to call multiple times)
async function ensureIndexes(collection) {
  try {
    await collection.createIndex({ code: 1 }, {
      unique: true,
      background: true,
      name: 'pieces_code_unique'
    });

    await collection.createIndex({ hash: 1 }, {
      unique: true,
      background: true,
      name: 'pieces_hash_unique'
    });

    await collection.createIndex({ when: 1 }, {
      background: true,
      name: 'pieces_when'
    });

    await collection.createIndex({ user: 1 }, {
      background: true,
      sparse: true,
      name: 'pieces_user'
    });

    console.log('📦 Pieces indexes ensured');
  } catch (error) {
    console.warn('Index creation warning (likely already exist):', error.message);
  }
}

// Upload source to S3 as .mjs file
async function uploadToS3(source, slug, bucket) {
  const s3Client = new S3Client({
    endpoint: `https://sfo3.digitaloceanspaces.com`,
    region: 'us-east-1',
    credentials: {
      accessKeyId: process.env.ART_KEY || process.env.DO_SPACES_KEY,
      secretAccessKey: process.env.ART_SECRET || process.env.DO_SPACES_SECRET,
    },
  });

  // Upload .mjs file
  const putCommand = new PutObjectCommand({
    Bucket: bucket,
    Key: `${slug}.mjs`,
    Body: source,
    ContentType: 'text/javascript',
  });

  await s3Client.send(putCommand);
  console.log(`✅ Uploaded to S3: ${bucket}/${slug}.mjs`);

  // Make it publicly readable
  const aclCommand = new PutObjectAclCommand({
    Bucket: bucket,
    Key: `${slug}.mjs`,
    ACL: "public-read",
  });

  await s3Client.send(aclCommand);
  console.log(`✅ Set public-read ACL for ${bucket}/${slug}.mjs`);
}

export async function handler(event, context) {
  console.log(`📥 Store piece request: ${event.httpMethod} ${event.path || event.rawUrl || 'unknown'}`);

  if (event.httpMethod === 'OPTIONS') {
    console.log(`✅ Handling OPTIONS preflight request`);
    return respond(204, '');
  }

  if (event.httpMethod !== 'POST') {
    return respond(405, { error: 'Method not allowed' });
  }

  let database;
  try {
    database = await connect();
  } catch (connectError) {
    console.error('❌ MongoDB connection failed:', connectError.message);
    return respond(503, { error: 'Database temporarily unavailable' });
  }

  try {
    const { source, name } = JSON.parse(event.body || '{}');

    // Validate input
    if (!source || typeof source !== 'string') {
      return respond(400, { error: 'Invalid source: must be a string' });
    }

    if (source.length > MAX_SOURCE_LENGTH) {
      return respond(400, {
        error: `Source too long: ${source.length} characters (max ${MAX_SOURCE_LENGTH})`
      });
    }

    // Validate it's a valid piece
    if (!validatePieceSource(source)) {
      return respond(400, {
        error: 'Invalid piece: must contain at least one of: export function boot, export function paint, export function sim, export function act, or export default'
      });
    }

    // Extract user from authorization (optional)
    let user;
    try {
      const authPromise = authorize(event.headers);
      const timeoutPromise = new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Auth timeout')), 3000)
      );

      user = await Promise.race([authPromise, timeoutPromise]);
      console.log(`👤 User authorized: ${user ? user.sub : 'none'}`);
    } catch (error) {
      console.log(`🔓 No user authorization (anonymous publish): ${error.message}`);
    }

    const collection = database.db.collection('pieces');
    await ensureIndexes(collection);

    // Hash source for deduplication
    const hash = crypto.createHash('sha256').update(source.trim()).digest('hex');
    console.log(`🔍 Source hash: ${hash.substring(0, 16)}...`);

    // Check for existing by hash first
    const existing = await collection.findOne({ hash });
    if (existing) {
      // Paranoid collision check: verify source actually matches
      if (existing.source === source.trim()) {
        await collection.updateOne(
          { hash },
          {
            $inc: { hits: 1 },
            $set: { lastAccessed: new Date() }
          }
        );
        console.log(`♻️  Found existing piece: ${existing.code}`);
        await database.disconnect();
        return respond(200, {
          code: existing.code,
          cached: true,
          url: `https://aesthetic.computer/${existing.code}`
        });
      } else {
        // Extremely rare: hash collision detected!
        console.error('💥 SHA-256 collision detected!', { hash });
      }
    }

    // Generate unique code
    const code = await generateUniqueCode(collection, {
      mode: 'inferred',
      sourceText: name || source.substring(0, 200),
      type: 'piece'
    });
    console.log(`✨ Generated code: ${code}`);

    // Determine slug (date-based path like other media)
    const now = new Date();
    const year = now.getFullYear();
    const month = String(now.getMonth() + 1).padStart(2, '0');
    const day = String(now.getDate()).padStart(2, '0');
    const slug = user
      ? `${user.sub}/${year}/${month}/${day}/${code}`
      : `${year}/${month}/${day}/${code}`;

    const bucket = user ? "user-aesthetic-computer" : "art-aesthetic-computer";

    // Upload to S3
    try {
      await uploadToS3(source, slug, bucket);
    } catch (s3Error) {
      console.error('❌ S3 upload failed:', s3Error.message);
      await database.disconnect();
      return respond(500, { error: 'Failed to upload piece to storage' });
    }

    // Create database record
    const doc = {
      code,
      slug,
      source: source.trim(),
      hash,
      when: now,
      lastAccessed: now,
      hits: 1,
      bucket,
    };

    // Add optional fields
    if (user?.sub) {
      doc.user = user.sub;
      console.log(`🔗 Linked to user: ${user.sub}`);
    }

    if (name) {
      doc.name = name;
    }

    try {
      const insertResult = await collection.insertOne(doc);
      console.log(`💾 Stored new piece: ${code}`);

      // Sync to ATProto in background (don't wait for it)
      const pieceId = insertResult.insertedId;
      const savedRecord = await collection.findOne({ _id: pieceId });

      if (savedRecord) {
        createMediaRecord(database, MediaTypes.PIECE, savedRecord, {
          userSub: user?.sub
        })
        .then(result => {
          if (result.error) {
            console.error(`⚠️  ATProto sync failed: ${result.error}`);
          } else {
            console.log(`✅ Synced piece to ATProto: ${result.rkey}`);
            collection.updateOne(
              { _id: pieceId },
              { $set: { "atproto.rkey": result.rkey } }
            ).catch(err => console.error(`⚠️  Failed to update rkey: ${err.message}`));
          }
        })
        .catch(err => console.error(`⚠️  ATProto sync error: ${err.message}`));
      }

      await database.disconnect();
      return respond(201, {
        code,
        cached: false,
        url: `https://aesthetic.computer/${code}`
      });

    } catch (insertError) {
      // Handle duplicate key errors (race conditions)
      if (insertError.code === 11000) {
        if (insertError.keyPattern?.hash) {
          const existing = await collection.findOne({ hash });
          if (existing) {
            await collection.updateOne(
              { hash },
              {
                $inc: { hits: 1 },
                $set: { lastAccessed: new Date() }
              }
            );
            console.log(`🔄 Race condition resolved: ${existing.code}`);
            await database.disconnect();
            return respond(200, {
              code: existing.code,
              cached: true,
              url: `https://aesthetic.computer/${existing.code}`
            });
          }
        } else if (insertError.keyPattern?.code) {
          await database.disconnect();
          return respond(500, { error: 'Code generation collision, please retry' });
        }
      }
      throw insertError;
    }

  } catch (error) {
    console.error('❌ Store piece error:', error);
    return respond(500, { error: 'Internal server error', details: error.message });
  }
}
