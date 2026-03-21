// Store Clock, 2026.01.02
// Caches clock melody strings and generates pronounceable short codes for QR codes

import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { publishProfileEvent } from "../../backend/profile-stream.mjs";
import crypto from 'crypto';

// Pronounceable code generation (CVCV pattern for singability)
// Examples: bako, milu, dena, tofi, ra9u, b4ko
// Prefers pure vowels (70%) but sometimes includes single-syllable digits (30%)
// Digits: 1,2,3,4,5,6,8,9 (skip 0="zero", 7="seven" - 2 syllables)
const CONSONANTS = 'bdfghjklmnprstvwz'.split(''); // 17 consonants (no hard-to-pronounce)
const VOWELS = 'aeiou'.split(''); // 5 vowels
const SINGABLE_DIGITS = '12345689'.split(''); // 8 single-syllable digits

function generatePronounceable(length = 4) {
  let code = '';
  for (let i = 0; i < length; i++) {
    if (i % 2 === 0) {
      // Consonant position
      code += CONSONANTS[Math.floor(Math.random() * CONSONANTS.length)];
    } else {
      // Vowel position - 70% pure vowel, 30% digit
      if (Math.random() < 0.7) {
        code += VOWELS[Math.floor(Math.random() * VOWELS.length)];
      } else {
        code += SINGABLE_DIGITS[Math.floor(Math.random() * SINGABLE_DIGITS.length)];
      }
    }
  }
  return code;
}

// Generate unique pronounceable code with collision checking
async function generateUniquePronounceable(collection, maxAttempts = 10) {
  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    // Start with 4 chars, increase length if many collisions
    const length = attempt < 5 ? 4 : (attempt < 8 ? 5 : 6);
    const code = generatePronounceable(length);
    
    const existing = await collection.findOne({ code });
    if (!existing) {
      return code;
    }
    console.log(`🔄 Code collision: ${code}, retrying...`);
  }
  
  // Fallback: add timestamp suffix
  return generatePronounceable(4) + Date.now().toString(36).slice(-2);
}

// Ensure indexes exist (reentrant - safe to call multiple times)
async function ensureIndexes(collection) {
  try {
    await collection.createIndex({ code: 1 }, { 
      unique: true, 
      background: true,
      name: 'clock_code_unique'
    });
    
    await collection.createIndex({ hash: 1 }, { 
      unique: true, 
      background: true,
      name: 'clock_hash_unique'
    });
    
    await collection.createIndex({ when: -1 }, { 
      background: true,
      name: 'clock_when'
    });
    
    await collection.createIndex({ user: 1 }, { 
      background: true,
      sparse: true,
      name: 'clock_user'
    });
    
    await collection.createIndex({ hits: -1 }, { 
      background: true,
      name: 'clock_hits'
    });
    
    console.log('📦 Clock indexes ensured');
  } catch (error) {
    console.warn('Index creation warning (likely already exist):', error.message);
  }
}

export async function handler(event, context) {
  console.log(`📥 Clock store request: ${event.httpMethod} ${event.path || event.rawUrl || 'unknown'}`);

  if (event.httpMethod === 'OPTIONS') {
    return respond(204, '');
  }

  let database;
  try {
    database = await connect();
  } catch (connectError) {
    console.error('❌ MongoDB connection failed:', connectError.message);
    return respond(503, { error: 'Database temporarily unavailable' });
  }

  try {
    const collection = database.db.collection('clocks');
    await ensureIndexes(collection);

    if (event.httpMethod === 'POST') {
      // Accept both 'source' (preferred) and 'melody' (legacy) for backwards compatibility
      const body = JSON.parse(event.body || '{}');
      const source = body.source || body.melody;
      
      if (!source || typeof source !== 'string' || source.length > 10000) {
        return respond(400, { error: 'Invalid source' });
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
        console.log(`🔓 No user authorization (anonymous cache): ${error.message}`);
      }

      let profileHandle = null;
      if (user?.sub) {
        try {
          const handleOrEmail = await getHandleOrEmail(user.sub);
          if (typeof handleOrEmail === "string" && handleOrEmail.startsWith("@")) {
            profileHandle = handleOrEmail;
          }
        } catch (err) {
          console.warn("⚠️  Could not resolve profile handle for clock event:", err?.message || err);
        }
      }

      const hash = crypto.createHash('sha256').update(source.trim()).digest('hex');
      console.log(`🔍 Source hash: ${hash.substring(0, 16)}...`);
      
      // Check for existing by hash first (deduplication)
      const existing = await collection.findOne({ hash });
      if (existing) {
        if (existing.source === source.trim()) {
          await collection.updateOne(
            { hash }, 
            { 
              $inc: { hits: 1 },
              $set: { lastAccessed: new Date() }
            }
          );
          console.log(`♻️ Found existing cache: ${existing.code}`);
          await database.disconnect();
          return respond(200, { code: existing.code, cached: true });
        } else {
          console.error('💥 SHA-256 collision detected!', { hash });
        }
      }

      // Generate unique pronounceable code
      const code = await generateUniquePronounceable(collection);
      console.log(`✨ Generated pronounceable code: ${code}`);

      // Create document (using 'source' for consistency with KidLisp schema)
      const doc = {
        code,
        source: source.trim(),
        hash,
        when: new Date(),
        lastAccessed: new Date(),
        hits: 1
      };

      if (user?.sub) {
        doc.user = user.sub;
        console.log(`🔗 Linked to user: ${user.sub}`);
      }

      try {
        await collection.insertOne(doc);
        console.log(`💾 Cached new melody: ${code}`);

        if (profileHandle) {
          publishProfileEvent({
            handle: profileHandle,
            event: {
              type: "clock",
              when: Date.now(),
              label: `Clock *${code}`,
              ref: code,
            },
            countsDelta: { clocks: 1 },
          }).catch((err) => {
            console.warn("⚠️  clock profile-event publish failed:", err?.message || err);
          });
        }

        await database.disconnect();
        return respond(201, { code, cached: false });

      } catch (insertError) {
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
              return respond(200, { code: existing.code, cached: true });
            }
          } else if (insertError.keyPattern?.code) {
            await database.disconnect();
            return respond(500, { error: 'Code generation collision, please retry' });
          }
        }
        throw insertError;
      }

    } else if (event.httpMethod === 'GET') {
      const code = event.queryStringParameters?.code;
      const recent = event.queryStringParameters?.recent;
      
      // Handle recent clocks feed
      if (recent) {
        const limit = Math.min(parseInt(event.queryStringParameters?.limit) || 50, 1000);
        const sortBy = event.queryStringParameters?.sort || 'recent';
        
        console.log(`📊 Recent clocks: limit=${limit}, sort=${sortBy}`);
        
        const pipeline = [
          {
            $lookup: {
              from: "@handles",
              localField: "user",
              foreignField: "_id",
              as: "handleInfo"
            }
          },
          {
            $addFields: {
              handle: { 
                $cond: {
                  if: { $gt: [{ $size: "$handleInfo" }, 0] },
                  then: { $concat: ["@", { $arrayElemAt: ["$handleInfo.handle", 0] }] },
                  else: null
                }
              }
            }
          },
          {
            $sort: sortBy === 'hits' ? { hits: -1, when: -1 } : { when: -1 }
          },
          { $limit: limit },
          {
            $project: {
              _id: 0,
              code: 1,
              source: 1,
              when: 1,
              hits: 1,
              user: 1,
              handle: 1
            }
          }
        ];
        
        const docs = await collection.aggregate(pipeline).toArray();
        
        const recentClocks = docs.map(doc => ({
          code: doc.code,
          source: doc.source,
          preview: doc.source.length > 40 ? doc.source.substring(0, 37) + "..." : doc.source,
          when: doc.when,
          hits: doc.hits,
          user: doc.user || null,
          handle: doc.handle || null
        }));
        
        console.log(`📤 Recent clocks: ${recentClocks.length}`);
        await database.disconnect();
        return respond(200, { recent: recentClocks, count: recentClocks.length });
      }
      
      // Handle single code lookup
      if (!code) {
        await database.disconnect();
        return respond(400, { error: 'Missing code parameter' });
      }

      console.log(`🔍 Looking up clock: ${code}`);
      
      const doc = await collection.findOne({ code: code.toLowerCase() });
      
      if (!doc) {
        console.log(`❌ Clock not found: ${code}`);
        await database.disconnect();
        return respond(404, { error: 'Clock not found' });
      }

      // Update hit count
      await collection.updateOne(
        { code: code.toLowerCase() }, 
        { 
          $inc: { hits: 1 },
          $set: { lastAccessed: new Date() }
        }
      );

      // Lookup handle if user exists
      let handle = null;
      if (doc.user) {
        const handleDoc = await database.db.collection('@handles').findOne({ _id: doc.user });
        if (handleDoc?.handle) {
          handle = `@${handleDoc.handle}`;
        }
      }

      console.log(`✅ Found clock: ${code} (${doc.hits + 1} hits)`);
      await database.disconnect();
      
      return respond(200, {
        code: doc.code,
        source: doc.source,
        when: doc.when,
        hits: doc.hits + 1,
        user: doc.user || null,
        handle
      });
    }

    await database.disconnect();
    return respond(405, { error: 'Method not allowed' });

  } catch (error) {
    console.error('❌ Clock store error:', error);
    try { await database?.disconnect(); } catch (e) {}
    return respond(500, { error: 'Internal server error' });
  }
}
