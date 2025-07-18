// Store KidLisp, 2025.01.16
// Caches KidLisp source code and generates short URLs for QR codes

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { customAlphabet } from 'nanoid';
import crypto from 'crypto';

const nanoid = customAlphabet('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', 4);

// Ensure indexes exist (reentrant - safe to call multiple times)
async function ensureIndexes(collection) {
  try {
    // Create unique index on code field
    await collection.createIndex({ code: 1 }, { 
      unique: true, 
      background: true,
      name: 'kidlisp_code_unique'
    });
    
    // Create unique index on hash field for deduplication
    await collection.createIndex({ hash: 1 }, { 
      unique: true, 
      background: true,
      name: 'kidlisp_hash_unique'
    });
    
    // Create index on when field for analytics/cleanup queries
    await collection.createIndex({ when: 1 }, { 
      background: true,
      name: 'kidlisp_when'
    });
    
    // Create index on user field for user-specific queries
    await collection.createIndex({ user: 1 }, { 
      background: true,
      sparse: true, // Only index documents that have the user field
      name: 'kidlisp_user'
    });
    
    console.log('📦 Kidlisp indexes ensured');
  } catch (error) {
    // Ignore index creation errors (they're likely already created)
    console.warn('Index creation warning (likely already exist):', error.message);
  }
}

export async function handler(event, context) {
  // Log all incoming requests for debugging
  console.log(`📥 Kidlisp store request: ${event.httpMethod} ${event.path || event.rawUrl || 'unknown'}`);
  console.log(`📊 Headers:`, Object.keys(event.headers || {}).length > 0 ? Object.keys(event.headers) : 'none');
  if (event.body) {
    console.log(`📦 Body length: ${event.body.length} characters`);
  }

  if (event.httpMethod === 'OPTIONS') {
    console.log(`✅ Handling OPTIONS preflight request`);
    return respond(200, '');
  }

  try {
    const database = await connect();
    const collection = database.db.collection('kidlisp');
    
    // Ensure indexes exist (safe to call multiple times)
    await ensureIndexes(collection);

    if (event.httpMethod === 'POST') {
      const { source } = JSON.parse(event.body || '{}');
      
      if (!source || typeof source !== 'string' || source.length > 50000) {
        return respond(400, { error: 'Invalid source' });
      }

      // Extract user from authorization (optional)
      let user;
      try {
        user = await authorize(event.headers);
        console.log(`👤 User authorized: ${user ? user.sub : 'none'}`);
      } catch (error) {
        console.log(`🔓 No user authorization (anonymous cache)`);
      }

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
          console.log(`♻️ Found existing cache: ${existing.code}`);
          await database.disconnect();
          return respond(200, { code: existing.code, cached: true });
        } else {
          // Extremely rare: hash collision detected!
          console.error('💥 SHA-256 collision detected!', { hash, existing: existing.source, new: source.trim() });
          // Continue to create new entry with different code
        }
      }

      // Generate unique nanoid with incremental length growth
      let code;
      let attempts = 0;
      let currentLength = 4; // Start with 4 characters
      const maxAttemptsPerLength = 25;
      const maxLength = 12;
      
      do {
        if (attempts >= maxAttemptsPerLength && currentLength < maxLength) {
          currentLength++; // Grow by 1 character
          attempts = 0; // Reset attempts for new length
          console.log(`🔄 Growing nanoid length to ${currentLength} characters`);
        }
        
        const generator = customAlphabet('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', currentLength);
        code = generator();
        attempts++;
        
        if (attempts > maxAttemptsPerLength && currentLength >= maxLength) {
          console.error(`💥 Failed to generate unique code after maximum attempts`);
          break;
        }
      } while (await collection.findOne({ code }));

      console.log(`🎲 Generated code: ${code} (attempt ${attempts})`);

      // Create document with optional user attribution (matching painting structure)
      const doc = {
        code,
        source: source.trim(),
        hash,
        when: new Date(),
        lastAccessed: new Date(),
        hits: 1
      };

      // Add user sub if authenticated (same as painting records)
      if (user?.sub) {
        doc.user = user.sub;
        console.log(`🔗 Linked to user: ${user.sub}`);
      }

      try {
        await collection.insertOne(doc);
        console.log(`💾 Cached new source: ${code}`);

        await database.disconnect();
        return respond(201, { code, cached: false });

      } catch (insertError) {
        // Handle duplicate key errors (race conditions)
        if (insertError.code === 11000) {
          if (insertError.keyPattern?.hash) {
            // Hash collision - return existing record
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
      
      if (!code) {
        return respond(400, { error: 'Code required' });
      }

      console.log(`🔍 Looking up code: ${code}`);

      const doc = await collection.findOne({ code });
      if (!doc) {
        await database.disconnect();
        return respond(404, { error: 'Not found' });
      }

      await collection.updateOne(
        { code }, 
        { 
          $inc: { hits: 1 },
          $set: { lastAccessed: new Date() }
        }
      );

      console.log(`📤 Retrieved source: ${code} (${doc.source.length} chars, ${doc.hits + 1} hits)`);

      await database.disconnect();
      return respond(200, { 
        source: doc.source,
        when: doc.when,
        hits: doc.hits + 1,
        user: doc.user || null
      });
    }

    await database.disconnect();
    return respond(405, { error: 'Method not allowed' });

  } catch (error) {
    console.error('❌ Kidlisp cache error:', error);
    return respond(500, { error: 'Internal server error' });
  }
}
