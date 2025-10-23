// Store KidLisp, 2025.01.16
// Caches KidLisp source code and generates short URLs for QR codes

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import crypto from 'crypto';

// Feature flag for Tezos integration (disabled by default until integration file exists)
const TEZOS_ENABLED = process.env.TEZOS_ENABLED === 'true';

// Dynamically extract KidLisp function names from kidlisp.mjs
async function getKidLispFunctionNames() {
  try {
    // Import the kidlisp module
    const kidlispModule = await import('../../public/aesthetic.computer/lib/kidlisp.mjs');
    
    // Create a temporary KidLisp instance to access the global environment
    const { KidLisp } = kidlispModule;
    if (!KidLisp) {
      throw new Error('KidLisp class not found in kidlisp.mjs');
    }
    
    const tempLisp = new KidLisp();
    const globalEnv = tempLisp.getGlobalEnv();
    
    // Extract all function names from the global environment
    const functionNames = Object.keys(globalEnv).filter(key => {
      const value = globalEnv[key];
      return typeof value === 'function' || 
             (typeof value === 'object' && value !== null);
    });
    
    // Add common colors that might appear in KidLisp code
    const commonColors = ['red', 'green', 'blue', 'yellow', 'white', 'black', 'gray', 'purple', 'orange', 'pink', 'brown', 'cyan', 'magenta'];
    
    return [...new Set([...functionNames, ...commonColors])];
    
  } catch (error) {
    console.warn('Failed to dynamically load KidLisp functions, using fallback list:', error.message);
    
    // Fallback to a basic list if dynamic loading fails
    return ['wipe', 'ink', 'line', 'box', 'circle', 'rect', 'def', 'later', 'scroll', 'resolution', 'gap', 'frame', 'brush', 'clear', 'cls', 'help', 'reset', 'dot', 'pixel', 'stamp', 'paste', 'copy', 'move', 'rotate', 'scale', 'translate', 'fill', 'stroke', 'point', 'arc', 'bezier', 'noise', 'random', 'sin', 'cos', 'tan', 'sqrt', 'abs', 'floor', 'ceil', 'round', 'min', 'max', 'pow', 'log', 'exp', 'atan2', 'dist', 'lerp', 'map', 'norm', 'constrain', 'hue', 'sat', 'bright', 'alpha', 'red', 'green', 'blue', 'rgb', 'hsb', 'gray', 'background', 'foreground', 'text', 'font', 'repeat', 'rep', 'choose', 'overtone', 'rainbow', 'mic', 'amplitude'];
  }
}

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

  // Log environment variables (without sensitive data)
  console.log(`🔧 Environment check:`, {
    mongoDbConfigured: !!process.env.MONGODB_CONNECTION_STRING,
    mongoDbNameConfigured: !!process.env.MONGODB_NAME,
    tezosEnabled: process.env.TEZOS_ENABLED !== 'false'
  });

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

      // Extract user from authorization (optional, with timeout)
      let user;
      try {
        // Add timeout to prevent Auth0 issues from slowing down caching
        const authPromise = authorize(event.headers);
        const timeoutPromise = new Promise((_, reject) => 
          setTimeout(() => reject(new Error('Auth timeout')), 3000)
        );
        
        user = await Promise.race([authPromise, timeoutPromise]);
        console.log(`👤 User authorized: ${user ? user.sub : 'none'}`);
      } catch (error) {
        console.log(`🔓 No user authorization (anonymous cache): ${error.message}`);
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

      // Generate unique code using shared module with intelligent inference
      const code = await generateUniqueCode(collection, {
        mode: 'inferred',
        sourceText: source,
        type: 'kidlisp'
      });
      console.log(`✨ Generated code: ${code}`);

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

        // 🪙 Tezos Integration: Attempt to mint KidLisp meme coin
        let tezosResult = null;
        if (TEZOS_ENABLED && user?.sub) {
          try {
            // Import Tezos integration (only when needed)
            const { integrateWithKidLispCache } = await import('../../../tezos/src/integration.js');
            
            console.log(`🪙 Attempting Tezos token mint for user: ${user.sub}`);
            tezosResult = await integrateWithKidLispCache(source.trim(), user, code);
            
            if (tezosResult.minted) {
              console.log(`✨ Tezos token minted successfully: Token ID ${tezosResult.tokenId} on ${tezosResult.network}`);
              
              // Update the document with Tezos information
              await collection.updateOne(
                { code },
                {
                  $set: {
                    tezos: {
                      minted: true,
                      tokenId: tezosResult.tokenId,
                      txHash: tezosResult.txHash,
                      creatorAddress: tezosResult.creatorAddress,
                      codeHash: tezosResult.codeHash,
                      network: tezosResult.network,
                      mintedAt: new Date()
                    }
                  }
                }
              );
            } else if (tezosResult.exists) {
              console.log(`🎯 Tezos token already exists: Token ID ${tezosResult.tokenId} on ${tezosResult.network}`);
              
              // Update with existing token info
              await collection.updateOne(
                { code },
                {
                  $set: {
                    tezos: {
                      minted: false,
                      exists: true,
                      tokenId: tezosResult.tokenId,
                      codeHash: tezosResult.codeHash,
                      network: tezosResult.network,
                      reason: tezosResult.reason,
                      checkedAt: new Date()
                    }
                  }
                }
              );
            } else {
              console.log(`⚠️ Tezos token operation skipped: ${tezosResult.reason}`);
              
              // Store the reason for debugging
              await collection.updateOne(
                { code },
                {
                  $set: {
                    tezos: {
                      minted: false,
                      exists: false,
                      reason: tezosResult.reason,
                      error: tezosResult.error,
                      attemptedAt: new Date()
                    }
                  }
                }
              );
            }
          } catch (tezosError) {
            console.error('🚨 Tezos integration error:', tezosError);
            
            // Store error information for debugging
            await collection.updateOne(
              { code },
              {
                $set: {
                  tezos: {
                    minted: false,
                    error: tezosError.message,
                    failedAt: new Date()
                  }
                }
              }
            );
          }
        } else if (!TEZOS_ENABLED) {
          console.log(`🚫 Tezos integration disabled by feature flag`);
        }

        await database.disconnect();
        
        // Include Tezos information in response
        const responseData = { 
          code, 
          cached: false,
          ...(tezosResult && { tezos: tezosResult })
        };
        
        return respond(201, responseData);

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
      const codes = event.queryStringParameters?.codes;
      const recent = event.queryStringParameters?.recent;
      
      // Handle recent codes feed (for $.mjs piece)
      if (recent) {
        const limit = parseInt(event.queryStringParameters?.limit) || 50;
        const maxLimit = 100000; // Very high limit for comprehensive searches
        const actualLimit = Math.min(limit, maxLimit);
        
        console.log(`📊 Recent codes request: limit=${actualLimit}`);
        
        // Aggregate pipeline to join with handles collection (like moods.mjs)
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
            $sort: { when: -1 }
          },
          {
            $limit: actualLimit
          },
          {
            $project: {
              _id: 0,
              code: 1,
              source: 1,
              when: 1,
              hits: 1,
              user: 1,
              handle: { 
                $cond: {
                  if: { $gt: [{ $size: "$handleInfo" }, 0] },
                  then: { $concat: ["@", { $arrayElemAt: ["$handleInfo.handle", 0] }] },
                  else: null
                }
              }
            }
          }
        ];
        
        const docs = await collection.aggregate(pipeline).toArray();
        
        // Create preview versions of source code (truncate long sources)
        const recentCodes = docs.map(doc => ({
          code: doc.code,
          source: doc.source,
          preview: doc.source.length > 40 ? doc.source.substring(0, 37) + "..." : doc.source,
          when: doc.when,
          hits: doc.hits,
          user: doc.user || null,
          handle: doc.handle || null
        }));
        
        console.log(`📤 Recent codes retrieved: ${recentCodes.length} codes`);
        
        await database.disconnect();
        return respond(200, {
          recent: recentCodes,
          count: recentCodes.length,
          limit: actualLimit
        });
      }
      
      // Handle batch retrieval of multiple codes
      if (codes) {
        console.log(`🔍 Batch lookup request for codes parameter: ${codes}`);
        
        let codeList;
        try {
          // Support both comma-separated string and JSON array format
          if (codes.startsWith('[')) {
            codeList = JSON.parse(codes);
          } else {
            codeList = codes.split(',').map(c => c.trim()).filter(c => c.length > 0);
          }
        } catch (error) {
          return respond(400, { error: 'Invalid codes format. Use comma-separated or JSON array.' });
        }
        
        if (!Array.isArray(codeList) || codeList.length === 0) {
          return respond(400, { error: 'Codes must be a non-empty array' });
        }
        
        if (codeList.length > 50) { // Limit batch size
          return respond(400, { error: 'Too many codes. Maximum 50 per request.' });
        }
        
        console.log(`🔍 Looking up ${codeList.length} codes: ${codeList.join(', ')}`);

        // Fetch all documents in a single database query
        const docs = await collection.find({ code: { $in: codeList } }).toArray();
        
        // Update hit counts for found documents
        if (docs.length > 0) {
          const foundCodes = docs.map(doc => doc.code);
          await collection.updateMany(
            { code: { $in: foundCodes } }, 
            { 
              $inc: { hits: 1 },
              $set: { lastAccessed: new Date() }
            }
          );
        }

        // Create response map with found and missing codes
        const results = {};
        const found = [];
        const missing = [];
        
        codeList.forEach(requestedCode => {
          const doc = docs.find(d => d.code === requestedCode);
          if (doc) {
            results[requestedCode] = {
              source: doc.source,
              when: doc.when,
              hits: doc.hits + 1,
              user: doc.user || null
            };
            found.push(requestedCode);
          } else {
            results[requestedCode] = null;
            missing.push(requestedCode);
          }
        });

        console.log(`📤 Batch retrieved: ${found.length} found, ${missing.length} missing`);
        if (found.length > 0) console.log(`✅ Found: ${found.join(', ')}`);
        if (missing.length > 0) console.log(`❌ Missing: ${missing.join(', ')}`);

        await database.disconnect();
        return respond(200, { 
          results,
          summary: {
            requested: codeList.length,
            found: found.length,
            missing: missing.length,
            foundCodes: found,
            missingCodes: missing
          }
        });
      }
      
      // Handle single code retrieval (existing functionality)
      if (!code) {
        return respond(400, { error: 'Code or codes parameter required' });
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
    console.error('❌ Error details:', {
      message: error.message,
      stack: error.stack,
      name: error.name
    });
    return respond(500, { error: 'Internal server error', details: error.message });
  }
}
