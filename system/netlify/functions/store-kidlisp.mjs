// Store KidLisp, 2025.01.16
// Caches KidLisp source code and generates short URLs for QR codes

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { createMediaRecord, MediaTypes } from "../../backend/media-atproto.mjs";
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
    
    // Create index on kept.network for filtering kept pieces by network
    await collection.createIndex({ "kept.network": 1 }, { 
      background: true,
      sparse: true, // Only index documents that have the kept field
      name: 'kidlisp_kept_network'
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
    return respond(204, '');
  }

  let database;
  try {
    database = await connect();
  } catch (connectError) {
    console.error('❌ MongoDB connection failed:', connectError.message);
    // Return a graceful error response instead of crashing
    return respond(503, { error: 'Database temporarily unavailable' });
  }

  try {
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
        const insertResult = await collection.insertOne(doc);
        console.log(`💾 Cached new source: ${code}`);
        
        // Sync to ATProto in background (don't wait for it)
        const kidlispId = insertResult.insertedId;
        const savedRecord = await collection.findOne({ _id: kidlispId });
        
        if (savedRecord) {
          createMediaRecord(database, MediaTypes.KIDLISP, savedRecord, { 
            userSub: user?.sub 
          })
          .then(result => {
            if (result.error) {
              console.error(`⚠️  ATProto sync failed: ${result.error}`);
            } else {
              console.log(`✅ Synced kidlisp to ATProto: ${result.rkey}`);
              // Update MongoDB with rkey (fire and forget)
              collection.updateOne(
                { _id: kidlispId },
                { $set: { "atproto.rkey": result.rkey } }
              ).catch(err => console.error(`⚠️  Failed to update rkey: ${err.message}`));
            }
          })
          .catch(err => console.error(`⚠️  ATProto sync error: ${err.message}`));
        }

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
      const stats = event.queryStringParameters?.stats;

      // Handle function usage statistics across all pieces
      if (stats === 'functions') {
        console.log(`📊 Function stats request`);

        // Fetch top pieces sorted by hits (covers most real usage)
        const scanLimit = parseInt(event.queryStringParameters?.limit) || 5000;
        const docs = await collection.find(
          {},
          { projection: { source: 1, hits: 1, _id: 0 } }
        ).sort({ hits: -1 }).limit(scanLimit).toArray();

        // Parse function calls from source code
        const rawCounts = {};   // unweighted: each piece counts once
        const weightedCounts = {}; // weighted by piece hits
        let totalHits = 0;

        const funcPattern = /\(\s*([a-zA-Z_+\-*/%?][a-zA-Z0-9_]*)/g;

        // Known bare-word commands (functions that work without parens)
        const bareCommands = new Set([
          'wipe', 'ink', 'line', 'box', 'circle', 'plot', 'point', 'flood',
          'scroll', 'spin', 'zoom', 'blur', 'contrast', 'suck', 'sort',
          'bake', 'fill', 'outline', 'stroke', 'nofill', 'nostroke',
          'resolution', 'mask', 'unmask', 'steal', 'putback',
          'rainbow', 'zebra', 'noise', 'unpan', 'resetSpin',
        ]);

        // Known CSS color names used as bare wipe commands
        const bareColors = new Set([
          'red', 'green', 'blue', 'yellow', 'orange', 'purple', 'pink',
          'cyan', 'magenta', 'black', 'white', 'gray', 'grey', 'brown',
          'lime', 'navy', 'teal', 'olive', 'maroon', 'aqua', 'fuchsia',
          'silver', 'gold', 'coral', 'salmon', 'khaki', 'indigo', 'violet',
          'turquoise', 'tomato', 'crimson', 'lavender', 'beige', 'plum',
          'orchid', 'tan', 'chocolate', 'sienna', 'peru', 'wheat',
          'deepskyblue', 'hotpink', 'springgreen', 'darkslategray',
        ]);

        for (const doc of docs) {
          const src = doc.source || '';
          const hits = doc.hits || 1;
          totalHits += hits;
          const seenInPiece = new Set(); // track unique functions per piece

          // Extract parenthesized function calls
          let match;
          funcPattern.lastIndex = 0;
          while ((match = funcPattern.exec(src)) !== null) {
            const fn = match[1];
            seenInPiece.add(fn);
          }

          // Detect bare-word commands (words at start of line or after comma)
          const tokens = src.split(/[,\n]/).map(t => t.trim().split(/\s+/)[0]);
          for (const token of tokens) {
            if (bareCommands.has(token)) seenInPiece.add(token);
            if (bareColors.has(token)) seenInPiece.add('wipe'); // bare color = implicit wipe
          }

          // Detect embedded piece references ($codeId)
          if (/\$[a-zA-Z0-9]+/.test(src)) seenInPiece.add('embed');

          // Detect timing expressions
          if (/\d+\.?\d*s[.!]?/.test(src)) seenInPiece.add('timing');

          // Detect fade gradient syntax
          if (/fade:/.test(src)) seenInPiece.add('fade');

          // Aggregate
          for (const fn of seenInPiece) {
            rawCounts[fn] = (rawCounts[fn] || 0) + 1;
            weightedCounts[fn] = (weightedCounts[fn] || 0) + hits;
          }
        }

        // Sort by weighted count descending
        const sorted = Object.entries(weightedCounts)
          .sort((a, b) => b[1] - a[1])
          .map(([name, weighted]) => ({
            name,
            pieces: rawCounts[name] || 0,
            weighted,
          }));

        await database.disconnect();
        return respond(200, {
          functions: sorted,
          total_pieces: docs.length,
          total_hits: totalHits,
        });
      }

      // Handle recent codes feed (for $.mjs piece)
      if (recent) {
        const limit = parseInt(event.queryStringParameters?.limit) || 50;
        const maxLimit = 100000; // Very high limit for comprehensive searches
        const actualLimit = Math.min(limit, maxLimit);
        const sortBy = event.queryStringParameters?.sort || 'recent'; // 'recent' or 'hits'
        const filterHandle = event.queryStringParameters?.handle; // Optional handle filter
        
        console.log(`📊 Codes request: limit=${actualLimit}, sort=${sortBy}, handle=${filterHandle || 'all'}`);
        
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
          // Add computed handle field for filtering
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
        ];
        
        // Add handle filter if specified
        if (filterHandle) {
          pipeline.push({
            $match: { handle: filterHandle.startsWith('@') ? filterHandle : `@${filterHandle}` }
          });
        }
        
        // Sort by hits or recent
        pipeline.push({
          $sort: sortBy === 'hits' ? { hits: -1, when: -1 } : { when: -1 }
        });
        
        pipeline.push({
          $limit: actualLimit
        });
        
        pipeline.push({
          $project: {
            _id: 0,
            code: 1,
            source: 1,
            when: 1,
            hits: 1,
            user: 1,
            kept: 1,    // Include kept status
            tezos: 1,   // Include legacy tezos field
            handle: 1   // Already computed
          }
        });
        
        const docs = await collection.aggregate(pipeline).toArray();
        
        // Create preview versions of source code (truncate long sources)
        const recentCodes = docs.map(doc => {
          const result = {
            code: doc.code,
            source: doc.source,
            preview: doc.source.length > 40 ? doc.source.substring(0, 37) + "..." : doc.source,
            when: doc.when,
            hits: doc.hits,
            user: doc.user || null,
            handle: doc.handle || null
          };
          
          // Include kept status if the piece was minted as a KEEP NFT
          if (doc.kept) {
            result.kept = {
              tokenId: doc.kept.tokenId,
              network: doc.kept.network || "mainnet",
              contractAddress: doc.kept.contractAddress || null,
              keptBy: doc.kept.keptBy || null,
              walletAddress: doc.kept.walletAddress || null,
            };
          }
          
          // Also check legacy tezos field (from server-side mints)
          if (doc.tezos?.minted) {
            result.kept = result.kept || {};
            result.kept.tokenId = result.kept.tokenId || doc.tezos.tokenId;
            result.kept.network = result.kept.network || doc.tezos.network || "mainnet";
            result.kept.contractAddress = result.kept.contractAddress || doc.tezos.contractAddress || null;
          }
          
          return result;
        });
        
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

        // Fetch all documents with handle lookup (aggregation pipeline)
        const docs = await collection.aggregate([
          { $match: { code: { $in: codeList } } },
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
          }
        ]).toArray();
        
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
            const result = {
              source: doc.source,
              when: doc.when,
              hits: doc.hits + 1,
              user: doc.user || null,
              handle: doc.handle || null
            };
            
            // Include kept status if the piece was minted as a KEEP NFT
            if (doc.kept) {
              result.kept = {
                tokenId: doc.kept.tokenId,
                network: doc.kept.network || "mainnet",
                txHash: doc.kept.txHash,
                keptAt: doc.kept.keptAt,
                keptBy: doc.kept.keptBy || null,
                walletAddress: doc.kept.walletAddress || null,
              };
            }
            
            // Also check legacy tezos field (from server-side mints)
            if (doc.tezos?.minted) {
              result.kept = result.kept || {};
              result.kept.tokenId = result.kept.tokenId || doc.tezos.tokenId;
              result.kept.network = result.kept.network || doc.tezos.network || "mainnet";
              result.kept.txHash = result.kept.txHash || doc.tezos.txHash;
              result.kept.keptAt = result.kept.keptAt || doc.tezos.mintedAt;
            }
            
            results[requestedCode] = result;
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

      // Use aggregation pipeline to fetch doc with handle lookup
      const pipeline = [
        { $match: { code } },
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
        { $limit: 1 }
      ];
      
      const docs = await collection.aggregate(pipeline).toArray();
      const doc = docs[0];
      
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
      
      // Build response with kept status if present
      const response = { 
        source: doc.source,
        when: doc.when,
        hits: doc.hits + 1,
        user: doc.user || null,
        handle: doc.handle || null,
      };
      
      // Include cached IPFS media if present (from bundle generation)
      if (doc.ipfsMedia) {
        response.ipfsMedia = {
          artifactUri: doc.ipfsMedia.artifactUri,
          thumbnailUri: doc.ipfsMedia.thumbnailUri,
          sourceHash: doc.ipfsMedia.sourceHash,
          createdAt: doc.ipfsMedia.createdAt,
          authorHandle: doc.ipfsMedia.authorHandle,
          depCount: doc.ipfsMedia.depCount,
          packDate: doc.ipfsMedia.packDate,
        };
      }
      
      // Include kept status if the piece was minted as a KEEP NFT
      if (doc.kept) {
        response.kept = {
          tokenId: doc.kept.tokenId,
          network: doc.kept.network || "mainnet",
          txHash: doc.kept.txHash,
          contractAddress: doc.kept.contractAddress,
          keptAt: doc.kept.keptAt,
          keptBy: doc.kept.keptBy || null,
          walletAddress: doc.kept.walletAddress || null,
        };
      }
      
      // Also check legacy tezos field (from server-side mints)
      if (doc.tezos?.minted) {
        response.kept = response.kept || {};
        response.kept.tokenId = response.kept.tokenId || doc.tezos.tokenId;
        response.kept.network = response.kept.network || doc.tezos.network || "mainnet";
        response.kept.txHash = response.kept.txHash || doc.tezos.txHash;
        response.kept.contractAddress = response.kept.contractAddress || doc.tezos.contract;
        response.kept.keptAt = response.kept.keptAt || doc.tezos.mintedAt;
        // Include on-chain artifact URIs
        response.kept.artifactUri = doc.tezos.artifactUri;
        response.kept.thumbnailUri = doc.tezos.thumbnailUri;
      }
      
      // Include pending rebake info if present (rebaked but not yet updated on chain)
      if (doc.pendingRebake) {
        response.pendingRebake = {
          artifactUri: doc.pendingRebake.artifactUri,
          thumbnailUri: doc.pendingRebake.thumbnailUri,
          createdAt: doc.pendingRebake.createdAt,
        };
      }
      
      return respond(200, response);
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
