// Store KidLisp, 2025.01.16
// Caches KidLisp source code and generates short URLs for QR codes

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import crypto from 'crypto';

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

      // Generate unique nanoid with smart inference from KidLisp source
      const { customAlphabet } = await import("nanoid");
      let code;
      let attempts = 0;
      let currentLength = 3; // Start with 3 characters (1-2 reserved for system)
      let useFullAlphabet = false; // Start with lowercase preference
      const maxAttemptsPerAlphabet = 15;
      const maxAttemptsPerLength = 30;
      const maxLength = 12;
      
      // Prefer lowercase + numbers first (more readable/typable)
      const lowercaseAlphabet = '0123456789abcdefghijklmnopqrstuvwxyz';
      const fullAlphabet = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
      
      // 🧠 Smart inference: Try to generate meaningful codes from source content
      function generateInferredCodes(source) {
        const codes = [];
        const cleanSource = source.trim().toLowerCase();
        
        // Extract KidLisp function names and create abbreviations
        const kidlispFunctions = ['wipe', 'ink', 'line', 'box', 'circle', 'rect', 'def', 'later', 'scroll', 'resolution', 'gap', 'frame', 'brush', 'clear', 'cls', 'help', 'reset', 'dot', 'pixel', 'stamp', 'paste', 'copy', 'move', 'rotate', 'scale', 'translate', 'fill', 'stroke', 'point', 'arc', 'bezier', 'noise', 'random', 'sin', 'cos', 'tan', 'sqrt', 'abs', 'floor', 'ceil', 'round', 'min', 'max', 'pow', 'log', 'exp', 'atan2', 'dist', 'lerp', 'map', 'norm', 'constrain', 'hue', 'sat', 'bright', 'alpha', 'red', 'green', 'blue', 'rgb', 'hsb', 'gray', 'background', 'foreground', 'text', 'font', 'repeat', 'rep', 'choose', 'overtone', 'rainbow', 'mic', 'amplitude'];
        
        // Find functions used in source and create smart abbreviations
        const foundFunctions = kidlispFunctions.filter(fn => 
          cleanSource.includes(`(${fn}`) || cleanSource.includes(` ${fn} `) || cleanSource.startsWith(fn)
        );
        
        if (foundFunctions.length > 0) {
          const primaryFunction = foundFunctions[0];
          
          // Strategy 1: Try smaller codes first (1-2 chars) for super short URLs, then pad to 3+ chars
          // Single letter codes for primary functions - pad to 3 chars
          const singleChar = primaryFunction.charAt(0);
          codes.push(singleChar + singleChar + '1'); // e.g., 'l' -> 'll1'
          codes.push(singleChar + '11'); // e.g., 'l' -> 'l11'
          codes.push(singleChar + primaryFunction.charAt(1) + '1'); // e.g., 'line' -> 'li1'
          
          // Two-letter combinations with numbers for common patterns - ensure 3+ chars
          if (cleanSource.includes('red')) codes.push(primaryFunction.charAt(0) + '1r'); // e.g., 'l1r'
          if (cleanSource.includes('blue')) codes.push(primaryFunction.charAt(0) + '2b'); // e.g., 'l2b'
          if (cleanSource.includes('green')) codes.push(primaryFunction.charAt(0) + '3g'); // e.g., 'l3g'
          if (cleanSource.includes('yellow')) codes.push(primaryFunction.charAt(0) + '4y'); // e.g., 'l4y'
          if (cleanSource.includes('white')) codes.push(primaryFunction.charAt(0) + '5w'); // e.g., 'l5w'
          if (cleanSource.includes('black')) codes.push(primaryFunction.charAt(0) + '6k'); // e.g., 'l6k'
          if (cleanSource.includes('purple')) codes.push(primaryFunction.charAt(0) + '7p'); // e.g., 'l7p'
          if (cleanSource.includes('orange')) codes.push(primaryFunction.charAt(0) + '8o'); // e.g., 'l8o'
          if (cleanSource.includes('gray')) codes.push(primaryFunction.charAt(0) + '9g'); // e.g., 'l9g'
          
          // Strategy 2: 1337 speak transformations for shorter, memorable codes (already 3+ chars mostly)
          const leetMap = {
            'line': 'l1n3', 'ink': '1nk3', 'box': 'b0x3', 'circle': 'c1rc', 'red': 'r3d1',
            'blue': 'blu3', 'green': 'gr33n', 'white': 'wh1t3', 'black': 'bl4ck',
            'repeat': 'r3p3', 'random': 'r4nd', 'noise': 'n01s3', 'rainbow': 'r41nb0'
          };
          
          foundFunctions.forEach(fn => {
            if (leetMap[fn]) {
              const leetCode = leetMap[fn];
              if (leetCode.length >= 3) codes.push(leetCode);
              // If leet code is too short, pad it
              else if (leetCode.length === 2) codes.push(leetCode + '1');
              else if (leetCode.length === 1) codes.push(leetCode + '11');
            }
          });
          
          // Strategy 3: First letters of each function used (e.g., "line ink red" -> "lir")
          if (foundFunctions.length >= 2 && foundFunctions.length <= 4) {
            const firstLetters = foundFunctions.map(fn => fn.charAt(0)).join('');
            if (firstLetters.length >= 3 && firstLetters.length <= 4) {
              codes.push(firstLetters);
            } else if (firstLetters.length === 2) {
              // Pad 2-char combinations to 3 chars
              codes.push(firstLetters + '1');
              codes.push(firstLetters + primaryFunction.charAt(1)); // e.g., 'li' -> 'lin'
            } else if (firstLetters.length === 1) {
              // Pad single chars to 3 chars
              codes.push(firstLetters + '11');
              codes.push(firstLetters + firstLetters + '1');
            }
          }
          
          // Strategy 4: Primary function + descriptive suffix (traditional approach) - ensure 3+ chars
          if (cleanSource.includes('red')) codes.push(primaryFunction.charAt(0) + 'r3d');
          if (cleanSource.includes('blue')) codes.push(primaryFunction.charAt(0) + 'blu');
          if (cleanSource.includes('green')) codes.push(primaryFunction.charAt(0) + 'grn');
          if (cleanSource.includes('yellow')) codes.push(primaryFunction.charAt(0) + 'ylw');
          if (cleanSource.includes('white')) codes.push(primaryFunction.charAt(0) + 'wht');
          if (cleanSource.includes('black')) codes.push(primaryFunction.charAt(0) + 'blk');
          
          // Strategy 5: Function abbreviations (e.g., "line" -> "ln", "circle" -> "cir") - ensure 3+ chars
          const abbreviations = {
            'line': 'ln3', 'circle': 'cir', 'rectangle': 'rect', 'repeat': 'rep3',
            'background': 'bg3', 'foreground': 'fg3', 'resolution': 'res3',
            'random': 'rnd', 'rainbow': 'rnb', 'amplitude': 'amp3'
          };
          
          foundFunctions.forEach(fn => {
            if (abbreviations[fn]) {
              const abbrev = abbreviations[fn];
              if (abbrev.length >= 3) {
                codes.push(abbrev);
              } else {
                // Pad short abbreviations
                codes.push(abbrev + '1');
              }
            }
          });
        }
        
        // Strategy 6: Extract meaningful sequences from simple expressions - ensure 3+ chars
        // Look for patterns like "line; ink red" -> "lir"
        const words = cleanSource.match(/[a-z]+/g) || [];
        if (words.length === 2 || words.length === 3) {
          const wordAbbrev = words.map(w => w.charAt(0)).join('');
          if (wordAbbrev.length >= 3 && wordAbbrev.length <= 4) {
            codes.push(wordAbbrev);
          } else if (wordAbbrev.length === 2) {
            // Pad to 3 chars
            codes.push(wordAbbrev + '1');
            codes.push(wordAbbrev + words[0].charAt(1)); // e.g., 'li' -> 'lin'
          } else if (wordAbbrev.length === 1) {
            // Pad to 3 chars
            codes.push(wordAbbrev + '11');
          }
        }
        
        // Strategy 7: For very short expressions, use direct mapping - ensure 3+ chars
        if (cleanSource.length <= 20) {
          // Simple color mappings with numbers - always 3+ chars
          const colorMap = {
            'red': 'red', 'blue': 'blu', 'green': 'grn', 'yellow': 'ylw',
            'white': 'wht', 'black': 'blk', 'gray': 'gry', 'purple': 'pur'
          };
          
          let simpleCode = '';
          Object.entries(colorMap).forEach(([color, abbrev]) => {
            if (cleanSource.includes(color) && simpleCode.length === 0) {
              simpleCode = foundFunctions[0]?.charAt(0) + abbrev; // e.g., 'lred', 'lblu'
            }
          });
          
          if (simpleCode.length >= 3) {
            codes.push(simpleCode);
          }
        }
        
        // Strategy 8: Super compact codes with numbers for ultra-short URLs - ensure 3+ chars
        if (foundFunctions.length > 0) {
          // Generate 3-char codes: letter + number + letter based on function count and colors
          const fnChar = foundFunctions[0].charAt(0);
          const colorCount = ['red', 'blue', 'green', 'yellow', 'white', 'black'].filter(c => cleanSource.includes(c)).length;
          if (colorCount > 0) {
            codes.push(fnChar + colorCount + 'c'); // e.g., 'l2c' for line with 2 colors
          }
          
          // Generate meaningful 3-char codes for common combinations
          if (cleanSource.includes('ink') && cleanSource.includes('red')) codes.push('ir1');
          if (cleanSource.includes('ink') && cleanSource.includes('blue')) codes.push('ib1');
          if (cleanSource.includes('line') && cleanSource.includes('red')) codes.push('lr1');
          if (cleanSource.includes('box') && cleanSource.includes('red')) codes.push('br1');
        }
        
        // Ensure all generated codes are valid, prioritizing shorter codes but minimum 3 chars
        const validCodes = codes.filter(code => 
          /^[a-z0-9]+$/.test(code) && 
          code.length >= 3 && 
          code.length <= 12 && // Allow growth up to maxLength for uniqueness
          /[a-z]/.test(code) // Must contain at least one letter
        );
        
        // Sort by length (shortest first) for better user experience, but all will be 3+ chars
        return [...new Set(validCodes)].sort((a, b) => a.length - b.length);
      }
      
      // Try inferred codes first, then fall back to random generation
      const inferredCodes = generateInferredCodes(source);
      console.log(`🧠 Generated ${inferredCodes.length} inferred codes: [${inferredCodes.join(', ')}]`);
      
      // First, try all inferred codes
      for (const inferredCode of inferredCodes) {
        if (!(await collection.findOne({ code: inferredCode }))) {
          code = inferredCode;
          console.log(`✨ Using inferred code: ${code}`);
          break;
        }
      }
      
      // If no inferred code worked, fall back to random generation
      if (!code) {
        console.log(`🎲 Falling back to random generation`);
        
        do {
          // Switch to full alphabet if we've tried enough lowercase attempts
          if (!useFullAlphabet && attempts >= maxAttemptsPerAlphabet) {
            useFullAlphabet = true;
            console.log(`🔄 Switching to full alphabet for length ${currentLength}`);
          }
          
          // Grow length and reset if we've exhausted both alphabets
          if (attempts >= maxAttemptsPerLength && currentLength < maxLength) {
            currentLength++; // Grow by 1 character
            attempts = 0; // Reset attempts for new length
            useFullAlphabet = false; // Start with lowercase preference again
            console.log(`🔄 Growing nanoid length to ${currentLength} characters`);
          }
          
          const alphabet = useFullAlphabet ? fullAlphabet : lowercaseAlphabet;
          const generator = customAlphabet(alphabet, currentLength);
          code = generator();
          attempts++;
          
          if (attempts > maxAttemptsPerLength && currentLength >= maxLength) {
            console.error(`💥 Failed to generate unique code after maximum attempts`);
            break;
          }
        } while (await collection.findOne({ code }));
      }

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
