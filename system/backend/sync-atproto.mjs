#!/usr/bin/env node
// sync-atproto.mjs
// Master ATProto synchronization script
// Handles all content types: paintings, moods, pieces, kidlisp
// 
// Features:
// 1. Cleans up duplicate ATProto records (keeps earliest by ref)
// 2. Syncs MongoDB → ATProto for missing records
// 3. Never creates duplicates
// 4. Can be run multiple times safely
// 5. Handles anonymous content → art account
// 6. Handles user content → personal accounts

import { connect } from "./database.mjs";
import { AtpAgent } from "@atproto/api";
import { 
  MediaTypes, 
  createMediaRecord, 
  deleteMediaRecord,
  getCollection,
  supportsAnonymous 
} from "./media-atproto.mjs";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

// Collection configurations - now using unified media-atproto module
const COLLECTIONS = {
  paintings: {
    mediaType: MediaTypes.PAINTING,
    mongoCollection: "paintings",
    atprotoCollection: getCollection(MediaTypes.PAINTING),
    hasAnonymous: supportsAnonymous(MediaTypes.PAINTING)
  },
  moods: {
    mediaType: MediaTypes.MOOD,
    mongoCollection: "moods",
    atprotoCollection: getCollection(MediaTypes.MOOD),
    hasAnonymous: supportsAnonymous(MediaTypes.MOOD)
  },
  pieces: {
    mediaType: MediaTypes.PIECE,
    mongoCollection: "pieces",
    atprotoCollection: getCollection(MediaTypes.PIECE),
    hasAnonymous: supportsAnonymous(MediaTypes.PIECE)
  },
  kidlisp: {
    mediaType: MediaTypes.KIDLISP,
    mongoCollection: "kidlisp",
    atprotoCollection: getCollection(MediaTypes.KIDLISP),
    hasAnonymous: supportsAnonymous(MediaTypes.KIDLISP)
  },
  tapes: {
    mediaType: MediaTypes.TAPE,
    mongoCollection: "tapes",
    atprotoCollection: getCollection(MediaTypes.TAPE),
    hasAnonymous: supportsAnonymous(MediaTypes.TAPE)
  }
};

/**
 * Wipe all ATProto data for a specific user (including anonymous/art-guest)
 * - Deletes all ATProto records
 * - Clears atproto.rkey fields from MongoDB
 * - Supports art.at.aesthetic.computer for anonymous content
 */
async function wipeUserAtprotoData(userHandle, options = {}) {
  const { dryRun = true, contentTypes = ['paintings', 'moods', 'pieces', 'kidlisp', 'tapes'] } = options;
  
  console.log(`\n🗑️  WIPE MODE: ${userHandle}`);
  console.log(`   Mode: ${dryRun ? '🔍 DRY RUN (no changes)' : '⚡ LIVE (deleting)'}`);
  console.log(`   Content types: ${contentTypes.join(', ')}\n`);
  
  const database = await connect();
  const users = database.db.collection("users");
  
  // Find user by handle
  const user = await users.findOne({ "atproto.handle": userHandle });
  
  if (!user) {
    console.error(`❌ User not found: ${userHandle}`);
    await database.disconnect();
    return;
  }
  
  if (!user.atproto?.did || !user.atproto?.password) {
    console.error(`❌ User has no ATProto account: ${userHandle}`);
    await database.disconnect();
    return;
  }
  
  console.log(`📋 Found user: ${user.atproto.handle} (${user.atproto.did})`);
  
  // Login to ATProto
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({
    identifier: user.atproto.did,
    password: user.atproto.password
  });
  
  const stats = {
    deleted: 0,
    mongoCleared: 0,
    errors: []
  };
  
  // Process each requested content type
  for (const [contentType, config] of Object.entries(COLLECTIONS)) {
    if (!contentTypes.includes(contentType)) {
      continue; // Skip this type
    }
    
    console.log(`\n   Processing ${contentType}...`);
    
    // 1. Fetch all ATProto records for this collection
    let atprotoRecords = [];
    let cursor = undefined;
    
    while (true) {
      const response = await agent.com.atproto.repo.listRecords({
        repo: user.atproto.did,
        collection: config.atprotoCollection,
        limit: 100,
        cursor
      });
      
      atprotoRecords.push(...response.data.records);
      if (!response.data.cursor) break;
      cursor = response.data.cursor;
    }
    
    console.log(`      Found ${atprotoRecords.length} ATProto records`);
    
    // 2. Delete all ATProto records
    if (atprotoRecords.length > 0) {
      for (const record of atprotoRecords) {
        const rkey = record.uri.split("/").pop();
        
        if (!dryRun) {
          try {
            await agent.com.atproto.repo.deleteRecord({
              repo: user.atproto.did,
              collection: config.atprotoCollection,
              rkey
            });
            stats.deleted++;
          } catch (error) {
            console.log(`      ❌ Failed to delete ${rkey}: ${error.message}`);
            stats.errors.push(`Failed to delete ${contentType}/${rkey}: ${error.message}`);
          }
        } else {
          stats.deleted++;
        }
      }
      console.log(`      ${dryRun ? 'Would delete' : 'Deleted'} ${atprotoRecords.length} records from ATProto`);
    }
    
    // 3. Clear atproto.rkey fields from MongoDB
    const collection = database.db.collection(config.mongoCollection);
    // For anonymous content (art-guest), match records WITHOUT user field
    // For user content, match specific user._id
    const isAnonymous = user._id === "art-guest";
    const mongoQuery = isAnonymous 
      ? { user: { $exists: false }, "atproto.rkey": { $exists: true } }
      : { user: user._id, "atproto.rkey": { $exists: true } };
    const mongoCount = await collection.countDocuments(mongoQuery);
    
    if (mongoCount > 0) {
      if (!dryRun) {
        const result = await collection.updateMany(
          mongoQuery,
          { $unset: { "atproto.rkey": "" } }
        );
        stats.mongoCleared += result.modifiedCount;
      } else {
        stats.mongoCleared += mongoCount;
      }
      console.log(`      ${dryRun ? 'Would clear' : 'Cleared'} atproto.rkey from ${mongoCount} MongoDB records`);
    }
  }
  
  // Summary
  console.log(`\n✨ WIPE SUMMARY:`);
  console.log(`   ATProto records deleted: ${stats.deleted}`);
  console.log(`   MongoDB rkeys cleared: ${stats.mongoCleared}`);
  
  if (stats.errors.length > 0) {
    console.log(`   Errors: ${stats.errors.length}`);
    stats.errors.forEach(err => console.log(`      ${err}`));
  }
  
  if (dryRun) {
    console.log(`\n💡 Run with 'live' argument to apply changes`);
  }
  
  await database.disconnect();
}

/**
 * Restore ATProto data for a specific user from MongoDB (including anonymous/art-guest)
 * - Creates ATProto records for all MongoDB items that don't have them
 * - Updates MongoDB with rkeys
 * - Supports art.at.aesthetic.computer for anonymous content
 */
async function restoreUserAtprotoData(userHandle, options = {}) {
  const { dryRun = true, contentTypes = ['paintings', 'moods', 'pieces', 'kidlisp', 'tapes'] } = options;
  
  console.log(`\n♻️  RESTORE MODE: ${userHandle}`);
  console.log(`   Mode: ${dryRun ? '🔍 DRY RUN (no changes)' : '⚡ LIVE (creating)'}`);
  console.log(`   Content types: ${contentTypes.join(', ')}\n`);
  
  const database = await connect();
  const users = database.db.collection("users");
  
  // Find user by handle
  const user = await users.findOne({ "atproto.handle": userHandle });
  
  if (!user) {
    console.error(`❌ User not found: ${userHandle}`);
    await database.disconnect();
    return;
  }
  
  if (!user.atproto?.did || !user.atproto?.password) {
    console.error(`❌ User has no ATProto account: ${userHandle}`);
    await database.disconnect();
    return;
  }
  
  console.log(`📋 Found user: ${user.atproto.handle} (${user.atproto.did})`);
  
  const allStats = [];
  
  // Process each requested content type
  for (const contentType of contentTypes) {
    if (!COLLECTIONS[contentType]) {
      console.log(`   ⚠️  Unknown content type: ${contentType}, skipping`);
      continue;
    }
    
    console.log(`\n   Syncing ${contentType}...`);
    const stats = await syncContentForUser(contentType, user, { dryRun, verbose: false });
    allStats.push(stats);
    
    console.log(`      MongoDB: ${stats.mongoTotal}, ATProto: ${stats.atprotoTotal}`);
    console.log(`      Duplicates cleaned: ${stats.duplicatesDeleted}`);
    console.log(`      Records ${dryRun ? 'to create' : 'created'}: ${stats.created}`);
    
    if (stats.errors.length > 0) {
      console.log(`      ❌ Errors: ${stats.errors.length}`);
      stats.errors.slice(0, 3).forEach(err => console.log(`         ${err}`));
    }
  }
  
  // Summary
  const totalCreated = allStats.reduce((sum, s) => sum + s.created, 0);
  const totalDuplicates = allStats.reduce((sum, s) => sum + s.duplicatesDeleted, 0);
  const totalErrors = allStats.reduce((sum, s) => sum + s.errors.length, 0);
  
  console.log(`\n✨ RESTORE SUMMARY:`);
  console.log(`   Records ${dryRun ? 'to create' : 'created'}: ${totalCreated}`);
  console.log(`   Duplicates cleaned: ${totalDuplicates}`);
  
  if (totalErrors > 0) {
    console.log(`   Errors: ${totalErrors}`);
  }
  
  if (dryRun) {
    console.log(`\n💡 Run with 'live' argument to apply changes`);
  }
  
  await database.disconnect();
}

/**
 * Sync a specific content type for a user
 */
async function syncContentForUser(contentType, user, options = {}) {
  const { dryRun = true, verbose = false } = options;
  const config = COLLECTIONS[contentType];
  const isAnonymous = user._id === "art-guest";
  
  const stats = {
    contentType,
    handle: user.atproto.handle,
    mongoTotal: 0,
    atprotoTotal: 0,
    duplicatesFound: 0,
    duplicatesDeleted: 0,
    missingInAtproto: 0,
    created: 0,
    errors: []
  };
  
  try {
    const database = await connect();
    const collection = database.db.collection(config.mongoCollection);
    
    // Login to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password
    });
    
    // 1. Fetch all MongoDB items for this user
    // Anonymous content: no user field. User content: match user._id
    const mongoQuery = isAnonymous 
      ? { user: { $exists: false } }
      : { user: user._id };
    
    const mongoItems = await collection.find(mongoQuery).sort({ when: 1 }).toArray();
    stats.mongoTotal = mongoItems.length;
    
    // 2. Fetch all ATProto records
    let atprotoRecords = [];
    let cursor = undefined;
    
    while (true) {
      const response = await agent.com.atproto.repo.listRecords({
        repo: user.atproto.did,
        collection: config.atprotoCollection,
        limit: 100,
        cursor
      });
      
      atprotoRecords.push(...response.data.records);
      if (!response.data.cursor) break;
      cursor = response.data.cursor;
    }
    
    stats.atprotoTotal = atprotoRecords.length;
    
    // 3. Build a map of ATProto records by ref (MongoDB _id)
    const atprotoByRef = new Map();
    const atprotoByRkey = new Map();
    const duplicateRefs = new Set();
    
    for (const record of atprotoRecords) {
      const ref = record.value.ref;
      const rkey = record.uri.split("/").pop();
      
      atprotoByRkey.set(rkey, record);
      
      if (ref) {
        if (atprotoByRef.has(ref)) {
          // Found a duplicate!
          duplicateRefs.add(ref);
          stats.duplicatesFound++;
        } else {
          atprotoByRef.set(ref, record);
        }
      }
    }
    
    // 4. Delete duplicates (keep only the first one for each ref)
    if (duplicateRefs.size > 0) {
      for (const ref of duplicateRefs) {
        // Find all records with this ref
        const dupes = atprotoRecords.filter(r => r.value.ref === ref);
        
        // Sort by creation time (earliest first) and keep the first one
        dupes.sort((a, b) => new Date(a.value.when) - new Date(b.value.when));
        const toDelete = dupes.slice(1); // Delete all but the first
        
        // Delete in batches of 50
        const DELETE_BATCH_SIZE = 50;
        for (let i = 0; i < toDelete.length; i += DELETE_BATCH_SIZE) {
          const batch = toDelete.slice(i, i + DELETE_BATCH_SIZE);
          
          if (!dryRun) {
            await Promise.all(batch.map(async (dupe) => {
              const rkey = dupe.uri.split("/").pop();
              
              try {
                await agent.com.atproto.repo.deleteRecord({
                  repo: user.atproto.did,
                  collection: config.atprotoCollection,
                  rkey
                });
                stats.duplicatesDeleted++;
              } catch (error) {
                stats.errors.push(`${user.atproto?.handle || 'anonymous'}: Failed to delete duplicate ${rkey}: ${error.message}`);
              }
            }));
          } else {
            stats.duplicatesDeleted += batch.length;
          }
        }
      }
    }
    
    // 5. Find MongoDB items that don't exist in ATProto
    const missingInAtproto = [];
    
    for (const item of mongoItems) {
      const refId = item._id.toString();
      
      // Check if this item exists in ATProto by ref
      if (!atprotoByRef.has(refId)) {
        // Check if MongoDB has an atproto.rkey that actually exists
        if (item.atproto?.rkey && atprotoByRkey.has(item.atproto.rkey)) {
          // The rkey exists, record is there but maybe needs ref update
          continue;
        }
        
        missingInAtproto.push(item);
        stats.missingInAtproto++;
      }
    }
    
    // 6. Create missing items in ATProto
    if (missingInAtproto.length > 0) {
      const mongoUpdates = [];
      
      // Process in batches of 10 concurrent creates (reduced to avoid rate limiting)
      const BATCH_SIZE = 10;
      for (let i = 0; i < missingInAtproto.length; i += BATCH_SIZE) {
        const batch = missingInAtproto.slice(i, i + BATCH_SIZE);
        
        if (!dryRun) {
          await Promise.all(batch.map(async (item, idx) => {
            const itemName = item.slug || item.text?.substring(0, 30) || item.code || item._id.toString();
            
            try {
              // Use unified media-atproto module
              const result = await createMediaRecord(
                database,
                config.mediaType,
                item,
                { userSub: isAnonymous ? null : user._id }
              );
              
              if (result.error) {
                throw new Error(result.error);
              }
              
              stats.created++;
              
              // Queue MongoDB update
              if (result.rkey) {
                mongoUpdates.push({
                  updateOne: {
                    filter: { _id: item._id },
                    update: { $set: { "atproto.rkey": result.rkey } }
                  }
                });
              }
              
            } catch (error) {
              stats.errors.push(`${user.atproto?.handle || 'anonymous'}: Failed to create ${itemName}: ${error.message}`);
            }
          }));
        } else {
          stats.created += batch.length;
        }
      }
      
      // Batch update MongoDB
      if (mongoUpdates.length > 0 && !dryRun) {
        try {
          await collection.bulkWrite(mongoUpdates);
        } catch (error) {
          stats.errors.push(`${user.atproto?.handle || 'anonymous'}: Failed to bulk update MongoDB: ${error.message}`);
        }
      }
    }
    
    await database.disconnect();
    
  } catch (error) {
    stats.errors.push(`Fatal error: ${error.message}`);
  }
  
  return stats;
}

/**
 * Sync all content types
 */
async function syncAll(options = {}) {
  const { 
    dryRun = true, 
    verbose = false, 
    userFilter = null,
    contentTypes = ['paintings', 'moods', 'pieces', 'kidlisp']
  } = options;
  
  console.log(`\n🔄 ATPROTO MASTER SYNC SCRIPT`);
  console.log(`   Mode: ${dryRun ? '🔍 DRY RUN (no changes)' : '⚡ LIVE (making changes)'}`);
  console.log(`   Content types: ${contentTypes.join(', ')}`);
  console.log(`   Time: ${new Date().toISOString()}\n`);
  
  const database = await connect();
  const allStats = [];
  
  // STEP 1: Sync anonymous content to art account
  const artUser = await database.db.collection("users").findOne({ _id: "art-guest" });
  
  if (!artUser) {
    console.error(`\n⚠️  Art account (art-guest) not found in MongoDB!`);
    console.error(`   Run: cd at && node scripts/setup-art-account.mjs\n`);
  } else {
    console.log(`\n🎨 Step 1: Syncing anonymous content to ${artUser.atproto.handle}...`);
    
    for (const contentType of contentTypes) {
      const config = COLLECTIONS[contentType];
      
      if (!config.hasAnonymous) {
        if (verbose) console.log(`   Skipping ${contentType} (no anonymous content)`);
        continue;
      }
      
      // Create a pseudo-user object for anonymous content
      const anonymousUser = {
        _id: null, // null user means anonymous
        atproto: artUser.atproto
      };
      
      const stats = await syncContentForUser(contentType, anonymousUser, { dryRun, verbose });
      allStats.push(stats);
      
      console.log(`   ${contentType.padEnd(10)}: ${stats.mongoTotal} in MongoDB, ${stats.atprotoTotal} in ATProto, ${stats.duplicatesDeleted} dupes, ${stats.created} to create`);
      
      if (stats.errors.length > 0) {
        console.log(`   ${''.padEnd(10)}  ⚠️  ${stats.errors.length} errors`);
      }
    }
  }
  
  // STEP 2: Sync user content
  console.log(`\n👥 Step 2: Syncing user content...`);
  
  const query = { "atproto.did": { $exists: true }, _id: { $ne: "art-guest" } };
  if (userFilter) {
    query["atproto.handle"] = new RegExp(userFilter, "i");
  }
  
  const users = await database.db.collection("users").find(query).toArray();
  console.log(`📋 Found ${users.length} users with ATProto accounts\n`);
  
  // Process users one at a time to avoid ATProto PDS rate limiting
  const USER_BATCH_SIZE = 1;
  let completedUsers = 0;
  const startTime = Date.now();
  
  for (let i = 0; i < users.length; i += USER_BATCH_SIZE) {
    const userBatch = users.slice(i, i + USER_BATCH_SIZE);
    const batchStart = Date.now();
    
    // Track batch stats
    let batchCreated = 0;
    let batchDuplicates = 0;
    let batchErrors = 0;
    
    await Promise.all(userBatch.map(async (user) => {
      const userStats = [];
      
      for (const contentType of contentTypes) {
        const stats = await syncContentForUser(contentType, user, { dryRun, verbose });
        allStats.push(stats);
        userStats.push(stats);
        
        // Aggregate batch stats
        batchCreated += stats.created;
        batchDuplicates += stats.duplicatesDeleted;
        batchErrors += stats.errors.length;
      }
      
      // Show summary for this user inline only if there's activity
      const userIssues = userStats.filter(s => s.duplicatesDeleted > 0 || s.created > 0 || s.errors.length > 0);
      
      completedUsers++;
      
      if (userIssues.length > 0) {
        const summary = userIssues.map(s => {
          const parts = [];
          if (s.duplicatesDeleted > 0) parts.push(`-${s.duplicatesDeleted}`);
          if (s.created > 0) parts.push(`+${s.created}`);
          if (s.errors.length > 0) parts.push(`❌${s.errors.length}`);
          return `${s.contentType}:${parts.join(',')}`;
        }).join(' ');
        console.log(`   ${user.atproto.handle.padEnd(40)} ${summary}`);
      }
    }));
    
    // Batch summary
    const batchTime = ((Date.now() - batchStart) / 1000).toFixed(1);
    const totalTime = ((Date.now() - startTime) / 1000).toFixed(0);
    const usersPerSec = (completedUsers / (Date.now() - startTime) * 1000).toFixed(1);
    const eta = users.length > completedUsers 
      ? ((users.length - completedUsers) / usersPerSec).toFixed(0) + 's'
      : 'done';
    
    console.log(`   📊 Batch ${Math.floor(i / USER_BATCH_SIZE) + 1}: ${userBatch.length} users in ${batchTime}s | Progress: ${completedUsers}/${users.length} (${usersPerSec}/s) | Created: ${batchCreated} | Dupes: ${batchDuplicates} | Errors: ${batchErrors} | ETA: ${eta}`);
  }
  
  console.log('\n');
  
  // STEP 3: Print summary
  console.log(`\n📊 SUMMARY BY CONTENT TYPE`);
  
  for (const contentType of contentTypes) {
    const typeStats = allStats.filter(s => s.contentType === contentType);
    if (typeStats.length === 0) continue;
    
    const totalMongo = typeStats.reduce((sum, s) => sum + s.mongoTotal, 0);
    const totalAtproto = typeStats.reduce((sum, s) => sum + s.atprotoTotal, 0);
    const totalDuplicates = typeStats.reduce((sum, s) => sum + s.duplicatesDeleted, 0);
    const totalCreated = typeStats.reduce((sum, s) => sum + s.created, 0);
    const totalErrors = typeStats.reduce((sum, s) => sum + s.errors.length, 0);
    
    console.log(`\n${contentType.toUpperCase()}:`);
    console.log(`   MongoDB records: ${totalMongo}`);
    console.log(`   ATProto records: ${totalAtproto}`);
    console.log(`   Duplicates to delete: ${totalDuplicates}`);
    console.log(`   Records to create: ${totalCreated}`);
    if (totalErrors > 0) {
      console.log(`   Errors: ${totalErrors}`);
    }
    
    // Show top users with issues
    const usersWithIssues = typeStats.filter(s => s.duplicatesDeleted > 0 || s.created > 0);
    if (usersWithIssues.length > 0 && verbose) {
      console.log(`   Users with issues: ${usersWithIssues.length}`);
      usersWithIssues.slice(0, 5).forEach(s => {
        console.log(`      ${s.handle.padEnd(45)} dupes:${s.duplicatesDeleted} missing:${s.created}`);
      });
    }
  }
  
  // Overall summary
  const totalDuplicates = allStats.reduce((sum, s) => sum + s.duplicatesDeleted, 0);
  const totalCreated = allStats.reduce((sum, s) => sum + s.created, 0);
  const totalErrors = allStats.reduce((sum, s) => sum + s.errors.length, 0);
  
  console.log(`\n✨ OVERALL:`);
  console.log(`   Total duplicates to delete: ${totalDuplicates}`);
  console.log(`   Total records to create: ${totalCreated}`);
  if (totalErrors > 0) {
    console.log(`   Total errors: ${totalErrors}`);
    
    // Group errors by type
    const allErrors = allStats.flatMap(s => s.errors);
    const errorGroups = {};
    for (const error of allErrors) {
      // Extract error type from message
      const match = error.match(/: (.+?):/);
      const type = match ? match[1] : 'Unknown';
      errorGroups[type] = (errorGroups[type] || 0) + 1;
    }
    
    console.log(`\n   Error breakdown:`);
    Object.entries(errorGroups).forEach(([type, count]) => {
      console.log(`      ${type}: ${count}`);
    });
    
    // Show sample errors
    console.log(`\n   Sample errors:`);
    allErrors.slice(0, 10).forEach(err => {
      console.log(`      ${err}`);
    });
  }
  
  if (dryRun) {
    console.log(`\n💡 Run with 'live' argument to apply changes`);
  }
  
  await database.disconnect();
}

// Parse command line arguments
const args = process.argv.slice(2);
const mode = args.find(a => a === 'live' || a === 'dry-run') || 'dry-run';
const verbose = args.includes('--verbose') || args.includes('-v');
const userFilter = args.find(a => a.startsWith('--user='))?.split('=')[1];
const wipeUser = args.find(a => a.startsWith('--wipe='))?.split('=')[1];
const restoreUser = args.find(a => a.startsWith('--restore='))?.split('=')[1];

// Parse content types
let contentTypes = ['paintings', 'moods', 'pieces', 'kidlisp', 'tapes'];
const contentTypeArg = args.find(a => a.startsWith('--types='));
if (contentTypeArg) {
  contentTypes = contentTypeArg.split('=')[1].split(',');
}

// Single type shortcuts
if (args.includes('--paintings-only')) contentTypes = ['paintings'];
if (args.includes('--moods-only')) contentTypes = ['moods'];
if (args.includes('--pieces-only')) contentTypes = ['pieces'];
if (args.includes('--kidlisp-only')) contentTypes = ['kidlisp'];
if (args.includes('--tapes-only')) contentTypes = ['tapes'];

const dryRun = mode !== 'live';

// Show usage if --help
if (args.includes('--help') || args.includes('-h')) {
  console.log(`
🔄 ATProto Master Sync Script

Usage: node sync-atproto.mjs [mode] [options]

Modes:
  dry-run          Show what would be changed (default)
  live             Actually make changes

Options:
  --verbose, -v              Show detailed progress
  --user=PATTERN            Only sync users matching pattern
  --wipe=HANDLE             Wipe all ATProto data for user (e.g., --wipe=jeffrey.at.aesthetic.computer)
  --restore=HANDLE          Restore ATProto data for user from MongoDB (e.g., --restore=jeffrey.at.aesthetic.computer)
  --types=TYPE1,TYPE2       Only sync specific types (paintings,moods,pieces,kidlisp,tapes)
  --paintings-only          Only sync paintings
  --moods-only              Only sync moods
  --pieces-only             Only sync pieces
  --kidlisp-only            Only sync kidlisp
  --tapes-only              Only sync tapes
  --help, -h                Show this help

Examples:
  node sync-atproto.mjs                          # Dry run all content types
  node sync-atproto.mjs live                     # Sync all content types
  node sync-atproto.mjs live --paintings-only    # Only sync paintings
  node sync-atproto.mjs live --user=jeffrey      # Only sync jeffrey's content
  node sync-atproto.mjs live --types=paintings,moods  # Only paintings and moods
  node sync-atproto.mjs dry-run --verbose        # See detailed dry run
  
  # Wipe examples
  node sync-atproto.mjs --wipe=jeffrey.at.aesthetic.computer  # Dry run wipe
  node sync-atproto.mjs live --wipe=jeffrey.at.aesthetic.computer  # Wipe jeffrey's ATProto data
  
  # Restore examples
  node sync-atproto.mjs --restore=jeffrey.at.aesthetic.computer  # Dry run restore
  node sync-atproto.mjs live --restore=jeffrey.at.aesthetic.computer  # Restore all content
  node sync-atproto.mjs live --restore=jeffrey.at.aesthetic.computer --paintings-only  # Only restore paintings
  node sync-atproto.mjs live --restore=jeffrey.at.aesthetic.computer --types=paintings,moods  # Only paintings and moods
`);
  process.exit(0);
}

// Handle wipe mode
if (wipeUser) {
  wipeUserAtprotoData(wipeUser, { dryRun, contentTypes })
    .then(() => {
      console.log(`\n✅ Done!\n`);
      process.exit(0);
    })
    .catch(error => {
      console.error('\n❌ Fatal error:', error);
      process.exit(1);
    });
} else if (restoreUser) {
  // Handle restore mode
  restoreUserAtprotoData(restoreUser, { dryRun, contentTypes })
    .then(() => {
      console.log(`\n✅ Done!\n`);
      process.exit(0);
    })
    .catch(error => {
      console.error('\n❌ Fatal error:', error);
      process.exit(1);
    });
} else {
  syncAll({ dryRun, verbose, userFilter, contentTypes })
    .then(() => {
      console.log(`\n✅ Done!\n`);
      process.exit(0);
    })
    .catch(error => {
      console.error('\n❌ Fatal error:', error);
      process.exit(1);
    });
}
